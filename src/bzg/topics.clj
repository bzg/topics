#!/usr/bin/env bb

;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: EPL-2.0.txt

;; Topics generates a static HTML/CSS/JS site from topics data.
;;
;; Usage:
;;   topics faq.json
;;   topics -i https://code.gouv.fr/data/faq.json
;;   topics -n faq.json              # ignore categories (flat list)
;;
;; Run topics -h for options.
;;
;; JSON format:
;;
;; [
;;   {
;;     "title": "Topic title",
;;     "content": "<p>HTML</p>",
;;     "category": "Category name"   // optional
;;   }
;; ]
;;

(ns bzg.topics
  (:require [cheshire.core :as json]
            [babashka.cli :as cli]
            [clj-yaml.core :as yaml]
            [clojure.edn :as edn]
            [babashka.fs :as fs]
            [babashka.curl :as curl]
            [clojure.string :as str]))

(def defaults
  {:format  "auto"
   :lang    "en"
   :title   "Topics"
   :tagline "Topics to explore"
   :footer  "<a href=\"https://codeberg.org/bzg/topics\">Topics</a>"})

(def cli-options
  {:input-file    {:alias :i :desc "Path or URL to input file (JSON, EDN, or YAML)" :ref "<file|url>"}
   :format        {:alias :f :desc "Format of topics file (json, edn, yaml, or auto)" :ref "<string>"
                   :validate #(contains? #{"auto" "json" "edn" "yaml"} %)}
   :lang          {:alias :l :desc "HTML lang attribute (e.g. en, fr, de)" :ref "<string>"}
   :no-categories {:alias :n :desc "Ignore categories (flat list of topics)" :type :boolean}
   :title         {:alias :T :desc "Website title" :ref "<string>"}
   :tagline       {:alias :L :desc "Website tagline" :ref "<string>"}
   :footer        {:alias :F :desc "Footer HTML" :ref "<string>"}
   :source        {:alias :s :desc "URL to display as content source" :ref "<url>"}
   :css           {:alias :C :desc "Custom CSS file to include (overrides default styles)" :ref "<file>"}
   :config        {:alias :c :desc "Path to configuration file (EDN format)" :ref "<file>"}
   :verbose       {:alias :v :desc "Enable verbose output" :type :boolean}
   :help          {:alias :h :desc "Show help" :type :boolean}})

(def ui-strings
  {:fr {:search-placeholder  "Rechercher"
        :clear-search        "Effacer la recherche"
        :no-search-results   "Aucun résultat ne correspond à votre recherche. Essayez avec d'autres termes."
        :no-category-results "Aucun résultat trouvé dans cette catégorie."
        :topics-count        "sujets"
        :content-source      "Source des contenus"
        :all-categories      "Toutes les catégories"
        :view-all-flat       "Voir la liste complète"
        :view-by-category    "Voir par catégorie"}
   :de {:search-placeholder  "Suchen"
        :clear-search        "Suche löschen"
        :no-search-results   "Keine Ergebnisse gefunden. Versuchen Sie es mit anderen Begriffen."
        :no-category-results "Keine Ergebnisse in dieser Kategorie gefunden."
        :topics-count        "Themen"
        :content-source      "Inhaltsquelle"
        :all-categories      "Alle Kategorien"
        :view-all-flat       "Vollständige Liste anzeigen"
        :view-by-category    "Nach Kategorie anzeigen"}
   :en {:search-placeholder  "Search"
        :clear-search        "Clear search"
        :no-search-results   "No results match your search. Try with other terms."
        :no-category-results "No results found in this category."
        :topics-count        "topics"
        :content-source      "Content source"
        :all-categories      "All categories"
        :view-all-flat       "View full list"
        :view-by-category    "View by category"}})

(def config-keys [:title :tagline :footer :source :css :lang :verbose])

(defn log [verbose & args] (when verbose (apply println args)))

;; Keep only maps with :title (category is optional)
(defn valid-topics [topics-data]
  (filter #(and (map? %) (:title %)) topics-data))

(defn detect-format [source format-option]
  (if (= format-option "auto")
    (cond
      (re-find #"(?i)\.edn$" source)        :edn
      (re-find #"(?i)\.(yaml|yml)$" source) :yaml
      :else                                 :json)
    (let [fmt (keyword format-option)]
      (or (#{:edn :yaml :json} fmt)
          (throw (ex-info (str "Unsupported format: " format-option)
                          {:format format-option}))))))

(defn http-url? [s]
  (or (str/starts-with? s "http://")
      (str/starts-with? s "https://")))

;; Forward declarations for AST handling functions (defined later)
(declare ast? flatten-ast-to-topics find-max-level number-ast-sections)

(defn load-topics-data [source format-option verbose]
  (try
    (log verbose "Loading Topics data from" source)
    (let [content (if (http-url? source)
                    (do
                      (log verbose "Detected HTTP(S) URL, using babashka.curl")
                      (let [{:keys [status body] :as resp} (curl/get source {:throw false})]
                        (when (or (nil? status) (>= status 400))
                          (throw (ex-info (str "HTTP error " status " when fetching " source)
                                          {:status status :response resp})))
                        body))
                    (slurp source))
          format  (detect-format source format-option)
          _       (log verbose "Using format:" (name format))
          parsed  (case format
                    :edn  (edn/read-string content)
                    :yaml (yaml/parse-string content)
                    (json/parse-string content true))]
      ;; Check if parsed data is an org-parse AST
      (if (ast? parsed)
        (let [numbered  (number-ast-sections parsed)
              max-level (find-max-level numbered)
              _         (log verbose "Detected org-parse AST, auto-detected max level:" max-level)
              flattened (flatten-ast-to-topics numbered max-level)]
          (log verbose "Flattened AST into" (count flattened) "topics")
          {:ok flattened})
        ;; Regular topics data
        (let [data  (cond
                      (sequential? parsed) parsed
                      (map? parsed)        [parsed]
                      :else (throw (ex-info "Topics data must be a list or map at the top level"
                                            {:parsed-type (type parsed)})))
              valid (valid-topics data)]
          (log verbose "Loaded" (count valid) "topics (filtered"
               (- (count data) (count valid)) "category headers or invalid entries)")
          {:ok valid})))
    (catch Exception e
      {:error (str "Error loading Topics data from " source ": " (.getMessage e))})))

(defn load-config-file [config-path verbose]
  (try
    (log verbose "Loading configuration from file:" config-path)
    (let [config-data (edn/read-string (slurp config-path))]
      (log verbose "Configuration loaded successfully")
      {:ok config-data})
    (catch Exception e
      {:error (str "Failed to load configuration file " config-path ": " (.getMessage e))})))

;; Normalize topics into a common structure with optional categories.
(defn categorize-topics [topics-data no-categories?]
  (->> topics-data
       (map (fn [{:keys [title content category custom_id]}]
              (cond-> {:title    (or title "")
                       :content  (or content "")
                       :category (when-not no-categories? category)}
                custom_id (assoc :custom_id custom_id))))))

;; AST Flattening (for org-parse AST input)
;;
;; These functions transform an org-parse AST into the topics format:
;; [{:title "..." :content "..." :category "..."} ...]

(defn section? [node] (= (:type node) "section"))

(defn render-node-for-topics
  "Render an AST node to HTML for topics content."
  [node]
  (case (:type node)
    "paragraph" (str "<p>" (:content node) "</p>")
    "list" (let [items (:items node)]
            (if (:description node)
              (str "<dl>"
                   (str/join "" (map render-node-for-topics items))
                   "</dl>")
              (let [tag (if (:ordered node) "ol" "ul")]
                (str "<" tag ">"
                     (str/join "" (map render-node-for-topics items))
                     "</" tag ">"))))
    "list-item" (if (:term node)
                  (str "<dt>" (:term node) "</dt>"
                       "<dd>" (or (:definition node) "")
                       (when (seq (:children node))
                         (str/join "" (map render-node-for-topics (:children node))))
                       "</dd>")
                  (str "<li>" (:content node)
                       (when (seq (:children node))
                         (str/join "" (map render-node-for-topics (:children node))))
                       "</li>"))
    "table" (let [rows (:rows node)
                 has-header (:has-header node)]
             (if (empty? rows) ""
                 (str "<table>"
                      (when has-header
                        (str "<thead><tr>"
                             (str/join "" (map #(str "<th>" % "</th>") (first rows)))
                             "</tr></thead>"))
                      "<tbody>"
                      (str/join ""
                                (map (fn [row]
                                       (str "<tr>"
                                            (str/join "" (map #(str "<td>" % "</td>") row))
                                            "</tr>"))
                                     (if has-header (rest rows) rows)))
                      "</tbody></table>")))
    "src-block" (str "<pre><code>" (:content node) "</code></pre>")
    "quote-block" (str "<blockquote><p>" (str/replace (:content node) #"\n\n+" "</p><p>") "</p></blockquote>")
    "fixed-width" (str "<pre>" (:content node) "</pre>")
    "footnote-def" (str "<div class=\"footnote\"><sup>" (:label node) "</sup> " (:content node) "</div>")
    "section" (let [h (min (:level node) 6)]
                (str "<section><h" h ">" (:title node) "</h" h ">"
                     (str/join "" (map render-node-for-topics (:children node)))
                     "</section>"))
    "block" (if (and (= (:block-type node) "export")
                     (= (:args node) "html"))
              (:content node)
              "") ;; Ignore non-html export blocks and other blocks
    "html-line" (str "<p>" (:content node) "</p>")
    "latex-line" "" ;; Ignore latex lines
    "comment" ""
    "property-drawer" ""
    ""))

(defn render-section-content-up-to-level
  "Render the children of a section as HTML, excluding subsections at or above target-level."
  [section target-level]
  (->> (:children section)
       (filter #(or (not (section? %))
                    (> (:level %) target-level)))
       (map render-node-for-topics)
       (str/join "")))

(defn get-property
  "Extract a property value from an org-parse section's properties list.
   Properties are stored as [[key value] [key value] ...]."
  [node prop-name]
  (when-let [props (:properties node)]
    (->> props
         (filter #(= (first %) prop-name))
         first
         second)))

(defn parse-options-string
  "Parse an #+OPTIONS: value string like 'toc:t H:2 num:t' into a map."
  [s]
  (when (and s (not (str/blank? s)))
    (into {}
          (for [[_ k v] (re-seq #"(\S+):(\S+)" s)]
            [(keyword (str/lower-case k))
             (case v
               "t" true
               "nil" false
               (try (Integer/parseInt v) (catch Exception _ v)))]))))

(defn number-ast-sections
  "Walk an org-parse AST (with string types from JSON) and annotate each section
   node with a :section-number string (e.g. '1', '1.1') when num:t is set."
  [ast]
  (let [options (parse-options-string (get-in ast [:meta :options]))
        num? (get options :num false)]
    (if-not num?
      ast
      (letfn [(number-children [children counters]
                (loop [[child & more] children
                       counters counters
                       result []]
                  (if (nil? child)
                    result
                    (if (section? child)
                      (let [level (:level child)
                            updated (-> counters
                                        (update level (fnil inc 0))
                                        (#(reduce (fn [m k] (dissoc m k))
                                                  %
                                                  (filter (fn [k] (> k level)) (keys %)))))
                            sec-num (str/join "." (map #(get updated % 1)
                                                       (range 1 (inc level))))
                            numbered-kids (number-children (:children child) updated)
                            numbered-child (assoc child
                                                  :section-number sec-num
                                                  :children numbered-kids)]
                        (recur more updated (conj result numbered-child)))
                      (recur more counters (conj result child))))))]
        (assoc ast :children (number-children (:children ast) {}))))))

(defn flatten-ast-to-topics
  "Flatten an org-parse AST to extract sections at exactly the target level.
   Returns a vector of maps with :title, :content (HTML), :category, and optionally :custom_id.
   The :category is the penultimate element of the path (parent section title).
   The :custom_id is extracted from the section's properties if present.
   This is compatible with the topics data format."
  [ast target-level]
  (letfn [(collect-sections [node current-path]
            (case (:type node)
              "document"
              (mapcat #(collect-sections % current-path) (:children node))

              "section"
              (let [new-path (conj current-path (:title node))]
                (if (= (:level node) target-level)
                  ;; At target level: emit this section with category = penultimate path element
                  (let [content   (render-section-content-up-to-level node target-level)
                        category  (-> new-path butlast last)
                        custom-id (get-property node "custom_id")
                        sec-num   (:section-number node)
                        title     (if sec-num
                                    (str sec-num " " (:title node))
                                    (:title node))]
                    [(cond-> {:title    title
                              :content  content
                              :category category}
                       custom-id (assoc :custom_id custom-id))])
                  ;; Not at target level: recurse into children
                  (if (< (:level node) target-level)
                    (mapcat #(collect-sections % new-path) (filter section? (:children node)))
                    ;; Beyond target level: ignore
                    [])))

              []))]
    (vec (collect-sections ast []))))

(defn ast?
  "Check if data looks like an org-parse AST (has :type 'document')."
  [data]
  (and (map? data) (= (:type data) "document")))

(defn find-max-level
  "Find the maximum section level in an AST."
  [ast]
  (letfn [(max-level [node]
            (let [current (if (= (:type node) "section") (:level node) 0)
                  children-max (reduce max 0 (map max-level (:children node)))]
              (max current children-max)))]
    (max-level ast)))

(defn topics-to-js-array [topics-data no-categories?]
  (-> topics-data
      (categorize-topics no-categories?)
      json/generate-string
      ;; Prevent script injection via JSON payload
      (str/replace "</" "<\\/")))

(def css-styles "
.visually-hidden { position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0,0,0,0); white-space: nowrap; border: 0; }
.grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 1.5rem; }
.card { padding: 1.5rem; border-radius: var(--pico-border-radius); border: 1px solid var(--pico-muted-border-color); transition: transform .2s, box-shadow .2s; }
.card:hover { transform: translateY(-4px); box-shadow: 0 8px 16px rgba(0,0,0,.1); text-decoration: none; }
.card h2 { margin-bottom: .5rem; font-size: 1.4rem; }
.card p { margin: 0; color: var(--pico-muted-color); }
.search-row { display: flex; gap: .5rem; align-items: center; margin-bottom: 2rem; }
.search-row input { flex: 1; margin: 0; }
.back-link { display: inline; }
.back-link::before { content: '← '; }
details { border: 1px solid var(--pico-muted-border-color); border-radius: var(--pico-border-radius); padding: 1rem; margin-bottom: 1rem; }
details summary { font-weight: 600; cursor: pointer; font-size: 1.2rem; }
details[open] summary { margin-bottom: .75rem; }
.permalink { margin-left: .5rem; text-decoration: none; opacity: 0; transition: opacity .2s; font-size: .85em; }
details summary:hover .permalink { opacity: .6; }
.permalink:hover { opacity: 1 !important; }
.hidden { display: none !important; }
.view-toggle { margin-bottom: 1.5rem; }
footer { text-align: center; font-size: .85rem; margin-top: 3rem; }
.noscript-content .category-section { margin-bottom: 2rem; }
.noscript-content .category-section h2 { border-bottom: 1px solid var(--pico-muted-border-color); padding-bottom: .5rem; margin-bottom: 1rem; }
.noscript-content article { border: 1px solid var(--pico-muted-border-color); border-radius: var(--pico-border-radius); padding: 1rem; margin-bottom: 1rem; }
.noscript-content article h2 { margin-top: 0; margin-bottom: .75rem; }
table { margin-bottom: 2rem; }")

;; Maps Clojure ui-strings keys to camelCase JS property names.
(def js-key-mapping
  {:no-search-results   :noSearchResults
   :no-category-results :noCategoryResults
   :topics-count        :topicsCount
   :all-categories      :allCategories
   :view-all-flat       :viewAllFlat
   :view-by-category    :viewByCategory
   :search-placeholder  :searchPlaceholder
   :clear-search        :clearSearch})

(defn ui-strings-for-js [ui-strings]
  (into {} (map (fn [[lang m]]
                  [lang (into {} (map (fn [[clj-key js-key]]
                                        [js-key (get m clj-key)])
                                      js-key-mapping))])
                ui-strings)))

(defn generate-js [topics-data ui-strings no-categories?]
  (let [all-strings-json (json/generate-string (ui-strings-for-js ui-strings))]
    (str "(function() {
  'use strict';
  const topicsData = " (topics-to-js-array topics-data no-categories?) ";
  const allStrings = " all-strings-json ";

  // Detect browser language and select appropriate strings
  function detectLanguage() {
    const lang = (navigator.language || navigator.userLanguage || 'en').toLowerCase();
    if (lang.startsWith('fr')) return 'fr';
    if (lang.startsWith('de')) return 'de';
    return 'en';
  }
  const strings = allStrings[detectLanguage()];

  let currentCategory = null;
  let currentSearch = '';
  let viewMode = 'categories'; // 'categories' or 'flat'
  const contentDiv = document.getElementById('topics-content');
  const homeLink = document.getElementById('home-link');

  // Create search row dynamically (won't exist for non-JS browsers)
  const searchRow = document.createElement('div');
  searchRow.className = 'search-row';
  searchRow.setAttribute('role', 'search');
  searchRow.innerHTML = '<label for=\"search-input\" class=\"visually-hidden\">' + strings.searchPlaceholder + '</label>' +
    '<input placeholder=\"' + strings.searchPlaceholder + '\" type=\"search\" id=\"search-input\" name=\"q\">' +
    '<button type=\"button\" class=\"secondary outline hidden\" id=\"clear-search\" aria-label=\"' + strings.clearSearch + '\">✕</button>';
  contentDiv.parentNode.insertBefore(searchRow, contentDiv);

  const searchInput = document.getElementById('search-input');
  const clearButton = document.getElementById('clear-search');

  function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
  }

  function normalizeText(text) {
    return String(text || '')
      .toLowerCase()
      .normalize('NFD')
      .replace(/[\\u0300-\\u036f]/g, '')
      .replace(/['’]/g, \"'\");
  }

  // Keep in sync with the Clojure slugify function.
  function slugify(text) {
    return String(text || '')
      .toLowerCase()
      .normalize('NFD')
      .replace(/[\\u0300-\\u036f]/g, '')
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-|-$/g, '');
  }

  function getTopicId(topic) {
    if (topic.custom_id) {
      // Sanitize custom_id: replace special chars and spaces with dashes
      return String(topic.custom_id)
        .replace(/[^a-zA-Z0-9_-]+/g, '-')
        .replace(/^-|-$/g, '');
    }
    return slugify(topic.title);
  }

  function getCategories() {
    const cats = {};
    topicsData.forEach(t => {
      const cat = (t.category == null || t.category === '') ? strings.allCategories : t.category;
      cats[cat] = (cats[cat] || 0) + 1;
    });
    return Object.entries(cats)
      .map(([name, count]) => ({ name, count }))
      .sort((a, b) => a.name.localeCompare(b.name));
  }

  function isSinglePseudoCategory(categories) {
    return categories.length === 1 && categories[0].name === strings.allCategories;
  }

  function hasRealCategories() {
    const categories = getCategories();
    return !isSinglePseudoCategory(categories);
  }

  function searchTopics(query) {
    const normalized = normalizeText(query);
    return topicsData.filter(t => {
      const title = normalizeText(t.title);
      const content = normalizeText(t.content);
      return title.includes(normalized) || content.includes(normalized);
    });
  }

  function getTopicsByCategory(category) {
    // In flat mode, all topics have category = null and we use a single
    // pseudo-category name (strings.allCategories) in the UI.
    if (!topicsData.length) return [];
    const anyCat = topicsData.find(t => t.category != null && t.category !== null && t.category !== '');
    if (!anyCat) {
      // flat mode: any category selection shows all topics
      return topicsData.slice();
    }
    return topicsData.filter(t => t.category === category);
  }

  function renderCategoriesGrid() {
    const categories = getCategories();
    let html = '<div class=\"view-toggle\"><span>' + topicsData.length + ' ' + strings.topicsCount + '</span> · <a href=\"#\" id=\"toggle-view\">' + strings.viewAllFlat + '</a></div>';
    html += '<nav class=\"grid\">';
    categories.forEach(cat => {
      html += `<a href=\"#\" class=\"card\" data-category=\"${escapeHtml(cat.name)}\">
        <h2>${escapeHtml(cat.name)}</h2>
        <p>${cat.count} ${strings.topicsCount}</p>
      </a>`;
    });
    return html + '</nav>';
  }

  function renderFlatList() {
    let html = '<div class=\"view-toggle\"><span>' + topicsData.length + ' ' + strings.topicsCount + '</span> · <a href=\"#\" id=\"toggle-view\">' + strings.viewByCategory + '</a></div>';
    topicsData.forEach(t => {
      const id = getTopicId(t);
      html += `<details id=\"${id}\">
        <summary>${t.title}<a href=\"#${id}\" class=\"permalink\" title=\"Permalink\">🔗</a></summary>
        <div>${t.content}</div>
      </details>`;
    });
    return html;
  }

  function renderTopicsList(topics, showBackLink = false) {
    if (topics.length === 0) {
      const msg = currentSearch ? strings.noSearchResults : strings.noCategoryResults;
      return `<p><em>${msg}</em></p>`;
    }
    let html = '';
    if (showBackLink || currentSearch) {
      html += '<div class=\"view-toggle\">';
      html += '<span>' + topics.length + ' ' + strings.topicsCount + '</span>';
      if (showBackLink) {
        html += ' · <a href=\"#\" class=\"back-link\" id=\"back-to-categories\">' + strings.allCategories + '</a>';
      }
      html += '</div>';
    }
    topics.forEach(t => {
      const id = getTopicId(t);
      html += `<details id=\"${id}\">
        <summary>${t.title}<a href=\"#${id}\" class=\"permalink\" title=\"Permalink\">🔗</a></summary>
        <div>${t.content}</div>
      </details>`;
    });
    return html;
  }

  function setupAriaExpanded() {
    document.querySelectorAll('details').forEach(details => {
      const summary = details.querySelector('summary');
      if (summary) {
        summary.setAttribute('aria-expanded', details.open ? 'true' : 'false');
        details.addEventListener('toggle', () => {
          summary.setAttribute('aria-expanded', details.open ? 'true' : 'false');
        });
      }
    });
  }

  function render() {
    let html;
    const categories = getCategories();

    // If URL has an anchor, switch to flat view so the target element exists
    if (window.location.hash && !currentSearch && !currentCategory
        && viewMode === 'categories' && hasRealCategories()) {
      viewMode = 'flat';
    }

    if (currentSearch) {
      html = renderTopicsList(searchTopics(currentSearch), false);
    } else if (currentCategory) {
      html = renderTopicsList(getTopicsByCategory(currentCategory), true);
    } else if (isSinglePseudoCategory(categories)) {
      // Only one pseudo-category (flat data): show all topics directly
      html = renderTopicsList(topicsData, false);
    } else if (viewMode === 'flat') {
      html = renderFlatList();
    } else {
      html = renderCategoriesGrid();
    }

    contentDiv.innerHTML = html;
    setupAriaExpanded();
    openAndScrollToHash();

    // Handle view toggle click
    const toggleLink = document.getElementById('toggle-view');
    if (toggleLink) {
      toggleLink.addEventListener('click', (e) => {
        e.preventDefault();
        viewMode = viewMode === 'categories' ? 'flat' : 'categories';
        render();
      });
    }

    if (currentCategory && !currentSearch) {
      const backLink = document.getElementById('back-to-categories');
      if (backLink) {
        backLink.addEventListener('click', (e) => {
          e.preventDefault();
          currentCategory = null;
          updateUrl();
          render();
        });
      }
    }

    document.querySelectorAll('[data-category]').forEach(el => {
      el.addEventListener('click', (e) => {
        e.preventDefault();
        currentCategory = el.dataset.category;
        currentSearch = '';
        searchInput.value = '';
        clearButton.classList.add('hidden');
        updateUrl();
        render();
      });
    });
  }

  function updateUrl() {
    const params = new URLSearchParams();
    if (currentSearch) params.set('q', currentSearch);
    else if (currentCategory) params.set('category', currentCategory);
    const newUrl = params.toString() ? window.location.pathname + '?' + params.toString() : window.location.pathname;
    history.pushState({}, '', newUrl);
  }

  function parseUrl() {
    const params = new URLSearchParams(window.location.search);
    currentSearch = params.get('q') || '';
    currentCategory = params.get('category') || null;
    if (currentSearch) {
      searchInput.value = currentSearch;
      clearButton.classList.remove('hidden');
    }
  }

  function openAndScrollToHash() {
    const hash = window.location.hash.slice(1);
    if (!hash) return;
    const el = document.getElementById(hash);
    if (el && el.tagName === 'DETAILS') {
      el.open = true;
      const summary = el.querySelector('summary');
      if (summary) summary.setAttribute('aria-expanded', 'true');
      el.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }
  }

  let searchTimeout;
  searchInput.addEventListener('input', (e) => {
    clearTimeout(searchTimeout);
    const value = e.target.value.trim();
    clearButton.classList.toggle('hidden', !value);
    searchTimeout = setTimeout(() => {
      currentSearch = value;
      if (value) currentCategory = null;
      updateUrl();
      render();
    }, 300);
  });

  clearButton.addEventListener('click', () => {
    searchInput.value = '';
    currentSearch = '';
    clearButton.classList.add('hidden');
    updateUrl();
    render();
    searchInput.focus();
  });

  homeLink.addEventListener('click', (e) => {
    e.preventDefault();
    currentSearch = '';
    currentCategory = null;
    searchInput.value = '';
    clearButton.classList.add('hidden');
    updateUrl();
    render();
  });

  window.addEventListener('popstate', () => { parseUrl(); render(); });
  window.addEventListener('hashchange', () => { render(); });
  parseUrl();
  render();
})();")))

(defn ui-str
  "Resolve a UI string key using the configured language, falling back to English."
  [config k]
  (let [lang (keyword (:lang config))]
    (get-in ui-strings [lang k] (get-in ui-strings [:en k]))))

(defn html-escape [s]
  (-> (or s "")
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&#39;")))

;; Keep in sync with the JS slugify in generate-js.
(defn slugify [text]
  (-> (or text "")
      str/lower-case
      (java.text.Normalizer/normalize java.text.Normalizer$Form/NFD)
      (str/replace #"[\u0300-\u036f]" "")
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-|-$" "")))

(defn get-topic-id
  "Return custom_id (sanitized) if present, otherwise slugify the title."
  [{:keys [title custom_id]}]
  (if custom_id
    (-> (str custom_id)
        (str/replace #"[^a-zA-Z0-9_-]+" "-")
        (str/replace #"^-|-$" ""))
    (slugify title)))

(defn generate-head [config css-file]
  (let [css-name (when css-file (str (fs/file-name css-file)))]
    (str "<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>" (html-escape (:title config)) "</title>
  <link rel=\"icon\" href=\"data:image/png;base64,iVBORw0KGgo=\">
  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css\">
  <style>" css-styles "</style>"
         (when css-name (str "\n  <link rel=\"stylesheet\" href=\"" css-name "\">")) "
</head>")))

(defn generate-header [config]
  (str "<header class=\"container\">
    <h1><a href=\"./\" id=\"home-link\">" (html-escape (:title config)) "</a></h1>
    <p>" (html-escape (:tagline config)) "</p>
  </header>"))

(defn generate-noscript-content
  "Generate static HTML content for browsers without JavaScript.
   Shows all topics as sections, grouped by category if categories exist.
   Uses the configured language for UI strings, falling back to English."
  [config topics-data no-categories?]
  (let [topics    (categorize-topics topics-data no-categories?)
        by-cat    (group-by :category topics)
        cats      (sort (keys by-cat))
        all-cat   (ui-str config :all-categories)
        ;; Check if we have real categories or just nil/empty
        has-cats? (some #(and % (not= % "")) cats)]
    (str "<noscript><div class=\"noscript-content\">"
         (if (and has-cats? (not no-categories?))
           ;; With categories: group topics under category headings
           (str/join "\n"
                     (for [cat cats
                           :let [cat-name (if (or (nil? cat) (= cat "")) all-cat cat)
                                 cat-topics (get by-cat cat)]]
                       (str "<section class=\"category-section\">"
                            "<h2>" (html-escape cat-name) "</h2>"
                            (str/join "\n"
                                      (for [topic cat-topics
                                            :let [id (get-topic-id topic)]]
                                        (str "<article id=\"" id "\">"
                                             "<h2>" (:title topic) "</h2>"
                                             "<div>" (:content topic) "</div>"
                                             "</article>")))
                            "</section>")))
           ;; Without categories: flat list of all topics
           (str/join "\n"
                     (for [topic topics
                           :let [id (get-topic-id topic)]]
                       (str "<article id=\"" id "\">"
                            "<h2>" (:title topic) "</h2>"
                            "<div>" (:content topic) "</div>"
                            "</article>"))))
         "</div></noscript>")))

(defn generate-main [config topics-data no-categories?]
  (str "<main class=\"container\" id=\"main-content\" tabindex=\"-1\">
    " (generate-noscript-content config topics-data no-categories?) "
    <div id=\"topics-content\" aria-live=\"polite\"></div>
  </main>"))

(defn generate-footer [config]
  (str "<footer class=\"container\">
    <p>" (when-let [src (:source config)]
           (str "<a target=\"_blank\" href=\"" (html-escape src) "\">" (ui-str config :content-source) "</a> · "))
       (:footer config) "</p>
  </footer>"))

(defn generate-html [config topics-data no-categories?]
  (let [css-file (:css config)]
    (str "<!DOCTYPE html>
<html lang=\"" (html-escape (:lang config)) "\">
" (generate-head config css-file) "
<body>
  " (generate-header config) "
  " (generate-main config topics-data no-categories?) "
  " (generate-footer config) "
  <script>" (generate-js topics-data ui-strings no-categories?) "</script>
</body>
</html>")))

(defn generate-site [opts]
  (let [verbose       (:verbose opts)
        no-categories (:no-categories opts)
        file-config   (when-let [config-path (:config opts)]
                        (let [result (load-config-file config-path verbose)]
                          (if (:error result)
                            (do (println (:error result)) (System/exit 1))
                            (:ok result))))
        ;; Only allow known config keys from file
        file-config   (some-> file-config (select-keys config-keys))
        ;; Correct merge order: Defaults -> File Config -> CLI Arguments
        config        (merge defaults file-config (select-keys opts config-keys))
        topics-result (load-topics-data (:input-file opts) (:format config) verbose)]
    (when (:error topics-result)
      (println (:error topics-result))
      (System/exit 1))
    (let [topics-data (:ok topics-result)]
      (when-let [css-file (:css config)]
        (let [target (str (fs/file-name css-file))]
          (if (fs/exists? css-file)
            (when-not (and (fs/exists? target) (fs/same-file? css-file target))
              (fs/copy css-file target {:replace-existing true})
              (log verbose "Copied:" css-file "->" target))
            (log verbose "CSS file not found:" css-file))))
      (spit "index.html" (generate-html config topics-data no-categories))
      (println "Generated: index.html"))))

(defn show-help []
  (println "Usage: topics [options] -i <file|url>")
  (println "\nGenerates a static HTML/CSS/JS site from topics data.\n\nOptions:")
  (println (cli/format-opts {:spec cli-options})))

(defn -main [& args]
  (try
    (let [{:keys [args opts]} (cli/parse-args args {:spec cli-options})
          ;; Auto-detect input file, config, and CSS
          opts (cond-> opts
                 (and (not (:input-file opts)) (seq args))
                 (assoc :input-file (first args))
                 (and (not (:config opts)) (fs/exists? "config.edn"))
                 (assoc :config "config.edn")
                 (and (not (:css opts)) (fs/exists? "custom.css"))
                 (assoc :css "custom.css"))]
      (when (:help opts) (show-help) (System/exit 0))
      (when (:verbose opts)
        (when (and (not (some #{"-c" "--config"} args))
                   (:config opts))
          (println "Auto-detected config file: config.edn"))
        (when (and (not (some #{"-C" "--css"} args))
                   (:css opts))
          (println "Auto-detected CSS file: custom.css")))
      (when-not (:input-file opts)
        (println "Error: topics file or URL is required\n")
        (show-help)
        (System/exit 1))
      (generate-site opts))
    (catch Exception e
      (println "ERROR:" (.getMessage e))
      (System/exit 1))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
