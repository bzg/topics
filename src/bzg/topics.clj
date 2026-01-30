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
;; Configuration file (EDN format, use with -c option):
;;
;; {:title   "My FAQ"
;;  :tagline "Frequently asked questions"
;;  :footer  "<a href=\"https://example.com\">My site</a>"
;;  :source  "https://example.com/data/faq.json"
;;  :lang    "fr"
;;  :css     "custom.css"}
;;
;; Configuration keys:
;;   :title   - Website title (default: "Topics")
;;   :tagline - Website tagline (default: "Topics to explore")
;;   :footer  - Footer HTML
;;   :source  - URL displayed as content source
;;   :lang    - Language: "en" or "fr" (default: "en")
;;   :css     - Custom CSS file to include

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
   :title   "Topics"
   :tagline "Topics to explore"
   :footer  "<a href=\"https://codeberg.org/bzg/topics\">Topics</a>"
   :lang    "en"})

(def cli-options
  {:input-file    {:alias :i :desc "Path or URL to input file (JSON, EDN, or YAML)" :ref "<file|url>"}
   :format        {:alias :f :desc "Format of topics file (json, edn, yaml, or auto)" :ref "<string>"
                   :validate #(contains? #{"auto" "json" "edn" "yaml"} %)}
   :no-categories {:alias :n :desc "Ignore categories (flat list of topics)" :type :boolean}
   :title         {:alias :T :desc "Website title" :ref "<string>"}
   :tagline       {:alias :L :desc "Website tagline" :ref "<string>"}
   :footer        {:alias :F :desc "Footer HTML" :ref "<string>"}
   :source        {:alias :s :desc "URL to display as content source" :ref "<url>"}
   :lang          {:alias :g :desc "Language: en or fr" :ref "<string>"}
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
        :skip-to-content     "Passer au contenu"
        :all-categories      "Toutes les catégories"
        :lang                "fr"}
   :en {:search-placeholder  "Search"
        :clear-search        "Clear search"
        :no-search-results   "No results match your search. Try with other terms."
        :no-category-results "No results found in this category."
        :topics-count        "topics"
        :content-source      "Content source"
        :skip-to-content     "Skip to content"
        :all-categories      "All categories"
        :lang                "en"}})

(def config-keys [:title :tagline :footer :source :lang :css :verbose])

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
      (if (#{:edn :yaml :json} fmt)
        fmt
        (throw (ex-info (str "Unsupported format: " format-option)
                        {:format format-option}))))))

(defn http-url? [s]
  (boolean (re-find #"^https?://" s)))

;; Forward declarations for AST handling functions (defined later)
(declare ast? flatten-ast-to-topics find-max-level)

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
        (let [max-level (find-max-level parsed)
              _         (log verbose "Detected org-parse AST, auto-detected max level:" max-level)
              flattened (flatten-ast-to-topics parsed max-level)]
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
       (map (fn [{:keys [title content category]}]
              {:title    (or title "")
               :content  (str (or content ""))
               :category (when-not no-categories? category)}))))

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
    "list" (let [tag (if (:ordered node) "ol" "ul")]
            (str "<" tag ">"
                 (str/join "" (map render-node-for-topics (:items node)))
                 "</" tag ">"))
    "list-item" (str "<li>" (:content node)
                    (when (seq (:children node))
                      (str/join "" (map render-node-for-topics (:children node))))
                    "</li>")
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
    "block" (str "<div class=\"block\">" (:content node) "</div>")
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

(defn flatten-ast-to-topics
  "Flatten an org-parse AST to extract sections at exactly the target level.
   Returns a vector of maps with :title, :content (HTML), and :category.
   The :category is the penultimate element of the path (parent section title).
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
                  (let [content  (render-section-content-up-to-level node target-level)
                        category (-> new-path butlast last)]
                    [{:title (:title node)
                      :content content
                      :category category}])
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
.skip-link { position: absolute; top: -40px; left: 0; background: var(--pico-primary); color: #fff; padding: .5rem; z-index: 100; }
.skip-link:focus { top: 0; }
.grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 1.5rem; }
.card { padding: 1.5rem; border-radius: var(--pico-border-radius); border: 1px solid var(--pico-muted-border-color); transition: transform .2s, box-shadow .2s; }
.card:hover { transform: translateY(-4px); box-shadow: 0 8px 16px rgba(0,0,0,.1); text-decoration: none; }
.card h3 { margin-bottom: .5rem; }
.card p { margin: 0; color: var(--pico-muted-color); }
.search-row { display: flex; gap: .5rem; align-items: center; margin-bottom: 2rem; }
.search-row input { flex: 1; margin: 0; }
.back-link { display: inline-block; margin-bottom: 1.5rem; }
.back-link::before { content: '← '; }
details { border: 1px solid var(--pico-muted-border-color); border-radius: var(--pico-border-radius); padding: 1rem; margin-bottom: 1rem; }
details summary { font-weight: 600; cursor: pointer; }
details[open] summary { margin-bottom: .75rem; }
.permalink { margin-left: .5rem; text-decoration: none; opacity: 0; transition: opacity .2s; font-size: .85em; }
details summary:hover .permalink { opacity: .6; }
.permalink:hover { opacity: 1 !important; }
.hidden { display: none !important; }
footer { text-align: center; font-size: .85rem; margin-top: 3rem; }")

(defn generate-js [topics-data lang no-categories?]
  (let [strings-json (json/generate-string
                      {:noSearchResults   (:no-search-results lang)
                       :noCategoryResults (:no-category-results lang)
                       :topicsCount       (:topics-count lang)
                       :allCategories     (:all-categories lang)})]
    (str "(function() {
  'use strict';
  const topicsData = " (topics-to-js-array topics-data no-categories?) ";
  const strings = " strings-json ";
  let currentCategory = null;
  let currentSearch = '';
  const contentDiv = document.getElementById('topics-content');
  const searchInput = document.getElementById('search-input');
  const clearButton = document.getElementById('clear-search');
  const homeLink = document.getElementById('home-link');

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

  function slugify(text) {
    return String(text || '')
      .toLowerCase()
      .normalize('NFD')
      .replace(/[\\u0300-\\u036f]/g, '')
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-|-$/g, '');
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
    let html = '<nav class=\"grid\">';
    categories.forEach(cat => {
      html += `<a href=\"#\" class=\"card\" data-category=\"${escapeHtml(cat.name)}\">
        <h3>${escapeHtml(cat.name)}</h3>
        <p>${cat.count} ${strings.topicsCount}</p>
      </a>`;
    });
    return html + '</nav>';
  }

  function renderTopicsList(topics, showBackLink = false) {
    if (topics.length === 0) {
      const msg = currentSearch ? strings.noSearchResults : strings.noCategoryResults;
      return `<p><em>${msg}</em></p>`;
    }
    let html = showBackLink ? `<a href=\"#\" class=\"back-link\" id=\"back-to-categories\">${strings.allCategories}</a>` : '';
    topics.forEach(t => {
      const slug = slugify(t.title);
      html += `<details id=\"${slug}\">
        <summary>${escapeHtml(t.title)}<a href=\"#${slug}\" class=\"permalink\" title=\"Permalink\">🔗</a></summary>
        <div>${t.content}</div>
      </details>`;
    });
    return html;
  }

  function render() {
    let html;
    const categories = getCategories();

    if (currentSearch) {
      html = renderTopicsList(searchTopics(currentSearch), false);
    } else if (currentCategory) {
      html = renderTopicsList(getTopicsByCategory(currentCategory), true);
    } else if (isSinglePseudoCategory(categories)) {
      // Only one pseudo-category (flat data): show all topics directly
      html = renderTopicsList(topicsData, false);
    } else {
      html = renderCategoriesGrid();
    }

    contentDiv.innerHTML = html;
    openAndScrollToHash();

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
  window.addEventListener('hashchange', openAndScrollToHash);
  parseUrl();
  render();
})();")))

(defn html-escape [s]
  (-> (or s "")
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&#39;")))

(defn generate-head [config css-file]
  (str "<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <title>" (html-escape (:title config)) "</title>
  <link rel=\"icon\" href=\"data:image/png;base64,iVBORw0KGgo=\">
  <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css\">
  <style>" css-styles "</style>"
       (when css-file "\n  <link rel=\"stylesheet\" href=\"custom.css\">") "
</head>"))

(defn generate-header [config]
  (str "<header class=\"container\">
    <h1><a href=\"./\" id=\"home-link\">" (html-escape (:title config)) "</a></h1>
    <p>" (html-escape (:tagline config)) "</p>
  </header>"))

(defn generate-main [lang]
  (str "<main class=\"container\" id=\"main-content\" tabindex=\"-1\">
    <div class=\"search-row\" role=\"search\">
      <input placeholder=\"" (:search-placeholder lang) "\" type=\"search\" id=\"search-input\" name=\"q\">
      <button type=\"button\" class=\"secondary outline hidden\" id=\"clear-search\" aria-label=\"" (:clear-search lang) "\">✕</button>
    </div>
    <div id=\"topics-content\" aria-live=\"polite\"></div>
  </main>"))

(defn generate-footer [config lang]
  (str "<footer class=\"container\">
    <p>" (when-let [src (:source config)]
           (str "<a target=\"_blank\" href=\"" src "\">" (:content-source lang) "</a> · "))
       (:footer config) "</p>
  </footer>"))

(defn generate-html [config topics-data no-categories?]
  (let [cfg-lang (some-> (:lang config) name keyword)
        lang     (or (get ui-strings cfg-lang)
                     (:en ui-strings))
        _        (when (and (:verbose config)
                            (not (contains? ui-strings cfg-lang)))
                   (println "Warning: unsupported lang" (:lang config) "- defaulting to en"))
        css-file (:css config)]
    (str "<!DOCTYPE html>
<html lang=\"" (html-escape (:lang lang)) "\" data-theme=\"light\">
" (generate-head config css-file) "
<body>
  <a href=\"#main-content\" class=\"skip-link\">" (:skip-to-content lang) "</a>
  " (generate-header config) "
  " (generate-main lang) "
  " (generate-footer config lang) "
  <script>" (generate-js topics-data lang no-categories?) "</script>
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
        (if (and (fs/exists? css-file)
                 (not (fs/same-file? css-file "custom.css")))
          (do
            (fs/copy css-file "custom.css" {:replace-existing true})
            (log verbose "Copied:" css-file "-> custom.css"))
          (log verbose "Skipping CSS copy: source matches destination or missing.")))
      (spit "index.html" (generate-html config topics-data no-categories))
      (println "Generated: index.html"))))

(defn show-help []
  (println "Usage: topics [options] <file|url>")
  (println "\nGenerates a static HTML/CSS/JS site from topics data.\n\nOptions:")
  (println (cli/format-opts {:spec cli-options}))
  (System/exit 0))

(defn -main [& args]
  (try
    (let [{:keys [args opts]} (cli/parse-args args {:spec cli-options})
          ;; If -i/--input-file not provided, use first positional arg
          opts (cond-> opts
                 (and (not (:input-file opts))
                      (seq args))
                 (assoc :input-file (first args)))]
      (when (:help opts) (show-help))
      (when-not (:input-file opts)
        (println "Error: topics file or URL is required")
        (println "You can either pass it as:")
        (println "  - a positional argument: topics <file|url>")
        (println "  - or with -i/--input-file:   topics -i <file|url>")
        (show-help))
      (generate-site opts))
    (catch Exception e
      (println "ERROR:" (.getMessage e))
      (System/exit 1))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
