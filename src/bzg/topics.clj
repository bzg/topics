#!/usr/bin/env bb

;; Copyright (c) Bastien Guerry
;; SPDX-License-Identifier: EPL-2.0
;; License-Filename: EPL-2.0.txt

;; Topics is a small web application exposing topics, loaded from a
;; local or distance json file.  You can try it quickly like this:
;;
;; ~$ topics -t https://code.gouv.fr/data/faq.json
;;
;; Then check http://localhost:8080
;;
;; Run topics -h for options.
;;
;; Here is an example json with "title", "content" and "path", which
;; last item is used to infer the category:
;;
;; [ {
;;   "title" : "Topic title",
;;   "content" : "<p>Content as HTML</p>",
;;   "path" : [ "Section", "Subsection (as category)" ]
;; } ]

(ns bzg.topics
  (:require [org.httpkit.server :as server]
            [cheshire.core :as json]
            [clojure.string :as str]
            [babashka.cli :as cli]
            [taoensso.timbre :as log]
            [selmer.parser :as selmer]))

(def cli-options
  {:topics         {:alias :t :desc "Path to topics JSON file" :ref "<file|url>"}
   :topics-sources {:alias :s :desc "Path to topics source URL" :ref "<url>"}
   :title          {:alias :T :desc "Website title (default \"Topics\")" :ref "<string>" :default "Topics"}
   :tagline        {:alias :L :desc "Website tagline (default \"Topics to explore\")" :ref "<string>" :default "Topics to explore"}
   :footer         {:alias :F :desc "Footer text (default: link to \"Topics\" source code)" :ref "<string>" :default "<a href=\"https://github.com/bzg/topics\">Topics</a>"}
   :log-level      {:alias :l :desc "Set log level: debug, info (the default), warn or error" :ref "<string>" :default "info" :coerce :string}
   :base-path      {:alias :b :desc "Base path for subdirectory deployment (e.g., /topics)" :ref "<path>" :default ""}
   :port           {:alias :p :desc "Port number for server (default 8080)" :ref "<int>" :default 8080 :coerce :int}
   :template       {:alias :I :desc "Path to custom HTML template file" :ref "<file>" :coerce :string}
   :help           {:alias :h :desc "Show help" :type :boolean}})

(def ui-strings
  {:fr {:search-placeholder      "Rechercher"
        :clear-search            "Effacer la recherche"
        :searching               "Recherche..."
        :no-search-results       "Aucun résultat ne correspond à votre recherche. Essayez avec d'autres termes."
        :no-category-results     "Aucun résultat trouvé dans cette catégorie."
        :topics-count            " sujets"
        :content-not-found-title "Contenu introuvable"
        :page-not-found-title    "Page non trouvée"
        :article-not-found       "L'article demandé n'existe pas"
        :page-not-found          "La page demandée n'existe pas"
        :check-url-search        "Vérifiez l'URL ou effectuez une nouvelle recherche."
        :check-url-home          "Vérifiez l'URL ou retournez à l'accueil."
        :home-title              "Accueil"
        :content-source          "Source des contenus"
        :lang                    "fr"}
   :en {:search-placeholder      "Search"
        :clear-search            "Clear search"
        :searching               "Searching..."
        :no-search-results       "No results match your search. Try with other terms."
        :no-category-results     "No results found in this category."
        :topics-count            " topics"
        :content-not-found-title "Content not found"
        :page-not-found-title    "Page not found"
        :article-not-found       "The requested article does not exist"
        :page-not-found          "The requested page does not exist"
        :check-url-search        "Check the URL or perform a new search."
        :check-url-home          "Check the URL or return to the homepage."
        :home-title              "Home"
        :content-source          "Content source"
        :lang                    "en"}})

(defn get-preferred-language [headers]
  (let [accept-language (get headers "accept-language" "en")]
    (if (str/starts-with? accept-language "fr")
      :fr
      :en)))

(defn with-base-path [path base-path]
  (str (str/replace base-path #"/$" "") path))

(defn safe-url-encode [^String s]
  (when (not-empty s)
    (-> s
        (java.net.URLEncoder/encode "UTF-8")
        (str/replace "+" "%20")  ;; Replace + with %20 for spaces
        (str/replace "%28" "(")  ;; Keep common characters readable
        (str/replace "%29" ")")
        (str/replace "%2C" ","))))

(defn safe-url-decode [^String s]
  (when (not-empty s)
    (try
      (java.net.URLDecoder/decode s "UTF-8")
      (catch Exception _
        (log/warn "Error decoding URL parameter:" s)))))

(defn load-topics-data [source]
  (try
    (log/info "Loading Topics data from" source)
    (let [content (slurp source)
          data    (json/parse-string content true)]
      (log/info "Loaded" (count data) "Topics items")
      data)
    (catch Exception e
      (log/error "Error loading Topics data from" source ":" (.getMessage e)))))

(defn strip-html [^String html]
  (when (not-empty html)
    (-> html
        (str/replace #"<[^>]*>" "")
        (str/replace #"&nbsp;" " ")
        (str/replace #"&lt;" "<")
        (str/replace #"&gt;" ">")
        (str/replace #"&amp;" "&")
        (str/replace #"&quot;" "\"")
        (str/replace #"&apos;" "'"))))

(defn sanitize-search-query [^String query]
  (when (not-empty query)
    (-> query
        (str/replace #"[<>]" "")
        (str/replace #"[\\'\";`]" "")
        (str/trim))))

(defn normalize-text [^String text]
  (when (not-empty text)
    (-> text
        (str/lower-case)
        ;; Replace diacritical marks
        (str/replace #"[àáâãäå]" "a")
        (str/replace #"[èéêë]" "e")
        (str/replace #"[ìíîï]" "i")
        (str/replace #"[òóôõö]" "o")
        (str/replace #"[ùúûü]" "u")
        (str/replace #"[ýÿ]" "y")
        (str/replace #"[ç]" "c")
        (str/replace #"[œ]" "oe")
        (str/replace #"[æ]" "ae")
        (str/replace #"[ñ]" "n")
        ;; Remove punctuation and special characters
        (str/replace #"[.,;:!?'\"/\\(\\)\\[\\]{}]" " ")
        ;; Collapse multiple spaces
        (str/replace #"\s+" " ")
        (str/trim))))

(defn search-topics [^String query topics-data]
  (when (not-empty query)
    (let [query-norm (normalize-text (sanitize-search-query query))]
      (filter (fn [{:keys [title content path]}]
                (or (str/includes? (normalize-text title) query-norm)
                    (str/includes? (normalize-text (strip-html content)) query-norm)
                    (some #(str/includes? (normalize-text %) query-norm) path)))
              topics-data))))

(defn get-categories [topics-data]
  (let [paths      (map :path topics-data)
        categories (distinct (map last paths))]
    (sort categories)))

(defn get-topics-by-category [category topics-data]
  (filter #(= (last (:path %)) category) topics-data))

(def config (atom {}))

(def default-template
  "<!DOCTYPE html>
<html lang=\"{{lang}}\" data-theme=\"light\">
  <head>
    <meta charset=\"utf-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
    <title>{{page-title}} - {{title}}</title>
    <link rel=\"icon\" href=\"data:image/png;base64,iVBORw0KGgo=\">
    <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css\">
    <script src=\"https://unpkg.com/htmx.org@2.0.0/dist/htmx.min.js\"></script>
    <style>
      /* Custom styles */
      .container {max-width: 1200px; margin: 0 auto; padding: 0 1rem;}
      header.site-header {padding: 1rem 0; background-color: #f8f9fa; margin-bottom: 2rem;}
      footer {margin-top: 3rem; padding: 2rem 0; background-color: #f8f9fa;}
      .category-card {height: 100%; display: flex; flex-direction: column;}
      .category-card > a {flex-grow: 1; display: flex; flex-direction: column; padding: 1.5rem; text-decoration: none; color: inherit; border: 1px solid #dee2e6; border-radius: 0.5rem; background-color: white; transition: transform 0.2s, box-shadow 0.2s;}
      .category-card > a:hover {transform: translateY(-5px); box-shadow: 0 10px 20px rgba(0, 0, 0, 0.1);}
      .grid {display: grid; grid-template-columns: repeat(auto-fill, minmax(300px, 1fr)); gap: 2rem;}
      details {margin-bottom: 1rem; border: 1px solid #dee2e6; border-radius: 0.5rem; padding: 1rem;}
      details summary {cursor: pointer; font-weight: bold; padding: 0.5rem 0;}
      details[open] summary {margin-bottom: 1rem;}
      .back-link {display: inline-flex; align-items: center; margin-bottom: 1rem;}
      .back-link::before {content: '←'; margin-right: 0.5rem;}
      .search-form {margin-bottom: 3rem;}
      .search-container {display: flex; gap: 0.5rem; margin-bottom: 2rem;}
      .search-container input[type=\"search\"] {flex-grow: 1;}
      .alert {padding: 1rem; border-radius: 0.5rem; margin-bottom: 1rem;}
      .alert-info {background-color: #e7f5ff; border: 1px solid #a5d8ff; color: #1971c2;}
      .alert-error {background-color: #ffe3e3; border: 1px solid #ffa8a8; color: #e03131;}
      .footer {text-align: center;font-size: .8rem;}
      .clear-button {border: 1px solid; border-radius: 1rem;}
      /* Added for HTMX search */
      .search-results {margin-top: 1rem;}
      .htmx-indicator {opacity: 0; transition: opacity 200ms ease-in;}
      .htmx-request .htmx-indicator {opacity: 1;}
      .htmx-request.htmx-indicator {opacity: 1;}
    </style>
  </head>
  <body>
    <header class=\"site-header\">
      <div class=\"container\">
        <div>
          <h1><a href=\"{{home-link}}\">{{title}}</a></h1>
          <p>{{tagline}}</p>
        </div>
      </div>
    </header>
    <main class=\"container\">
      {% ifequal content-type \"home\" %}
      <!-- Search component -->
      <div class=\"search-container\">
        <input placeholder=\"{{search-placeholder}}\"
               type=\"search\"
               id=\"search-input\"
               name=\"q\"
               value=\"{{search-query|safe}}\"
               hx-get=\"{{home-link}}\"
               hx-push-url=\"true\"
               hx-trigger=\"keyup changed delay:300ms, search\"
               hx-target=\"#topics-content\"
               hx-indicator=\".htmx-indicator\">
        <button type=\"button\"
                class=\"clear-button{% if search-query|empty? %} hidden{% endif %}\"
                title=\"{{clear-search}}\"
                aria-label=\"{{clear-search}}\"
                hx-get=\"{{home-link}}\"
                hx-push-url=\"true\"
                hx-target=\"#topics-content\"
                onclick=\"document.getElementById('search-input').value=''\">X</button>
        <div class=\"htmx-indicator\">
          <small>{{searching}}</small>
        </div>
      </div>

      <!-- START-HTMX-CONTENT -->
      <div id=\"topics-content\">
	{% if search-query|not-empty %}
	<!-- Search results -->
	{% if topics|empty? %}
	<div class=\"alert alert-info\">
          <p>{{no-search-results}}</p>
	</div>
	{% else %}
	<div>
          {% for topic in topics %}
          <details>
            <summary>{{topic.title}}</summary>
            <div>{{topic.content|safe}}</div>
          </details>
          {% endfor %}
	</div>
	{% endif %}
	{% else %}
	{% if category|not-empty %}
	<!-- Category results -->
	{% if topics|empty? %}
        <div class=\"alert alert-info\">
          <p>{{no-category-results}}</p>
        </div>
	{% else %}
        <div>
          {% for topic in topics %}
          <details>
            <summary>{{topic.title}}</summary>
            <div>{{topic.content|safe}}</div>
          </details>
          {% endfor %}
        </div>
	{% endif %}
	{% else %}
	<!-- Categories grid -->
	<div class=\"grid\">
          {% for cat-item in categories-with-counts %}
          <div class=\"category-card\">
            <a href=\"{{home-link}}?category={{cat-item.name|url-encode}}\"
               hx-get=\"{{home-link}}?category={{cat-item.name|url-encode}}\"
               hx-push-url=\"true\"
               hx-target=\"#topics-content\">
              <h3>{{cat-item.name}}</h3>
              <p>{{cat-item.count}} {{topics-count}}</p>
            </a>
          </div>
          {% endfor %}
	</div>
	{% endif %}
	{% endif %}
      </div>
      <!-- END-HTMX-CONTENT -->
      {% endifequal %}

      {% ifequal content-type \"error\" %}
      <!-- Error pages -->
      <div class=\"search-container\">
        <input placeholder=\"{{search-placeholder}}\"
               type=\"search\"
               id=\"search-input\"
               name=\"q\"
               value=\"\"
               hx-get=\"{{home-link}}\"
               hx-push-url=\"true\"
               hx-trigger=\"keyup changed delay:300ms, search\"
               hx-target=\"#topics-content\"
               hx-indicator=\".htmx-indicator\">
        <div class=\"htmx-indicator\">
          <small>{{searching}}</small>
        </div>
      </div>
      <div id=\"topics-content\">
        <h2>{% ifequal error-type \"not-found\" %}{{content-not-found-title}}{% else %}{{page-not-found-title}}{% endifequal %}</h2>
        <div class=\"alert alert-error\">
          <h3>{% ifequal error-type \"not-found\" %}{{article-not-found}}{% else %}{{page-not-found}}{% endifequal %}</h3>
          <p>{% ifequal error-type \"not-found\" %}{{check-url-search}}{% else %}{{check-url-home}}{% endifequal %}</p>
        </div>
      </div>
      {% endifequal %}
    </main>
    <footer>
      <div class=\"container\">
        <div class=\"footer\"><p><a target=\"new\" href=\"{{source}}\">{{content-source}}</a> · {{footer|safe}}</p>
        </div>
      </div>
    </footer>
  </body>
</html>")

;; Add special markers to identify the HTMX content section
(def htmx-content-start-marker "<!-- START-HTMX-CONTENT -->")
(def htmx-content-end-marker "<!-- END-HTMX-CONTENT -->")

(selmer/add-filter! :url-encode safe-url-encode)
(selmer/add-filter! :get (fn [m k] (get m k)))

(defn set-template! [template-path]
  (if-not template-path
    (swap! config assoc :template default-template)
    (try
      (log/info "Loading template from file:" template-path)
      (swap! config assoc :template (slurp template-path))
      (catch Exception e
        (log/error "Failed to load template file:" template-path)
        (log/error (.getMessage e))
        (swap! config assoc :template default-template)))))

(defn prepare-template-data [topics-data search-query category base-path]
  (let [filtered-topics (cond
                          (not-empty search-query) (search-topics search-query topics-data)
                          (not-empty category)     (get-topics-by-category category topics-data)
                          :else                    nil)
        categories (get-categories topics-data)
        category-counts (into {} (map (fn [c] [c (count (get-topics-by-category c topics-data))]) categories))
        categories-with-counts (map (fn [cat] {:name cat :count (get category-counts cat)}) categories)]
    {:search-query search-query
     :category category
     :topics filtered-topics
     :categories categories
     :category-counts category-counts
     :categories-with-counts categories-with-counts
     :home-link (with-base-path "/" base-path)}))

(defn extract-htmx-content [rendered-template]
  (let [start-idx (str/index-of rendered-template htmx-content-start-marker)
        end-idx (str/index-of rendered-template htmx-content-end-marker)]
    (when (and start-idx end-idx (< start-idx end-idx))
      (let [content-start (+ start-idx (count htmx-content-start-marker))
            content-end end-idx]
        (str/trim (subs rendered-template content-start content-end))))))

(defn render-page [content-type data title tagline footer source base-path lang-key]
  (let [lang (get ui-strings lang-key)]
    (selmer/render
     (:template @config)
     (merge
      ;; Base template data
      {:content-type content-type
       :title title
       :tagline tagline
       :footer footer
       :source source
       :home-link (with-base-path "/" base-path)
       :page-title (get data :page-title (:home-title lang))}
      lang
      data))))

(defn home-page [topics-data base-path search-query category lang-key title tagline footer source]
  (let [template-data (prepare-template-data topics-data search-query category base-path)]
    (render-page
     "home"
     (assoc template-data :page-title (get-in ui-strings [lang-key :home-title]))
     title tagline footer source base-path lang-key)))

(defn error-page [error-type base-path lang-key title tagline footer source]
  (render-page
   "error"
   {:error-type error-type
    :page-title (if (= error-type :not-found)
                  (get-in ui-strings [lang-key :content-not-found-title])
                  (get-in ui-strings [lang-key :page-not-found-title]))}
   title tagline footer source base-path lang-key))

(defn strip-base-path [^String uri ^String base-path]
  (let [base-len (count base-path)]
    (if (and (seq base-path)
             (str/starts-with? uri base-path))
      (let [path (subs uri base-len)]
        (if (str/starts-with? path "/")
          path
          (str "/" path)))
      uri)))

(defn parse-query-string [^String query-string]
  (when (not-empty query-string)
    (try
      (into {}
            (for [pair (str/split query-string #"&")]
              (let [[k v] (str/split pair #"=" 2)]  ;; Limit to 2 parts
                [(keyword (safe-url-decode k))
                 (safe-url-decode (or v ""))])))  ;; Handle missing values
      (catch Exception e
        (log/error "Error parsing query string:" (.getMessage e))))))

(defn create-app [topics-data settings]
  (fn [{:keys [request-method uri query-string headers]}]
    (let [path            (strip-base-path uri (:base-path settings))
          params          (parse-query-string query-string)
          search-query    (:q params)
          category        (:category params)
          is-htmx-request (get headers "hx-request")
          lang-key        (get-preferred-language headers)]
      (case [request-method path]
        [:get "/"]
        (if is-htmx-request
          ;; For HTMX requests, render the full page but extract just the content portion
          (let [full-page (home-page
                           topics-data
                           (:base-path settings)
                           search-query
                           category
                           lang-key
                           (:title settings)
                           (:tagline settings)
                           (:footer settings)
                           (:source settings))
                htmx-content (extract-htmx-content full-page)]
            {:status  200
             :headers {"Content-Type" "text/html; charset=utf-8"}
             :body    htmx-content})
          ;; Return full page for direct browser requests
          {:status  200
           :headers {"Content-Type" "text/html; charset=utf-8"}
           :body    (home-page
                     topics-data
                     (:base-path settings)
                     search-query
                     category
                     lang-key
                     (:title settings)
                     (:tagline settings)
                     (:footer settings)
                     (:source settings))})
        [:get "/robots.txt"]
        {:status  200
         :headers {"Content-Type" "text/plain"}
         :body    "User-agent: *\nAllow: /\n"}
        ;; Default - return 404
        {:status 404
         :headers {"Content-Type" "text/html; charset=utf-8"}
         :body (error-page
                :not-found
                (:base-path settings)
                lang-key
                (:title settings)
                (:tagline settings)
                (:footer settings)
                (:source settings))}))))

(defn show-help []
  (println "Usage: topics [options]")
  (println "\nOptions:")
  (println
   (cli/format-opts
    {:spec (->> cli-options
                (map (fn [[k v]] [k (dissoc v :default)]))
                (into (sorted-map)))}))
  (System/exit 0))

(defn -main [& args]
  (try ;; Parse command line arguments with simplified handling
    (let [opts (cli/parse-opts args {:spec cli-options})]
      (when (:help opts) (show-help))
      (log/merge-config! {:min-level (keyword (:log-level opts))})
      ;; Initialize template
      (set-template! (:template opts))
      ;; Load Topics data
      (let [topics-data (load-topics-data (:topics opts))]
        ;; Start the server
        (log/info (str "Starting server at http://localhost:" (:port opts)))
        (if (empty? (:base-path opts))
          (log/info "Running at root path /")
          (log/info "Running at base path:" (:base-path opts)))
        (log/info "Site title:" (:title opts))
        (log/info "Site tagline:" (:tagline opts))
        (log/info "Topics source:" (:topics-sources opts))
        (server/run-server
         (create-app topics-data (assoc opts :source (:topics-sources opts)))
         {:port (:port opts)})
        (log/info "Server started. Press Ctrl+C to stop.")
        @(promise)))
    (catch Exception e
      (log/error "ERROR:" (.getMessage e))
      (System/exit 1))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
