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
;;   "content" : "<p>Content as HTML",
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
  {;; App contents options
   :contents         {:alias :c :desc "Path to contents JSON file" :default "topics.json"}
   :contents-sources {:alias :C :desc "Path to contents source" :default "[local]"}
   ;; App UI options
   :title            {:alias :t :desc "Website title" :default "Topics"}
   :tagline          {:alias :T :desc "Website tagline" :default "A few topics to explore"}
   :footer           {:alias :f :desc "Footer text" :default "Made with <a href=\"https://github.com/bzg/topics\">Topics</a>"}
   ;; App options
   :log-level        {:alias :l        :desc    "Set log level (debug, info, warn, error)"
                      :ref   "<level>" :default "info" :coerce :string}
   :base-path        {:alias :b :desc "Base path for subdirectory deployment (e.g., /topics)" :default ""}
   :port             {:alias :p :desc "Port number for server" :default 8080 :coerce :int}
   :help             {:alias :h :desc "Show help" :type :boolean}})

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
  (when html
    (-> html
        (str/replace #"<[^>]*>" "")
        (str/replace #"&nbsp;" " ")
        (str/replace #"&lt;" "<")
        (str/replace #"&gt;" ">")
        (str/replace #"&amp;" "&")
        (str/replace #"&quot;" "\"")
        (str/replace #"&apos;" "'"))))

(defn sanitize-search-query [^String query]
  (when query
    (-> query
        (str/replace #"[<>]" "")
        (str/replace #"[\\'\";`]" "")
        (str/trim))))

(defn normalize-text [^String text]
  (when text
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

(defn search-topics [query topics-data]
  (when (seq query)
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

;; Define the layout template
(def layout-template
  "<!DOCTYPE html>
<html lang=\"fr\" data-theme=\"light\">
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
  .search-container {display: flex; gap: 0.5rem;}
  .search-container input[type=\"search\"] {flex-grow: 1;}
  .alert {padding: 1rem; border-radius: 0.5rem; margin-bottom: 1rem;}
  .alert-info {background-color: #e7f5ff; border: 1px solid #a5d8ff; color: #1971c2;}
  .alert-error {background-color: #ffe3e3; border: 1px solid #ffa8a8; color: #e03131;}
  .footer {text-align: center;font-size: .8rem;}
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
  {{content|safe}}
  </main>

  <footer>
  <div class=\"container\">
  <div class=\"footer\"><p><a target=\"new\" href=\"{{source}}\">Source des contenus</a> · {{footer|safe}}</p>
  </div>
  </footer>
  </body>
  </html>")

;; Updated page-layout function
(defn page-layout [page-title content title tagline footer source base-path]
  (selmer/render
   layout-template
   {:page-title page-title
    :content    content
    :title      title
    :tagline    tagline
    :footer     footer
    :source     source
    :home-link  (with-base-path "/" base-path)}))

(defn home-content [topics-data base-path]
  (str "<div>
  <div class=\"search-container\">
    <input placeholder=\"Rechercher\"
           type=\"search\"
           id=\"search-input\"
           name=\"q\"
           hx-get=\"" (with-base-path "/search-results" base-path) "\"
           hx-trigger=\"keyup changed delay:300ms, search\"
           hx-target=\"#search-results\"
           hx-indicator=\".htmx-indicator\">
    <div class=\"htmx-indicator\">
      <small>Recherche...</small>
    </div>
  </div>

  <div id=\"search-results\" class=\"search-results\">
    <!-- Search results will appear here -->
  </div>

  <h2>Catégories</h2>
  <div class=\"grid\">"
       (str/join "\n"
                 (for [category (get-categories topics-data)]
                   (str "<div class=\"category-card\">
  <a href=\"" (with-base-path "/category" base-path) "?name=" (safe-url-encode category) "\">
  <h3>" category "</h3>
  <p>" (count (get-topics-by-category category topics-data)) " questions</p>
  </a>
  </div>")))
       "</div>
  </div>"))

(defn category-content [category-name category-topics base-path]
  (str "<div>
  <a href=\"" (with-base-path "/" base-path) "\" class=\"back-link\">Retour à l'accueil</a>
  <h2>" category-name "</h2>
  <div>"
       (str/join "\n"
                 (for [item category-topics]
                   (str "<details>
  <summary>" (:title item) "</summary>
  <div>" (:content item) "</div>
  </details>")))
       "</div>
  </div>"))

(defn search-content [query results base-path]
  (str "<div>
  <a href=\"" (with-base-path "/" base-path) "\" class=\"back-link\">Retour à l'accueil</a>
  <h2>Résultats de recherche</h2>
  <p>Résultats pour \"" query "\" (" (count results) ") :</p>
  <div>"
       (if (empty? results)
         "<div class=\"alert alert-info\">
  <h3>Aucun résultat</h3>
  <p>Aucun résultat ne correspond à votre recherche. Essayez avec d'autres termes.</p>
  </div>"
         (str "<div>"
              (str/join "\n"
                        (for [item results]
                          (str "<details>
  <summary>" (:title item) "</summary>
  <div>" (:content item) "</div>
  </details>")))
              "</div>"))
       "</div>
  </div>"))

(defn search-results-content [query results]
  (if (empty? query)
    ""  ;; Empty string when no query
    (str "<div>"
         (if (empty? results)
           "<div class=\"alert alert-info\">
  <p>Aucun résultat ne correspond à votre recherche. Essayez avec d'autres termes.</p>
  </div>"
           (str "<p>Résultats pour \"" query "\" (" (count results) ") :</p>"
                (str/join "\n"
                          (for [item results]
                            (str "<details>
  <summary>" (:title item) "</summary>
  <div>" (:content item) "</div>
  </details>")))))
         "</div>")))

(defn topics-content [item base-path]
  (str "<div>
  <a href=\"javascript:history.back()\" class=\"back-link\">Retour</a>
  <article>
  <h2>" (:title item) "</h2>
  <div>
  " (:content item) "
  </div>
  <p>
  Catégorie : <a href=\"" (with-base-path "/category" base-path) "?name=" (safe-url-encode (last (:path item))) "\">" (last (:path item)) "</a>
  </p>
  </article>
  </div>"))

(defn error-content [base-path type]
  (let [title   (if (= type :not-found) "Contenu introuvable" "Page non trouvée")
        message (if (= type :not-found)
                  "L'article demandé n'existe pas"
                  "La page demandée n'existe pas")
        action  (if (= type :not-found)
                  "Vérifiez l'URL ou effectuez une nouvelle recherche."
                  "Vérifiez l'URL ou retournez à l'accueil.")]
    (str "<div>
  <a href=\"" (with-base-path "/" base-path) "\" class=\"back-link\">Retour à l'accueil</a>
  <h2>" title "</h2>
  <div class=\"alert alert-error\">
  <h3>" message "</h3>
  <p>" action "</p>
  </div>
  </div>")))

(defn strip-base-path [uri base-path]
  (let [base-len (count base-path)]
    (if (and (seq base-path)
             (str/starts-with? uri base-path))
      (let [path (subs uri base-len)]
        (if (str/starts-with? path "/")
          path
          (str "/" path)))
      uri)))

(defn parse-query-string [query-string]
  (when query-string
    (try
      (into {}
            (for [pair (str/split query-string #"&")]
              (let [[k v] (str/split pair #"=" 2)]  ;; Limit to 2 parts
                [(keyword (safe-url-decode k))
                 (safe-url-decode (or v ""))])))  ;; Handle missing values
      (catch Exception e
        (log/error "Error parsing query string:" (.getMessage e))))))

(defn html-response [status title content settings]
  {:status  status
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body    (page-layout
             title content (:title settings) (:tagline settings)
             (:footer settings) (:source settings) (:base-path settings))})

(defn fragment-response [content]
  {:status  200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body    content})

(defn create-app [topics-data settings]
  (fn [{:keys [request-method uri query-string]}]
    (let [path   (strip-base-path uri (:base-path settings))
          params (parse-query-string query-string)]
      (case [request-method path]
        [:get "/"]
        (html-response 200 "Accueil"
                       (home-content topics-data (:base-path settings))
                       settings)
        [:get "/robots.txt"]
        {:status  200
         :headers {"Content-Type" "text/plain"}
         :body    "User-agent: *\nAllow: /\n"}
        [:get "/category"]
        (let [category-name   (:name params)
              category-topics (get-topics-by-category category-name topics-data)]
          (html-response 200
                         (str "Catégorie : " category-name)
                         (category-content category-name category-topics (:base-path settings))
                         settings))
        [:get "/search"]
        (let [query   (:q params)
              results (search-topics query topics-data)]
          (html-response 200
                         (str "Résultats pour : " query)
                         (search-content query results (:base-path settings))
                         settings))
        ;; New endpoint for HTMX search results
        [:get "/search-results"]
        (let [query   (:q params)
              results (search-topics query topics-data)]
          (fragment-response
           (search-results-content query results)))
        [:get "/topics"]
        (let [id   (:id params)
              item (first (filter #(= (:title %) id) topics-data))]
          (if item
            (html-response 200
                           (:title item)
                           (topics-content item (:base-path settings))
                           settings)
            (html-response 404
                           "404"
                           (error-content (:base-path settings) :not-found)
                           settings)))
        ;; Default route - 404
        (html-response 404
                       "Page non trouvée"
                       (error-content (:base-path settings) :page-not-found)
                       settings)))))

(defn show-help []
  (println "Usage: topics [options]")
  (println "\nOptions:")
  (println (cli/format-opts {:spec cli-options}))
  (System/exit 0))

(defn -main [& args]
  (try
    ;; Parse command line arguments with simplified handling
    (let [opts (cli/parse-opts args {:spec cli-options})]
      (when (:help opts) (show-help))
      (log/merge-config! {:min-level (keyword (:log-level opts))})
      ;; Load Topics data
      (let [topics-data (load-topics-data (:contents opts))]
        ;; Start the server
        (log/info (str "Starting server at http://localhost:" (:port opts)))
        (if (empty? (:base-path opts))
          (log/info "Running at root path /")
          (log/info "Running at base path:" (:base-path opts)))
        (log/info "Site title:" (:title opts))
        (log/info "Site tagline:" (:tagline opts))
        (log/info "Topics source:" (:contents-sources opts))
        (server/run-server (create-app topics-data opts) {:port (:port opts)})
        (log/info "Server started. Press Ctrl+C to stop.")
        @(promise)))
    (catch Exception e
      (log/error "ERROR:" (.getMessage e))
      (.printStackTrace e)
      (System/exit 1))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
