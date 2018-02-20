(ns site-diff-checker.core
  (:require [clojure.string :as s]
            [clojure.edn :as edn]
            [clojure.core.reducers :as r]
            [clj-http.client :as client]
            [clojure.tools.cli :as cli]
            [manifold.deferred :as d]
            [taoensso.timbre :as log])
  (:gen-class))

;;Other

(defn update-vals [m f & args]
  (r/reduce (fn [r k v] (assoc r k (apply f k v args))) {} m))

(defn map-kv
  ([f & maps]
    (into {} (apply map f maps))))

(defn val? [pred? val]
  (when (pred? val)
    val))

(defn log-request-error [url err]
  (log/info
    {:summary (str "Request failed for " url) :error err}))

(defonce program-name "site-diff")

;; CLI Option Decleration

(defn parse-url [url-str]
  (if (some #(s/starts-with? url-str %) '("http://" "https://"))
    url-str
    (str "https://" url-str)))

(defn read-uris [filename]
  (edn/read-string (slurp filename)))

(def cli-opts
  [["-h" "--help" "Print this help message"]
   [nil "--verbose" "Verbose output"]
   [nil "--human-readable" "Print for humans"
    :default true
    :id :human-readable]
   [nil "--not-human-readable"
    :id :human-readable
    :parse-fn not]
   ["-v" "--version" "Version of software"]
   ["-q" "--quick" "Only compare to local status codes from --uri-file"]
   ["-o" "--old-url URL" "Url where the old server runs"
    :id :old-url
    :parse-fn parse-url
    :default "https://www.studentaanhuis.nl"]
   ["-n" "--new-url URL" "Url where the server to be checked runs"
    :id :new-url
    :parse-fn parse-url
    :default "http://localhost:3000"]
   ["-u" "--uri URI" "Uri to be checked"
    :id :uri
    :parse-fn #(if (s/starts-with? % "/") % (str "/" %))]
   ["-f" "--uri-file FILE" "Uri file with edn formatted [uri -> status] hashmap"
    :id :uri-map
    :parse-fn (comp edn/read-string slurp)]
   [nil "--output-results FILE" "not implemented"]])

(defn usage [options-summary]
  (->>
   ["This program finds differences in uri between websites."
    ""
    "Usage: site-diff [options]"
    ""
    "NOTE: --uri and --uri-file are mutually exclusive options."
    ""
    "Options:"
    options-summary]
   (s/join \newline)))

(defonce custom-err ["--uri & --uri-file both/neither specified"])
(defn custom-opts [opts]
  (or
    (and (:uri opts) (:uri-map opts))
    (and (not (:uri opts)) (not (:uri-map opts)))))

(defn error-msg [[err & errors]]
  (str program-name ": " err ". See '" program-name " --help'."
    (if (not (empty? errors))
      (reduce #(str %1 \newline %2) "\nOther errors found:" errors) "")))

;; HTTP Reqs

(defn single-request [full-url]
  (let [dres (d/deferred)]
    (client/head
      full-url
      {:async? true :throw-exceptions false}
      (fn [resp]
        (d/success! dres (select-keys resp [:status :trace-redirects])))
      (fn [err]
        (log-request-error full-url err)
        (d/success! dres {:status 504 :trace-redirects [] :cause err})))
    dres))

(defn request-batch [{:keys [uri-map old-url new-url quick]}]
  (client/with-async-connection-pool
    {:timeout 5 :threads 4 :insecure? false :default-per-route 10}
    (-> uri-map
      (update-vals
        (fn [k v]
          (if (or (nil? (get v :cache)) (not quick))
            (assoc v :cache (single-request (str old-url k))))))
      (update-vals #(assoc %2 :new (single-request (str new-url %1)))))))

(defn request-pair [old-url new-url uri]
  (request-batch
    {:uri-map {uri {}}
     :new-url new-url
     :old-url old-url}))

;; Application logic

(defn force-vals [uri-map]
  (update-vals uri-map
    (fn [_ {:keys [cache new] :as val}]
      (assoc val :cache @cache :new @new))))

(defn check-redirects
  [old-url new-url {old-redirs :trace-redirects} {new-redirs :trace-redirects}]
  (cond
    (and (empty? old-redirs) (not-empty new-redirs))
      :only-new-redirs
    (not-empty old-redirs)
      (if (every?
            (fn [[old new]] (= old new))
            (map #(vector (subs %1 (count old-url)) (subs %2 (count new-url)))
              old-redirs
              new-redirs))
        :all-redirs-eq
        :both-redirs-not-eq)
    :default :no-redirs))

(defn update-redirects [uri-map {:keys [old-url new-url]}]
  (update-vals uri-map
    (fn [_ {:keys [cache new] :as v}]
      (assoc v :redirects
        (check-redirects old-url new-url
          cache
          new)))))

(defn compare-status [{old-status :status} {new-status :status}]
  (cond
    (> old-status 400) :old-url-error
    (> new-status 400) :new-url-error
    (= new-status old-status 200) :url-match))

(defn update-statuses [uri-map]
  (update-vals uri-map
    (fn [_ {:keys [cache new] :as val}]
      (assoc val :status (compare-status cache new)))))

(defn status-stats [uri-map]
  (let [grouped (->
                  (group-by (comp :status val) uri-map)
                  (update-vals (fn [_ uris] (mapv key uris))))
        freqs (update-vals grouped (fn [_ v] (count v)))]
    [grouped freqs]))

(defn redir-stats [uri-map]
  (let [grouped (->
                  (group-by (comp :redirects val) uri-map)
                  (update-vals (fn [_ uris] (mapv key uris))))
        freqs (update-vals grouped (fn [_ v] (count v)))]
    [grouped freqs]))

(defn human-readable-summary
  ([s r] (human-readable-summary s r {}))
  ([[status-group status-freq]
    [redirs-group redirs-freq]
    {:keys [verbose]}]
   (str
     "Uri Stats: "
     "OK " (:url-match status-freq) ", "
     "ERROR " (:new-url-error status-freq) ", "
     "WARN " (:old-url-error status-freq) "\n"
     "Unmatched URIs:"
     "\n  "
     (s/join "\n  " (:new-url-error status-group))
     "\n\n"
     (if verbose
       (str
         "Old URLs gave Error:"
         "\n  "
         (s/join "\n  " (:old-url-error status-group))
         "\n\n"
         "URL Status Match:"
         "\n  "
         (s/join "\n  " (:url-match status-group))
         "\n\n"))
     "Redirect Stats: "
     "OK " (+ (:no-redirs redirs-freq) (:all-redirs-eq redirs-freq)) ", "
     "ERROR " (:both-redirs-not-eq redirs-freq) ", "
     "WARN " (:only-new-redirs redirs-freq) "\n"
     "Unmatched URI Redirects:"
     "\n  "
     (s/join "\n  " (:both-redirs-not-eq redirs-group))
     "\n\n"
     (if verbose
       (str
         "URIs Redirecting on New:"
         "\n  "
         (s/join "\n  " (:only-new-redirs redirs-group))
         "\n\n")))))

(defn urimap-results [options]
  (let [updated-results (-> options
                          request-batch
                          force-vals
                          update-statuses
                          (update-redirects options))]
    (if (:human-readable options)
      (human-readable-summary
        (status-stats updated-results)
        (redir-stats updated-results))
      updated-results)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [{:keys [options summary errors]} (cli/parse-opts args cli-opts)]
    (println "\n"
      (cond
        (:help options) (usage summary)
        (:version options) (str "site-diff v1")
        errors (error-msg errors)
        (custom-opts options) (error-msg custom-err)
        (:uri-map options) (urimap-results options)
        (:uri options) (urimap-results
                         (assoc-in options [:uri-map] {(get options :uri) {}}))
        ))))
