(ns site-diff-checker.core
  (:require [clojure.string :as s]
            [clojure.edn :as edn]
            [clj-http.client :as client]
            [clojure.tools.cli :as cli]
            [clojure.core.async :as a])
  (:gen-class))

;;Other

(defn or-pred [a-fn b-fn]
  (fn [val]
    (or
      (a-fn val)
      (b-fn val))))

(defonce program-name "site-diff")

;; CLI Option Decleration

(defn parse-url [domain uri]
  (if (s/starts-with? uri "/")
    (str domain uri)
    uri))

(defn read-uris [filename]
  (edn/read-string (slurp filename)))

(def cli-opts
  [["-h" "--help" "Print this help message"]
  [nil "--version" "Version of software"]
  ["-v" nil "Verbosity level"
   :id :verbosity
   :default 0
   :default-desc "-vv = human readable"
   :assoc-fn (fn [m k _] (update m k inc))]
   ["-n" "--new-url URL" "Url where the server to be checked runs"
    :id :domain
    :default "http://localhost:3000"]
   ["-f" "--uri-file FILE" "Uri file with edn formatted list with {:uri ...}"
    :id :all-uri
    :parse-fn (comp edn/read-string slurp)]
   ["-e" "--error-only" "Show only pages that need fixing"
    :id :error]
   [nil "--output-results FILE" "not implemented"]])

(defn usage [options-summary]
  (->>
   ["This program finds shit we need to finish in a week."
    ""
    "Usage: site-diff arg [options]"
    ""
    "Arguments:"
    "page"
    "redirect"
    "both"
    ""
    "Options:"
    options-summary]
   (s/join \newline)))

(defonce custom-err ["--uri-file not specified"])
(defn custom-opts [{:keys [arguments options] :as cli-opts}]
  (if (and (:all-uri options) (count arguments))
    (case (first arguments)
      "redirect" (assoc options :restype :only-redirs)
      "page" (assoc options :restype :only-pages)
      (assoc options :restype :both))))

(defn error-msg [[err & errors]]
  (str program-name ": " err ". See '" program-name " --help'."
    (if (not (empty? errors))
      (reduce #(str %1 \newline %2) "\nOther errors found:" errors) "")))

;; HTTP Reqs

(defn single-request [domain {:keys [uri] :as result}]
  (let [ch (a/chan)]
    (client/head
      (parse-url domain uri)
      {:async? true :throw-exceptions false}
      (fn [resp]
        (a/put! ch
          (-> resp
            (select-keys [:status :trace-redirects])
            (merge result)))
            (a/close! ch))
      (fn [err]
        (a/put! ch
          (merge result
            {:status 504 :trace-redirects [] :cause err}))
        (a/close! ch)))
    ch))

(defn batch-request
  ([domain uri-map]
    (client/with-async-connection-pool
      {:timeout 5 :threads 4 :insecure? false :default-per-route 100}
      (into [] (map (partial single-request domain)) uri-map))))

;; Application logic

(defn check-redirect [{:keys [redir trace-redirects]}]
  (if redir
    (and
      (seq trace-redirects)
      (s/ends-with? (last trace-redirects) redir))
    (empty? trace-redirects)))

(def only-pages (filter check-redirect))

(def only-errors
  (filter (or-pred
    (comp :redir)
    (comp not #(<= 200 % 202) :status))))

(def bad-redirs
  (filter (comp not check-redirect)))

(defn make-readable [{:keys [redir uri trace-redirects status]}]
  (if redir
    (str uri " -> " (last trace-redirects) " -- should be " redir)
    (str uri " -> status: " status)))

(def readable-mode (map make-readable))

(defn verbose-printer [res-ch]
  (a/thread
    (loop [new-result (a/<!! res-ch)]
      (when new-result
        (println new-result)
        (recur (a/<!! res-ch))))))

(defn stats-printer [res-ch]
  (let [stats (a/<!! (a/into [] res-ch))]
    (println (count stats))))

(defn fetch-opts [{:keys [restype all-uri]}]
  (into []
    (case restype
      :only-pages only-pages
      :only-redirs bad-redirs
      (map identity)) all-uri))

(defn print-opts [{:keys [verbosity error]}]
  (comp
    (if error only-errors identity)
    (if (= 2 verbosity) readable-mode identity)))

(defn print-intro [{:keys [restype error]}]
  (println
    "Printing/Counting "
    (if error "only errors " "")
    (case restype
      :only-redirs "of redirects."
      :only-pages "of pages."
      "both pages and redirects.")))

(defn get-results [{:keys [domain all-uri] :as opts}]
  (let [all-ch (a/merge (batch-request domain (fetch-opts opts)))
        res-ch (a/pipe all-ch (a/chan 5 (print-opts opts)))]
    (print-intro opts)
    (if (= (:verbosity opts) 0)
      (stats-printer res-ch)
      (verbose-printer res-ch))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let
    [{:keys [arguments options summary errors] :as parsed-opts}
       (cli/parse-opts args cli-opts)]
    (cond
      (:help options) (println (usage summary))
      (:version options) (println "site-diff v1.1")
      errors (println (error-msg errors))
      (:all-uri options)
        (if-let [opts (custom-opts parsed-opts)]
          (get-results opts)
          (println (error-msg custom-err))))))
