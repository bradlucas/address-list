(ns address-list.core
  (:gen-class :main true)
  (:require [clojure.java.io :as io])
  (:use [clojure-csv.core :as csv])
  (:use [clojure.tools.cli :only (cli)]))


;; https://www.briandunning.com/sample-data/
;;
;; "first_name","last_name","company_name","address","city","county","state","zip","phone1","phone2","email","web"

;; ------------------------------------------------------------------------
;; Print Functions
;; ------------------------------------------------------------------------
(defn print-email [m]
  (println (format "%s" (:email m))))

(defn print-label [m]
  (println (format "%s %s" (:first m)  (:last m)))
  (println (format "%s" (:street1 m)))
  (if (not (.equals "" (:street2 m))) (println (format "%s" (:street2 m))))
  (println (format "%s, %s %s"  (:city m)  (:state m)  (:zip m)))
  (println ""))

(defn print-csv [m]
  "Needs quotes around strings"
  (println (format "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"" (:first m) (:last m) (:street1 m) (:street2 m) (:city m) (:state m) (:zip m))))


;; ------------------------------------------------------------------------
;; Filter Functions
;; ------------------------------------------------------------------------
(defn no-email [m]
  (.equals "" (:email m)))

(defn has-email [m]
  (not (no-email m)))


;; ------------------------------------------------------------------------
;; Process Function
;; ------------------------------------------------------------------------
(defn record-to-map [vector-record]
  (let [first (nth vector-record 0)
        last (nth vector-record 1)
        street1 (nth vector-record 3)
        street2 ""
        city (nth vector-record 4)
        state (nth vector-record 5)
        zip (nth vector-record 6)
        email (nth vector-record 10)
        map {:first first :last last :street1 street1 :street2 street2 :city city :state state :zip zip :email email }]
    map))

(defn process-file [file filter-fn report-fn]
  (doseq [m (remove #(not (filter-fn %))
                    (with-open [rdr (io/reader file)]
                      (doall (map #(record-to-map (first (csv/parse-csv %))) (rest (line-seq rdr))))))]
    (report-fn m)))

(defn parse-file [file]
  (with-open [rdr (io/reader file)]
    (doall (map #(first (csv/parse-csv %)) (rest (line-seq rdr))))))

(defn get-email-from-vector [v]
  (nth v 10))

(defn get-map-from-vector [v]
    (let [first (nth v 0)
        last (nth v 1)
        street1 (nth v 3)
        street2 ""
        city (nth v 4)
        state (nth v 5)
        zip (nth v 6)
        email (nth v 10)
        map {:first first :last last :street1 street1 :street2 street2 :city city :state state :zip zip :email email }]
      map))

(defn parse-row-email [row]
  (nth (first (csv/parse-csv row)) 9))

(defn process-email [file]
  (with-open [rdr (io/reader file)]
    (remove #(.equals ""  %) (doall (map #(parse-row-email %) (rest (line-seq rdr)))))))

(defn process-address [file]
  (with-open [rdr (io/reader file)]
    (remove #(not (.equals "" (:email %))) (doall (map #(record-to-map (first (csv/parse-csv %))) (rest (line-seq rdr)))))))

(defn foo [file]
  (with-open [rdr (io/reader file)]
    (csv/parse-csv (first (rest (line-seq rdr))))))

(defn emailflag [flag]
  (.equals (.toLowerCase flag) "-e"))


;; read the file into a collection of maps
;; filter collection based on criteria (those with email or those without)     -- has-email,  no-email
;; print the result in a specific format (email only, mailing label or csv)    -- email, label, csv

;; Usage:
;; friends-list FILTER-FLAG PRINT-FLAG FILENAME
;;
;; FILTER-FLAG     -- has-email,  no-email
;; PRINT-FLAG     -- email, label, csv
(defn get-email-filter [f]
  (if f
    has-email
    no-email))

(defn get-print-filter [f]
  (cond
   (= f "csv") print-csv
   (= f "email") print-email
   (= f "label") print-label
   true nil))

(defn run
  "Print out the options and the arguments"
  [opts args banner]
  (let [e (get-email-filter (:email opts))
        p (get-print-filter (:print opts))
        ]
    (if (and (and e p) (first args))
      (process-file (first args) e p)
      (println banner))))


(defn -main [& args]
  (let [[opts args banner]
        (cli args
             ["-h" "--help" "Show help" :flag true :default false]
             ["-e" "--email" "Include only records with email" :flag true :default false]
             ["-p" "--print" "Print output type (email, label, csv)" :default "label"]
             )]
    (when (:help opts)
      (println banner)
      (System/exit 0))
    (run opts args banner)))
