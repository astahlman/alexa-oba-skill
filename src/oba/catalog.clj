(ns oba.catalog
  (:require [oba.bus-data :as bus-data]
            [clojure.test :refer :all])
  (:gen-class))

(defn- download-all-stops
  "Download all stops from OBA and write them to data/all-stops.edn"
  [dst-file]
  (let [stops (bus-data/fetch-all-stops)]
    (spit dst-file (prn-str stops))
    stops))

(def all-stops (clojure.edn/read-string (slurp "data/all-stops.edn")))

(with-test
  (defn- generate-street-name-variations
    "Generate stop name for a specific stop"
    [{:keys [generic specific]}]
    [specific
     (clojure.string/join " " [specific generic])])
  (is (= #{"leary" "leary way"}
         (set
          (generate-street-name-variations {:specific "leary"
                                          :generic "way"})))))

(defn- generate-street-names
  "Generate spoken-form stop name variations for every stop"
  ([]
   (generate-street-names all-stops))
  ([stops]
   (sort
    (remove empty?
            (set
             (flatten
              (for [stop (flatten (map #(seq (:stop %)) stops))]
                (generate-street-name-variations stop))))))))

(defn- generate-route-names
  "Generate spoken-form route name variations for every stop"
  ([]
   (generate-route-names all-stops))
  ([stops]
   (sort (seq (set (map :route stops))))))

(defn- generate-catalogs
  "Generate new catalogs for the RouteName and Street slots"
  ([] (generate-catalogs all-stops))
  ([stops]
   (let [routes (generate-route-names stops)
         streets (generate-street-names stops)]
     (clojure.java.io/make-parents "interaction-model/catalogs/RouteName.txt")
     (spit "interaction-model/catalogs/RouteName.txt"
           (clojure.string/join "\n" routes))
     (spit "interaction-model/catalogs/Street.txt"
           (clojure.string/join "\n" streets)))))

(defn -main
  "Update our list of stops from OneBusAway and store it in data/"
  ([] (-main "data/all-stops.edn"))
  ([dst-file]
   (println "Downloading stops from OneBusAway...")
   (let [stops (download-all-stops dst-file)]
     (println "Done.")
     (println "Generating custom slot values...")
     (generate-catalogs stops)
     (println "Done."))))
