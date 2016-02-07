(ns oba.bus-data
  (:require [clojure.test :refer :all]
            [clojure.edn]
            [oba.bus :as bus]))

(def all-stops (clojure.edn/read-string (slurp "data/all-stops.edn")))

(declare parse-street-name)

(with-test
  (defn filter-stops
    [{:keys [route street-one street-two direction] :as query}]
    (let [stop (set
                (remove nil?
                        (map (comp :specific parse-street-name)
                             [street-one street-two])))]
      (into []
            (comp
             (filter (fn [x] (or (not route) (= route (:route x)))))
             (filter (fn [x] (or (empty? stop)
                                (= stop
                                   (set
                                    (map :specific (:stop x)))))))
             (filter (fn [x] (or (not direction) (= direction (:direction x))))))
            all-stops)))
  (is (= 2 (count (filter-stops {:route "44"
                                 :street-one "15th"
                                 :street-two "market street"}))))
  (is (= "1_29215"
         (:stop-id
          (first
           (filter-stops {:route "44"
                          :street-one "15th"
                          :street-two "market street"
                          :direction "E"})))))
  (is (= "1_29700"
         (:stop-id
          (first
           (filter-stops {:route "44"
                          :street-one "market"
                          :street-two "15th avenue"
                          :direction "W"}))))))

(with-test
  (defn parse-stop-name
    "Extract the street(s) from a OBA stop name"
    [stop-name]
    (set
     (map parse-street-name
          (clojure.string/split stop-name #" & "))))
  (are [stop-name specific-streets] (= specific-streets
                                       (set
                                        (map :specific
                                             (parse-stop-name stop-name))))
    "NW Market St & 15th Ave N" #{"market" "15th"}
    "International Blvd & S 176th St - Bay 1" #{"international" "176th"}
    "Alki Ave SW & Harbor Ave SW" #{"alki" "harbor"}
    "S Albro Pl & Stanley Ave S" #{"albro" "stanley"}
    "Auburn Sounder Station - Bay 4" #{"auburn sounder station"}
    "Edmonds P&R" #{"edmonds park and ride"})) ;; TODO: I-95

(declare parse-street-name)

(with-test
  (defn parse-street-name
    "Extract the direction, specific, and generic name from a spoken-form odonym"
    [street-name]
    (if street-name
      (let [directions {"n" "N"
                        "s" "S"
                        "e" "E"
                        "w" "W"
                        "ne" "NE"
                        "northeast" "NE"
                        "nw" "NW"
                        "northwest" "NW"
                        "se" "SE"
                        "southeast" "SE"
                        "sw" "SW"
                        "southwest" "SW"}
            generics {"st" "street"
                      "street" "street"
                      "way" "way"
                      "ave" "avenue"
                      "avenue" "avenue"
                      "blvd" "boulevard"
                      "boulevard" "boulevard"
                      "rd" "road"
                      "road" "road"
                      "hwy" "highway"
                      "highway" "highway"
                      "dr" "drive"
                      "drive" "drive"
                      "ct" "court"
                      "court" "court"
                      "pl" "place"
                      "place" "place"
                      "ln" "lane"
                      "lane" "lane"
                      "pkwy" "parkway"
                      "parkway" "parkway"
                      "vis" "vista"
                      "vista" "vista"
                      "pt" "point"
                      "point" "point"
                      "cir" "circle"
                      "circle" "circle"}
            abbrevs {"p&r" "park and ride"
                     "sr-" "S. R. "
                     "sr" "S. R."
                     "mt." "mount"
                     "st." "saint"
                     "comm" "community"
                     "hs" "high school"
                     "tc" "transit center"
                     "cc" "convention center"
                     "intl" "international"
                     "int'l" "international"
                     "uw" "U. W."
                     "trm" "terminal"
                     "acs" "access"
                     "expr" "express"
                     "pkg" "parking"
                     "bldg" "building"
                     "hosp" "hospital"}
            substitutions (conj directions generics)
            transformations [;; Station X - Bay 1 -> Station X 
                             #(clojure.string/replace % #"(?i) - Bay \d+" "")]
            street-name (reduce #(%2 %1) street-name transformations)
            tokens (-> street-name
                       (clojure.string/lower-case)
                       (clojure.string/trim)
                       (clojure.string/split #" ")
                       (#(remove empty? %))
                       ((fn [t] (map #(get abbrevs % %) t))))]
        {:direction (some directions tokens)
         :generic (some generics tokens)
         :specific (clojure.string/join " " (remove substitutions tokens))})))
  (are [x y] (= x ((juxt :specific :generic :direction) (parse-street-name y)))
    ["montlake" "boulevard" "NE"] "Montlake Blvd NE"
    ["market" "street" "NW"] "NW Market St"
    ["roosevelt" "way" "NE"] "Roosevelt Way NE"
    ["green lake" "way" "N"] "Green Lake Way N"
    ["46th" "street" "N"] "N 46th St"
    ["7th" "avenue" "NE"] "7th Ave NE"
    ["7th" "avenue" nil] "7th avenue"
    ["7th" nil nil] "7th"
    ["tukwila transit center" nil nil] "Tukwila TC"
    ["auburn sounder station" nil nil] "Auburn Sounder Station - Bay 14"))

;; 15341 stops total, so cartesian product is too big for one slot
;; to encompass both stop identifiers
(defn fetch-all-stops
  "Fetch every stop from OneBusAway"
  []
  (let [interval 250] ;; Let's be nice clients
    (doall
     (for [route (bus/fetch-routes)
           stop (do (Thread/sleep interval)
                    (bus/fetch-stops (:id route)))]
       {:agency (:agencyId route)
        :route (:shortName route)
        :route-id (:id route)
        :raw-stop-name (:name stop)
        :stop (parse-stop-name (:name stop))
        :stop-id (:id stop)
        :direction (:direction stop)}))))
