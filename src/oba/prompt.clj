(ns oba.prompt
  (:require
   [oba.bus-data :as buses]
   [clojure.test :refer :all]))

(defn- infer-slots
  "Take a query and infer any other slots based on the already
  provided slots"
  [query]
  (let [stops (and (:route query) (buses/filter-stops query))]
    (if-let [the-stop (and (= 1 (count stops)) (first stops))]
      (assoc query
             :stop-id (:stop-id the-stop))
      (if (and (:route query) (:street-one query))
        (assoc query :direction {:options (map :direction stops)})
        query))))

(with-test
  (defn resolve-query
    "Populate the query with as many slot values as possible and
     return the next slot we should fill."
    [query]
    (let [query (infer-slots query)]
      (if (:stop-id query)
        {:next-prompt nil
         :stop-id (:stop-id query)}
        (some identity
              (for [slot [:route :street-one :direction]]
                (if (or (nil? (slot query))
                        (:options (slot query)))
                  {:next-prompt {:slot slot
                                 :options (:options (slot query))}
                   :query query}))))))

  (let [empty-query {:route nil :street-one nil :street-two nil :direction nil}
        route-only {:route "44" :street-one nil :street-two nil :direction nil}
        route-and-stop {:route "44"
                        :street-one "market street"
                        :street-two "15th"
                        :direction nil}
        complete-query {:route "44"
                        :street-one "market street"
                        :street-two "15th"
                        :direction "E"}]
    (is (= {:slot :route
            :options nil}
           (:next-prompt (resolve-query empty-query))))
    (is (= {:slot :street-one
            :options nil}
           (:next-prompt (resolve-query route-only))))
    (is (= {:slot :direction
            :options ["E" "W"]}
           (:next-prompt (resolve-query route-and-stop))))
    (is (nil? (:next-prompt (resolve-query complete-query))))))

