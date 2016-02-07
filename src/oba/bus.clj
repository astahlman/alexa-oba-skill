(ns oba.bus
  (:require [clojure.test :refer :all]
            [clojure.data.json :as json]
            [clj-http.client :as client]
            [oba.secret :refer [fetch-api-key]]))

(def ^:const king-county-metro 1)
(def ^:const route-44-id "1_100224")
(def ^:const route-44-market-and-15th-id "1_29215")

(defn http-get
  ([resource] (http-get resource nil))
  ([resource id]
   (let [base-url "http://api.pugetsound.onebusaway.org/api/where/"
         oba-key (fetch-api-key)
         url (str base-url resource "/" id ".json?key=" oba-key)
         response (client/get url)]
     {:status (:status response)
      :body (json/read-str (:body response)
                           :key-fn keyword)})))

(defn fetch-current-time []
  (get-in (http-get "current-time") [:body :currentTime]))

(with-test
  (defn fetch-routes
    ([] (fetch-routes king-county-metro))
    ([route]
     (let [response (http-get "routes-for-agency" route)]
       (get-in response [:body :data :list]))))
  (is (> (count (fetch-routes)) 10)))

(with-test
  (defn fetch-stops [route-id]
    (let [response (http-get "stops-for-route" route-id)]
      (get-in response [:body :data :references :stops])))
  (is (= 2 (count (filter
                   #(= (:name %) "NW Market St & 15th Ave NW")
                   (fetch-stops route-44-id))))))

(with-test
  (defn fetch-arrival-times [stop-id]
    (let [response (http-get "arrivals-and-departures-for-stop" stop-id)]
      (map :predictedArrivalTime
           (get-in response [:body :data :entry :arrivalsAndDepartures]))))
  (is (integer? (first (fetch-arrival-times route-44-market-and-15th-id)))))

(defn fetch-relative-arrival-times-minutes [stop-id]
  (let [arrival-times (fetch-arrival-times stop-id)
        now (fetch-current-time)]
    (map (fn [t]
           (int (/ (- t now) (* 1000 60)))) arrival-times)))
