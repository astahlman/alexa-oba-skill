(ns oba.core
  (:require [clojure.data.json :as json]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [oba.phrases :as phrases]
            [oba.bus :as bus]
            [oba.prompt :as prompt]
            [oba.bus-data :as bus-data]
            [clojure.edn :as edn]
            [clojure.test :refer :all]
            [oba.utils :refer [same-shape?]])
  (:gen-class
   :implements [com.amazonaws.services.lambda.runtime.RequestStreamHandler]))

(declare log key->keyword make-response java-fy)
(declare on-request on-intent consolidate respond-or-prompt)

(defn -handleRequest
  "Entry point for our Lambda request handler.
   is - Input stream with JSON representing request
   os - Ouput stream of our response, also JSON"
  [this is os context]
  (let [w (io/writer os)
        response (-> is
                     (io/reader)
                     (json/read :key-fn key->keyword)
                     (on-request))]
    (json/write response w :key-fn (comp java-fy name))
    (pprint response)
    (.flush w)))

(defmulti on-request (fn [req]
                       (get-in req [:request :type])))

(defmethod on-request "LaunchRequest" [req]
  (log "Handling launch request")
  (pprint req)
  (make-response {:text phrases/modal-welcome-prompt
                  :reprompt phrases/modal-welcome-reprompt
                  :should-end-session? false}))

(defmethod on-request "SessionEndedRequest" [req]
  (log "Handling end request")
  (pprint req)
  (make-response {:output-speech {:type "PlainText"
                                  :text "Bye!"}
                  :should-end-session? true}))

(defmethod on-request "IntentRequest" [req]
  (log "Handling intent request")
  (pprint req)
  (on-intent
   (get-in req [:request :intent])
   (edn/read-string (get-in req [:session :attributes :data]))))

(defmulti on-intent
  (fn [intent context]
    (:name intent)))

(defmethod on-intent "AMAZON.HelpIntent" [intent context]
  (make-response {:text phrases/get-help-response}))

(defmethod on-intent "NextBusIntent"
  [intent context]
  (respond-or-prompt intent context))

(defmethod on-intent "SupplyRouteIntent"
  [intent context]
  (respond-or-prompt intent context))

(defmethod on-intent "SupplyDirectionIntent"
  [intent context]
  (respond-or-prompt intent context))

(defmethod on-intent "SupplyStopIntent"
  [intent context]
  (respond-or-prompt intent context))

(defn respond-or-prompt [intent context]
  (let [{:keys [next-prompt query stop-id]} (prompt/resolve-query
                                             (consolidate intent context))]
    (if next-prompt
      (case (:slot next-prompt)
        :route (make-response
                {:text "Which route should I look up?"
                 :context query})
        :street-one (make-response
                     {:text (str "Which stop on route " (:route query) "?")
                      :context query})
        :direction (make-response
                    {:text
                     (str "Which direction are you going: "
                          (clojure.string/join " or " (map {"E" "east"
                                                            "W" "west"
                                                            "N" "north"
                                                            "S" "south"}
                                                           (:options next-prompt)))
                          "?")
                     :context query}))
      (let [arrivals (bus/fetch-relative-arrival-times-minutes stop-id)
            [next-arr & upcoming] (filter pos? arrivals)]
        (make-response {:text (str "The next bus will arrive in " next-arr
                                   " minutes. There are also buses arriving in "
                                   (clojure.string/join " and " upcoming)
                                   " minutes")
                        :context query
                        :should-end-session? true})))))

(with-test
  (defn consolidate
    "Merge the new args in intent with the existing context's query"
    [intent context]
    (letfn [(merge-stops [query]
              (let [[s1 s2] ((juxt :street-one :street-two) query)]
                (if (or s1 s2)
                  (assoc query :stop (set (remove nil? [s1 s2])))
                  query)))
            (clear-options [context]
              (into {}
                    (for [[k v] context]
                      (if-not (:options v) [k v] [k nil]))))
            (canonicalize-directions [query]
              (update-in query [:direction] {"east" "E"
                                             "west" "W"
                                             "north" "N"
                                             "south" "S"}))]
      (let [slots (:slots intent)
            clean-context (clear-options context)]
        (conj
         clean-context
         (canonicalize-directions
          (merge-stops
           (into {}
                 (for [[k v] slots]
                   [k (:value v)]))))))))
  ;; TODO: Test weird stops without &, e.g., Sounder Bus Station Bay 4
  (let [supply-stop-intent {:name "SupplyStopIntent"
                            :slots {:street-one
                                    {:name "StreetOne"
                                     :value "market"}
                                    :street-two
                                    {:name "StreetTwo"
                                     :value "leary"}}}
        supply-route-intent {:name "SupplyRouteIntent"
                             :slots {:route
                                     {:name "Route"
                                      :value "40"}}}
        supply-direction-intent {:name "SupplyDirectionIntent"
                                 :slots {:direction
                                         {:name "Direction"
                                          :value "east"}}}]
    (is (same-shape? (consolidate
                      supply-route-intent
                      nil)
                     {:route "40"}))
    (is (same-shape? (consolidate
                      supply-stop-intent
                      {:route "44" :stop nil :direction nil})
                     {:route "44"
                      :stop #{"leary" "market"}
                      :direction nil}))
    (is (same-shape? (consolidate
                      supply-direction-intent
                      {:route "40"
                       :stop #{"leary" "market"}
                       :street-one "leary"
                       :street-two "market"
                       :direction {:options ["east" "west"]}})
                     {:route "40"
                      :stop #{"leary" "market"}
                      :direction "E"}))))

(defn make-response [{:keys [text reprompt context should-end-session?]}]
  (let [response {:output-speech {:type "PlainText"
                                  :text text}
                  :should-end-session? (boolean should-end-session?)}
        response (if reprompt
                   (assoc response
                          :reprompt
                          {:output-speech {:type "PlainText"
                                           :text reprompt}})
                   response)]
    (assoc {:version "1" :response nil}
           :response response
           :sessionAttributes {:data (prn-str context)})))

(defn log
  "TODO: Implement this"
  [msg]
  (println (str "INFO: " msg)))

(with-test
  (defn key->keyword [key-string]
    (-> key-string
        (s/replace #"([a-z])([A-Z])" "$1-$2")
        (s/replace #"([A-Z]+)([A-Z])" "$1-$2")
        (s/lower-case)
        (keyword)))
  (is (= :street-one (key->keyword "StreetOne")))
  (is (= :street-one (key->keyword "streetOne"))))

(with-test
  (defn java-fy
    "Go from clojure-case to javaCase for JSON responses"
    [s]
    (letfn [(remove-special [s] (clojure.string/replace s #"[!?]" ""))]
      (let [tokens (clojure.string/split s #"[-_]")]
        (remove-special
         (clojure.string/join ""
                              (conj (map clojure.string/capitalize (rest tokens))
                                    (first tokens)))))))
  (is (= "foo" (java-fy "foo")))
  (is (= "fooBar" (java-fy "foo-bar")))
  (is (= "fooBar" (java-fy "foo_bar")))
  (is (= "shouldEndSession" (java-fy "should-end-session?"))))
