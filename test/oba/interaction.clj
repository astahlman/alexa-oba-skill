(ns oba.interaction
  (:require [clojure.test :refer :all]
            [oba.core :as core]))

(def request-template
  {:session
   {:session-id "SessionId.11a77ba8-c371-4346-ad23-26c2dc27bc0a"
    :application {:application-id "amzn1.echo-sdk-ams.app.a6b43718-8307-479b-aed0-b167961c2fec"}
    :user {:user-id "amzn1.echo-sdk-account.AEAQDAXYVRZ2O5O46F4F4FJJU3S3IUNLO4OAVKBAYH3KC2OIF56QA"}
    :new true}
   :request
   {:type "IntentRequest"
    :request-id "EdwRequestId.c9071dd1-7975-4737-badd-c39cf0a34b93"
    :timestamp "2016-02-27T18:21:45Z"
    :intent {:name "NextBusIntent"
             :slots {:street-two {:name "StreetTwo"}
                     :direction {:name "Direction"}
                     :street-one {:name "StreetOne"}
                     :route {:name "Route"}}}}})

(def next-bus-intent {:name "NextBusIntent"
                      :slots {:street-two {:name "StreetTwo"}
                              :direction {:name "Direction"}
                              :street-one {:name "StreetOne"}
                              :route {:name "Route"}}})

(def supply-route-intent {:name "SupplyRouteIntent"
                          :slots {:route {:name "Route"
                                          :value "40"}}})

(defn supply-stop-intent
  [street-1 street-2]
  {:name "SupplyStopIntent"
   :slots {:street-one {:name "StreetOne"
                        :value street-1}
           :street-two {:name "StreetTwo"
                        :value street-2}}})

(def supply-direction-intent {:name "SupplyDirectionIntent"
                              :slots {:direction {:name "Direction"
                                                  :value "east"}}})

(defn user-
  ([s]
   (case s
     "When is the next bus?" (assoc-in
                              request-template
                              [:request :intent]
                              next-bus-intent)
     "When is the next forty?" (assoc-in
                                request-template
                                [:request :intent]
                                (assoc-in next-bus-intent
                                          [:slots :route :value]
                                          "40"))
     "When is the next forty at leary and 15th?" (assoc-in
                                                  request-template
                                                  [:request :intent]
                                                  (-> next-bus-intent
                                                      (assoc-in [:slots :route :value]
                                                                "40")
                                                      (assoc-in [:slots :street-one :value]
                                                                "leary")
                                                      (assoc-in [:slots :street-two :value]
                                                                "15th")))
     "forty" (assoc-in request-template
                       [:request :intent]
                       supply-route-intent)
     "leary and 15th" (assoc-in request-template
                                [:request :intent]
                                (supply-stop-intent "leary" "15th"))
     "westlake and aloha" (assoc-in request-template
                                    [:request :intent]
                                    (supply-stop-intent "westlake" "aloha"))
     "east" (assoc-in request-template
                      [:request :intent]
                      supply-direction-intent))))

(defn check-response [request response-checker]
  (let [response (core/on-request request)]
    (let [actual (get-in response [:response :output-speech :text])
          {:keys [ok error]} (response-checker actual)]
      (if ok
        {:ctx (get response :sessionAttributes)}
        {:error error}))))

(defn test-interactions
  "Accepts a list, elements alternate between a request object and a
  function to check the response"
  [interactions]
  (loop [{:keys [interactions ctx]}
         {:interactions interactions
          :ctx nil}]
    (let [[req response-checker & xs] interactions]
      (cond
        (and req response-checker)
        (let [req (assoc-in req [:session :attributes] ctx)]
          (let [{:keys [ctx error]} (check-response req response-checker)]
            (if-not error
              (recur {:interactions xs
                      :ctx ctx})
              error)))
        (or req response-checker)
        (throw (Exception. "test-interaction expects even number of forms"))
        :else true))))

(defprotocol ResponseChecker
  (alexa- [spec]))

(extend-type java.lang.String
  ResponseChecker
  (alexa- [s]
    (fn [actual]
      (if (= s actual)
        {:ok true}
        {:error
         (str "Expected '" s "'; Got: '" actual "'")}))))

(extend-type java.util.regex.Pattern
  ResponseChecker
  (alexa- [re]
    (fn [actual]
      (if (re-matches re actual)
        {:ok true}
        {:error
         (str "Expected a string matching pattern '" re "'; Got: '" actual "'")}))))

(testing "We can look up an arrival starting with no slots filled"
  (test-interactions
   [(user- "When is the next bus?")
    (alexa- "Which route should I look up?")
    (user- "forty")
    (alexa- "Which stop on route 40?")
    (user- "leary and 15th")
    (alexa- "Which direction are you going: east or west?")
    (user- "east")
    (alexa- #"^The next bus will arrive in \d+ minutes.( There are also buses arriving in \d+( and \d+){0,1} minutes){0,1}$")]))

(testing "We can look up an arrival starting with only the route"
  (test-interactions
   [(user- "When is the next forty?")
    (alexa- "Which stop on route 40?")
    (user- "leary and 15th")
    (alexa- "Which direction are you going: east or west?")
    (user- "east")
    (alexa- #"^The next bus will arrive in \d+ minutes.( There are also buses arriving in \d+( and \d+){0,1} minutes){0,1}$")]))

(testing "We can look up an arrival starting with the route and stop"
  (test-interactions
   [(user- "When is the next forty at leary and 15th?")
    (alexa- "Which direction are you going: east or west?")
    (user- "east")
    (alexa- #"^The next bus will arrive in \d+ minutes.( There are also buses arriving in \d+( and \d+){0,1} minutes){0,1}$")]))

(testing "If the route has only one stop at the given streets, don't ask the direction"
  (test-interactions
   [(user- "When is the next forty?")
    (alexa- "Which stop on route 40?")
    (user- "westlake and aloha")
    (alexa- #"^The next bus will arrive in \d+ minutes.( There are also buses arriving in \d+( and \d+){0,1} minutes){0,1}$")]))
