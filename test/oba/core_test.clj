(ns oba.core-test
  (:require [oba.core :refer :all]
            [oba.phrases :as phrases]
            [clojure.test :refer :all]
            [oba.utils :refer [same-shape?]]))

(declare a-request)

(deftest test-handle-launch-request
  (testing "We welcome the user when they open the skill"
    (let [launch-request (assoc a-request
                                :request
                                {:type "LaunchRequest"
                                 :request-id :irrelevant
                                 :timestamp :irrelevant})]
      (is (same-shape? (:response (on-request launch-request))
                       {:output-speech {:type "PlainText"
                                        :text phrases/modal-welcome-prompt}
                        :reprompt {:output-speech {:type "PlainText"
                                                   :text phrases/modal-welcome-reprompt}}
                        :should-end-session? false})))))

(deftest test-help-intent
  (testing "When a user asks for help, we tell them how to use the skill"
    (let [help-request (assoc a-request
                              :request
                              {:type "IntentRequest"
                               :intent {:name "AMAZON.HelpIntent"}})]
      (is (same-shape? (:response (on-request help-request))
                       {:output-speech {:type "PlainText"
                                        :text phrases/get-help-response}
                        :should-end-session? false})))))

(def a-request
  {:version "1",
   :session {:new false,
             :session-id "session-xyz",
             :application {:application-id "app-123"},
             :attributes {}},
   :request nil})
