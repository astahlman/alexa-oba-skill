(ns oba.utils
  (:require [clojure.test :refer :all]))

(defn same-shape? [actual expected]
  (cond (= actual expected) true
        (and (map? actual) (map? expected))
        (every? true?
                (for [k-exp (keys expected)]
                  (let [v-exp (get expected k-exp)
                        v-act (get actual k-exp)]
                    (same-shape? v-act v-exp))))))

;; This doesn't handle every case, e.g., checking the shape of a map
;; which is an element of any collection other than a map
(testing "Meta-test of the same-shape? function"
  (is (same-shape? {:a [1 2]
                    :c {:d "f"}
                    :e nil}
                   {:a [1 2]
                    :c {:d "f"}}))
  (is (same-shape? {:a {:b 1 :c {:d 1}
                        :e "this key not checked"}}
                   {:a {:b 1 :c {:d 1}}}))
  (is (same-shape? {:a {:b 1 :c #{{:a 1} {:b 2}}
                        :e "this key not checked"}}
                   {:a {:b 1 :c #{{:a 1} {:b 2}}}}))
  (is (not (same-shape? {:a [1 2]
                         :c {:d "f"}
                         :e nil}
                        {:a [1 2]
                         :c {:d "f"}
                         :e 5})))
  (is (not (same-shape? {:a 1}
                        {:a 3})))
  (is (not (same-shape? {:a 13}
                        {:b 13}))))
