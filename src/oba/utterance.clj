(ns oba.utterance
  (:require [clojure.test :refer :all]))

(defn- do-tokenize
  [{:keys [tokens cur s]}]
  (let [special {\[ {:type :alt-s :value "["}
                 \] {:type :alt-e :value "]"}
                 \( {:type :maybe-s :value "("}
                 \) {:type :maybe-e :value ")"}
                 \| {:type :delim :value "|"}}]
    (if-let [[c & xs] s]
      (if-let [token (get special c)]
        (do-tokenize {:tokens (if cur
                                (conj tokens (assoc cur :type :text) token)
                                (conj tokens token))
                      :cur nil
                      :s xs})
        (do-tokenize {:tokens tokens
                      :cur (update-in cur [:value] #(str % c))
                      :s xs}))
      (if cur
        (conj tokens (assoc cur :type :text))
        tokens))))

(with-test
  (defn- tokenize
    [s]
    (for [token (do-tokenize {:tokens [] :cur nil :s s})]
      (if (= :text (:type token))
        (update-in token [:value] clojure.string/trim)
        token)))
  (is (= [{:type :text :value "when is"} {:type :alt-s :value "["}
          {:type :text :value "the"} {:type :delim :value "|"}
          {:type :text :value "my"} {:type :alt-e :value "]"}
          {:type :text :value "next bus"} {:type :maybe-s :value "("}
          {:type :alt-s :value "["} {:type :text :value "arriving"}
          {:type :delim :value "|"} {:type :text :value "coming"}
          {:type :alt-e :value "]"} {:type :maybe-e :value ")"}]
         (tokenize "when is [the|my] next bus ([arriving|coming])")))
  (is (= [{:type :maybe-s :value "("}
          {:type :text :value "the"}
          {:type :maybe-e :value ")"}
          {:type :text :value "next bus"}]
         (tokenize "(the) next bus"))))

(defn- token->sym
  [t]
  (case (:type t)
    :text (str \" (:value t) \")
    :alt-s "(choice"
    :alt-e ")"
    :maybe-s "(maybe"
    :maybe-e ")"
    :delim ""))

(defn- build-s-exp [s]
  (load-string (str "["
                    (clojure.string/join " " (map token->sym (tokenize s)))
                    "]")))


(declare choice maybe)
(defn choice [& xs] (into [:choices] xs))
(defn maybe [& xs] (apply choice (conj [""] xs)))

;; Stolen from https://gist.github.com/bfoz/72e561a83e534f5aa078
(defn- expand-atom [atom prefix]
  (cond
    (sequential? (first prefix)) (map #(expand-atom atom %) prefix)
    (string? atom) (flatten (map #(str % " " atom) prefix))
    (= :choices (first atom)) (map #(expand-atom % prefix) (rest atom))
    (sequential? atom) (reduce #(expand-atom %2 %1) prefix atom)
    :else (throw (Exception. "Unexpected atom type"))))

(defn- gen-utterances [x]
  (map (comp #(clojure.string/replace % #"\s+" " ")
             clojure.string/trim)
       (flatten (expand-atom x [""]))))

(with-test
  (defn expand-utterance [s]
    (gen-utterances (build-s-exp s)))
  (let [next-bus-utterance "[When is|When's] [the|my] next ([northbound|eastbound|southbound|westbound]) [bus|{Route}] ([arriving|coming]) (at {Stop} (and {Stop}))"
        utterances (expand-utterance next-bus-utterance)]
    (is (some #(= % "When's my next eastbound {Route} at {Stop} and {Stop}") utterances))))
