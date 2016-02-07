(defproject oba "0.1.0"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/data.json "0.2.6"]
                 [com.amazonaws/aws-lambda-java-core "1.0.0"]
                 [com.amazonaws/aws-java-sdk-kms "1.10.59"]
                 [clj-http "2.0.1"]]
  :plugins [[lein-environ "1.0.0"]]
  :aot :all
  :profiles {:dev {:env {:squiggly {:checkers [:eastwood :kibit]}}}})

