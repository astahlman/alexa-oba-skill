(ns oba.secret
  (:import [java.nio.ByteBuffer]
           [java.nio.file.Files]
           [java.io.File]
           [com.amazonaws.services.kms.AWSKMSClient]
           [com.amazonaws.services.kms.model.DecryptRequest]))

(def client (com.amazonaws.services.kms.AWSKMSClient.))
(.setEndpoint client "https://kms.us-west-2.amazonaws.com")

(def req (com.amazonaws.services.kms.model.DecryptRequest.))
(.setCiphertextBlob req (java.nio.ByteBuffer/wrap
                         (java.nio.file.Files/readAllBytes
                          (.toPath (java.io.File. "data/encrypted-api-key")))))

(defn fetch-api-key []
  (String. (.array (.getPlaintext (.decrypt client req)))))
