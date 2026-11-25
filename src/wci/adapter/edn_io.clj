(ns wci.adapter.edn-io
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn read-one
  "Read one EDN value. Do not accept trailing forms or executable reader syntax."
  [source]
  (with-open [r (java.io.PushbackReader. (io/reader source :encoding "UTF-8"))]
    (let [end (Object.)
          opts {:eof end}
          value (edn/read opts r)]
      (when (or (identical? end value) (not (identical? end (edn/read opts r))))
        (throw (ex-info "expected exactly one EDN value" {})))
      value)))
