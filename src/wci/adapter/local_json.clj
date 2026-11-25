(ns wci.adapter.local-json
  "One bounded JSON document from a local reader; no raw payload in errors."
  (:require [cheshire.core :as json]
            [cheshire.factory :as json-factory]
            [wci.adapter :as adapter])
  (:import [java.io BufferedReader StringReader]))

(defn- check-depth! [^String text]
  ;; Limit recursive decoder depth without mistaking a quoted bracket for a new object.
  (loop [i 0, depth 0, quoted? false, escaped? false]
    (when (< i (.length text))
      (let [c (.charAt text i) next-i (inc i)]
        (cond
          escaped? (recur next-i depth quoted? false)
          (and quoted? (= \\ c)) (recur next-i depth true true)
          (= \" c) (recur next-i depth (not quoted?) false)
          quoted? (recur next-i depth true false)
          (or (= \[ c) (= \{ c))
          (do (when (>= depth 64) (adapter/fail! :contract "local JSON nesting exceeds its limit"))
              (recur next-i (inc depth) false false))
          (or (= \] c) (= \} c)) (recur next-i (dec depth) false false)
          :else (recur next-i depth false false))))))

(def ^:private strict-json
  (delay (json-factory/make-json-factory {:strict-duplicate-detection true})))

(defn parse-one [text]
  (check-depth! text)
  (try
    (binding [json-factory/*json-factory* @strict-json]
      (with-open [r (BufferedReader. (StringReader. text))]
        (let [values (seq (json/parsed-seq r))]
          (when (or (nil? values) (next values))
            (adapter/fail! :contract "local reader must emit exactly one JSON value"))
          (first values))))
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Exception _ (adapter/fail! :contract "local reader stdout is not valid JSON"))))
