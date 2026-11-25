(ns wci.adapter-exec-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [wci.adapter :as adapter]
            [wci.config :as config]
            [wci.core :as core])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private cfg-doc (delay (config/load-config "test/fixtures/exec_channel.edn")))
(def ^:private live {:mode :overview :live true :as-of "2026-11-26T09:30:00+09:00" :horizon-days 14})

(defn- failure-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(deftest external-json-is-a-thin-live-channel
  (let [cfg (first (:channels @cfg-doc))
        desc (adapter/describe cfg)
        answer (adapter/answer cfg live)
        [clip wheel] (:findings answer)]
    (is (= [:live-only :none #{:source :expected :state :awaits}]
           ((juxt :as-of-support :question :attests) desc)))
    (is (= "2026-11-26T09:00:00+09:00" (:data-through answer)))
    (is (= "Clip clearance sample" (:title clip)))
    (is (= "2026-11-26T08:30:00+09:00" (get-in clip [:times :source])))
    (is (= "2026-12-03" (get-in clip [:times :expected :at])))
    (is (= "Needs review" (get-in clip [:state :text])))
    (is (= :owner (get-in clip [:awaits :who])))
    (is (not (contains? wheel :awaits)))
    (is (= ["https://catalog.invalid/samples/clip"] (mapv :locator (:refs clip))))
    (is (not (str/includes? (pr-str answer) "Fixture-only text")))
    (is (= [clip] (:findings (adapter/expand cfg "fixture-clip" (assoc live :mode :follow)))))
    (is (= :ref-gone (:wci/error-kind (failure-data #(adapter/expand cfg "absent" (assoc live :mode :follow))))))
    (is (= :unsupported-time (:wci/error-kind (failure-data #(adapter/answer cfg (assoc live :live false))))))
    (let [q (adapter/answer cfg (assoc live :mode :question :question "clip"))]
      (is (= [] (:findings q)))
      (is (string? (get-in q [:uninterpreted :reason]))))))

(deftest a-source-attested-same-as-link-crosses-channel-kinds
  (let [file (.toFile (Files/createTempFile "wci-exec-link-" ".json" (make-array FileAttribute 0)))
        src {"through" "2026-11-26T00:30:00Z"
             "items" [{"id" "label-in-catalog", "labels" ["Fixture comparison sample"]
                       "recordedAt" "2026-11-25T16:00:00Z", "expectedOn" "2026-12-03", "phase" "READY"
                       "record" "https://catalog.invalid/samples/compare"
                       "origin" {"channel" "tracker", "key" "lamp/evidence"}}]}
        ref [:assemble {:role [:literal :same-as] :label [:literal "Open linked tracker evidence"]
                        :channel [:get ["origin" "channel"]] :key [:get ["origin" "key"]]}]
        cfg (-> (config/load-config "channels.example.edn")
                (update :channels conj (-> (first (:channels @cfg-doc))
                                           (assoc-in [:opts :argv] ["cat" "--" (str file)])
                                           (update-in [:opts :finding 1 :refs 1] conj ref)))
                config/validate-config)
        opts {:now (:as-of live) :token-secret "test-secret-0123456789"}]
    (try
      (spit file (json/generate-string src))
      (let [overview (core/run cfg {:op :overview :scope {:include ["catalog"]}} opts)
            link (get-in overview [:findings 0 :refs 1])
            followed (core/run cfg {:op :follow :ref (:ref link)} opts)]
        (is (= [:same-as "tracker"] ((juxt :role :channel) link)))
        (is (= :success (:outcome followed)))
        (is (= ["tracker" "Comparison reading is ready for review"]
               ((juxt :channel :title) (first (:findings followed)))))
        (is (= [:record :contact] (mapv :role (get-in followed [:findings 0 :refs])))))
      (finally (Files/deleteIfExists (.toPath file))))))

(deftest executable-and-mapping-declarations-are-closed
  (doseq [[k v] [[:argv "cat -- fixture.json"]
                 [:argv ["/bin/sh" "-c" "exit 0"]]
                 [:argv ["env" "bash"]]
                 [:argv ["cat" (str "bad" \u0000 "argument")]]
                 [:new-field true]
                 [:finding [:assemble {:title [:regex "x"]}]]
                 [:finding (assoc-in (get-in @cfg-doc [:channels 0 :opts :finding]) [1 :title] [:regex "x"])]]]
    (testing (str k " rejected at config load")
      (is (= 3 (:wci/exit-code
                (failure-data #(config/validate-config (assoc-in @cfg-doc [:channels 0 :opts k] v)))))))))

(deftest malformed-or-incomplete-input-is-an-error-not-false-empty
  (let [file (.toFile (Files/createTempFile "wci-exec-" ".json" (make-array FileAttribute 0)))
        cfg (assoc-in (first (:channels @cfg-doc)) [:opts :argv] ["cat" "--" (str file)])
        source (slurp "test/fixtures/exec_catalog.json")
        run #(failure-data (fn [] (adapter/answer % live)))]
    (try
      (doseq [[label bad] [["trailing document" (str source " {}")]
                           ["non-JSON" "plain text"]
                           ["duplicate JSON key" (str/replace source "\"items\": [" "\"items\": [], \"items\": [")]
                           ["duplicate finding key" (str/replace source "fixture-wheel" "fixture-clip")]
                           ["missing required row" (str/replace source "\"labels\"" "\"elsewhere\"")]
                           ["unknown enum" (str/replace source "WAITING" "UNKNOWN")]
                           ["invalid calendar" (str/replace source "2026-12-03" "2026-02-30")]
                           ["explicit null" (str/replace source "\"CHECK\"" "null")]
                           ["excess nesting" (str (apply str (repeat 65 "[")) (apply str (repeat 65 "]")))]]]
        (testing label
          (spit file bad)
          (is (= :contract (:wci/error-kind (run cfg))))))
      (spit file source)
      (is (= :contract (:wci/error-kind (run (assoc-in cfg [:opts :attests] #{:source})))))
      (finally (Files/deleteIfExists (.toPath file))))
    (is (= :unreadable (:wci/error-kind (run cfg))))))
