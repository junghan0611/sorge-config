(ns wci.adapter-exec-test
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [wci.adapter :as adapter]
            [wci.config :as config]
            [wci.core :as core])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private cfg-doc (delay (config/load-config "test/fixtures/exec_channel.edn")))
(def ^:private catalog-text (delay (slurp "test/fixtures/exec_catalog.json")))
(def ^:private live {:mode :overview :live true :as-of "2026-11-26T09:30:00+09:00" :horizon-days 14})

(defn- failure-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- fake-output! [program payload]
  ;; The synthetic executable, not WCI, supplies its own interpreter via its shebang.
  ;; printf receives a single literal, even for deliberately malformed JSON.
  (spit program (str "#!/bin/sh\nprintf '%s' '" (str/replace payload "'" "'\\''") "'\n")))

(defn- with-fake-cli [payload f]
  (let [program (.toFile (Files/createTempFile "wci-fake-cli-" ".sh" (make-array FileAttribute 0)))]
    (try
      (fake-output! program payload)
      (when-not (.setExecutable program true true) (throw (ex-info "cannot prepare fake CLI" {})))
      (f program [(str program) "--json"])
      (finally (Files/deleteIfExists (.toPath program))))))

(deftest external-json-is-a-thin-live-channel
  (with-fake-cli
   @catalog-text
   (fn [_ argv]
     (let [cfg (assoc-in (first (:channels @cfg-doc)) [:opts :argv] argv)
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
         (is (string? (get-in q [:uninterpreted :reason]))))))))

(deftest a-source-attested-same-as-link-crosses-channel-kinds
  (let [src {"through" "2026-11-26T00:30:00Z"
             "items" [{"id" "label-in-catalog" "labels" ["Fixture comparison sample"]
                       "recordedAt" "2026-11-25T16:00:00Z" "expectedOn" "2026-12-03" "phase" "READY"
                       "record" "https://catalog.invalid/samples/compare"
                       "origin" {"channel" "tracker" "key" "lamp/evidence"}}]}
        ref [:assemble {:role [:literal :same-as] :label [:literal "Open linked tracker evidence"]
                        :channel [:get ["origin" "channel"]] :key [:get ["origin" "key"]]}]
        opts {:now (:as-of live) :token-secret "test-secret-0123456789"}]
    (with-fake-cli
     (json/generate-string src)
     (fn [_ argv]
       (let [cfg (-> (config/load-config "channels.example.edn")
                     (update :channels conj (-> (first (:channels @cfg-doc))
                                                (assoc-in [:opts :argv] argv)
                                                (update-in [:opts :finding 1 :refs 1] conj ref)))
                     config/validate-config)
             overview (core/run cfg {:op :overview :scope {:include ["catalog"]}} opts)
             link (get-in overview [:findings 0 :refs 1])
             followed (core/run cfg {:op :follow :ref (:ref link)} opts)]
         (is (= [:same-as "tracker"] ((juxt :role :channel) link)))
         (is (= :success (:outcome followed)))
         (is (= ["tracker" "Comparison reading is ready for review"]
                ((juxt :channel :title) (first (:findings followed)))))
         (is (= [:record :contact] (mapv :role (get-in followed [:findings 0 :refs])))))))))

(deftest commented-issue-example-is-one-attachable-channel
  (let [issue (->> (str/split-lines (slurp "channels.example.edn"))
                   (keep #(second (re-matches #"^;;> (.*)$" %)))
                   (str/join "\n") edn/read-string)
        src {"issues" [{"number" 17 "title" "Review the sample's translation"
                        "createdAt" "2026-11-25T23:00:00Z"
                        "url" "https://issues.invalid/items/17"}]}]
    (with-fake-cli
     (json/generate-string src)
     (fn [_ argv]
       (let [cfg (-> {:channels [(assoc-in issue [:opts :argv] argv)]} config/validate-config :channels first)
             [row] (:findings (adapter/answer cfg live))]
         (is (= "synthetic-issues" (:id cfg)))
         (is (= 17 (:key row)))
         (is (= "Review the sample's translation" (:title row)))
         (is (= "2026-11-26T08:00:00+09:00" (get-in row [:times :source])))
         (is (= [:record "https://issues.invalid/items/17"] ((juxt :role :locator) (first (:refs row))))))))))

(deftest executable-and-mapping-declarations-are-closed
  (doseq [[k v] [[:argv "synthetic-reader --json"]
                 [:argv ["/bin/sh" "-c" "exit 0"]]
                 [:argv ["env" "bash"]]
                 [:argv ["synthetic-reader" (str "bad" \u0000 "argument")]]
                 [:new-field true]
                 [:finding [:assemble {:title [:regex "x"]}]]
                 [:finding (assoc-in (get-in @cfg-doc [:channels 0 :opts :finding]) [1 :title] [:regex "x"])]]]
    (testing (str k " rejected at config load")
      (is (= 3 (:wci/exit-code
                (failure-data #(config/validate-config (assoc-in @cfg-doc [:channels 0 :opts k] v)))))))))

(deftest flow-summary-is-one-line-with-bounded-live-history
  (let [row (fn [id at] {:id id :title (str "Source item " id) :at at})
        source [(row "latest" "2026-11-26T08:00:00+09:00")
                (row "old" "2026-11-12T09:29:59+09:00")
                (row "lower" "2026-11-12T09:30:00+09:00")
                (row "cut" (:as-of live))]
        config (fn [argv] {:channels [{:id "flow" :kind :exec :title "Synthetic flow"
                                      :opts {:argv argv :rows-path [] :attests #{:source} :overview :summary
                                             :finding [:assemble {:key [:get ["id"]] :title [:get ["title"]]
                                                                  :times [:assemble {:source [:kst [:get ["at"]]]}]
                                                                  :refs [:literal []]}]}}]})
        run-opts {:now (:as-of live) :token-secret "test-secret-0123456789"}]
    (with-fake-cli
     (json/generate-string source)
     (fn [program argv]
       (let [cfg (config/validate-config (config argv))
             r (core/run cfg {:op :overview} run-opts)
             [line] (:findings r)
             ref (get-in line [:refs 0 :ref])
             follow (core/run cfg {:op :follow :ref ref} run-opts)]
         (is (= [:success 1 "Synthetic flow"] [(:outcome r) (count (:findings r)) (:title line)]))
         (is (= "2 in the last 14 days (returned rows)" (get-in line [:gist :text])))
         (is (= {:last-movement "2026-11-26T08:00:00+09:00"} (:times line)))
         (is (= [:history false] ((juxt :role :terminal) (first (:refs line)))))
         (is (= ["Source item latest" "Source item lower" "Source item old"] (mapv :title (:findings follow))))
         (fake-output! program "[]")
         (let [empty-line (first (:findings (core/run cfg {:op :overview} run-opts)))]
           (is (= "0 in the last 14 days (returned rows)" (get-in empty-line [:gist :text])))
           (is (= {} (:times empty-line))))
         (fake-output! program (json/generate-string source))
         (let [channel (assoc-in (first (:channels cfg)) [:opts :finding 1 :times] [:literal {}])]
           (is (= :contract (:wci/error-kind (failure-data #(adapter/answer channel live)))))))))))

(deftest sender-projection-carries-only-the-display-name
  (let [src [{:id "a" :label "Sample" :origin {:person {:display "Casey Example" :address "hidden@example.invalid" :id "hidden-id"}}}
             {:id "b" :label "No author" :origin {:person {:display nil}}}
             {:id "c" :label "Missing author"}]
        display [:get ["origin" "person" "display"] :optional]
        finding [:assemble {:key [:get ["id"]] :title [:get ["label"]] :times [:literal {}] :refs [:literal []]
                            :by [:when-present display display]
                            :state [:literal {:text "Recorded" :asserted-by "Synthetic register"}]}]]
    (with-fake-cli
     (json/generate-string src)
     (fn [_ argv]
       (let [cfg (config/validate-config
                  {:channels [{:id "authored" :kind :exec
                               :opts {:argv argv :rows-path [] :attests #{:state} :finding finding}}]})
             r (core/run cfg {:op :overview} {:now (:as-of live) :token-secret "test-secret-0123456789"})
             rendered (core/->json r)]
         (is (= :success (:outcome r)))
         (is (= ["Casey Example"] (into [] (keep :by) (:findings r))))
         (is (not (str/includes? rendered "hidden@example.invalid")))
         (is (not (str/includes? rendered "hidden-id"))))))))

(deftest guards-project-only-source-authorized-fields
  (let [rows [{:id "missing"} {:id "null" :value nil} {:id "false" :value false}
              {:id "other" :value "accepted-state"} {:id "true" :value true :link "https://feed.invalid/item"}]
        value [:get ["value"] :optional]
        record [:assemble {:role [:literal :record] :label [:literal "Open item"]
                           :surface [:literal {:kind "Synthetic feed"}]
                           :locator [:get ["link"]]}]
        finding [:assemble {:key [:get ["id"]] :title [:literal "Synthetic item"] :times [:literal {}]
                            :state [:when-present value [:literal {:text "Value present" :asserted-by "Synthetic feed"}]]
                            :refs [:assemble [[:when-eq value true record]]]}]]
    (with-fake-cli
     (json/generate-string {:items rows})
     (fn [_ argv]
       (let [cfg (-> {:channels [{:id "guards" :kind :exec
                                  :opts {:argv argv :rows-path ["items"] :attests #{:state} :finding finding}}]}
                     config/validate-config :channels first)
             answer #(:findings (adapter/answer % live))
             projected (answer cfg)]
         (is (= [false false true true true] (mapv #(contains? % :state) projected)))
         (is (= [0 0 0 0 1] (mapv #(count (:refs %)) projected)))
         (is (= "https://feed.invalid/item" (get-in (last projected) [:refs 0 :locator])))
         (is (= [0 1 0 0 0]
                (mapv #(count (:refs %))
                      (answer (assoc-in cfg [:opts :finding 1 :refs 1 0]
                                        [:when-eq value nil [:literal {:role :record :label "Null marker"
                                                                      :surface {:kind "Synthetic feed"}
                                                                      :locator "synthetic:null"}]])))))
         (is (= :contract
                (:wci/error-kind (failure-data #(answer (assoc-in cfg [:opts :finding 1 :title]
                                                                  [:when-present value [:literal "Required title"]])))))))))))

(deftest malformed-or-incomplete-input-is-an-error-not-false-empty
  (let [source @catalog-text
        run #(failure-data (fn [] (adapter/answer % live)))]
    (with-fake-cli
     source
     (fn [program argv]
       (let [cfg (assoc-in (first (:channels @cfg-doc)) [:opts :argv] argv)]
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
             (fake-output! program bad)
             (is (= :contract (:wci/error-kind (run cfg))))))
         (fake-output! program source)
         (is (= :contract (:wci/error-kind (run (assoc-in cfg [:opts :attests] #{:source})))))
         (spit program "#!/bin/sh\nexit 7\n")
         (is (= :unreadable (:wci/error-kind (run cfg)))))))))
