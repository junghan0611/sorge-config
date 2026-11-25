(ns wci.adapter-semantic-memory-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [wci.adapter :as adapter]
            [wci.config :as config]
            [wci.core :as core])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private sample (delay (config/load-config "test/fixtures/semantic_channels.edn")))
(def ^:private query "concept with spaces and 'quotes' ; $(not a command)")
(def ^:private req {:mode :question :live true :as-of "2026-11-26T09:00:00+09:00"
                    :question query :horizon-days 14})
(def ^:private opts {:now (:as-of req) :token-secret "test-secret-0123456789"})
(def ^:private session-argv ["search-sessions" query "--limit" "5" "--source" "pi"])
(def ^:private garden-argv ["search-md" query "--limit" "5"])

(defn- shell-literal [s] (str "'" (str/replace s "'" "'\\''") "'"))

(defn- with-fake-cli [expected payload exit f]
  (let [program (.toFile (Files/createTempFile "wci-fake-semantic-" ".sh" (make-array FileAttribute 0)))]
    (try
      (spit program (str "#!/bin/sh\n"
                         "[ \"$ANDENKEN_DISABLE_RECALL_TRACKING\" = 1 ] || exit 98\n"
                         "[ \"$#\" = " (count expected) " ] || exit 97\n"
                         (apply str (map #(str "[ \"$1\" = " (shell-literal %) " ] || exit 96\nshift\n") expected))
                         "printf '%s' " (shell-literal (json/generate-string payload)) "\n"
                         "exit " exit "\n"))
      (when-not (.setExecutable program true true) (throw (ex-info "cannot prepare fake reader" {})))
      (f (str program))
      (finally (Files/deleteIfExists (.toPath program))))))

(defn- configured [index program]
  (-> {:channels [(assoc-in (get-in @sample [:channels index]) [:opts :program] program)]}
      config/validate-config))

(defn- ask [cfg] (core/run cfg {:op :ask :question query} opts))
(defn- fails-with [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:wci/error-kind (ex-data e)))))

(deftest hit-references-never-carry-excerpts-or-claims
  (doseq [[index argv payload expected] [[0 session-argv
                                          {:count 1 :fallback false
                                           :results [{:file "/synthetic/corpus/session.jsonl" :line 14 :source "pi"
                                                      :text "private fake excerpt" :score 0.9 :timestamp "query index timestamp"}]}
                                          ["/synthetic/corpus/session.jsonl:14"]]
                                         [1 garden-argv
                                          {:count 1 :results [{:file "/synthetic/garden/concept.md" :denoteId "20000101T000000"
                                                               :text "private fake excerpt" :score 0.9 :indexedAt "export mtime"}]}
                                          ["/synthetic/garden/concept.md" "20000101T000000"]]]]
    (with-fake-cli
     argv payload 0
     (fn [program]
       (let [cfg (configured index program)
             desc (adapter/describe (first (:channels cfg)))
             res (ask cfg)
             [row] (:findings res)]
         (is (= [:semantic :live-only] ((juxt :question :as-of-support) desc)))
         (is (= :success (:outcome res)))
         (is (= expected (mapv :locator (:refs row))))
         (is (every? #(= [:record true] ((juxt :role :terminal) %)) (:refs row)))
         (is (= {} (:times row)))
         (is (not (contains? row :gist)))
         (is (not (str/includes? (pr-str res) "private fake excerpt")))
         (is (not (contains? (first (:coverage res)) :data-through)))
         (is (str/starts-with? (get-in res [:coverage 0 :matched-by]) "semantic-memory ")))))))

(deftest the-configured-session-source-is-an-argv-constraint
  (with-fake-cli
   (assoc session-argv 5 "claude")
   {:count 1 :fallback false :results [{:file "/synthetic/claude/thread.jsonl" :line 4 :source "claude"}]} 0
   (fn [program]
     (let [cfg (config/validate-config
                (assoc-in (configured 0 program) [:channels 0 :opts :track :source] :claude))
           response (ask cfg)]
       (is (= :success (:outcome response)))
       (is (= "semantic-memory sessions axis (claude)" (get-in response [:coverage 0 :matched-by])))
       (is (= "/synthetic/claude/thread.jsonl:4" (get-in response [:findings 0 :refs 0 :locator])))))))

(deftest zero-and-reader-failure-are-separate-outcomes
  (with-fake-cli
   session-argv {:count 0 :fallback false :results []} 0
   (fn [program]
     (let [response (ask (configured 0 program))]
       (is (= :success (:outcome response)))
       (is (= :empty (get-in response [:coverage 0 :status]))))))
  (with-fake-cli
   garden-argv {:error "No md embedding provider available"} 1
   (fn [program]
     (let [response (ask (configured 1 program))]
       (is (= :error (:outcome response)))
       (is (= :unreadable (get-in response [:failures 0 :kind])))))))

(deftest refused-or-meaningless-requests-do-not-execute
  ;; The example program does not exist; no call can succeed or look empty by accident.
  (let [cfg (get-in @sample [:channels 0])]
    (is (= :bad-request (fails-with #(adapter/answer cfg (assoc req :question "--limit 100")))))
    (is (= :bad-request (fails-with #(adapter/answer cfg (assoc req :question "  --with-excerpt")))))
    (is (= :unsupported-time (fails-with #(adapter/answer cfg (assoc req :live false)))))
    (doseq [mode [:overview :where]]
      (is (string? (get-in (adapter/answer cfg (assoc req :mode mode)) [:uninterpreted :reason]))))))

(deftest fallback-and-mislabeled-session-results-fail-closed
  (doseq [[label payload] [["fallback claimed" {:count 0 :fallback true :results []}]
                           ["fallback unspecified" {:count 0 :results []}]
                           ["wrong harness" {:count 1 :fallback false
                                             :results [{:file "/synthetic/corpus/session.jsonl" :line 7 :source "claude"}]}]
                           ["non-line coordinate" {:count 1 :fallback false
                                                    :results [{:file "/synthetic/corpus/snapshot.sqlite" :line 7 :source "pi"}]}]]]
    (testing label
      (with-fake-cli
       session-argv payload 0
       (fn [program]
         (let [response (ask (configured 0 program))]
           (is (= :contract (get-in response [:failures 0 :kind])))))))))
