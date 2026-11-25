(ns wci.conformance-test
  "첫 조각의 적합성: 성격이 다른 합성 채널 둘(T=구조형 트래커 :replay, X=시각 찍힌 노트
  :live-only·:question :none·:source 만 증언)을 같은 호출자가 같은 계약으로 부른다.
  사례 여섯 + 종단 walk. 판정은 전부 코어 몫이고, 어댑터는 원천만 읽는다."
  (:require [clojure.test :refer [deftest is testing]]
            [wci.client :as w]
            [wci.config :as config]
            [wci.core :as core]
            [wci.spec :as spec]))

(def now "2026-11-26T09:00:00+09:00")
(def cfg (delay (config/load-config "channels.example.edn")))
(defn- c ([] (c @cfg)) ([conf] (w/connect {:config conf :now now :token-secret "conformance-secret-0123"})))
(defn- status [r id] (:status (first (filter #(= id (:channel %)) (or (:coverage r) (get-in r [:partial :coverage]))))))
(defn- titles [r] (mapv :title (:findings r)))

(deftest case-1-overview-from-empty-hands
  (let [r (w/ok! (w/overview (c)))]
    (is (spec/valid? :wci/success r))
    (is (= [:ok :ok] [(status r "tracker") (status r "notes")]))
    (testing "grouped by attested area, then by what is due first"
      (is (= ["Bench lamp trial" "Dial calibration follow-up" "Hinge fit follow-up"] (take 3 (titles r)))))
    (testing "the notes channel contributes on its own terms (source time only)"
      (is (some #(= "notes" (:channel %)) (:findings r))))
    (is (<= (count (.getBytes (core/->json r) "UTF-8")) (:default-max-bytes spec/constants)))))

(deftest case-2-due-in-14-days-and-still-for-7
  (let [r (w/ok! (w/ask (c) [[:expected :within [0 14]] [:last-movement :before -7]]))]
    (is (= ["Bench lamp trial" "Dial calibration follow-up" "Hinge fit follow-up"] (titles r))
        "strict <: the 09:00:00 cutoff and the 12-10 due boundary stay out")
    (is (= [[:expected :asc] [:last-movement :asc]] (take 2 (:order r))))
    (testing "a channel that cannot attest due dates says so; it is not 'no matches'"
      (is (= :uninterpreted (status r "notes")))
      (is (re-find #"does not attest" (:reason (second (:coverage r))))))))

(deftest case-3-excluded-is-not-empty-is-not-uninterpreted
  (let [r (w/ok! (w/ask (c) "no such term" {:exclude ["notes"]}))]
    (is (= [:empty :excluded] [(status r "tracker") (status r "notes")]))
    (is (= "title keyword" (:matched-by (first (:coverage r))))))
  (is (= :uninterpreted (status (w/ok! (w/ask (c) "lamp")) "notes"))))

(deftest case-4-unreadable-is-an-error-with-bounded-partial-evidence
  (let [broken (assoc-in @cfg [:channels 1 :opts :fail] :unreadable)
        r (w/overview (c broken))]
    (is (= :error (:outcome r)))
    (is (= [{:kind :unreadable :channel "notes"}] (mapv #(select-keys % [:kind :channel]) (:failures r))))
    (is (= {:exclude ["notes"]} (:retry r)))
    (is (every? #(= "tracker" (:channel %)) (get-in r [:partial :findings])))
    (is (false? (get-in r [:partial :truncated])) "all tracker evidence fits the default budget")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unreadable notes" (w/ok! r)))
    (testing "the retry it suggests succeeds"
      (is (= :success (:outcome (w/overview (c broken) {:exclude ["notes"]})))))))

(deftest case-5-past-as-of-is-rebuilt-or-refused
  (let [r (w/overview (c) {:as-of "2026-11-19T08:59:59+09:00"})]
    (is (= [:unsupported-time] (mapv :kind (:failures r))) "notes cannot rebuild the past"))
  (let [r (w/ok! (w/overview (c) {:as-of "2026-11-19T08:59:59+09:00" :include ["tracker"]}))
        lamp (first (filter #(= "Bench lamp trial" (:title %)) (:findings r)))]
    (is (= "2026-11-19T08:59:59+09:00" (:as-of r)))
    (is (= "The trial is waiting for its first measurements." (get-in lamp [:gist :text]))
        "the view as it was known then, rebuilt from source history")
    (is (nil? (get-in lamp [:times :expected])))))

(deftest case-6-budget-continuation-without-overlap
  (let [cl   (c)
        full (titles (w/ok! (w/overview cl {:max-items 50})))
        p1   (w/ok! (w/overview cl {:max-items 2}))
        pages (loop [r p1 acc [(titles p1)]]
                (if-let [rest-ref (:rest r)]
                  (let [n (w/ok! (w/follow cl rest-ref))] (recur n (conj acc (titles n))))
                  acc))]
    (is (true? (:truncated p1)))
    (is (= (- (count full) 2) (get-in p1 [:rest :count])))
    (is (= full (vec (apply concat pages))))
    (is (every? #(<= (count %) 2) pages))))

(deftest walk-from-an-item-to-its-record-and-its-owner
  (let [cl (c)
        r  (w/ok! (w/ask cl "bench lamp"))
        {:keys [ends stopped calls]} (w/walk cl r {:max-calls 4})]
    (is (= :floor stopped))
    (is (= 2 calls) "history, then evidence")
    (is (= #{:record :contact} (set (map :role ends))))
    (is (= {:via :entwurf-v2 :target "20000101T000000-abcdef" :intent "fire-and-forget"}
           (select-keys (w/contact-call (first (filter #(= :contact (:role %)) ends)))
                        [:via :target :intent])))
    (is (some #(re-find #"Bench lamp trial · D-7 · moved 7d ago · awaits human" %) (w/lines r)))))
