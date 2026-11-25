(ns wci.core-test
  "코어를 어댑터 없이 검증한다. :stub kind 는 설정 :opts 의 함수로 답을 지어낸다."
  (:require [clojure.test :refer [deftest is testing]]
            [wci.adapter :as adapter]
            [wci.core :as core]
            [wci.spec :as spec]
            [wci.time :as t]))

(def now "2026-11-26T09:00:00+09:00")
(def opts {:now now :token-secret "test-secret-0123456789"})

(def base-desc {:kind "stub" :title "Stub" :as-of-support :replay :question :keyword
                :attests #{:source :last-movement :expected :awaits :area :gist :state}
                :contact-liveness :none})

(defmethod adapter/describe :stub [cfg] (merge base-desc (get-in cfg [:opts :describe])))
(defmethod adapter/answer :stub [cfg req] ((get-in cfg [:opts :answer]) req))
(defmethod adapter/expand :stub [cfg k req] ((or (get-in cfg [:opts :expand])
                                               (fn [_ _] (adapter/fail! :ref-gone "gone"))) k req))

(defn- ch
  ([id findings] (ch id findings {}))
  ([id findings extra]
   (merge-with merge
               {:id id :kind :stub
                :opts {:answer (fn [_] {:findings findings :data-through "2026-12-31"})}}
               extra)))

(defn- run [channels req] (core/run {:channels channels} req opts))

(defn- f [k & {:as m}]
  (merge {:key k :title (str "Item " k)
          :times {:source "2026-11-20T10:00+09:00" :last-movement "2026-11-24T10:00+09:00"}
          :refs []}
         m))

(defn- status [resp id] (:status (first (filter #(= id (:channel %)) (:coverage resp)))))
(defn- kinds [resp] (mapv :kind (:failures resp)))

(deftest excluded-empty-and-ok-are-different
  (let [a (ch "a" [(f "x")]) b (ch "b" [])]
    (is (= [:ok :empty] [(status (run [a b] {:op :overview}) "a") (status (run [a b] {:op :overview}) "b")]))
    (is (= :excluded (status (run [a b] {:op :overview :scope {:exclude ["b"]}}) "b")))
    (is (= :excluded (status (run [a b] {:op :overview :scope {:include ["a"]}}) "b")))))

(deftest a-failed-channel-is-an-error-with-bounded-partial
  (let [a (ch "a" [(f "x" :refs [{:role :same-as :label "same work in b" :channel "b" :key "y"}
                                 {:role :more :label "more" :key "x/more"}])])
        b (ch "b" nil {:opts {:answer (fn [_] (throw (java.io.IOException. "disk gone")))}})
        r (run [a b] {:op :overview})]
    (is (= :error (:outcome r)))
    (is (= [:unreadable] (kinds r)))
    (is (= {:exclude ["b"]} (:retry r)))
    (testing "I4: no finding or same-as reference into the failed channel"
      (is (= ["a"] (mapv :channel (get-in r [:partial :findings]))))
      (is (= [:more] (mapv :role (get-in r [:partial :findings 0 :refs])))))
    (testing "every failure is listed, not just the first"
      (let [c (ch "c" nil {:opts {:answer (fn [_] (adapter/fail! :unreadable "no"))}})]
        (is (= [:unreadable :unreadable] (kinds (run [a b c] {:op :overview}))))
        (is (= {:exclude ["b" "c"]} (:retry (run [a b c] {:op :overview}))))))))

(deftest adapter-contract-violations-are-errors
  (let [bad (fn [answer] (kinds (run [(ch "a" nil {:opts {:answer (constantly answer)}})] {:op :overview})))]
    (is (= [:contract] (bad {:nope 1})) "shape")
    (is (= [:contract] (bad {:findings [(f "x" :times {:source "2026-02-30T10:00+09:00"})]})) "calendar")
    (is (= [:contract] (bad {:findings [(f "x" :times {:source "2026-11-26T09:00+09:00"})]})) "knowledge cut")
    (is (= [:contract] (bad {:findings [(f "x")] :uninterpreted {:reason "r"}})) "I8")
    (is (= [:contract] (bad {:findings [(f "x") (f "x")]})) "duplicate keys")
    (is (= [:contract] (bad {:findings [(f (Object.))]})) "non-EDN key")
    (is (= [:contract] (kinds (run [(ch "a" nil {:opts {:answer (fn [_] (throw (RuntimeException. "bug")))}})]
                                   {:op :overview}))) "unclassified throw")
    (is (= [:contract] (kinds (run [(ch "a" [] {:opts {:describe {:question :none}}})]
                                   {:op :ask :question "x"}))) ":question :none yet interpreted")))

(deftest uninterpreted-when-the-role-is-not-attested
  (let [r (run [(ch "a" [(f "x")] {:opts {:describe {:attests #{:source}}}})]
               {:op :ask :where [[:expected :within [0 14]]]})]
    (is (= :uninterpreted (status r "a")))
    (is (re-find #":expected" (get-in r [:coverage 0 :reason])))
    (is (= [] (:findings r)))))

(deftest stale-sources-cannot-prove-absence
  (let [old  {:findings [(f "x" :times {:last-movement "2026-11-01T10:00+09:00"})]}
        q    {:op :ask :where [[:last-movement :before -7]]}
        with (fn [dt] (run [(ch "a" nil {:opts {:answer (constantly (cond-> old dt (assoc :data-through dt)))}})] q))]
    (is (= :stale (status (with "2026-11-20T00:00+09:00") "a")))
    (is (= :stale (status (with nil) "a")) "no :data-through, no absence claim")
    (is (= :ok (status (with "2026-11-26") "a")) "a day reaches its end")
    (is (= :ok (status (with "2026-11-26T09:00+09:00") "a")))))

(deftest predicates-are-strict-and-day-aware
  (let [items [(f "edge" :times {:last-movement "2026-11-19T09:00:00+09:00"})
               (f "in" :times {:last-movement "2026-11-19T08:59:59+09:00"})
               (f "bound" :times {:last-movement {:before "2026-11-19"}})
               (f "due-last" :times {:expected {:at "2026-12-09" :asserted-by "s"}})
               (f "due-out" :times {:expected {:at "2026-12-10" :asserted-by "s"}})
               (f "overdue" :times {:expected {:at "2026-11-25" :asserted-by "s"}})]
        titles (fn [where] (set (map :title (:findings (run [(ch "a" items)] {:op :ask :where where})))))]
    (is (= #{"Item in" "Item bound"} (titles [[:last-movement :before -7]])))
    (is (= #{"Item due-last"} (titles [[:expected :within [0 14]]])))
    (is (= #{"Item overdue"} (titles [[:expected :within [-7 0]]])))))

(deftest time-and-request-errors
  (let [live-only (ch "a" [(f "x")] {:opts {:describe {:as-of-support :live-only}}})]
    (is (= [:unsupported-time] (kinds (run [live-only] {:op :overview :scope {:as-of "2026-11-20"}}))))
    (is (= :ok (status (run [live-only] {:op :overview}) "a"))))
  (let [a (ch "a" [])]
    (doseq [[why req] {"future as-of"     {:op :overview :scope {:as-of "2026-11-27"}}
                       "calendar as-of"   {:op :overview :scope {:as-of "2026-02-30"}}
                       "unknown channel"  {:op :overview :scope {:include ["zz"]}}
                       "include∩exclude"  {:op :overview :scope {:include ["a"] :exclude ["a"]}}
                       "both question and where" {:op :ask :question "q" :where [[:awaits :is :human]]}
                       "neither"          {:op :ask}
                       "empty within"     {:op :ask :where [[:expected :within [5 5]]]}}]
      (is (= [:bad-request] (kinds (run [a] req))) why))))

(deftest budget-continuation-is-a-total-order
  (let [items (for [i (range 5)] (f (str "k" i) :times {:last-movement "2026-11-24T10:00+09:00"}))
        a (ch "a" (vec (take 3 items))) b (ch "b" (vec (drop 3 items)))
        full (run [a b] {:op :overview :budget {:max-items 50}})
        page (fn [r] [(mapv (juxt :channel :title) (:findings r)) r])
        [p1 r1] (page (run [a b] {:op :overview :budget {:max-items 2}}))
        [p2 r2] (page (run [a b] {:op :follow :ref (get-in r1 [:rest :ref :ref])}))
        [p3 r3] (page (run [a b] {:op :follow :ref (get-in r2 [:rest :ref :ref])}))]
    (is (= [true 3] [(:truncated r1) (get-in r1 [:rest :count])]) "I1")
    (is (= (mapv (juxt :channel :title) (:findings full)) (concat p1 p2 p3)) "no overlap, no skip")
    (is (= [false nil true] [(:truncated r3) (:rest r3) (:floor r3)]))
    (testing "ties break by channel then key"
      (is (= [["a" "Item k0"] ["a" "Item k1"]] p1)))))

(deftest byte-budget-and-too-small
  (let [fat (fn [k] (f k :refs (vec (for [i (range 3)] {:role :more :label (str "more " i) :key [k i]}))))
        r   (run [(ch "a" (mapv fat ["p" "q" "r"]))] {:op :overview :budget {:max-bytes 2400}})]
    (is (<= (count (.getBytes (core/->json r) "UTF-8")) 2400))
    (is (:truncated r))
    (is (pos? (count (:findings r))))
    (is (= [:budget-too-small]
           (kinds (run [(ch "a" (mapv fat ["p"]))] {:op :overview :budget {:max-bytes 512}}))))))

(deftest follow-ends-and-reference-errors
  (let [leaf {:key "leaf" :title "Leaf" :times {:source "2026-11-20"}
              :refs [{:role :record :label "open" :surface {:kind "issue"} :locator "https://x.invalid/1"}
                     {:role :contact :label "ask owner"
                      :contact {:address {:kind :repo-owner :repo "~/repos/gh/sample"}}}]}
        a (ch "a" [(f "x" :refs [{:role :more :label "deeper" :key "leaf"}])]
              {:opts {:expand (fn [k _] (if (= k "leaf") {:findings [leaf]} (adapter/fail! :ref-gone "gone")))}})
        r   (run [a] {:op :overview})
        tok (get-in r [:findings 0 :refs 0 :ref])
        d   (run [a] {:op :follow :ref tok})]
    (is (false? (:floor r)))
    (is (= [:record :contact] (mapv :role (get-in d [:findings 0 :refs]))))
    (is (every? :terminal (get-in d [:findings 0 :refs])))
    (is (true? (:floor d)) "only terminal refs below")
    (is (= [:unknown-ref] (kinds (run [a] {:op :follow :ref (str "x" (subs tok 1))}))))
    (is (= [:ref-expired] (kinds (core/run {:channels [a]} {:op :follow :ref tok}
                                           (assoc opts :now (t/shift-days (t/parse-kst now) 4))))))
    (is (= [:forbidden] (kinds (run [(assoc a :id "renamed")] {:op :follow :ref tok}))))
    (let [gone (ch "a" [(f "x" :refs [{:role :more :label "deeper" :key "missing"}])]
                   {:opts {:expand (fn [_ _] (adapter/fail! :ref-gone "gone"))}})
          t2 (get-in (run [gone] {:op :overview}) [:findings 0 :refs 0 :ref])]
      (is (= [:ref-gone] (kinds (run [gone] {:op :follow :ref t2})))))))

(deftest channels-lists-capabilities
  (let [r (run [(ch "a" []) (ch "b" [] {:opts {:describe {:as-of-support :live-only}}})] {:op :channels})]
    (is (spec/valid? :wci/channels r))
    (is (= [:replay :live-only] (mapv :as-of-support (:channels r)))))
  (is (= [:contract] (kinds (run [(ch "a" [] {:opts {:describe {:question :maybe}}})] {:op :channels})))))

(deftest missing-key-is-a-start-error
  (is (= 3 (try (core/run {:channels [] :token {:key-env "WCI_TEST_SURELY_UNSET_9F3A"}} {:op :channels} {:now now})
                nil
                (catch clojure.lang.ExceptionInfo e (:wci/exit-code (ex-data e)))))))
