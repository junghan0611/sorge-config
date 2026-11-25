(ns wci.spec-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [wci.spec :as spec]))

(defn- inline-enum
  "registry 형태 안에서 [field [:enum …]] 의 값 집합을 찾는다."
  [k field]
  (let [found (atom nil)]
    (walk/postwalk (fn [x]
                     (when (and (vector? x) (= field (first x)) (vector? (last x)) (= :enum (first (last x))))
                       (reset! found (set (rest (last x)))))
                     x)
                   (spec/form k))
    @found))

(deftest every-registry-entry-compiles
  (doseq [k (keys (:registry spec/spec-map))]
    (is (some? (spec/schema k)) (str k))))

(deftest branches-and-enums-stay-in-step
  (testing "wire refs = channel ref seeds + the core-only :rest"
    (is (= (conj (spec/multi-keys :channel/ref-seed) :rest) (spec/multi-keys :wci/ref))))
  (testing "request ops"
    (is (= #{:channels :overview :ask :follow} (spec/multi-keys :wci/request))))
  (testing "channel request modes cover every caller mode"
    (is (= #{:overview :question :where :follow} (inline-enum :channel/request :mode))))
  (testing "a channel can attest every time role a predicate can name"
    (is (every? (spec/enum-values :wci/attest) (spec/enum-values :wci/time-role))))
  (testing "the order chain can name every time role plus the total-order tail"
    (is (= (into (spec/enum-values :wci/time-role) #{:area-movement :area :channel :key})
           (let [f (spec/form :wci/order)] (set (rest (second (nth f 2))))))))
  (testing "every error kind the core emits exists in the spec"
    (is (every? (spec/enum-values :wci/error-kind)
                #{:unreadable :unsupported-time :contract :unknown-ref :ref-expired :ref-gone
                  :forbidden :bad-request :budget-too-small}))))

(deftest validate!-raises
  (is (= {:op :channels} (spec/validate! :wci/request {:op :channels})))
  (let [e (try (spec/validate! :wci/request {:op :ask}) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (= :wci/request (:wci/schema (ex-data e))))
    (is (some? (:wci/explain (ex-data e))))))
