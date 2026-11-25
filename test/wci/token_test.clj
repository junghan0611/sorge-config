(ns wci.token-test
  (:require [clojure.test :refer [deftest is testing]]
            [wci.token :as token]))

(def k (token/derive-key "test-secret-0123456789"))

(defn- kind-of [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:wci/error-kind (ex-data e)))))

(deftest round-trip
  (let [body {:t :ref :c "tracker" :k {:path "a/b" :n 3} :a 1 :l true}
        tok  (token/seal k body 2000)]
    (is (= body (token/open k tok 1999)))
    (testing "opaque: two seals of one body differ (random IV)"
      (is (not= tok (token/seal k body 2000))))))

(deftest forged-tampered-foreign-and-expired
  (let [tok (token/seal k {:x 1} 2000)
        flip (fn [s i] (str (subs s 0 i) (if (= \A (nth s i)) \B \A) (subs s (inc i))))]
    (is (= :unknown-ref (kind-of #(token/open k (flip tok 20) 1))))
    (is (= :unknown-ref (kind-of #(token/open (token/derive-key "another-secret-0123456") tok 1))))
    (is (= :unknown-ref (kind-of #(token/open k "!!!not-a-token!!!" 1))))
    (is (= :unknown-ref (kind-of #(token/open k "AAAA" 1))))
    (testing "expiry is checked only after authenticity"
      (is (= :ref-expired (kind-of #(token/open k tok 2000))))
      (is (= :unknown-ref (kind-of #(token/open k (flip tok 20) 5000)))))))

(deftest keys-come-from-the-environment-or-fail-at-start
  (let [exit-of (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:wci/exit-code (ex-data e)))))]
    (is (= 3 (exit-of #(token/derive-key "short"))))
    (is (= 3 (exit-of #(token/key-from-env "WCI_TEST_SURELY_UNSET_9F3A"))))))
