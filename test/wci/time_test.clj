(ns wci.time-test
  (:require [clojure.test :refer [deftest is testing]]
            [wci.time :as t])
  (:import [java.util TimeZone]))

(defn- invalid? [f]
  (try (f) false (catch clojure.lang.ExceptionInfo e (contains? (ex-data e) :wci/time-invalid))))

(deftest strict-calendar
  (doseq [bad ["2026-02-30T10:00+09:00" "2026-11-26T25:00+09:00" "2026-11-26T10:61+09:00"
               "2026-11-26T10:00:60+09:00" "2026-11-26T10:00+00:00" "2026-11-26 10:00+09:00"]]
    (is (invalid? #(t/parse-kst bad)) bad))
  (is (invalid? #(t/parse-date "2026-02-29")))
  (is (= (t/parse-date "2028-02-29") (t/parse-kst "2028-02-29T00:00+09:00")))
  (is (= (t/parse-kst "2026-11-26T09:00+09:00") (t/parse-kst "2026-11-26T09:00:00+09:00"))))

(deftest days-are-intervals
  (let [d (t/parse-date "2026-12-03")]
    (is (= {:start d :end (+ d t/day-ms)} (t/span "2026-12-03")))
    (testing ":expected keys on the end of its day, other roles on the start"
      (is (= (+ d t/day-ms) (t/role-key :expected "2026-12-03")))
      (is (= d (t/role-key :source "2026-12-03")))
      (is (= d (t/role-key :last-movement "2026-12-03")))))
  (testing "intervals are half-open and need from < to"
    (is (= {:start (t/parse-date "2026-03-01") :end (t/parse-date "2026-03-03")}
           (t/span {:from "2026-03-01" :to "2026-03-03"})))
    (is (invalid? #(t/span {:from "2026-03-01" :to "2026-03-01"})))
    (is (invalid? #(t/span {:from "2026-03-02T10:00+09:00" :to "2026-03-02T09:00+09:00"})))))

(deftest printing-and-shifting
  (is (= "2026-11-26T09:00:00+09:00" (t/format-kst (t/parse-kst "2026-11-26T09:00+09:00"))))
  (is (= (t/parse-kst "2026-11-19T09:00+09:00")
         (t/shift-days (t/parse-kst "2026-11-26T09:00+09:00") -7))))

(deftest the-shell-time-zone-does-not-change-answers
  (let [probe    (fn [] [(t/parse-kst "2026-11-26T00:30+09:00") (t/parse-date "2026-11-26")
                         (t/format-kst 1795651200000) (t/role-key :expected "2026-11-26")])
        expected [1795620600000 1795618800000 "2026-11-26T09:00:00+09:00" 1795705200000]]
    (is (= expected (probe)))
    ;; babashka 는 기본 시간대를 바꿀 수 없다. JVM 에서만 셸 TZ 를 돌려 본다.
    (when-not (System/getProperty "babashka.version")
      (let [original (TimeZone/getDefault)]
        (try
          (doseq [z ["UTC" "Asia/Seoul" "Pacific/Kiritimati" "America/Los_Angeles"]]
            (TimeZone/setDefault (TimeZone/getTimeZone ^String z))
            (is (= expected (probe)) z))
          (finally (TimeZone/setDefault original)))))))
