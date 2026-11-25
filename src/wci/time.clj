(ns wci.time
  "KST 시간축. +09:00 고정이고 시스템 시간대(TZ)와 무관하다.

  내부 표현은 epoch millis(long). 문자열로 정렬하지 않는다.
  - KST 시각       → 순간 t.          span = [t, t]
  - 날짜 YYYY-MM-DD → 그날 하루 구간.  span = [00:00, 다음날 00:00)
  - {:from :to}    → 반개방 구간.     span = [start(from), start(to)), from < to
  역할별 정렬·판정 키: :expected 는 span 끝, 나머지는 span 시작 (spec :wci/order 주석).
  달력상 무효(2월 30일, 25시)는 ex-info {:wci/time-invalid v} 로 터진다 (I6)."
  (:import [java.time Instant LocalDate OffsetDateTime ZoneOffset]
           [java.time.format DateTimeFormatter DateTimeParseException ResolverStyle]))

(def ^ZoneOffset kst (ZoneOffset/ofHours 9))

(def ^:const day-ms 86400000)

(def ^:private kst-re #"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}(:\d{2})?\+09:00$")
(def ^:private date-re #"^\d{4}-\d{2}-\d{2}$")

(def ^:private ^DateTimeFormatter kst-parser
  (-> (DateTimeFormatter/ofPattern "uuuu-MM-dd'T'HH:mm[:ss]XXX")
      (.withResolverStyle ResolverStyle/STRICT)))

(def ^:private ^DateTimeFormatter date-parser
  (-> (DateTimeFormatter/ofPattern "uuuu-MM-dd")
      (.withResolverStyle ResolverStyle/STRICT)))

(def ^:private ^DateTimeFormatter kst-printer
  (DateTimeFormatter/ofPattern "uuuu-MM-dd'T'HH:mm:ssXXX"))

(defn- invalid! [v why]
  (throw (ex-info (str "invalid time " (pr-str v) ": " why)
                  {:wci/time-invalid v})))

(defn kst? [v] (and (string? v) (boolean (re-matches kst-re v))))
(defn date? [v] (and (string? v) (boolean (re-matches date-re v))))

(defn parse-kst
  "KST 시각 문자열 → epoch ms. 모양·달력·오프셋을 엄격히 본다."
  ^long [s]
  (when-not (kst? s) (invalid! s "not a KST (+09:00) timestamp"))
  (try
    (.toEpochMilli (.toInstant (OffsetDateTime/parse ^String s kst-parser)))
    (catch DateTimeParseException e
      (invalid! s (.getMessage e)))))

(defn parse-date
  "날짜 문자열 → 그날 00:00 KST 의 epoch ms."
  ^long [s]
  (when-not (date? s) (invalid! s "not a YYYY-MM-DD date"))
  (try
    (-> (LocalDate/parse ^String s date-parser)
        (.atStartOfDay kst)
        .toInstant
        .toEpochMilli)
    (catch DateTimeParseException e
      (invalid! s (.getMessage e)))))

(defn point-start
  "점(:wci/point) → 시작 ms. 날짜는 그날 00:00."
  ^long [p]
  (cond
    (kst? p)  (parse-kst p)
    (date? p) (parse-date p)
    :else     (invalid! p "not a point (KST timestamp or date)")))

(defn span
  "시각 값(:wci/when) → {:start ms :end ms}."
  [v]
  (cond
    (kst? v)  (let [t (parse-kst v)] {:start t :end t})
    (date? v) (let [d (parse-date v)] {:start d :end (+ d day-ms)})
    (and (map? v) (contains? v :from) (contains? v :to))
    (let [s (point-start (:from v))
          e (point-start (:to v))]
      (when-not (< s e) (invalid! v "interval needs from < to"))
      {:start s :end e})
    :else (invalid! v "not a time value")))

(defn role-key
  "정렬·판정 키. :expected 는 구간 끝(마감 03-09 = 그날 끝까지), 나머지는 구간 시작."
  ^long [role v]
  (let [{:keys [start end]} (span v)]
    (if (= role :expected) end start)))

(defn shift-days
  "ms 에 n 일을 더한다. KST 는 서머타임이 없어 하루 = 86400초."
  ^long [ms n]
  (+ (long ms) (* (long n) day-ms)))

(defn kst-day
  "KST 달력 날짜의 일련번호. 두 시각의 날짜 차(D-n)를 셀 때 쓴다."
  ^long [ms]
  (Math/floorDiv (+ (long ms) (* 9 3600 1000)) (long day-ms)))

(defn format-kst
  "epoch ms → \"YYYY-MM-DDTHH:mm:ss+09:00\"."
  [ms]
  (.format kst-printer (OffsetDateTime/ofInstant (Instant/ofEpochMilli ms) kst)))

(defn now-ms ^long [] (System/currentTimeMillis))
