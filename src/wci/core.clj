(ns wci.core
  "코어 엔진: (run config request [opts]) → 응답 봉투.

  어댑터는 원천을 읽어 :channel/answer 만 돌려준다. 나머지는 전부 여기 몫이다:
  요청 검증 → 채널 선택 → 시점 판정 → 어댑터 답 검증(:contract) → 술어 판정 →
  커버리지 유도(:ok/:empty/:excluded/:uninterpreted/:stale) → 전순서 정렬 →
  예산과 이어받기(:rest) → 토큰 → 봉투 → 불변식 I1–I9 자가검증.

  자가검증이 깨지면 그것은 응답이 아니라 코어의 버그다. ex-info {:wci/internal …}
  로 터진다. 잘못된 봉투를 내보내지 않는다."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [wci.adapter :as adapter]
            [wci.spec :as spec]
            [wci.time :as t]
            [wci.token :as token])
  (:import [clojure.lang ExceptionInfo]))

(def ^:private C (:constants spec/spec-map))

;; ───────────────────────── 직렬화와 크기 ─────────────────────────

(defn- json-escape [^StringBuilder sb ^String s]
  (.append sb \")
  (dotimes [i (count s)]
    (let [c (.charAt s i)]
      (case c
        \" (.append sb "\\\"")
        \\ (.append sb "\\\\")
        \newline (.append sb "\\n")
        \return (.append sb "\\r")
        \tab (.append sb "\\t")
        (if (< (int c) 0x20)
          (.append sb (format "\\u%04x" (int c)))
          (.append sb c)))))
  (.append sb \"))

(defn- json-write [^StringBuilder sb x]
  (cond
    (nil? x)        (.append sb "null")
    (boolean? x)    (.append sb (if x "true" "false"))
    (string? x)     (json-escape sb x)
    (keyword? x)    (json-escape sb (subs (str x) 1))
    (integer? x)    (.append sb (str x))
    (map? x)        (do (.append sb "{")
                        (loop [es (seq x) first? true]
                          (when-let [[k v] (first es)]
                            (when-not first? (.append sb ","))
                            (json-escape sb (if (keyword? k) (subs (str k) 1) (str k)))
                            (.append sb ":")
                            (json-write sb v)
                            (recur (next es) false)))
                        (.append sb "}"))
    (set? x)        (json-write sb (vec (sort-by #(if (keyword? %) (subs (str %) 1) (str %)) x)))
    (sequential? x) (do (.append sb "[")
                        (loop [xs (seq x) first? true]
                          (when xs
                            (when-not first? (.append sb ","))
                            (json-write sb (first xs))
                            (recur (next xs) false)))
                        (.append sb "]"))
    :else (throw (ex-info (str "cannot write " (type x) " as JSON") {:wci/internal :json}))))

(defn ->json
  "CLI 출력 직렬화. 예산은 이 바이트(UTF-8)로 잰다 (spec :default-max-bytes 주석).
  JVM 과 babashka 가 같은 바이트를 내도록 라이브러리 대신 여기서 쓴다:
  UTF-8 그대로, 이스케이프는 \" \\ 와 제어문자만, 집합은 정렬해서 배열로."
  ^String [x]
  (let [sb (StringBuilder.)]
    (json-write sb x)
    (.toString sb)))

(defn- byte-size ^long [x]
  (alength (.getBytes (->json x) "UTF-8")))

(defn- clip [s n]
  (let [s (str s)]
    (if (> (count s) n) (str (subs s 0 (dec n)) "…") s)))

;; ───────────────────────── 실패의 모양 ─────────────────────────

(defn- request-error!
  "채널과 무관한 요청 단위 실패. 에러 봉투로 간다 (:partial 없음)."
  [kind msg]
  (throw (ex-info msg {::request-error kind})))

(def ^:private error-kinds (spec/enum-values :wci/error-kind))

(defn- classify
  "어댑터가 던진 것 → :wci/error-kind. 계약 밖의 예외는 IO 면 :unreadable, 아니면 :contract."
  [^Throwable e]
  (let [k (when (instance? ExceptionInfo e) (:wci/error-kind (ex-data e)))]
    (cond
      (contains? error-kinds k) k
      (some? (:wci/time-invalid (ex-data e))) :contract
      (instance? java.io.IOException e) :unreadable
      :else :contract)))

(defn- failure [ch-id ^Throwable e]
  {:kind    (classify e)
   :channel ch-id
   :message (clip (or (ex-message e) (str (class e))) 300)})

(defn- contract! [msg]
  (adapter/fail! :contract msg))

;; ───────────────────────── 어댑터 호출 ─────────────────────────

(defn- ensure-kind! [kind]
  (when-not (contains? (methods adapter/describe) kind)
    (adapter/load-kind! kind)))

(defn- describe! [cfg]
  (ensure-kind! (:kind cfg))
  (let [d (adapter/describe cfg)]
    (when-not (spec/valid? :channel/describe d)
      (contract! (str "describe violates :channel/describe " (pr-str (spec/explain :channel/describe d)))))
    d))

(defn- edn-roundtrip? [k]
  (try (= k (edn/read-string (pr-str k))) (catch Exception _ false)))

(defn- canon
  "원천 키의 정규 문자열 — 전순서의 마지막 고리. 맵·집합은 정렬해 찍는다."
  [k]
  (letfn [(c [x]
            (cond
              (map? x)  (str "{" (str/join " " (sort (map (fn [[a b]] (str (c a) " " (c b))) x))) "}")
              (set? x)  (str "#{" (str/join " " (sort (map c x))) "}")
              (sequential? x) (str "[" (str/join " " (map c x)) "]")
              :else (pr-str x)))]
    (c k)))

(defn- check-times!
  "finding 안의 모든 시각을 엄격 파싱하고(I6), 지식 상한(as-of)을 넘는 기록이 없는지 본다."
  [as-of {:keys [times gist awaits refs]}]
  (let [before-cut! (fn [what v]
                      (when (and v (>= (:start (t/span v)) as-of))
                        (contract! (str what " " (pr-str v) " is not before as-of: knowledge cut leak"))))
        lm (:last-movement times)]
    (before-cut! "source" (:source times))
    (if (and (map? lm) (contains? lm :before))
      (t/point-start (:before lm))
      (before-cut! "last-movement" lm))
    (some-> (:expected times) :at t/span)
    (before-cut! "gist time" (:at gist))
    (before-cut! "awaits since" (:since awaits))
    (doseq [r refs
            :let [la (get-in r [:contact :last-active])]
            :when (map? la)]
      (t/parse-kst (:at la))
      (t/parse-kst (:observed-at la)))))

(defn- check-answer!
  "어댑터 답을 계약대로 검증한다. 위반은 :contract."
  [desc creq as-of ans]
  (when-not (spec/valid? :channel/answer ans)
    (contract! (str "answer violates :channel/answer " (pr-str (spec/explain :channel/answer ans)))))
  (let [{:keys [findings uninterpreted data-through]} ans]
    (when (and uninterpreted (seq findings))                                      ; I8
      (contract! "an uninterpreted answer carries findings"))
    (when (and (= :question (:mode creq)) (= :none (:question desc)) (not uninterpreted))
      (contract! "channel declares :question :none but interpreted a question"))
    (some-> data-through t/point-start)
    (doseq [f findings]
      (when-not (edn-roundtrip? (:key f))
        (contract! (str "finding key is not a plain EDN value: " (pr-str (:key f)))))
      (doseq [r (:refs f) :when (contains? r :key)]
        (when-not (edn-roundtrip? (:key r))
          (contract! (str "ref key is not a plain EDN value: " (pr-str (:key r))))))
      (check-times! as-of f))
    (let [ks (map (comp canon :key) findings)]
      (when-not (= (count ks) (count (distinct ks)))                               ; I7 전제
        (contract! "duplicate finding keys in one answer")))
    ans))

(defn- call-channel
  "어댑터 하나를 부르고 검증한다. {:desc :answer :read-at} 또는 {:failure …}."
  [cfg now-fn f]
  (try
    (let [desc (describe! cfg)
          res  (f desc)]
      (assoc res :desc desc :read-at (now-fn)))
    (catch Exception e
      {:failure (failure (:id cfg) e)})))

;; ───────────────────────── 시각 역할과 술어 ─────────────────────────

(defn- lm-bound
  "{:before p} 형태면 상한 ms."
  [f]
  (let [v (get-in f [:times :last-movement])]
    (when (and (map? v) (contains? v :before))
      (t/point-start (:before v)))))

(defn- role-key
  "판정·정렬 키(ms) 또는 nil. :last-movement 의 상한만 아는 값은 여기서 nil."
  [f role]
  (let [times (:times f)]
    (case role
      :expected      (some->> (get-in times [:expected :at]) (t/role-key :expected))
      :last-movement (let [v (:last-movement times)]
                       (when (and v (not (and (map? v) (contains? v :before))))
                         (t/role-key :last-movement v)))
      :source        (some->> (:source times) (t/role-key :source)))))

(defn- absence? [[role op]] (and (= role :last-movement) (= op :before)))

(defn- pred-role [[role]] role)

(defn- pred-match?
  "술어는 엄격한 <. [:last-movement :before -7] = last-movement < as-of−7d."
  [as-of f [role op arg]]
  (if (= role :awaits)
    (= arg (get-in f [:awaits :who]))
    (let [cut #(t/shift-days as-of %)
          k   (role-key f role)]
      (case op
        :within (let [[a b] arg] (boolean (and k (<= (cut a) k) (< k (cut b)))))
        :before (cond
                  k (< k (cut arg))
                  (= role :last-movement) (if-let [b (lm-bound f)] (<= b (cut arg)) false)
                  :else false)
        :after  (boolean (and k (>= k (cut arg))))))))

(defn- overview-match?
  "overview 의 뜻 (코어가 고정한다): horizon 안의 움직임(없으면 원 기록 시각),
  as-of 앞뒤 horizon 안의 예정, 또는 원천이 단언한 :awaits·:state·:gist.
  단언은 다음 단언으로 바뀌기 전까지 현재다 — horizon 과 무관하다 (v0.2.3)."
  [as-of h f]
  (let [lo (t/shift-days as-of (- h))
        hi (t/shift-days as-of h)
        mv (or (role-key f :last-movement) (role-key f :source))
        ex (role-key f :expected)]
    (boolean
     (or (some? (:awaits f)) (some? (:state f)) (some? (:gist f))
         (and mv (<= lo mv) (< mv as-of))
         (and ex (<= lo ex) (< ex hi))))))

;; ───────────────────────── 정렬 (I7) ─────────────────────────

(def ^:private tail [[:channel :asc] [:key :asc]])

(defn- dedupe-by-field [links]
  (reduce (fn [acc [f :as l]] (if (some #(= f (first %)) acc) acc (conj acc l))) [] links))

(defn order-for
  "요청 모드와 술어 역할에서 정렬 체인을 유도한다. 마지막 고리는 [:channel :key]."
  [mode where]
  (dedupe-by-field
   (case mode
     ;; v0.2.4: 오늘 움직인 영역이 먼저. [:area :asc] 는 같은 시각의 두 영역을 갈라 덩어리를 지킨다.
     :overview (into [[:area-movement :desc] [:area :asc] [:expected :asc] [:last-movement :desc] [:source :desc]] tail)
     :where    (-> (for [[role op] where :when (not= role :awaits)]
                     [role (cond (= role :expected) :asc
                                 (= op :before)     :asc
                                 :else              :desc)])
                   vec
                   (into [[:last-movement :desc] [:source :desc]])
                   (into tail))
     (into [[:last-movement :desc] [:source :desc]] tail))))

(defn- sort-val [{:keys [channel finding key-str] :as it} field]
  (case field
    :area-movement (:area-mv it)
    :area          (get-in finding [:area :key])
    :expected      (role-key finding :expected)
    :last-movement (or (role-key finding :last-movement) (lm-bound finding))
    :source        (role-key finding :source)
    :channel       channel
    :key           key-str))

(defn- sort-vals [order item]
  (mapv #(sort-val item (first %)) order))

(defn- cmp-vals
  "고리별 비교. 결측(nil)은 방향과 무관하게 그 고리의 맨 뒤."
  [order a b]
  (loop [i 0]
    (if (= i (count order))
      0
      (let [x (nth a i) y (nth b i) dir (second (nth order i))
            c (cond (and (nil? x) (nil? y)) 0
                    (nil? x) 1
                    (nil? y) -1
                    :else (cond-> (compare x y) (= dir :desc) -))]
        (if (zero? c) (recur (inc i)) c)))))

(defn- movement
  "한 finding 의 움직임 = max(last-movement(상한 포함), source). 없으면 nil."
  [f]
  (let [xs (keep identity [(role-key f :last-movement) (lm-bound f) (role-key f :source)])]
    (when (seq xs) (apply max xs))))

(defn- with-area-movement
  "v0.2.4 :area-movement — area 안의 최신 움직임. area 없는 finding 은 자기 움직임이 곧 그 값."
  [items]
  (let [by-area (reduce (fn [m {:keys [finding]}]
                          (let [k (get-in finding [:area :key]) mv (movement finding)]
                            (if (and k mv) (update m k #(if % (max % mv) mv)) m)))
                        {} items)]
    (mapv (fn [{:keys [finding] :as it}]
            (assoc it :area-mv (if-let [k (get-in finding [:area :key])] (get by-area k) (movement finding))))
          items)))

(defn- sort-items [order items]
  (let [keyed (mapv #(assoc % :sv (sort-vals order %)) (with-area-movement items))
        sorted (vec (sort #(cmp-vals order (:sv %1) (:sv %2)) keyed))]
    (doseq [[a b] (partition 2 1 sorted)]
      (when-not (neg? (cmp-vals order (:sv a) (:sv b)))
        (throw (ex-info "I7: sort is not a total order" {:wci/internal :I7 :a (:sv a) :b (:sv b)}))))
    sorted))

;; ───────────────────────── 토큰과 선 (wire) ─────────────────────────

(defn- encode-body
  "토큰 본문을 짧은 벡터로. 불투명하므로 모양은 코어만 안다 (v0.2.6 축소).
   ref  [0 채널 키]           live — 따라갈 때 지금 다시 읽는다
        [0 채널 키 as-of초]   과거 as-of — 그 시점으로 다시 세운다
   rest [1 [0 원요청] 커서 예산?]   또는   [1 [1 채널 키 as-of초?] 커서 예산?]
  정렬 체인은 싣지 않는다 — 원 요청에서 다시 유도된다."
  [{:keys [t c k a l src after b]}]
  (let [ref-v (fn [c k a l] (cond-> [c k] (not l) (conj (quot (long a) 1000))))]
    (case t
      :ref  (into [0] (ref-v c k a l))
      :rest (let [s (if-let [q (:q src)]
                      [0 q]
                      (let [{:keys [c k a l]} (:x src)] (into [1] (ref-v c k a l))))]
              (cond-> [1 s after] b (conj b))))))

(defn- decode-body [v]
  (let [ref-of (fn [[c k a-sec]] {:c c :k k :a (some-> a-sec (* 1000)) :l (nil? a-sec)})
        shaped? (fn [x tag lo hi] (and (vector? x) (= tag (first x)) (<= lo (count x) hi)))]
    (cond
      (shaped? v 0 3 4) (assoc (ref-of (subvec v 1)) :t :ref)
      (shaped? v 1 3 4) (let [[_ s after b] v]
                          {:t     :rest
                           :src   (cond (shaped? s 0 2 2) {:q (second s)}
                                        (shaped? s 1 3 4) {:x (ref-of (subvec s 1))}
                                        :else (request-error! :unknown-ref "unrecognised continuation"))
                           :after after
                           :b     b})
      :else (request-error! :unknown-ref "unrecognised reference body"))))

(defn- seal [ctx body]
  (let [tok (token/seal (:key ctx) (encode-body body) (+ (:now ctx) (:ttl-ms ctx)))]
    (when-not (spec/valid? :wci/ref-token tok)
      (throw (ex-info (str "sealed reference of " (count tok) " chars violates :wci/ref-token")
                      {:wci/internal :ref-token-size})))
    tok))

(defn- wire-ref [ctx {:keys [channel as-of live]} seed]
  (case (:role seed)
    (:record :contact) (assoc seed :terminal true)
    :same-as {:role :same-as :terminal false :label (:label seed) :channel (:channel seed)
              :ref (seal ctx {:t :ref :c (:channel seed) :k (:key seed) :a as-of :l live})}
    ;; :more/:history/:evidence/:owner — 시드에 :channel 이 있으면 같은 설정의 다른 채널로 건너간다
    (cond-> {:role (:role seed) :terminal false :label (:label seed)
             :ref (seal ctx {:t :ref :c (or (:channel seed) channel) :k (:key seed) :a as-of :l live})}
      (:channel seed) (assoc :channel (:channel seed)))))

(defn- wire-finding [ctx cut {:keys [channel finding]}]
  (cond-> {:channel channel
           :title   (:title finding)
           :times   (:times finding)
           :refs    (mapv #(wire-ref ctx (assoc cut :channel channel) %) (:refs finding))}
    (:gist finding)   (assoc :gist (:gist finding))
    (:awaits finding) (assoc :awaits (:awaits finding))
    (:state finding)  (assoc :state (:state finding))
    (:area finding)   (assoc :area (:area finding))
    (:by finding)     (assoc :by (:by finding))))

;; ───────────────────────── 봉투 ─────────────────────────

(defn- budget-of [budget]
  {:max-items (or (:max-items budget) (:default-max-items C))
   :max-bytes (min (or (:max-bytes budget) (:default-max-bytes C)) (:max-bytes-cap C))})

(defn- floor? [findings rest]
  (and (nil? rest) (not-any? #(false? (:terminal %)) (mapcat :refs findings))))

(defn- success-envelope
  "정렬된 items 에서 예산 안의 앞부분을 싣는다. 넘치면 :truncated + :rest (I1)."
  [ctx {:keys [as-of live observed coverage order items src budget]}]
  (let [{:keys [max-items max-bytes]} (budget-of budget)
        cut   {:as-of as-of :live live}
        total (count items)
        build (fn [n]
                (let [shown    (subvec items 0 n)
                      findings (mapv #(wire-finding ctx cut %) shown)
                      rest     (when (< n total)
                                 {:count (- total n)
                                  :ref   {:role :rest :terminal false
                                          :label (str (- total n) " more in this order")
                                          :ref (seal ctx {:t :rest :src src
                                                          :after (:sv (peek shown))
                                                          :b budget})}})]
                  (cond-> {:spec-version spec/version
                           :outcome      :success
                           :as-of        (t/format-kst as-of)
                           :observed-at  observed
                           :coverage     coverage
                           :order        order
                           :findings     findings
                           :floor        (floor? findings rest)
                           :truncated    (some? rest)}
                    rest (assoc :rest rest))))]
    (loop [n (min max-items total)]
      (let [env (build n)]
        (cond
          (and (<= (byte-size env) max-bytes) (or (pos? n) (zero? total))) env
          (pos? n) (recur (dec n))
          :else (request-error! :budget-too-small
                                (str "even an empty answer needs " (byte-size env)
                                     " bytes; max-bytes is " max-bytes)))))))

(defn- error-envelope
  "실패 목록은 자르지 않는다. :partial 만 예산에 맞춰 줄이고(:truncated 로 밝힌다),
  그래도 안 맞으면 통째로 뺀다."
  [ctx {:keys [as-of live observed failures retry partial budget order]}]
  (let [{:keys [max-items max-bytes]} (budget-of budget)
        base (cond-> {:spec-version spec/version
                      :outcome      :error
                      :observed-at  observed
                      :failures     (mapv #(update % :message clip 300) failures)}
               as-of (assoc :as-of (t/format-kst as-of))
               retry (assoc :retry retry))]
    (if-not partial
      base
      (let [cut   {:as-of as-of :live live}
            items (:items partial)
            with  (fn [n] (assoc base :partial {:coverage  (:coverage partial)
                                                :findings  (mapv #(wire-finding ctx cut %)
                                                                 (subvec items 0 n))
                                                :truncated (< n (count items))}))]
        (loop [n (min max-items (count items))]
          (let [env (with n)]
            (cond
              (<= (byte-size env) max-bytes) env
              (pos? n) (recur (dec n))
              :else base)))))))

(defn- request-error-envelope [ctx kind msg budget]
  (error-envelope ctx {:observed (t/format-kst (:now ctx))
                       :failures [{:kind kind :message msg}]
                       :budget   budget}))

;; ───────────────────────── 질의 (overview / ask / rest) ─────────────────────────

(defn- channel-ids [config] (mapv :id (:channels config)))

(defn- resolve-scope!
  "I5 와 시점. → {:as-of ms :live bool :selected #{ids}}"
  [ctx config {:keys [as-of include exclude]}]
  (let [known (set (channel-ids config))
        unknown (remove known (concat include exclude))]
    (when (seq unknown)
      (request-error! :bad-request (str "unknown channel id " (str/join ", " (distinct unknown)))))
    (when-let [both (seq (filter (set include) exclude))]
      (request-error! :bad-request (str "channel both included and excluded: " (str/join ", " both))))
    (let [now (:now ctx)
          at  (when as-of
                (try (t/point-start as-of)
                     (catch ExceptionInfo e (request-error! :bad-request (ex-message e)))))]
      (when (and at (> at now))
        (request-error! :bad-request "as-of is in the future; it is a knowledge cut, not a forecast"))
      {:as-of    (or at now)
       :live     (nil? at)
       :selected (set (remove (set exclude) (if include include (channel-ids config))))})))

(defn- check-where! [where]
  (doseq [[_ op arg :as p] where]
    (when (and (= op :within) (not (< (first arg) (second arg))))
      (request-error! :bad-request (str "empty :within range in " (pr-str p))))))

(defn- coverage-for
  "어댑터 답 + 모드 → [coverage-entry kept-findings]. 판정은 코어가 한다."
  [ch-id {:keys [desc answer read-at]} {:keys [mode where as-of]}]
  (let [{:keys [findings uninterpreted data-through matched-by]} answer
        base (cond-> {:channel ch-id :read-at (t/format-kst read-at)}
               data-through (assoc :data-through data-through))
        missing (when (= mode :where)
                  (seq (remove (:attests desc) (distinct (map pred-role where)))))
        stale-why (when (and (= mode :where) (some absence? where))
                    (cond
                      (nil? data-through)
                      "absence needs :data-through; the channel did not attest how far it reaches"
                      (< (:end (t/span data-through)) as-of)
                      (str "source reaches only " data-through "; cannot prove no movement up to as-of")))]
    (cond
      uninterpreted
      [(assoc base :status :uninterpreted :reason (:reason uninterpreted)) []]

      missing
      [(assoc base :status :uninterpreted
              :reason (clip (str "channel does not attest " (str/join ", " missing)) 200)) []]

      stale-why
      [(assoc base :status :stale :reason (clip stale-why 200)) []]

      :else
      (let [h    (:overview-horizon-days C)
            kept (case mode
                   :overview (filterv #(overview-match? as-of h %) findings)
                   :where    (filterv (fn [f] (every? #(pred-match? as-of f %) where)) findings)
                   findings)
            mb   (or matched-by
                     (case mode
                       :overview (str "overview: moved, expected within " h "d, or awaiting")
                       :where    (clip (str "where " (pr-str where)) 120)
                       :follow   "follow"
                       nil))]
        [(cond-> (assoc base :status (if (seq kept) :ok :empty))
           mb (assoc :matched-by (clip mb 120)))
         kept]))))

(defn- channel-request [{:keys [mode question where as-of live]}]
  (cond-> {:mode mode :as-of (t/format-kst as-of) :live live
           :horizon-days (:overview-horizon-days C)}
    question (assoc :question question)
    where    (assoc :where where)))

(defn- item
  "어댑터 finding → 내부 항목. 설정이 증언한 채널 :area 를 area 없는 finding 에 찍는다 (v0.2.3)."
  [cfg f]
  {:channel (:id cfg)
   :finding (if (and (:area cfg) (not (:area f))) (assoc f :area (:area cfg)) f)
   :key-str (canon (:key f))})

(defn- map-channels
  "채널마다 f 를 부른다. 병렬이 기본이다 — 결과의 순서와 내용은 설정 순서와 정렬이 정하므로
  순차와 같다. kind 구현은 먼저 이 스레드에서 싣는다 (동시 require 를 피한다)."
  [ctx f cfgs]
  (doseq [cfg cfgs] (try (ensure-kind! (:kind cfg)) (catch Exception _ nil)))
  (if (:parallel ctx)
    (mapv deref (mapv #(future (f %)) cfgs))
    (mapv f cfgs)))

(defn- fan-out
  "선택된 채널을 (병렬로) 부르고 설정 순서대로 모은다. → {:failures [...] :coverage [...] :items [...]}"
  [ctx config {:keys [selected live] :as plan}]
  (let [creq    (channel-request plan)
        chosen  (filterv #(contains? selected (:id %)) (:channels config))
        results (zipmap (map :id chosen)
                        (map-channels
                         ctx
                         (fn [cfg]
                           (call-channel
                            cfg #(:now ctx)
                            (fn [desc]
                              (when (and (not live) (= :live-only (:as-of-support desc)))
                                (adapter/fail! :unsupported-time
                                               "channel reads only the present; it cannot rebuild a past as-of"))
                              {:answer (check-answer! desc creq (:as-of plan) (adapter/answer cfg creq))})))
                         chosen))]
    (reduce
     (fn [acc cfg]
       (let [id (:id cfg)]
         (if-not (contains? selected id)
           (update acc :coverage conj {:channel id :status :excluded})
           (let [res (get results id)]
             (if-let [f (:failure res)]
               (-> acc (update :failures conj f) (update :failed conj id))
               (let [[cov kept] (coverage-for id res plan)]
                 (-> acc
                     (update :coverage conj cov)
                     (update :items into (map #(item cfg %))
                             kept))))))))
     {:failures [] :failed #{} :coverage [] :items []}
     (:channels config))))

(defn- strip-refs-into
  "I4: :partial 에 실패 채널로 건너가는 ref(:same-as, :channel 을 단 follow ref)를 남기지 않는다."
  [failed items]
  (mapv (fn [it] (update-in it [:finding :refs]
                            (fn [rs] (filterv #(not (failed (:channel %))) rs))))
        items))


(defn- label-attached-refs [config coverage as-of items]
  (let [ok (into #{} (keep #(when (= :ok (:status %)) (:channel %))) coverage)
        targets (into {} (map (juxt :id identity)) (filter #(ok (:id %)) (:channels config)))
        label (memoize (fn [seed]
                        (when-let [target (targets (:channel seed))]
                          (adapter/ref-label target seed {:as-of (t/format-kst as-of)}))))]
    (mapv (fn [item]
            (update-in item [:finding :refs]
                       (fn [refs] (mapv #(if-let [text (label %)] (assoc % :label text) %) refs))))
          items)))

(defn- after-cursor [order cursor items]
  (if cursor
    (filterv #(pos? (cmp-vals order (:sv %) cursor)) items)
    items))

(defn- retry-scope
  "v0.2.5 :retry — 다시 보낼 scope 전체. 원 scope 가 :include 를 썼으면 실패 채널을 거기서
  빼고, 아니면 :exclude 에 더한다. 읽을 채널이 하나도 안 남으면 retry 는 없다."
  [config scope dropped]
  (when (seq dropped)
    (let [scope (or scope {})
          gone  (set dropped)]
      (if-let [inc (:include scope)]
        (let [left (vec (remove gone inc))]
          (when (seq left) {:scope (assoc scope :include left)}))
        (let [ex   (vec (distinct (concat (:exclude scope) dropped)))
              left (remove (set ex) (channel-ids config))]
          (when (seq left) {:scope (assoc scope :exclude ex)}))))))

(defn- answer-query
  "plan = {:mode :question :where :as-of :live :selected :order :src :budget :after}"
  [ctx config plan]
  (let [{:keys [failures failed coverage items]} (fan-out ctx config plan)
        order    (:order plan)
        observed (t/format-kst (:now ctx))]
    (if (seq failures)
      (let [dropped (distinct (keep #(when (#{:unreadable :unsupported-time :contract} (:kind %))
                                       (:channel %)) failures))]
        (error-envelope ctx {:as-of    (:as-of plan) :live (:live plan) :observed observed
                             :failures failures
                             :retry    (retry-scope config (get-in plan [:src :q :scope]) dropped)
                             :partial  {:coverage coverage
                                        :items (sort-items order (strip-refs-into failed items))}
                             :budget   (:budget plan)}))
      (success-envelope ctx {:as-of (:as-of plan) :live (:live plan) :observed observed
                             :coverage coverage :order order
                             :items (label-attached-refs config coverage (:as-of plan)
                                                         (after-cursor order (:after plan) (sort-items order items)))
                             :src (:src plan) :budget (:budget plan)}))))

(defn- query-plan [ctx config {:keys [op question where scope budget]}]
  (when where (check-where! where))
  (let [mode (cond (= op :overview) :overview question :question :else :where)]
    (merge (resolve-scope! ctx config scope)
           {:mode mode :question question :where where
            :order (order-for mode where)
            :src {:q (cond-> {:op op} question (assoc :question question) where (assoc :where where)
                             scope (assoc :scope scope))}
            :budget budget})))

;; ───────────────────────── follow ─────────────────────────

(defn- expand-once
  "ref 하나를 펼친다. → {:env 에러봉투} 또는 {:as-of :live :coverage :items}."
  [ctx config {:keys [c k a l]} budget]
  (let [cfg (first (filter #(= c (:id %)) (:channels config)))]
    (when-not cfg
      (request-error! :forbidden "this reference points to a channel that is not attached here"))
    (let [as-of (if l (:now ctx) a)
          plan  {:mode :follow :as-of as-of :live (boolean l)}
          creq  (channel-request plan)
          res   (call-channel cfg #(:now ctx)
                              (fn [desc]
                                (when (and (not l) (= :live-only (:as-of-support desc)))
                                  (adapter/fail! :unsupported-time
                                                 "channel reads only the present; it cannot rebuild a past as-of"))
                                {:answer (check-answer! desc creq as-of (adapter/expand cfg k creq))}))]
      (if-let [f (:failure res)]
        {:env (error-envelope ctx {:as-of as-of :live l :observed (t/format-kst (:now ctx))
                                   :failures [f] :budget budget})}
        (let [[cov kept] (coverage-for c res plan)]
          {:as-of as-of :live (boolean l) :coverage [cov]
           :items (mapv #(item cfg %) kept)})))))

(defn- answer-expand [ctx config x order after budget]
  (let [{:keys [env as-of live coverage items]} (expand-once ctx config x budget)]
    (or env
        (success-envelope ctx {:as-of as-of :live live :observed (t/format-kst (:now ctx))
                               :coverage coverage :order order
                               :items (after-cursor order after (sort-items order items))
                               :src {:x x} :budget budget}))))

(defn- follow-rest
  "이어받기. 원래 요청을 다시 돌리고 커서 뒤부터 싣는다. 원래가 live 면 지금 다시 읽는다.
  정렬 체인은 원 요청에서 다시 유도한다 (토큰에 싣지 않는다)."
  [ctx config {:keys [src after b]} budget]
  (let [budget (or budget b)]
    (cond
      (:q src) (answer-query ctx config (merge (query-plan ctx config (:q src))
                                               {:after after :budget budget :src src}))
      (:x src) (answer-expand ctx config (:x src) (order-for :follow nil) after budget)
      :else    (request-error! :unknown-ref "unrecognised continuation"))))

(defn- follow [ctx config {:keys [ref budget]}]
  (let [body (decode-body
              (try (token/open (:key ctx) ref (:now ctx))
                   (catch ExceptionInfo e
                     (request-error! (:wci/error-kind (ex-data e)) (ex-message e)))))]
    (case (:t body)
      :ref  (answer-expand ctx config (select-keys body [:c :k :a :l]) (order-for :follow nil) nil budget)
      :rest (follow-rest ctx config body budget)
      (request-error! :unknown-ref "unrecognised reference body"))))

;; ───────────────────────── channels ─────────────────────────

(defn- list-channels [ctx config]
  (let [res (mapv vector (:channels config)
                  (map-channels ctx #(call-channel % (fn [] (:now ctx)) (constantly {})) (:channels config)))
        failures (keep (comp :failure second) res)
        observed (t/format-kst (:now ctx))]
    (if (seq failures)
      (error-envelope ctx {:observed observed :failures (vec failures)})
      {:spec-version spec/version
       :outcome      :success
       :observed-at  observed
       :channels     (mapv (fn [[cfg {:keys [desc]}]]
                             (cond-> {:channel          (:id cfg)
                                      :kind             (name (:kind cfg))
                                      :title            (or (:title cfg) (:title desc))
                                      :as-of-support    (:as-of-support desc)
                                      :question         (:question desc)
                                      :attests          (:attests desc)
                                      :contact-liveness (:contact-liveness desc)}
                               (:source-tz desc) (assoc :source-tz (:source-tz desc))))
                           res)})))

;; ───────────────────────── 자가검증 (I1–I9) ─────────────────────────
;; 명세 머리의 불변식과 1:1. 각 불변식이 어디서 지켜지는지:
;;   I1 truncated ⇔ rest        만들 때 success-envelope · 내보내기 전 verify!
;;   I2 finding 채널 = :ok 커버리지   만들 때 coverage-for · verify! (본 응답과 :partial 둘 다)
;;   I3 floor ⇒ non-terminal 없음   만들 때 floor? · verify!
;;   I4 partial ↛ 실패 채널        만들 때 strip-refs-into · verify!
;;   I5 include∩exclude, 모르는 id   요청 입구 resolve-scope! (:bad-request) — 응답에는 흔적이 없다
;;   I6 달력·구간                  어댑터 답 check-times! (:contract) · 요청 as-of (:bad-request) · verify! (선의 모든 시각)
;;   I7 전순서                     어댑터 답의 키 중복 (:contract) · sort-items (인접 비교) — 선에는 :key 가 없다
;;   I8 uninterpreted ⇒ 빈 findings 어댑터 답 check-answer! (:contract) · 선에서는 I2 가 덮는다
;;   I9 지식 상한                  어댑터 답 check-times! (:contract) · verify! (선의 finding)
;; verify! 가 터지면 응답이 아니라 코어 버그다 (CLI exit 4).

(defn- internal! [inv msg data]
  (throw (ex-info (str inv ": " msg) (assoc data :wci/internal inv))))

(defn- wire-times
  "선에 실린 finding 하나의 [이름 값] 시각들."
  [f]
  (let [{:keys [source last-movement expected]} (:times f)]
    (concat
     (when source [[:source source]])
     (cond (and (map? last-movement) (contains? last-movement :before)) [[:last-movement-bound (:before last-movement)]]
           last-movement [[:last-movement last-movement]])
     (when expected [[:expected (:at expected)]])
     (when-let [x (get-in f [:gist :at])] [[:gist x]])
     (when-let [x (get-in f [:awaits :since])] [[:awaits x]])
     (for [r (:refs f) :let [la (get-in r [:contact :last-active])] :when (map? la)
           x [(:at la) (:observed-at la)]]
       [:contact x]))))

(defn- verify-findings!
  "I2·I6·I9 를 finding 묶음 하나(본 응답 또는 :partial)에 적용한다."
  [as-of coverage findings]
  (let [ok  (set (keep #(when (= :ok (:status %)) (:channel %)) coverage))
        cut (some-> as-of t/parse-kst)]
    (doseq [c coverage :let [d (:data-through c)] :when d]
      (try (t/point-start d) (catch ExceptionInfo e (internal! :I6 (ex-message e) {}))))
    (doseq [f findings]
      (when-not (ok (:channel f))                                                  ; I2
        (internal! :I2 (str "finding from channel without :ok coverage: " (:channel f)) {}))
      (doseq [[role v] (wire-times f)]
        (let [{:keys [start]} (try (t/span v) (catch ExceptionInfo e (internal! :I6 (ex-message e) {})))]
          (when (and cut (#{:source :last-movement :gist :awaits} role) (>= start cut))   ; I9
            (internal! :I9 (str (name role) " " v " is not before as-of") {})))))))

(defn- verify!
  [resp]
  (when-not (spec/valid? :wci/response resp)
    (internal! :schema "response violates :wci/response" {:explain (spec/explain :wci/response resp)}))
  (doseq [v (concat [(:as-of resp) (:observed-at resp)]
                    (map :read-at (concat (:coverage resp) (get-in resp [:partial :coverage]))))
          :when v]
    (try (t/parse-kst v) (catch ExceptionInfo e (internal! :I6 (ex-message e) {}))))      ; I6
  (when (and (= :success (:outcome resp)) (contains? resp :findings))
    (when-not (= (:truncated resp) (contains? resp :rest))                              ; I1
      (internal! :I1 ":truncated must equal presence of :rest" {}))
    (verify-findings! (:as-of resp) (:coverage resp) (:findings resp))
    (when (and (:floor resp) (some #(false? (:terminal %)) (mapcat :refs (:findings resp)))) ; I3
      (internal! :I3 ":floor true with a non-terminal ref" {})))
  (when-let [p (:partial resp)]
    (verify-findings! (:as-of resp) (:coverage p) (:findings p))
    (let [failed (set (keep :channel (:failures resp)))]
      (doseq [f (:findings p)]                                                          ; I4
        (when (failed (:channel f))
          (internal! :I4 "partial carries a failed channel's finding" {}))
        (when (some #(failed (:channel %)) (:refs f))
          (internal! :I4 "partial carries a reference into a failed channel" {})))))
  resp)

;; ───────────────────────── 입구 ─────────────────────────

(defn- now-of
  "지금 = 초 단위로 내림한 ms. 선(wire)의 KST 는 초까지만 싣는다. 코어가 밀리초로 판정하면
  채널이 받은 as-of 와 코어가 판정한 as-of 가 달라진다 — 그 시각까지 읽은 채널이 :stale 로 보인다."
  [opts]
  (let [n (:now opts)
        ms (cond (nil? n)    (t/now-ms)
                 (int? n)    n
                 (string? n) (t/point-start n)
                 :else (throw (ex-info ":now must be KST text or epoch ms" {:wci/exit-code 3})))]
    (- ms (mod ms 1000))))

(defn context
  "config + opts → 실행 문맥. 키가 없으면 여기서 터진다 (기동 에러).
  opts: {:now KST|ms, :token-secret str, :fan-out :parallel|:sequential}
  — :now·:token-secret 은 테스트·재현용(운영은 env), :fan-out 기본 :parallel."
  [config opts]
  (let [key-env (get-in config [:token :key-env] token/default-key-env)
        ttl-h   (get-in config [:token :ttl-hours] token/default-ttl-hours)]
    {:now    (now-of opts)
     :key    (if-let [s (:token-secret opts)] (token/derive-key s) (token/key-from-env key-env))
     :ttl-ms (* (long ttl-h) 3600 1000)
     :parallel (not= :sequential (:fan-out opts))}))

(defn run
  "config(검증된 :channel/config) + request(:wci/request) → :wci/response.
  요청 위반·참조 문제는 에러 봉투로, 설정·키 문제는 ex-info(:wci/exit-code 3)로 간다."
  ([config request] (run config request {}))
  ([config request opts]
   (let [ctx (context config opts)]
     (verify!
      (try
        (when-not (spec/valid? :wci/request request)
          (request-error! :bad-request
                          (str "request violates :wci/request " (pr-str (spec/explain :wci/request request)))))
        (case (:op request)
          :channels (list-channels ctx config)
          :follow   (follow ctx config request)
          (answer-query ctx config (query-plan ctx config request)))
        (catch ExceptionInfo e
          (if-let [k (::request-error (ex-data e))]
            (request-error-envelope ctx k (ex-message e)
                                    (when (spec/valid? :wci/budget (:budget request)) (:budget request)))
            (throw e))))))))
