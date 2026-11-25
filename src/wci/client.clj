(ns wci.client
  "code mode 표면. 에이전트가 짧은 스크립트로 부르는 함수들이다. 전부 in-process.

    (require '[wci.client :as w])
    (def c (w/connect))                                   ; 키·설정이 없으면 여기서 터진다
    (-> (w/overview c) w/ok! w/lines)                     ; 빈손 시작 → 한 줄씩
    (-> (w/ask c [[:expected :within [0 14]] [:last-movement :before -7]]) w/ok! w/lines)
    (w/walk c (first (w/refs r :history)) {:max-calls 4}) ; 고리를 따라 종단(record/contact)까지
    (map w/contact-call (:ends *1))                       ; 보낼 호출의 인자만. 보내지 않는다

  에러 봉투는 ok! 가 예외로 바꾼다. 스크립트는 :outcome 을 확인하지 않아도 된다."
  (:require [clojure.string :as str]
            [wci.core :as core]
            [wci.time :as t]))

(defn connect
  "설정과 키를 지금 확인한다. opts:
   :config        경로 또는 이미 검증된 설정 map (기본 ./channels.local.edn)
   :now           고정 시각 (KST 문자열 또는 ms) — 재현·테스트용
   :token-secret  키 문자열 — 테스트용. 운영은 설정의 :token :key-env 환경변수"
  ([] (connect {}))
  ([{:keys [config now token-secret] :or {config "channels.local.edn"}}]
   (let [cfg  (if (map? config) config ((requiring-resolve 'wci.config/load-config) config))
         opts (cond-> {} now (assoc :now now) token-secret (assoc :token-secret token-secret))]
     (core/context cfg opts)
     {:config cfg :opts opts})))

(defn- run [c req] (core/run (:config c) req (:opts c)))

(defn- scope-and-budget [{:keys [as-of include exclude max-items max-bytes]}]
  (let [scope  (cond-> {} as-of (assoc :as-of as-of)
                       include (assoc :include (vec include)) exclude (assoc :exclude (vec exclude)))
        budget (cond-> {} max-items (assoc :max-items max-items) max-bytes (assoc :max-bytes max-bytes))]
    (cond-> {} (seq scope) (assoc :scope scope) (seq budget) (assoc :budget budget))))

(defn token-of
  "토큰 문자열, ref map, 또는 :rest 묶음 → 토큰. 종단 ref 는 따라갈 수 없어 터진다."
  [x]
  (cond
    (string? x) x
    (and (map? x) (string? (:ref x))) (:ref x)
    (and (map? x) (map? (:ref x))) (token-of (:ref x))
    (and (map? x) (:terminal x))
    (throw (ex-info (str "terminal " (name (:role x)) " reference: nothing further through this interface")
                    {:ref x}))
    :else (throw (ex-info "not a reference" {:value x}))))

;; ───────────────────────── 네 동사 ─────────────────────────

(defn channels [c] (run c {:op :channels}))

(defn overview
  "빈손 시작. opts: :as-of :include :exclude :max-items :max-bytes"
  ([c] (overview c {}))
  ([c opts] (run c (merge {:op :overview} (scope-and-budget opts)))))

(defn ask
  "q 가 문자열이면 자유 질문(채널이 해석), 벡터면 역할 술어(코어가 판정)."
  ([c q] (ask c q {}))
  ([c q opts]
   (run c (merge {:op :ask}
                 (if (string? q) {:question q} {:where (vec q)})
                 (scope-and-budget opts)))))

(defn follow
  "ref(또는 토큰, :rest 묶음)를 한 단계 따라간다. opts: :max-items :max-bytes"
  ([c r] (follow c r {}))
  ([c r opts]
   (run c (merge {:op :follow :ref (token-of r)}
                 (select-keys (scope-and-budget opts) [:budget])))))

;; ───────────────────────── 다루기 ─────────────────────────

(defn ok!
  "에러 봉투면 ex-info 로 터진다 (ex-data :wci/response 에 봉투 전체). 아니면 그대로."
  [r]
  (if (= :error (:outcome r))
    (throw (ex-info (str "wci error: "
                         (str/join "; " (map #(str (name (:kind %))
                                                   (when (:channel %) (str " " (:channel %)))
                                                   ": " (:message %))
                                             (:failures r))))
                    {:wci/response r}))
    r))

(defn refs
  "응답의 ref 들. 각 ref 에 :of (어느 finding 제목) 를 붙인다. :rest 도 포함.
  role 은 키워드 하나 또는 집합."
  ([r] (refs r nil))
  ([r role]
   (let [want? (cond (nil? role) any? (set? role) role :else #{role})
         all   (concat (for [f (:findings r) x (:refs f)] (assoc x :of (:title f)))
                       (when-let [x (get-in r [:rest :ref])] [(assoc x :of "rest")]))]
     (filterv #(want? (:role %)) all))))

(defn- days-between [from-ms to-ms]
  (long (Math/floor (/ (double (- to-ms from-ms)) t/day-ms))))

(defn- age-bits [as-of {:keys [times awaits state]}]
  (let [ex (some-> (get-in times [:expected :at]) t/span :start)
        lm (:last-movement times)
        lm-ms (cond (nil? lm) nil
                    (and (map? lm) (:before lm)) nil
                    :else (t/role-key :last-movement lm))
        src (some->> (:source times) (t/role-key :source))]
    (cond-> []
      ex    (conj (let [d (- (t/kst-day ex) (t/kst-day as-of))]
                    (cond (pos? d) (str "D-" d) (zero? d) "D-day" :else (str "D+" (- d)))))
      lm-ms (conj (str "moved " (days-between lm-ms as-of) "d ago"))
      (and (map? lm) (:before lm)) (conj (str "moved before " (:before lm)))
      (and (nil? lm) src) (conj (str "recorded " (days-between src as-of) "d ago"))
      awaits (conj (str "awaits " (name (:who awaits))))
      state  (conj (:text state)))))

(defn lines
  "응답 → 사람이 읽는 한 줄들. 나이는 응답의 :as-of 에서 유도한다.
  D-n 은 KST 달력 날짜 차(마감 12-03, 기준 11-26 → D-7), 'n일 전'은 경과 시간의 내림.
  읽지 못한 채널·해석 못 한 채널·멈춘 채널도 한 줄씩 남긴다. 조용히 빠지지 않는다."
  [r]
  (let [as-of (some-> (:as-of r) t/parse-kst)
        body  (or (when (= :error (:outcome r)) (:partial r)) r)
        cov   (for [c (:coverage body) :when (not (#{:ok} (:status c)))]
                (str "· " (:channel c) ": " (name (:status c)) (when (:reason c) (str " — " (:reason c)))))
        line  (fn [f]
                (str (:title f)
                     (when as-of
                       (let [bits (age-bits as-of f)] (when (seq bits) (str " · " (str/join " · " bits)))))
                     " · " (:channel f)
                     (when-let [g (get-in f [:gist :text])] (str " — " g))))
        ;; 소속(area)이 바뀌는 자리에 머리줄 하나. 같은 리포의 카드와 커밋이 한 덩어리로 읽힌다.
        fl    (second
               (reduce (fn [[prev out] f]
                         (let [a (:area f)]
                           [a (cond-> out
                                (and a (not= (:key a) (:key prev))) (conj (str "▸ " (:label a)))
                                true (conj (line f)))]))
                       [nil []] (:findings body)))]
    (vec (concat
          (when (= :error (:outcome r))
            (for [f (:failures r)]
              (str "ERROR " (name (:kind f)) (when (:channel f) (str " " (:channel f))) ": " (:message f))))
          fl
          (when-let [n (get-in r [:rest :count])] [(str "… " n " more (follow the :rest ref)")])
          (when (and (:truncated r) (nil? (get-in r [:rest :count]))) ["… more (follow the :rest ref)"])
          cov
          (when (:floor r) ["(floor: nothing further below in this scope)"])))))

(def default-walk-roles #{:more :history :evidence :owner :same-as})

(defn walk
  "start(응답 또는 ref) 에서 non-terminal ref 를 깊이 우선으로 따라간다.
  채널을 건너는 :same-as 도 따른다. record/contact 종단은 :ends 로 모은다.
  각 단계는 ok! 를 거친다 — 에러면 그 자리에서 터진다.
  opts: :max-calls (기본 6)  :roles (기본 default-walk-roles)  :until (fn [response] → 멈춤?)
  → {:steps [{:via ref :response r}] :ends [ref …] :calls n :stopped :floor|:max-calls|:until}"
  ([c start] (walk c start {}))
  ([c start {:keys [max-calls roles until] :or {max-calls 6 roles default-walk-roles}}]
   (let [expand (fn [r] (let [rs (refs r)]
                          [(filterv #(and (false? (:terminal %)) (roles (:role %))) rs)
                           (filterv :terminal rs)]))
         [stack0 ends0] (if (:outcome start) (expand (ok! start)) [[start] []])]
     (loop [stack (vec (reverse stack0)) steps [] ends ends0 calls 0]
       (cond
         (empty? stack)
         {:steps steps :ends ends :calls calls :stopped :floor}

         (>= calls max-calls)
         {:steps steps :ends ends :calls calls :stopped :max-calls}

         :else
         (let [via (peek stack)
               r   (ok! (follow c via))
               [next-refs terms] (expand r)
               steps (conj steps {:via via :response r})
               ends  (into ends terms)]
           (if (and until (until r))
             {:steps steps :ends ends :calls (inc calls) :stopped :until}
             (recur (into (pop stack) (reverse next-refs)) steps ends (inc calls)))))))))

(defn contact-call
  "contact ref → 그 담당자에게 닿는 호출의 인자 map. 보내지 않는다. 보내는 건 호출자다.
   :garden-id     → 살아 있는 형제에게 entwurf_v2
   :repo-owner    → 그 리포의 로컬 절대경로(:cwd)에서 새 형제를 연다
   :issue-tracker → 그 작업면에 요청을 남긴다"
  [ref]
  (when-not (= :contact (:role ref))
    (throw (ex-info "not a contact reference" {:ref ref})))
  (let [{:keys [address draft scope last-active]} (:contact ref)
        base (cond-> {:label (:label ref)}
               scope       (assoc :scope scope)
               last-active (assoc :last-active last-active)
               draft       (assoc :draft draft))]
    (merge base
           (case (:kind address)
             :garden-id     {:via :entwurf-v2 :target (:id address) :intent "fire-and-forget"}
             :repo-owner    (cond-> {:via :fresh-sibling :cwd (:cwd address)}
                              (:repo address)   (assoc :repo (:repo address))
                              (:doc-id address) (assoc :doc-id (:doc-id address)))
             :issue-tracker {:via :issue :url (:url address)}))))
