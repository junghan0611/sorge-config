(ns wci.adapter.exec
  "Trusted local argv -> bounded JSON -> a closed WCI finding. No evaluator or shell.
  The mapping vocabulary stops at seven operations: no regex, and only
  :when-eq and :when-present may conditionally project a value."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [malli.core :as m]
            [wci.adapter :as adapter]
            [wci.adapter.edn-io :as edn-io]
            [wci.adapter.local-json :as json]
            [wci.adapter.local-process :as process]
            [wci.time :as time])
  (:import [java.time DateTimeException LocalDate OffsetDateTime ZoneOffset]
           [java.time.format DateTimeFormatter]))

(def json-path
  [:vector {:max 16} [:or [:string {:min 1}] [:int {:min 0 :max Integer/MAX_VALUE}]]])

(def ^:private expression
  [:schema {:registry
            {::expr [:or
                     [:tuple [:= :get] json-path]
                     [:tuple [:= :get] json-path [:= :optional]]
                     [:tuple [:= :get] json-path [:map {:closed true} [:default :any]]]
                     [:tuple [:= :literal] :any]
                     [:tuple [:= :enum] [:ref ::expr] [:map-of :any :any]]
                     [:tuple [:= :kst] [:ref ::expr]]
                     [:tuple [:= :kst] [:ref ::expr] [:map {:closed true} [:truncate [:= :second]]]]
                     [:tuple [:= :when-eq] [:ref ::expr] :any [:ref ::expr]]
                     [:tuple [:= :when-present] [:ref ::expr] [:ref ::expr]]
                     [:tuple [:= :assemble]
                      [:or [:map-of :keyword [:ref ::expr]] [:vector [:ref ::expr]]]]]}}
   [:ref ::expr]])

(def ^:private shells
  #{"sh" "ash" "bash" "dash" "zsh" "ksh" "mksh" "fish" "csh" "tcsh" "nu"
    "busybox" "env" "cmd" "cmd.exe" "powershell" "pwsh"})

(defn- reader-program? [program]
  (and (string? program) (<= 1 (count program) 1024) (re-matches #"[a-zA-Z0-9_./+-]+" program)
       (let [basename (.toLowerCase (.getName (io/file program)) java.util.Locale/ROOT)]
         (and (seq basename) (not (contains? shells basename))))))

(def argv-schema
  [:and [:vector {:min 1 :max 33} [:and [:string {:max 4096}] [:re "^[^\\u0000]*$"]]]
   [:fn #(and (vector? %) (reader-program? (first %)))]])

(def ^:private json-keys-schema [:set {:max 128} [:string {:min 1 :max 120}]])

(def opts-schema
  [:map {:closed true}
   [:argv argv-schema]
   [:attests [:set :wci/attest]]
   [:rows-path json-path]
   [:overview {:optional true} [:= :summary]]
   [:summary {:optional true} [:map {:closed true} [:timeless [:= :keep]]]]
   [:allowed-json-keys {:optional true} json-keys-schema]
   [:drop-json-keys {:optional true} json-keys-schema]
   [:finding [:tuple [:= :assemble]
              [:map {:closed true}
               [:key expression] [:title expression] [:times expression] [:refs expression]
               [:gist {:optional true} expression] [:awaits {:optional true} expression]
               [:by {:optional true} expression]
               [:area {:optional true} expression] [:state {:optional true} expression]]]]
   [:data-through {:optional true} expression]])

(def ^:private schemas
  (delay (let [opts {:registry (merge (m/default-schemas)
                                     (:registry (edn-io/read-one (io/resource "wci.edn"))))}]
           {:finding (m/schema :channel/finding opts)
            :point (m/schema :wci/point opts)})))
(def ^:private missing (Object.))

(defn- lookup [data path]
  (reduce (fn [value k]
            (if (or (and (string? k) (map? value)) (and (int? k) (vector? value)))
              (get value k missing)
              missing))
          data path))

(defn calendar-time
  ([value] (calendar-time value nil))
  ([value {:keys [truncate]}]
   (try
     (if (and (string? value) (re-matches #"[0-9]{4}-[0-9]{2}-[0-9]{2}" value))
       (str (LocalDate/parse value))
       (let [t (when (string? value) (OffsetDateTime/parse value))]
         (when (or (nil? t) (and (not= :second truncate) (not (zero? (.getNano t)))))
           (adapter/fail! :contract "mapped timestamp needs an explicit offset and whole seconds"))
         (let [t (if (= :second truncate) (.withNano t 0) t)
               stamp (.format (.withOffsetSameInstant t (ZoneOffset/ofHours 9))
                              DateTimeFormatter/ISO_OFFSET_DATE_TIME)]
           (when-not (re-matches #"[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\+09:00" stamp)
             (adapter/fail! :contract "mapped timestamp exceeds the WCI calendar range"))
           stamp)))
     (catch DateTimeException _ (adapter/fail! :contract "mapped timestamp is not an offset time or calendar day")))))

(defn- project [data [op arg option branch]]
  (case op
    :get (let [v (lookup data arg)]
           (cond
             (and (map? option) (or (identical? missing v) (nil? v))) (:default option)
             (and (identical? missing v) (not= :optional option))
             (adapter/fail! :contract "required mapped JSON path is absent")
             :else v))
    :literal arg
    :enum (let [v (project data arg)]
            (cond
              (identical? missing v) missing
              (contains? option v) (get option v)
              :else (adapter/fail! :contract "mapped JSON value is outside the declared enum")))
    :kst (let [v (project data arg)] (if (identical? missing v) missing (calendar-time v option)))
    :when-eq (let [v (project data arg)]
               (if (and (not (identical? missing v)) (= v option))
                 (project data branch)
                 missing))
    :when-present (let [v (project data arg)]
                    (if (or (identical? missing v) (nil? v))
                      missing
                      (project data option)))
    :assemble (if (map? arg)
                (into {} (keep (fn [[k expr]]
                                 (let [v (project data expr)]
                                   (when-not (identical? missing v) [k v])))) arg)
                (into [] (remove #(identical? missing %)) (map #(project data %) arg)))))

(defn- row-roles [result]
  (concat (keys (:times result)) (filter #(contains? result %) [:awaits :area :gist :state])))

(defn- stable-key? [k]
  (or (and (string? k) (<= 1 (count k) 256)) (integer? k)))

(defn read-json!
  "Run one trusted fixed argv with a minimal environment and strict JSON output."
  [argv]
  (when-not (and (vector? argv) (every? string? argv) (reader-program? (first argv)))
    (adapter/fail! :bad-request "invalid argv for local JSON reader"))
  (let [env (cond-> {"LC_ALL" "C" "TZ" "UTC"}
              (System/getenv "PATH") (assoc "PATH" (System/getenv "PATH")))
        {:keys [exit stdout]} (process/execute! argv (constantly true) env)]
    (when-not (zero? exit) (adapter/fail! :unreadable "local JSON reader exited unsuccessfully"))
    (json/parse-one stdout)))

(defn- checked-metadata [allowed drop-keys value]
  (cond
    (map? value) (reduce-kv (fn [out k v]
                              (cond
                                ;; Discard the entire value, without visiting child keys.
                                (contains? drop-keys k) out
                                (contains? allowed k) (assoc out k (checked-metadata allowed drop-keys v))
                                :else (adapter/fail! :contract "local JSON reader returned a field outside the allowed metadata")))
                            {} value)
    (vector? value) (mapv #(checked-metadata allowed drop-keys %) value)
    :else value))

(defn- metadata-doc [doc allowed drop-keys]
  (if (or allowed drop-keys) (checked-metadata allowed drop-keys doc) doc))

(defn- mapped-answer [{:keys [argv rows-path finding data-through attests allowed-json-keys drop-json-keys]} {:keys [live]}]
  (when-not live (adapter/fail! :unsupported-time "fixed argv source cannot replay past knowledge"))
  (let [doc (metadata-doc (read-json! argv) allowed-json-keys drop-json-keys)
        rows (lookup doc rows-path)
        bound (if data-through (project doc data-through) missing)]
    (when-not (vector? rows) (adapter/fail! :contract "mapped JSON rows must be an array"))
    (when-not (identical? missing bound)
      (when-not (m/validate (:point @schemas) bound)
        (adapter/fail! :contract "mapped coverage bound must be a normalized time or calendar day"))
      (calendar-time bound))
    (let [items (mapv (fn [row]
                        (let [result (project row finding) k (:key result)]
                          (when-not (and (m/validate (:finding @schemas) result)
                                         (stable-key? k)
                                         (every? #(or (not (contains? % :key)) (stable-key? (:key %))) (:refs result))
                                         (every? attests (row-roles result)))
                            (adapter/fail! :contract "mapped finding violates its declared fields or roles"))
                          result)) rows)]
      (when-not (= (count items) (count (into #{} (map :key) items)))
        (adapter/fail! :contract "mapped findings have duplicate source keys"))
      (cond-> {:findings items}
        (not (identical? missing bound)) (assoc :data-through bound)))))

(defmethod adapter/describe :exec [{:keys [title opts]}]
  {:kind "exec" :title (or title "Local JSON reader")
   :as-of-support :live-only :question :none :attests (:attests opts) :contact-liveness :none})

;; Mapped source keys are strings/integers, so these cannot collide with one.
(def ^:private summary-key [:exec/summary])
(def ^:private history-key [:exec/history])

(defn- keep-timeless? [opts] (= :keep (get-in opts [:summary :timeless])))

(defn- flow-rows [opts findings as-of]
  (let [cut (time/point-start as-of)]
    (into []
          (comp (map (fn [{:keys [times] :as row}]
                       (let [at (:source times)]
                         (cond
                           (and (keep-timeless? opts) (empty? times)) {:row row}
                           (m/validate (:point @schemas) at) {:row row :at at :ms (time/point-start at)}
                           :else (adapter/fail! :contract "flow summary requires a source point for every dated row")))))
                (filter #(or (nil? (:ms %)) (< (:ms %) cut))))
          findings)))

(defn- flow-summary [{:keys [opts] :as cfg} {:keys [as-of horizon-days]} {:keys [findings]}]
  (let [rows (flow-rows opts findings as-of)
        dated (filterv #(contains? % :ms) rows)
        lower (time/shift-days (time/point-start as-of) (- horizon-days))
        recent (count (filter #(<= lower (:ms %)) dated))
        latest (when (seq dated) (:at (apply max-key :ms dated)))
        undated (- (count rows) (count dated))]
    {:matched-by "fixed local argv; count within the returned source window"
     :findings [{:key summary-key :title (or (:title cfg) "Local JSON reader")
                 :times (cond-> {} latest (assoc :last-movement latest))
                 :gist {:text (str recent " in the last " horizon-days " days (returned rows)"
                                  (when (keep-timeless? opts) (str " · " undated " without a timestamp")))
                        :asserted-by "Local JSON source"}
                 :refs [{:role :history :label "Open source history" :key history-key}]}]}))

(defmethod adapter/answer :exec [{:keys [opts] :as cfg} {:keys [mode] :as req}]
  (let [result (mapped-answer opts req)]
    (cond
      (= :question mode)
      (assoc result :findings [] :uninterpreted {:reason "fixed argv source does not accept free-text questions"})
      (and (= :overview mode) (= :summary (:overview opts))) (flow-summary cfg req result)
      :else (assoc result :matched-by "fixed local argv; live JSON mapping only"))))

(defmethod adapter/expand :exec [{:keys [opts]} key {:keys [as-of] :as req}]
  (let [result (mapped-answer opts req)]
    (if (and (= :summary (:overview opts)) (= history-key key))
      (assoc result :findings (mapv :row (flow-rows opts (:findings result) as-of)))
      (let [hits (filterv #(= key (:key %)) (:findings result))]
        (if (seq hits)
          (assoc result :findings hits)
          (adapter/fail! :ref-gone "mapped source key is no longer present"))))))
