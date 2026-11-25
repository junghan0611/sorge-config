(ns wci.adapter.exec
  "Trusted local argv -> bounded JSON -> a closed WCI finding. No evaluator or shell."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [malli.core :as m]
            [wci.adapter :as adapter]
            [wci.adapter.edn-io :as edn-io]
            [wci.adapter.local-json :as json]
            [wci.adapter.local-process :as process])
  (:import [java.time DateTimeException LocalDate OffsetDateTime ZoneOffset]
           [java.time.format DateTimeFormatter]))

(def ^:private json-path
  [:vector {:max 16} [:or [:string {:min 1}] [:int {:min 0 :max Integer/MAX_VALUE}]]])

(def ^:private expression
  [:schema {:registry
            {::expr [:or
                     [:tuple [:= :get] json-path]
                     [:tuple [:= :get] json-path [:= :optional]]
                     [:tuple [:= :literal] :any]
                     [:tuple [:= :enum] [:ref ::expr] [:map-of :any :any]]
                     [:tuple [:= :kst] [:ref ::expr]]
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

(def opts-schema
  [:map {:closed true}
   [:argv [:and [:vector {:min 1 :max 33} [:and [:string {:max 4096}] [:re "^[^\\u0000]*$"]]]
           [:fn #(and (vector? %) (reader-program? (first %)))]]]
   [:attests [:set :wci/attest]]
   [:rows-path json-path]
   [:finding [:tuple [:= :assemble]
              [:map {:closed true}
               [:key expression] [:title expression] [:times expression] [:refs expression]
               [:gist {:optional true} expression] [:awaits {:optional true} expression]
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

(defn- calendar-time [value]
  (try
    (if (and (string? value) (re-matches #"[0-9]{4}-[0-9]{2}-[0-9]{2}" value))
      (str (LocalDate/parse value))
      (let [t (when (string? value) (OffsetDateTime/parse value))]
        (when (or (nil? t) (not (zero? (.getNano t))))
          (adapter/fail! :contract "mapped timestamp needs an explicit offset and whole seconds"))
        (let [stamp (.format (.withOffsetSameInstant t (ZoneOffset/ofHours 9))
                             DateTimeFormatter/ISO_OFFSET_DATE_TIME)]
          (when-not (re-matches #"[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\+09:00" stamp)
            (adapter/fail! :contract "mapped timestamp exceeds the WCI calendar range"))
          stamp)))
    (catch DateTimeException _ (adapter/fail! :contract "mapped timestamp is not an offset time or calendar day"))))

(defn- project [data [op arg option]]
  (case op
    :get (let [v (lookup data arg)]
           (when (and (identical? missing v) (not= :optional option))
             (adapter/fail! :contract "required mapped JSON path is absent"))
           v)
    :literal arg
    :enum (let [v (project data arg)]
            (cond
              (identical? missing v) missing
              (contains? option v) (get option v)
              :else (adapter/fail! :contract "mapped JSON value is outside the declared enum")))
    :kst (let [v (project data arg)] (if (identical? missing v) missing (calendar-time v)))
    :assemble (if (map? arg)
                (into {} (keep (fn [[k expr]]
                                 (let [v (project data expr)]
                                   (when-not (identical? missing v) [k v])))) arg)
                (into [] (remove #(identical? missing %)) (map #(project data %) arg)))))

(defn- row-roles [result]
  (concat (keys (:times result)) (filter #(contains? result %) [:awaits :area :gist :state])))

(defn- stable-key? [k]
  (or (and (string? k) (<= 1 (count k) 256)) (integer? k)))

(defn- mapped-answer [{:keys [argv rows-path finding data-through attests]} {:keys [live]}]
  (when-not live (adapter/fail! :unsupported-time "fixed argv source cannot replay past knowledge"))
  (let [_ (when-not (and (vector? argv) (every? string? argv) (reader-program? (first argv)))
            (adapter/fail! :bad-request "invalid argv for local JSON reader"))
        env (cond-> {"LC_ALL" "C" "TZ" "UTC"}
              (System/getenv "PATH") (assoc "PATH" (System/getenv "PATH")))
        {:keys [exit stdout]} (process/execute! argv (constantly true) env)]
    (when-not (zero? exit) (adapter/fail! :unreadable "local JSON reader exited unsuccessfully"))
    (let [doc (json/parse-one stdout)
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
          (not (identical? missing bound)) (assoc :data-through bound))))))

(defmethod adapter/describe :exec [{:keys [title opts]}]
  {:kind "exec" :title (or title "Local JSON reader")
   :as-of-support :live-only :question :none :attests (:attests opts) :contact-liveness :none})

(defmethod adapter/answer :exec [{:keys [opts]} {:keys [mode] :as req}]
  (let [result (mapped-answer opts req)]
    (if (= :question mode)
      (assoc result :findings [] :uninterpreted {:reason "fixed argv source does not accept free-text questions"})
      (assoc result :matched-by "fixed local argv; live JSON mapping only"))))

(defmethod adapter/expand :exec [{:keys [opts]} key req]
  (let [result (mapped-answer opts req)
        hits (filterv #(= key (:key %)) (:findings result))]
    (if (seq hits)
      (assoc result :findings hits)
      (adapter/fail! :ref-gone "mapped source key is no longer present"))))
