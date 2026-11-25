(ns wci.adapter.semantic-memory
  "Read-only semantic candidates. The index proposes pointers; only the source establishes facts."
  (:require [clojure.string :as str]
            [wci.adapter :as adapter]
            [wci.adapter.local-json :as json]
            [wci.adapter.local-process :as process]))

(def opts-schema
  [:map {:closed true}
   [:program [:and [:string {:max 1024}] [:re "^/[^\\u0000\\r\\n]+$"]]]
   [:track [:multi {:dispatch :axis}
            [:sessions [:map {:closed true} [:axis [:= :sessions]] [:source [:enum :pi :claude]]]]
            [:garden [:map {:closed true} [:axis [:= :garden]]]]]]
   [:limit {:optional true} [:int {:min 1 :max 20}]]])

(defn- query! [question]
  (when (or (str/blank? question) (str/includes? question "\u0000")
            (str/starts-with? (str/triml question) "--"))
    (adapter/fail! :bad-request "semantic question must be text, not an option"))
  question)

(defn- search-argv [{:keys [program track limit] :or {limit 5}} question]
  (let [{:keys [axis source]} track
        query (query! question)]
    (case axis
      :sessions [program "search-sessions" query "--limit" (str limit) "--source" (name source)]
      :garden [program "search-md" query "--limit" (str limit)])))

(defn- matches [{:keys [track limit] :or {limit 5} :as opts} question]
  (let [argv (search-argv opts question)
        ;; The installed reader needs HOME for its own deployment configuration.
        ;; Do not import user credentials or persist recall inside WCI.
        env (into {"ANDENKEN_DISABLE_RECALL_TRACKING" "1" "LC_ALL" "C" "TZ" "UTC"}
                  (keep #(when-let [v (System/getenv %)] [% v])) ["HOME" "PATH"])
        {:keys [exit stdout]} (process/execute! argv (constantly true) env)]
    (when-not (zero? exit)
      (adapter/fail! :unreadable "semantic reader, index or embedding provider is unavailable"))
    (let [{:strs [results count fallback state error degraded] :as doc} (json/parse-one stdout)]
      (when (or error (= "absent" state) (= true degraded))
        (adapter/fail! :unreadable "semantic reader did not complete the requested search"))
      (when-not (and (map? doc) (vector? results) (every? map? results)
                     (integer? count) (= count (clojure.core/count results)) (<= 0 count limit))
        (adapter/fail! :contract "semantic reader returned an invalid result envelope"))
      (when (and (= :sessions (:axis track)) (not (false? fallback)))
        (adapter/fail! :contract "session reader did not affirm a sessions-only result"))
      results)))

(defn- local-path? [s]
  (and (string? s) (<= 2 (count s) 2048) (re-matches #"/[^\u0000\r\n]+" s)))

(defn- record-ref [label kind accepts locator]
  {:role :record :label label :surface {:kind kind :accepts accepts} :locator locator})

(defn- session-hit [source {:strs [file line] :as hit}]
  (when-not (and (= (name source) (get hit "source"))
                 (local-path? file) (str/ends-with? file ".jsonl") (integer? line) (pos? line))
    (adapter/fail! :contract "session hit has no same-axis JSONL file and line"))
  {:key [:session file line] :title "Session candidate" :times {}
   :refs [(record-ref "Open reported session position" "Session JSONL" "absolute path:line"
                      (str file ":" line))]})

(defn- garden-hit [{:strs [file denoteId]}]
  (let [id (when (and (string? denoteId) (re-matches #"[0-9]{8}T[0-9]{6}" denoteId)) denoteId)
        path (when (local-path? file) file)]
    (when-not (or path id)
      (adapter/fail! :contract "garden hit has no file or Denote id"))
    {:key (if id [:garden :denote id] [:garden :path path])
     :title "Garden note candidate" :times {}
     :refs (cond-> []
             path (conj (record-ref "Open reported garden file" "Garden Markdown" "absolute path" path))
             id (conj (record-ref "Find reported Denote id" "Garden note" "Denote id" id)))}))

(defn- axis-label [{:keys [axis source]}]
  (case axis
    :sessions (str "semantic-memory sessions axis (" (name source) ")")
    :garden "semantic-memory garden axis"))

(defmethod adapter/describe :semantic-memory [{:keys [title opts]}]
  {:kind "semantic-memory" :title (or title (axis-label (:track opts)))
   :as-of-support :live-only :question :semantic :attests #{} :contact-liveness :none})

(defn- distinct-pointers [xs]
  (second (reduce (fn [[seen out] item]
                    (let [k (:key item)]
                      (if (contains? seen k) [seen out] [(conj seen k) (conj out item)])))
                  [#{} []] xs)))

(defmethod adapter/answer :semantic-memory [{:keys [opts]} {:keys [mode live question]}]
  (when-not live (adapter/fail! :unsupported-time "semantic index cannot replay past knowledge"))
  (if (= :question mode)
    (let [{:keys [axis source] :as track} (:track opts)
          hits (matches opts question)
          items (map (if (= axis :sessions) #(session-hit source %) garden-hit) hits)]
      {:matched-by (axis-label track) :findings (distinct-pointers items)})
    {:findings [] :uninterpreted {:reason "semantic index requires a free-text question"}}))

(defmethod adapter/expand :semantic-memory [_ _ _]
  (adapter/fail! :ref-gone "semantic search exposes only terminal source references"))
