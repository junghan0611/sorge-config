(ns wci.adapter.git-source
  "Local read-only access to Git objects, without ref discovery beyond the configured ref."
  (:require [clojure.string :as str]
            [wci.adapter :as adapter]
            [wci.adapter.local-process :as process])
  (:import [java.time DateTimeException OffsetDateTime ZoneOffset]
           [java.time.format DateTimeFormatter]))

(def root-schema [:re "^[^\\u0000\\r\\n]+$"])
(def rev-schema
  [:re "^(?:HEAD|[0-9a-f]{40}|[0-9a-f]{64}|refs/(?:heads|tags|remotes)/[a-zA-Z0-9][a-zA-Z0-9._/-]*)$"])
(def path-schema
  [:re "^(?:(?!\\.{1,2}(?:/|$)|(?i:\\.git)(?:/|$))[^/\\\\:\\u0000\\r\\n]+/)*(?!\\.{1,2}$|(?i:\\.git)$)[^/\\\\:\\u0000\\r\\n]+$"])
(def ^:private oid-pattern #"(?:[0-9a-f]{40}|[0-9a-f]{64})")

(defn- command [root args]
  (process/execute!
   (into ["git" "--no-pager" "--no-replace-objects" "--no-lazy-fetch"
          "--literal-pathspecs" "-C" root] args)
   #(str/starts-with? % "GIT_")
   {"GIT_CONFIG_NOSYSTEM" "1" "GIT_CONFIG_GLOBAL" "/dev/null"
    "GIT_TERMINAL_PROMPT" "0" "GIT_ALLOW_PROTOCOL" "" "LC_ALL" "C"}))

(defn- checked [root args]
  (let [{:keys [exit stdout]} (command root args)]
    (when-not (zero? exit)
      (adapter/fail! :unreadable "cannot read configured Git history"))
    stdout))

(defn- oid! [s]
  (when-not (re-matches oid-pattern s)
    (adapter/fail! :contract "invalid object id in Git output"))
  s)

(defn context [{:keys [root rev] :or {rev "HEAD"}}]
  (when-not (and (re-matches (re-pattern (second root-schema)) (or root ""))
                 (re-matches (re-pattern (second rev-schema)) rev))
    (adapter/fail! :bad-request "invalid local Git reader options"))
  (let [revision (str rev "^{commit}")]
    {:root root :tip (oid! (str/trim (checked root ["rev-parse" "--verify" "--end-of-options" revision])))}))

(defn require-working-root! [{:keys [root]}]
  (when-not (and (= "true" (str/trim (checked root ["rev-parse" "--is-inside-work-tree"])))
                 (= "\n" (checked root ["rev-parse" "--show-prefix"])))
    (adapter/fail! :bad-request "NEXT root must be the root of a Git working tree")))

(defn require-complete! [{:keys [root]}]
  (case (str/trim (checked root ["rev-parse" "--is-shallow-repository"]))
    "false" nil
    "true" (adapter/fail! :unsupported-time "shallow history cannot reconstruct a past view")
    (adapter/fail! :contract "invalid Git shallow repository status"))
  (let [{:keys [exit stdout]} (command root ["config" "--get-regexp" "^remote\\..*\\.promisor$" "^(true|yes|on|1)$"])]
    (cond
      (= 1 exit) nil
      (and (= 0 exit) (not (str/blank? stdout)))
      (adapter/fail! :unsupported-time "promisor history cannot guarantee local past reconstruction")
      :else (adapter/fail! :unreadable "cannot determine local Git history completeness"))))

(defn as-kst [s]
  (try
    (-> (OffsetDateTime/parse s)
        (.withOffsetSameInstant (ZoneOffset/ofHours 9))
        (.format DateTimeFormatter/ISO_OFFSET_DATE_TIME))
    (catch DateTimeException _ (adapter/fail! :contract "invalid Git timestamp"))))

(defn before? [when cutoff]
  (.isBefore (OffsetDateTime/parse when) (OffsetDateTime/parse cutoff)))

(defn shift-days [at days]
  (-> (OffsetDateTime/parse at) (.plusDays days) (.format DateTimeFormatter/ISO_OFFSET_DATE_TIME)))

(defn history-since-key [at] [:git/history-since at])

(defn- nul-records [out width]
  (if (empty? out)
    []
    (let [parts (str/split out #"\u0000" -1)
          fields (pop parts)]
      (when-not (and (= "" (peek parts)) (zero? (mod (count fields) width)))
        (adapter/fail! :contract "invalid NUL-delimited Git output"))
      (partition width fields))))

(defn- log-output [{:keys [root tip]} fmt args]
  (checked root (into ["log" "--no-patch" "--no-color" "--no-notes" "--no-show-signature"
                       "--encoding=UTF-8" "-z" (str "--format=" fmt)]
                      (concat args [tip "--"]))))

(defn commits [ctx]
  (mapv (fn [[oid committed-at title message]]
          {:oid (oid! oid) :at (as-kst committed-at) :title title :message message})
        (nul-records (log-output ctx "%H%x00%cI%x00%s%x00%B" []) 4)))

(defn recordings
  "Object id and committer time only; omit messages for aggregation."
  ([ctx] (recordings ctx []))
  ([ctx args]
   (mapv (fn [[oid at]] {:oid (oid! oid) :at (as-kst at)})
         (nul-records (log-output ctx "%H%x00%cI" args) 2))))

(defn first-parent-at
  "Find the committed tree before the exclusive cut. Timestamp inversions are ambiguous."
  [ctx cutoff]
  (require-complete! ctx)
  (let [chain (recordings ctx ["--first-parent"])]
    (when (some (fn [[child parent]] (before? (:at child) (:at parent))) (partition 2 1 chain))
      (adapter/fail! :unsupported-time "first-parent recording times do not establish an ordered cut"))
    (or (some #(when (before? (:at %) cutoff) %) chain)
        (adapter/fail! :unsupported-time "no committed tree precedes the knowledge cut"))))

(defn file-at
  "Read a regular committed blob, not a symlink, submodule or smudged working copy."
  [{:keys [root]} oid path]
  (when-not (re-matches (re-pattern (second path-schema)) path)
    (adapter/fail! :bad-request "invalid local Git path"))
  (when-let [row (ffirst (nul-records (checked root ["ls-tree" "-z" "--full-tree" (oid! oid) "--" path]) 1))]
    (let [[_ blob path-found] (re-matches #"100(?:644|755) blob ([0-9a-f]+)\t([^\u0000]+)" row)]
      (when-not (and blob (= path path-found))
        (adapter/fail! :contract "configured Git path is not a regular committed file"))
      (checked root ["cat-file" "blob" (oid! blob)]))))

(defn file-change-at [{:keys [root]} oid path]
  (let [out (checked root ["log" "--first-parent" "-1" "--no-patch" "--no-notes"
                           "--no-show-signature" "--format=%cI" (oid! oid) "--" path])]
    (when-not (str/blank? out)
      (as-kst (str/trim out)))))
