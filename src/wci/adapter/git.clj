(ns wci.adapter.git
  "Titles, recording times, and explicit issue refs from the configured Git ref."
  (:require [clojure.string :as str]
            [wci.adapter :as adapter]
            [wci.adapter.git-source :as git])
  (:import [java.util Locale]))

(def opts-schema
  [:map {:closed true}
   [:root git/root-schema]
   [:rev {:optional true} git/rev-schema]
   [:issue-base {:optional true} [:re "^https://[a-zA-Z0-9.-]+(?::[0-9]+)?(?:/[a-zA-Z0-9._~-]+)*/issues$"]]])

(def ^:private origin "configured Git ref; reachable committer history only")

(defn- issue-refs [base message]
  ;; A bare issue number means nothing unless this source declares its issue surface.
  (when base
    (for [n (distinct (map second (re-seq #"(?<![\p{L}\p{N}/#])#([1-9][0-9]*)(?![\p{L}\p{N}])" message)))]
      {:role :record :label (str "Open referenced issue #" n)
       :surface {:kind "Issue tracker" :accepts "issue URL"}
       :locator (str base "/" n)})))

(defn- finding [{:keys [root issue-base]} {:keys [oid at title message]}]
  {:key oid :title title :times {:source at}
   :refs (into [{:role :record :label "Open the Git commit"
                 :surface {:kind "Git commit" :accepts "local repository and object id"}
                 :locator (str root "@" oid)}]
               (issue-refs issue-base message))})

(defn- visible
  ([read-records cfg req] (visible read-records cfg req false))
  ([read-records {:keys [opts]} {:keys [live as-of]} require-coverage?]
   (let [ctx (git/context opts)]
     (when (or require-coverage? (not live))
       (try (git/require-complete! ctx)
            (catch clojure.lang.ExceptionInfo e
              (if (and live (= :unsupported-time (:wci/error-kind (ex-data e))))
                (adapter/fail! :unreadable "configured Git history cannot attest repository movement")
                (throw e)))))
     (filterv #(git/before? (:at %) as-of) (read-records ctx)))))

(def ^:private repo-history-key [:git/history])

(defn- repository-answer [{:keys [area title] :as cfg} {:keys [as-of horizon-days] :as req} require-coverage?]
  (let [rows (visible git/recordings cfg req require-coverage?)
        latest (reduce (fn [best row] (if (or (nil? best) (git/before? best (:at row))) (:at row) best)) nil rows)
        lower (git/shift-days as-of (- horizon-days))
        recent-count (count (remove #(git/before? (:at %) lower) rows))]
    (cond-> {:matched-by origin
             :findings (if latest
                         [{:key [:git/overview]
                           :title (or (:label area) title "Git history")
                           :times {:last-movement latest}
                           :gist {:text (str recent-count " commits in the last " horizon-days " days")
                                  :asserted-by "Reachable Git history"}
                           :refs [{:role :history :label "Open commit history" :key repo-history-key}]}]
                         [])}
      ;; Coverage is the configured ref and knowledge cut, not unfetched work.
      require-coverage? (assoc :data-through as-of))))

(defn- matches? [^String title ^String question]
  (str/includes? (.toLowerCase title Locale/ROOT) (.toLowerCase question Locale/ROOT)))

(defmethod adapter/describe :git [{:keys [opts title]}]
  (git/context opts)
  {:kind "git" :title (or title "Git history")
   :as-of-support :replay :question :keyword
   :attests #{:source :last-movement :gist} :contact-liveness :none})

(defmethod adapter/answer :git [{:keys [opts] :as cfg} {:keys [mode question where] :as req}]
  (let [roles (into #{} (map first) where)]
    (cond
      (= :overview mode) (repository-answer cfg req false)
      (and (= :where mode) (= #{:last-movement} roles)) (repository-answer cfg req true)
      (and (= :where mode) (some #(not= :source %) roles))
      {:findings [] :uninterpreted {:reason "repository movement and commit recording time have different query scopes"}}
      :else
      (let [rows (visible git/commits cfg req)
            search? (= :question mode)]
        {:matched-by (str (when search? "title keyword; ") origin)
         :findings (into [] (comp (filter #(or (not search?) (matches? (:title %) question)))
                                 (map #(finding opts %))) rows)}))))

(defn- history-since? [key]
  (and (vector? key) (= 2 (count key)) (= :git/history-since (first key)) (string? (second key))))

(defmethod adapter/ref-label :git [{:keys [opts]} {:keys [role key]} {:keys [as-of]}]
  (when (and (= :history role) (history-since? key))
    (try
      (let [ctx (git/context opts)
            _ (git/require-complete! ctx)
            n (transduce (filter #(and (git/before? (second key) (:at %))
                                       (git/before? (:at %) as-of)))
                         (completing (fn [n _] (inc n))) 0 (git/recordings ctx))]
        (str "Commits after this handoff (" n ")"))
      (catch clojure.lang.ExceptionInfo e
        (when-not (#{:unreadable :unsupported-time} (:wci/error-kind (ex-data e)))
          (throw e))))))

(defmethod adapter/expand :git [{:keys [opts] :as cfg} key req]
  (let [rows (visible git/commits cfg req)]
    (cond
      (= key repo-history-key) {:matched-by origin :findings (mapv #(finding opts %) rows)}
      (history-since? key)
      {:matched-by (str origin "; newer than handoff")
       :findings (into [] (comp (filter #(git/before? (second key) (:at %))) (map #(finding opts %))) rows)}
      :else (if-let [commit (some #(when (= key (:oid %)) %) rows)]
              {:matched-by origin :findings [(finding opts commit)]}
              (adapter/fail! :ref-gone "commit is not in the configured history at the knowledge cut")))))
