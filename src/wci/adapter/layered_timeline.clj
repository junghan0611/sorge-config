(ns wci.adapter.layered-timeline
  "Read a live, layered event feed without turning observed events into proof of absence."
  (:require [wci.adapter :as adapter]
            [wci.adapter.exec :as exec]
            [wci.time :as time]))

(def opts-schema
  [:map {:closed true}
   [:argv exec/argv-schema]
   [:events-path exec/json-path]
   [:errors-path exec/json-path]
   [:dropped-count-path {:optional true} exec/json-path]
   [:layer-path exec/json-path]
   [:instant-path exec/json-path]
   [:day-path exec/json-path]
   [:title-path exec/json-path]
   ;; Ordered source fields that jointly identify one event, not just its subject.
   [:identity-paths [:vector {:min 1 :max 12} exec/json-path]]
   [:asserted-by [:string {:min 1 :max 80}]]])

(defn- check-completeness! [{:keys [errors-path dropped-count-path]} doc]
  (let [errors (get-in doc errors-path)
        dropped (if dropped-count-path (get-in doc dropped-count-path) 0)]
    (when-not (and (or (map? errors) (vector? errors)) (integer? dropped) (<= 0 dropped))
      (adapter/fail! :contract "timeline lacks valid failure counters"))
    (when (or (seq errors) (pos? dropped))
      (adapter/fail! :unreadable "timeline contains failed sections or unparseable event times"))))

(defn- event-time [{:keys [instant-path day-path]} event]
  (let [instant (get-in event instant-path)
        at (exec/calendar-time (if (nil? instant) (get-in event day-path) instant))]
    (when-not (= (nil? instant) (= 10 (count at)))
      (adapter/fail! :contract "timeline event time does not match its instant or day field"))
    at))

(defn- event-key [paths event layer at]
  (let [parts (mapv #(get-in event %) paths)]
    (when-not (and (some some? parts)
                   (every? #(or (nil? %) (integer? %) (and (string? %) (<= 1 (count %) 256))) parts))
      (adapter/fail! :contract "timeline event lacks a stable source identity"))
    [:timeline layer at parts]))

(defn- event-row [{:keys [layer-path title-path identity-paths asserted-by] :as opts} cut event]
  (when-not (map? event) (adapter/fail! :contract "timeline event must be an object"))
  (let [layer (get-in event layer-path)]
    (case layer
      "reported" nil
      ("recorded" "planned")
      (let [at (event-time opts event)
            ;; A new recording may arrive during the live read after the open cut.
            visible? (or (= layer "planned") (< (time/point-start at) cut))]
        (when visible?
          (let [title (get-in event title-path)]
            (when-not (and (string? title) (<= 1 (count title) 200))
              (adapter/fail! :contract "timeline event lacks a bounded source title"))
            {:key (event-key identity-paths event layer at)
             :title title
             :times (if (= layer "planned") {:expected {:at at :asserted-by asserted-by}} {:source at})
             :refs []})))
      (adapter/fail! :contract "timeline event has an unknown layer"))))

(defn- live! [{:keys [live]}]
  (when-not live (adapter/fail! :unsupported-time "live timeline cannot replay past knowledge")))

(defn- events [{:keys [argv events-path] :as opts} {:keys [as-of]}]
  (let [doc (exec/read-json! argv)
        rows (get-in doc events-path)
        cut (time/point-start as-of)]
    (when-not (vector? rows) (adapter/fail! :contract "timeline events must be an array"))
    (check-completeness! opts doc)
    (let [findings (into [] (keep #(event-row opts cut %)) rows)]
      (when-not (= (count findings) (count (into #{} (map :key) findings)))
        (adapter/fail! :contract "timeline events have duplicate source identities"))
      {:matched-by "live timeline; recorded or planned event times"
       :findings findings})))

(defmethod adapter/describe :layered-timeline [{:keys [title]}]
  {:kind "layered-timeline" :title (or title "Layered timeline")
   :as-of-support :live-only :question :none :attests #{:source :expected} :contact-liveness :none})

(defmethod adapter/answer :layered-timeline [{:keys [opts]} {:keys [mode where] :as req}]
  (live! req)
  (cond
    (= mode :question) {:findings [] :uninterpreted {:reason "fixed timeline does not accept free-text questions"}}
    (and (= mode :where) (some #(not (#{:source :expected} (first %))) where))
    {:findings [] :uninterpreted {:reason "timeline attests event times, not aggregate movement or its absence"}}
    :else (events opts req)))

(defmethod adapter/expand :layered-timeline [{:keys [opts]} key req]
  (live! req)
  (let [result (events opts req)
        findings (filterv #(= key (:key %)) (:findings result))]
    (if (seq findings)
      (assoc result :findings findings)
      (adapter/fail! :ref-gone "timeline event is no longer in the source feed"))))
