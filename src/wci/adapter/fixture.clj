(ns wci.adapter.fixture
	"Synthetic EDN history. Each row is an assertion visible strictly after recorded-at."
	(:require [clojure.java.io :as io]
	          [clojure.string :as str]
	          [malli.core :as m]
	          [wci.adapter :as adapter]
	          [wci.adapter.edn-io :as edn-io])
	(:import [java.time OffsetDateTime DateTimeException]
	         [java.util Locale]))

(def opts-schema
	[:map {:closed true}
	 [:path [:string {:min 1}]]
	 [:fail {:optional true} [:= :unreadable]]])

(def ^:private source-schema
	(delay
		(m/schema
		 [:map {:closed true}
		  [:describe :channel/describe]
		  [:data-through :wci/kst]
		  [:rows [:vector
		          [:map {:closed true}
		           [:recorded-at :wci/kst]
		           [:entry? :boolean]
		           [:finding :channel/finding]]]]]
		 {:registry (merge (m/default-schemas)
		                   (:registry (edn-io/read-one (io/resource "wci.edn"))))})))

(defn- timestamp [s]
	(try
		(.toInstant (OffsetDateTime/parse s))
		(catch DateTimeException _ (adapter/fail! :contract "invalid fixture recording time"))))

(defn- source [{:keys [opts]}]
	(when (= :unreadable (:fail opts))
		(adapter/fail! :unreadable "synthetic source cannot be read"))
	(let [doc (try
	            (edn-io/read-one (io/file (:path opts)))
	            (catch java.io.IOException _ (adapter/fail! :unreadable "cannot read fixture source"))
	            (catch Exception _ (adapter/fail! :contract "invalid fixture source EDN")))]
		(when-not (m/validate @source-schema doc)
			(adapter/fail! :contract "fixture source violates its declaration"))
		(let [rows (mapv #(assoc % :t (timestamp (:recorded-at %))) (:rows doc))
		      identities (map (juxt (comp :key :finding) :t) rows)
		      upper (timestamp (:data-through doc))]
			(when (or (not= (count identities) (count (distinct identities)))
			          (some #(not (.isBefore (:t %) upper)) rows))
				(adapter/fail! :contract "ambiguous fixture history or invalid coverage bound"))
			(assoc doc :rows rows))))

(defn- visible [{:keys [describe rows]} {:keys [live as-of]}]
	(when (and (not live) (= :live-only (:as-of-support describe)))
		(adapter/fail! :unsupported-time "fixture source has no replay history"))
	(let [cut (timestamp as-of)]
		(reduce (fn [acc {:keys [t finding] :as row}]
		          (let [key (:key finding) old (get acc key)]
		            (if (and (.isBefore t cut) (or (nil? old) (.isAfter t (:t old))))
							(assoc acc key row)
							acc)))
		        {} rows)))

(defn- folded [^String s] (.toLowerCase s Locale/ROOT))

(defmethod adapter/describe :fixture [{:keys [title] :as cfg}]
	(cond-> (:describe (source cfg)) title (assoc :title title)))

(defmethod adapter/answer :fixture [cfg {:keys [mode question] :as req}]
	(let [{:keys [describe data-through] :as doc} (source cfg)
	      rows (visible doc req)
	      entries (into [] (comp (filter :entry?) (map :finding)) (vals rows))
	      base {:data-through data-through}]
		(if (= :question mode)
			(if (= :keyword (:question describe))
				(assoc base :matched-by "title keyword"
				       :findings (filterv #(str/includes? (folded (:title %)) (folded question)) entries))
				(assoc base :uninterpreted {:reason "source does not interpret text questions"} :findings []))
			;; The core, not this adapter, judges canonical overview and role predicates.
			(assoc base :findings entries))))

(defmethod adapter/expand :fixture [cfg key req]
	(let [{:keys [data-through] :as doc} (source cfg)]
		(if-let [finding (get-in (visible doc req) [key :finding])]
			{:data-through data-through :findings [finding]}
			(adapter/fail! :ref-gone "fixture record is not visible at the requested cut"))))
