(ns wci.adapter-fixture-test
	(:require [clojure.java.io :as io]
	          [clojure.test :refer [deftest is testing]]
	          [malli.core :as m]
	          [wci.adapter :as adapter]
	          [wci.adapter.edn-io :as edn-io]
	          [wci.config :as config]))

(def ^:private sample (delay (:channels (config/load-config "channels.example.edn"))))
(def ^:private schemas
	(delay {:registry (merge (m/default-schemas)
	                        (:registry (edn-io/read-one (io/resource "wci.edn"))))}))
(def ^:private live {:mode :overview :live true :as-of "2026-11-26T09:00:00+09:00" :horizon-days 14})

(defn- failure-kind [f]
	(try (f) nil (catch clojure.lang.ExceptionInfo e (:wci/error-kind (ex-data e)))))

(deftest synthetic-channel-observations
	(let [[t x] @sample]
		(testing "overview and where leave judgement and budgets to the core"
			(let [where (assoc live :mode :where :where [[:expected :within [0 14]] [:last-movement :before -7]])
			      t-answer (adapter/answer t where)
			      by-key (into {} (map (juxt :key identity)) (:findings t-answer))
			      x-answer (adapter/answer x where)
			      x-finding (first (:findings x-answer))]
				(doseq [cfg [t x]]
					(is (m/validate :channel/describe (adapter/describe cfg) @schemas))
					(is (m/validate :channel/answer (adapter/answer cfg live) @schemas)))
				(is (= #{"lamp" "dial" "hinge" "upper" "cutoff" "recent"} (set (keys by-key))))
				(is (= "2026-12-10" (get-in by-key ["upper" :times :expected :at])))
				(is (= "2026-11-19T09:00:00+09:00" (get-in by-key ["cutoff" :times :last-movement])))
				(is (= "2026-11-19T09:00:01+09:00" (get-in by-key ["recent" :times :last-movement])))
				(is (= "2026-11-19T08:59:59+09:00" (get-in by-key ["lamp" :times :last-movement])))
				(is (= #{:source} (:attests (adapter/describe x))))
				(is (= #{:source} (set (keys (:times x-finding)))))))
		(testing "interpreted zero differs from no interpretation"
			(let [query (assoc live :mode :question :question "No matching term")]
				(is (= [] (:findings (adapter/answer t query))))
				(is (= "title keyword" (:matched-by (adapter/answer t query))))
				(is (string? (get-in (adapter/answer x query) [:uninterpreted :reason])))
				(is (= ["lamp"] (mapv :key (:findings (adapter/answer t (assoc query :question "LAMP"))))))))
		(testing "unreadable is not empty; replay is a capability"
			(let [bad (assoc-in x [:opts :fail] :unreadable)
			      past (assoc live :live false :as-of "2026-11-19T08:59:59+09:00")
			      lamp (first (filter #(= "lamp" (:key %)) (:findings (adapter/answer t past))))]
				(is (= :unreadable (failure-kind #(adapter/answer bad live))))
				(is (= :unreadable (failure-kind #(adapter/describe bad))))
				(is (= :unsupported-time (failure-kind #(adapter/answer x past))))
				(is (= "The trial is waiting for its first measurements." (get-in lamp [:gist :text])))
				(is (nil? (get-in lamp [:times :expected])))
				(is (= [] (:refs lamp)))))))

(deftest synthetic-follow-ends-at-record-and-contact
	(let [[t _] @sample
	      query (assoc live :mode :question :question "bench lamp")
	      follow (assoc live :mode :follow)
	      history (first (get-in (adapter/answer t query) [:findings 0 :refs]))
	      history-ans (adapter/expand t (:key history) follow)
	      evidence (first (get-in history-ans [:findings 0 :refs]))
	      evidence-ans (adapter/expand t (:key evidence) follow)
	      [record contact] (get-in evidence-ans [:findings 0 :refs])]
		(is (= [:history :evidence :record :contact] (mapv :role [history evidence record contact])))
		(is (= "https://tracker.invalid/reading/lamp-1" (:locator record)))
		(is (= :garden-id (get-in contact [:contact :address :kind])))
		(is (every? #(not (contains? % :key)) [record contact]))
		(is (m/validate :channel/answer history-ans @schemas))
		(is (m/validate :channel/answer evidence-ans @schemas))
		(is (= :ref-gone (failure-kind #(adapter/expand t (:key history)
		                                             (assoc follow :live false :as-of "2026-11-16T09:00:00+09:00")))))))
