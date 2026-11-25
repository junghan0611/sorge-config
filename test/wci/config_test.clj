(ns wci.config-test
	(:require [clojure.test :refer [deftest is testing]]
	          [wci.config :as config])
	(:import [java.nio.file Files]
	         [java.nio.file.attribute FileAttribute]))

(defn- exit-code [f]
	(try (f) nil (catch clojure.lang.ExceptionInfo e (:wci/exit-code (ex-data e)))))

(deftest configuration-validation
	(let [cfg (config/load-config "channels.example.edn")]
		(is (= ["tracker" "notes"] (mapv :id (:channels cfg))))
		(doseq [[label bad] [["outer key" (assoc cfg :surprise true)]
		                     ["inner key" (assoc-in cfg [:channels 0 :opts :shell] "false")]
		                     ["required path" (update-in cfg [:channels 0] dissoc :opts)]
		                     ["unknown kind" (assoc-in cfg [:channels 0 :kind] :absent-kind)]
		                     ["namespaced kind" (assoc-in cfg [:channels 0 :kind] :elsewhere/fixture)]
		                     ["missing kind methods" (assoc-in cfg [:channels 0 :kind] :edn-io)]
		                     ["duplicate id" (assoc-in cfg [:channels 1 :id] "tracker")]]]
			(testing label (is (= 3 (exit-code #(config/validate-config bad))))))))

(deftest file-input-errors
	(let [path (Files/createTempFile "wci-config-" ".edn" (make-array FileAttribute 0))]
		(try
			(doseq [s ["" "{} {}" "#=(+ 1 2)"]]
				(spit (.toFile path) s)
				(is (= 3 (exit-code #(config/load-config (.toFile path))))))
			(finally (Files/deleteIfExists path)))
		(is (= 3 (exit-code #(config/load-config (.toFile path)))))))
