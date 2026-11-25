(ns wci.config
	"Read and validate machine-local channel declarations before using them."
	(:require [clojure.java.io :as io]
	          [malli.core :as m]
	          [wci.adapter :as adapter]
	          [wci.adapter.edn-io :as edn-io]))

(def ^:private registry
	(delay (merge (m/default-schemas)
	              (:registry (edn-io/read-one (io/resource "wci.edn"))))))

(defn- config-error! [message]
	(throw (ex-info message {:wci/exit-code 3 :wci/error-kind :bad-request})))

(defn- validate-opts! [{:keys [id kind opts] :or {opts {}}}]
	;; The kind names an installed implementation; it is not a file path or code.
	(when-not (and (simple-keyword? kind) (re-matches #"[a-z][a-z0-9-]*" (name kind)))
		(config-error! (str "invalid kind name for channel " id)))
	(let [schema (try
	               (adapter/load-kind! kind)
	               (some-> (ns-resolve (symbol (str "wci.adapter." (name kind))) 'opts-schema) deref)
	               (catch Exception _ nil)
	               (catch LinkageError _ nil))]
		(when-not (and schema (every? #(contains? (methods %) kind)
		                           [adapter/describe adapter/answer adapter/expand]))
			(config-error! (str "missing channel implementation or opts-schema for " id)))
		;; Check the root is actually a closed map; configuration has no extra opts.
		(when-not (try
		            (let [s (m/schema schema {:registry @registry})]
			            (and (= :map (m/type s)) (true? (:closed (m/properties s)))
			                 (m/validate s opts)))
		            (catch Exception _ false))
			(config-error! (str "invalid opts for channel " id)))))

(defn validate-config
	"Return the declaration unchanged, or throw an exit-3 configuration error."
	[cfg]
	(when-not (m/validate :channel/config cfg {:registry @registry})
		(config-error! "invalid channel configuration"))
	(let [ids (mapv :id (:channels cfg))]
		(when-not (= (count ids) (count (distinct ids)))
			(config-error! "duplicate channel id")))
	(run! validate-opts! (:channels cfg))
	cfg)

(defn load-config
	"Read a single EDN document. Relative source paths are resolved by adapters from cwd."
	([] (load-config "channels.local.edn"))
	([path]
	 (validate-config
	  (try
	   (edn-io/read-one (io/file path))
	   (catch java.io.IOException _ (config-error! "cannot read channel configuration"))
	   (catch Exception _ (config-error! "invalid channel configuration EDN"))))))
