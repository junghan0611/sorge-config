(ns wci.adapter-layered-timeline-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [wci.adapter :as adapter]
            [wci.config :as config]
            [wci.core :as core])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private now "2026-11-26T09:00:00+09:00")
(def ^:private request {:mode :overview :live true :as-of now :horizon-days 14})
(def ^:private opts {:now now :token-secret "test-secret-0123456789"})
(def ^:private layers
  [{:layer "recorded" :entity "sample" :event "registration" :label "Specimen registered" :instant "2026-11-25T23:00:00Z"}
   {:layer "reported" :entity "sample" :event "meeting" :label "Claimed meeting" :day "2026-11-25"}
   {:layer "planned" :entity "sample" :event "expiry" :label "Specimen due" :instant nil :day "2026-12-01"}
   {:layer "recorded" :entity "sample" :event "close" :label "At the exclusive cut" :instant now}])
(def ^:private doc {:events layers :errors {} :dropped 0})

(defn- trial [document exit f]
  (let [file (.toFile (Files/createTempFile "wci-fake-timeline-" ".sh" (make-array FileAttribute 0)))]
    (try
      (spit file (str "#!/bin/sh\n[ \"$#\" -eq 1 ] && [ \"$1\" = recent ] || exit 98\nprintf '%s' '"
                      (str/replace (json/generate-string document) "'" "'\\''") "'\nexit " exit "\n"))
      (when-not (.setExecutable file true true) (throw (ex-info "cannot prepare fake timeline" {})))
      (f (config/validate-config
          {:channels [{:id "samples" :kind :layered-timeline
                       :opts {:argv [(str file) "recent"] :events-path ["events"] :errors-path ["errors"]
                              :dropped-count-path ["dropped"] :layer-path ["layer"] :instant-path ["instant"]
                              :day-path ["day"] :title-path ["label"] :identity-paths [["entity"] ["event"]]
                              :asserted-by "Synthetic register"}}]}))
      (finally (Files/deleteIfExists (.toPath file))))))

(defn- run [cfg where] (core/run cfg (if where {:op :ask :where where} {:op :overview}) opts))

(deftest layered-dates-keep-their-own-assertion
  (trial doc 0
         (fn [cfg]
           (let [channel (first (:channels cfg))
                 found (:findings (adapter/answer channel request))
                 [recorded planned] found]
             (is (= #{:source :expected} (:attests (adapter/describe channel))))
             (is (= 2 (count found)))
             (is (not= (:key recorded) (:key planned)))
             (is (= {:source "2026-11-26T08:00:00+09:00"} (:times recorded)))
             (is (= {:expected {:at "2026-12-01" :asserted-by "Synthetic register"}} (:times planned)))
             (is (= [recorded] (:findings (adapter/expand channel (:key recorded) (assoc request :mode :follow)))))
             (is (= ["Specimen due"] (mapv :title (:findings (run cfg [[:expected :within [0 14]]])))))
             (is (= ["Specimen registered"] (mapv :title (:findings (run cfg [[:source :after -7]])))))
             (is (= :uninterpreted (get-in (run cfg [[:last-movement :before -7]]) [:coverage 0 :status])))))))

(deftest incomplete-and-malformed-feeds-never-become-empty-success
  (doseq [[data exit expected] [[(assoc doc :errors {"segment" "not observed"}) 0 :unreadable]
                               [(assoc doc :dropped 1) 0 :unreadable]
                               [(dissoc doc :errors) 0 :contract]
                               [(update doc :events conj (first layers)) 0 :contract]
                               [(assoc-in doc [:events 0 :layer] "future") 0 :contract]
                               [(assoc-in doc [:events 0 :instant] "2026-11-26") 0 :contract]
                               [doc 7 :unreadable]]]
    (trial data exit #(is (= expected (get-in (run % nil) [:failures 0 :kind])))))
  (trial (assoc doc :events []) 0 #(is (= :empty (get-in (run % nil) [:coverage 0 :status]))))
  (trial doc 7 #(is (= :unsupported-time
                      (get-in (core/run % {:op :overview :scope {:as-of "2026-11-25"}} opts) [:failures 0 :kind])))))
