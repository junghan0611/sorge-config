(ns wci.adapter-local-test
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [wci.adapter :as adapter]
            [wci.adapter.edn-io :as edn-io]
            [wci.config :as config]
            [wci.core :as core])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private schemas
  (delay {:registry (merge (m/default-schemas)
                           (:registry (edn-io/read-one (io/resource "wci.edn"))))}))

(def ^:private git-env
  (into {"GIT_CONFIG_NOSYSTEM" "1" "GIT_CONFIG_GLOBAL" "/dev/null"
         "GIT_TERMINAL_PROMPT" "0" "GIT_ALLOW_PROTOCOL" "file"}
        (remove #(str/starts-with? (key %) "GIT_")) (System/getenv)))

(def ^:private live {:mode :overview :live true :as-of "2026-11-26T09:00:00+09:00" :horizon-days 14})
(def ^:private past (assoc live :live false :as-of "2026-11-24T09:00:00+09:00"))

(defn- git! [dir & argv]
  (let [{:keys [exit out err]} (apply sh/sh "git" (concat argv [:dir dir :env git-env]))]
    (when-not (zero? exit) (throw (ex-info (str "synthetic Git setup failed: " err) {:exit exit})))
    (str/trim out)))

(defn- commit! [dir date title body]
  (git! dir "add" ".")
  (let [env (assoc git-env "GIT_COMMITTER_DATE" date "GIT_AUTHOR_DATE" date)
        {:keys [exit err]} (sh/sh "git" "commit" "-m" title "-m" body :dir dir :env env)]
    (when-not (zero? exit) (throw (ex-info (str "synthetic commit failed: " err) {:exit exit}))))
  (git! dir "rev-parse" "HEAD"))

(defn- temp-directory [f]
  (let [root (.toFile (Files/createTempDirectory "wci-local-" (make-array FileAttribute 0)))]
    (try (f root) (finally (doseq [file (reverse (file-seq root))] (io/delete-file file))))))

(defn- card [gist awaits guide]
  (str "# Fixture handoff\n\n# RAIL\n\n- [ ] Inspect sample ← CURRENT\n\n"
       "# NOW — Fixture inspection\n\n- Current: " gist "\n"
       "- Awaits: " awaits "\n"
       "- Read: [Guide](" guide "), `../elsewhere.md`\n"
       "\n```text\n- Awaits: external\n# RAIL\n```\n"))

(defn- owner! [root id]
  (spit (io/file root "AGENTS.md")
        (str "# Fixture role\n\n<!-- wci:owner-doc " id " -->\n"
             "\n```text\n<!-- wci:owner-doc 20000101T110000 -->\n```\n")))

(defn- repo! [root]
  (git! root "init" "--template=" "--initial-branch=main")
  (git! root "config" "user.name" "Synthetic Author")
  (git! root "config" "user.email" "author@fixture.invalid")
  (spit (io/file root "NEXT.md") (card "Prepare the first reading." "owner" "docs/old-guide.md"))
  (owner! root "20000101T000000")
  (let [old (commit! root "2026-11-16T08:00:00-05:00" "Prepare test specimen" "Refs: #17, #17; outside#99")]
    (spit (io/file root "NEXT.md") (card "Review the second reading." "human" "docs/new-guide.md"))
    (owner! root "20000101T010000")
    (let [new (commit! root "2026-11-24T09:00:00+09:00" "Record test specimen" "Private fixture-only body")]
      (spit (io/file root "MARKER") "non-card change")
      (commit! root "2026-11-25T08:00:00+09:00" "Add fixture marker" "")
      {:old old :new new})))

(defn- cfg [kind root opts]
  (-> {:channels [{:id "sample" :kind kind :opts (assoc opts :root (str root))}]}
      config/validate-config :channels first))

(defn- fails-with [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:wci/error-kind (ex-data e)))))

(deftest git-adapter-reads-only-reachable-history
  (temp-directory
   (fn [root]
     (let [{:keys [old new]} (repo! root)
           cfg (cfg :git root {:rev "refs/heads/main" :issue-base "https://tracker.invalid/fixture/issues"})
           query {:mode :where :where [[:source :after -14]]}
           historical (adapter/answer cfg (merge past query))
           live-answer (adapter/answer cfg (merge live query))
           [item] (:findings historical)]
       (is (m/validate :channel/describe (adapter/describe cfg) @schemas))
       (is (m/validate :channel/answer historical @schemas))
       (is (m/validate :channel/answer live-answer @schemas))
       (is (str/includes? (:matched-by historical) "reachable"))
       (is (= old (:key item)))
       (is (= {:source "2026-11-16T22:00:00+09:00"} (:times item)))
       (is (= ["https://tracker.invalid/fixture/issues/17"]
              (mapv :locator (rest (:refs item)))))
       (is (not (str/includes? (pr-str live-answer) "Private fixture-only body")))
       (is (= [new] (mapv :key (:findings (adapter/answer cfg (assoc live :mode :question :question "RECORD TEST"))))))
       (is (= :ref-gone (fails-with #(adapter/expand cfg new (assoc past :mode :follow)))))
       (is (= old (get-in (adapter/expand cfg old (assoc past :mode :follow)) [:findings 0 :key])))
       (let [no-issue-base (update cfg :opts dissoc :issue-base)]
         (is (= 1 (count (get-in (adapter/answer no-issue-base (merge past query)) [:findings 0 :refs])))))))))

(deftest git-overview-summarizes-one-area-and-keeps-records-for-follow
  (temp-directory
   (fn [root]
     (repo! root)
     (let [cfg (assoc (cfg :git root {}) :area {:key "bench" :label "Bench trials"})
           response (adapter/answer cfg (assoc live :horizon-days 2))
           [row] (:findings response)
           [history] (:refs row)
           followed (adapter/expand cfg (:key history) (assoc live :mode :follow))]
       (is (m/validate :channel/answer response @schemas))
       (is (= 1 (count (:findings response))))
       (is (= ["Bench trials" "2026-11-25T08:00:00+09:00" "2 commits in the last 2 days"]
              [(:title row) (get-in row [:times :last-movement]) (get-in row [:gist :text])]))
       (is (= :history (:role history)))
       (is (= ["Add fixture marker" "Record test specimen" "Prepare test specimen"]
              (mapv :title (:findings followed))))
       (is (string? (get-in (adapter/answer cfg (assoc live :mode :where :where [[:last-movement :before -7]]))
                            [:uninterpreted :reason])))))))

(deftest next-adapter-replays-card-links-and-ownership-at-the-same-cut
  (temp-directory
   (fn [root]
     (let [{:keys [old]} (repo! root)
           cfg (cfg :next-card root {:repo "fixture/sample"})
           historical (adapter/answer cfg past)
           [item] (:findings historical)
           [record more contact] (:refs item)
           read (get-in (adapter/expand cfg (:key more) (assoc past :mode :follow)) [:findings 0 :refs 0])
           committed (first (:findings (adapter/answer cfg live)))]
       (is (m/validate :channel/describe (adapter/describe cfg) @schemas))
       (is (m/validate :channel/answer historical @schemas))
       (is (str/includes? (:matched-by historical) "committed first-parent file only"))
       (is (= "Prepare the first reading." (get-in item [:gist :text])))
       (is (= :owner (get-in item [:awaits :who])))
       (is (= "CURRENT" (get-in item [:state :text])))
       (is (= "2026-11-16T22:00:00+09:00" (get-in item [:times :source])))
       (is (str/ends-with? (:locator record) (str "@" old ":NEXT.md")))
       (is (= [:more "읽을 곳 1"] ((juxt :role :label) more)))
       (is (str/ends-with? (:locator read) (str "@" old ":docs/old-guide.md")))
       (is (= {:kind :repo-owner :cwd (.getCanonicalPath root)
               :repo "fixture/sample" :doc-id "20000101T000000"}
              (get-in contact [:contact :address])))
       (let [no-slug (adapter/answer (update cfg :opts dissoc :repo) past)]
         (is (= {:kind :repo-owner :cwd (.getCanonicalPath root) :doc-id "20000101T000000"}
                (get-in no-slug [:findings 0 :refs 2 :contact :address]))))
       (is (= 3 (count (:refs item))))
       (is (= "2026-11-24T09:00:00+09:00" (get-in committed [:times :source])))
       (is (= :unsupported-time (fails-with #(adapter/answer cfg (assoc past :as-of "2026-11-16T22:00:00+09:00")))))
       (is (= :unsupported-time (fails-with #(adapter/answer (assoc-in cfg [:opts :path] "Absent.md") past))))
       (testing "live content does not borrow committed time or overwrite history"
         (spit (io/file root "NEXT.md") (card "An uncommitted reading needs review." "human" "docs/work-guide.md"))
         (owner! root "20000101T020000")
         (let [work (first (:findings (adapter/answer cfg live)))]
           (is (= "An uncommitted reading needs review." (get-in work [:gist :text])))
           (is (= {} (:times work)))
           (is (= "20000101T020000" (get-in work [:refs 2 :contact :address :doc-id])))
           (is (= historical (adapter/answer cfg past)))))
       (spit (io/file root "NEXT.md") "# A bare handoff card\n")
       (is (= {:title "A bare handoff card" :times {}}
              (select-keys (get-in (adapter/answer cfg live) [:findings 0]) [:title :times :gist :state :awaits])))))))

(deftest handoff-skill-variants-do-not-require-current
  (temp-directory
   (fn [root]
     (repo! root)
     (let [cfg (cfg :next-card root {})
           file (io/file root "NEXT.md")
           rail "# RAIL — current\n- [ ] **Current checkpoint** ← CURRENT: inspect the specimen\n"
           body {"stem" (str rail "# NOW — Trial stem\n- Stem: Complete trials\n- Next: Review reading\n- Detour: None\n- Return: Repeat after review\n- Blocker: None\n")
                 "meta" (str rail "# NOW\n- Hot group: Trials\n- Next: Review reading\n# ACTIVE\n## Trials\n- Current: An area-specific statement not the current overview\n# DORMANT\n")
                 "duplicated outline" (str rail "# RAIL — details\n- [x] Preliminary work\n# NOW\n- Next: Review reading\n# RECENT\n## NOW — Old card\n- Current: Old content\n")}]
       (doseq [[label text] body]
         (testing label
           (spit file text)
           (let [result (adapter/answer cfg live) row (get-in result [:findings 0])]
             (is (m/validate :channel/answer result @schemas))
             (is (= "CURRENT" (get-in row [:state :text])))
             (is (not (contains? row :gist)))
             (is (not (contains? row :awaits))))))
       (testing "duplicates with identical claims are fine; conflict is not"
         (spit file (str rail "# NOW\n- **Current:** One reading\n- Current: One reading\n"))
         (is (= "One reading" (get-in (adapter/answer cfg live) [:findings 0 :gist :text])))
         (spit file (str rail "# NOW\n- Current: First reading\n- Current: Different reading\n"))
         (is (= :contract (fails-with #(adapter/answer cfg live)))))
       (testing "clipping is explicit, bounded and safe at a surrogate boundary"
         (spit file (str rail "# NOW\n- Current: " (apply str (repeat 187 "x")) "🧪" (apply str (repeat 60 "x")) "\n"))
         (let [text (get-in (adapter/answer cfg live) [:findings 0 :gist :text])]
           (is (<= (count text) 200))
           (is (str/ends-with? text " … [clipped]"))
           (is (not-any? #(Character/isSurrogate %) text))))))))

(deftest folded-reading-links-and-cross-channel-history-have-separate-follows
  (temp-directory
   (fn [root]
     (repo! root)
     (let [git (assoc (cfg :git root {}) :id "fixture-git")
           next (assoc (cfg :next-card root {:git-channel "fixture-git"}) :id "fixture-next")
           cfg (config/validate-config {:channels [git next]})
           opts {:now (:as-of live) :token-secret "test-secret-0123456789"}
           ask #(core/run cfg % opts)
           overview #(ask {:op :overview :scope {:include ["fixture-next"]}})
           find-ref (fn [result role] (first (filter #(= role (:role %)) (get-in result [:findings 0 :refs]))))
           history (find-ref (overview) :history)]
       (is (= "fixture-git" (:channel history)))
       (is (= ["Add fixture marker"] (mapv :title (:findings (ask {:op :follow :ref (:ref history)})))))
       (let [file (io/file root "NEXT.md")
             targets (map #(str "`docs/item-" % ".md`") (range 6))]
         (spit file (str "# RAIL\n- [ ] Check specimen ← CURRENT\n# NOW\n- Current: View the six readings.\n- Read: "
                         (str/join ", " targets) "\n"))
         (let [r (overview) more (find-ref r :more)
               detail (ask {:op :follow :ref (:ref more) :budget {:max-items 2}})]
           (is (= "읽을 곳 6" (:label more)))
           (is (= 1 (count (filter #(= :record (:role %)) (get-in r [:findings 0 :refs])))))
           (is (nil? (find-ref r :history)) "an uncommitted card has no attested edit time")
           (is (= [true 4] [(:truncated detail) (get-in detail [:rest :count])]))
           (is (every? #(= [:record] (mapv :role (:refs %))) (:findings detail)))))))))

(deftest next-as-of-does-not-read-unmerged-branch-state
  (temp-directory
   (fn [root]
     (let [{:keys [old]} (repo! root)
           cfg (cfg :next-card root {})]
       (git! root "switch" "-c" "experiment" old)
       (spit (io/file root "SIDE") "irrelevant side changes")
       (commit! root "2026-11-17T09:00:00+09:00" "Run side experiment" "")
       (git! root "switch" "main")
       (let [env (assoc git-env "GIT_COMMITTER_DATE" "2026-11-25T11:00:00+09:00")
             {:keys [exit]} (sh/sh "git" "merge" "--no-ff" "-m" "Merge test experiment" "experiment"
                                  :dir root :env env)]
         (is (zero? exit)))
       (let [lifted (adapter/answer cfg (assoc past :as-of "2026-11-19T09:00:00+09:00"))
             [card] (:findings lifted)]
         (is (= "Prepare the first reading." (get-in card [:gist :text])))
         (is (str/ends-with? (get-in card [:refs 0 :locator]) (str "@" old ":NEXT.md"))))))))

(deftest shallow-history-cannot-certify-replay
  (temp-directory
   (fn [dir]
     (let [root (io/file dir "source")
           shallow (io/file dir "partial")]
       (.mkdir root)
       (repo! root)
       (git! dir "clone" "--depth=1" (str "file://" (.getAbsolutePath root)) (str shallow))
       (doseq [kind [:git :next-card]]
         (is (= :unsupported-time (fails-with #(adapter/answer (cfg kind shallow {}) past)))))))))
