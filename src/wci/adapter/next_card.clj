(ns wci.adapter.next-card
  "A deliberately narrow reader for a source-authored NEXT RAIL/NOW card."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [wci.adapter :as adapter]
            [wci.adapter.git-source :as git]
            [wci.adapter.local-process :as process])
  (:import [java.nio.file Files LinkOption NoSuchFileException Paths]
           [java.nio.file.attribute BasicFileAttributes]
           [java.util Locale]))

(def opts-schema
  [:map {:closed true}
   [:root git/root-schema]
   [:rev {:optional true} git/rev-schema]
   [:path {:optional true} git/path-schema]
   [:git-channel {:optional true} :wci/channel-id]
   [:repo {:optional true} [:and [:string {:max 120}]
                            [:re "^[a-zA-Z0-9._-]+/[a-zA-Z0-9._-]+$"]]]])

(def ^:private card-origin "NEXT RAIL/NOW; current working file")
(def ^:private history-origin "NEXT RAIL/NOW; committed first-parent file only")
(def ^:private attester "NEXT handoff card")
(def ^:private max-file-bytes (* 256 1024))

(defn- live-file [root path]
  (when-not (re-matches (re-pattern (second git/path-schema)) path)
    (adapter/fail! :bad-request "invalid NEXT source path"))
  (try
    (let [root (.toRealPath (Paths/get root (make-array String 0)) (make-array LinkOption 0))
          file (.resolve root ^String path)
          opts (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])
          attr (Files/readAttributes file BasicFileAttributes opts)]
      (when-not (and (.isRegularFile attr) (= file (.toRealPath file (make-array LinkOption 0))))
        (adapter/fail! :contract "NEXT source must be a regular file under its configured root"))
      (with-open [in (Files/newInputStream file (into-array java.nio.file.OpenOption
                                                          [LinkOption/NOFOLLOW_LINKS]))]
        (let [bytes (.readNBytes in (inc max-file-bytes))]
          (when (> (alength bytes) max-file-bytes)
            (adapter/fail! :unreadable "NEXT source exceeds the local read limit"))
          (process/decode-utf8 bytes))))
    (catch NoSuchFileException _ nil)
    (catch java.io.IOException _ (adapter/fail! :unreadable "cannot read NEXT source"))))

(defn- card-lines [text]
  ;; Text inside fenced examples is not a source declaration.
  (loop [lines (seq (str/split-lines text)), marker nil, out []]
    (if-let [line (first lines)]
      (let [[_ fence suffix] (re-matches #"^ {0,3}(`{3,}|~{3,})(.*)$" line)]
        (cond
          (and fence (nil? marker)) (recur (next lines) fence out)
          (and fence (= (first fence) (first marker))
               (>= (count fence) (count marker)) (str/blank? suffix)) (recur (next lines) nil out)
          marker (recur (next lines) marker out)
          :else (recur (next lines) nil (conj out line))))
      out)))

(defn- sections [text]
  (let [lines (card-lines text)
        headings (keep-indexed
                  (fn [i s]
                    (when-let [[_ marks heading] (re-matches #"^(#{1,6})\s+(.+?)\s*#*\s*$" s)]
                      {:i i :depth (count marks) :heading heading
                       :section (second (re-find #"^(RAIL|NOW|RECENT|LEDGER|ACTIVE|DORMANT)\b" heading))})) lines)
        main (take-while #(not (contains? #{"RECENT" "LEDGER" "ACTIVE" "DORMANT"} (:section %))) headings)
        nows (filter #(= "NOW" (:section %)) main)
        first-now (:i (first nows))
        rails (filter #(and (= "RAIL" (:section %)) (or (nil? first-now) (< (:i %) first-now))) main)
        doc-title (:heading (first (take-while (comp nil? :section) main)))]
    (letfn [(body [{:keys [i depth]}]
              (let [end (or (some #(when (and (> (:i %) i)
                                              (or (:section %) (<= (:depth %) depth))) (:i %)) headings)
                            (count lines))]
                (subvec lines (inc i) end)))]
      {:rail (mapcat body rails) :now (mapcat body nows)
       :heading (some-> (first nows) :heading (str/replace #"^NOW\s*(?:[—–:-]\s*)?" ""))
       :doc-title doc-title})))

(def ^:private now-field
  #"^\s*[-*+]\s+(?:\*\*)?(Current|Read|State|Awaits|Next|Blocker|Verify|Do not touch|Stem|Detour|Return|Hot group)(?:\*\*)?:\s*(?:\*\*)?(.*)$")

(defn- now-fields [lines]
  (reduce
   (fn [out line]
     (if-let [[_ k raw] (re-matches now-field line)]
       (let [v (str/trim raw)]
         (cond
           (str/blank? v) out
           (and (contains? out k) (not= (get out k) v))
           (adapter/fail! :contract (str "NEXT has conflicting declarations for " k))
           :else (assoc out k v)))
       out))
   {} lines))

(defn- current-state [rail]
  (let [marked (set (keep #(some-> (re-find #"^\s*[-*+]\s+\[[ xX]\].*←\s*CURRENT\b.*$" %) str/trim) rail))]
    (when (> (count marked) 1)
      (adapter/fail! :contract "NEXT has conflicting RAIL coordinates"))
    (when (seq marked) "CURRENT")))

(defn- clip [text maximum]
  (if (<= (count text) maximum)
    text
    (let [suffix " … [clipped]"
          end (- maximum (count suffix))
          end (if (Character/isHighSurrogate (.charAt ^String text (dec end))) (dec end) end)]
      (str (str/trimr (subs text 0 end)) suffix))))

(defn- owner-doc-id [text]
  (when text
    (let [ids (set (keep (fn [s]
                          (or (second (re-matches #"^\s*<!--\s*wci:owner-doc\s+([0-9]{8}T[0-9]{6})\s*-->\s*$" s))
                              (second (re-matches #"^#{1,6}\s+담당자 문서\s+[—–-]\s+denote id\s+`([0-9]{8}T[0-9]{6})`\s*$" s))))
                        (card-lines text)))]
      (when (> (count ids) 1)
        (adapter/fail! :contract "conflicting owner document declarations"))
      (first ids))))

(defn- record-ref [label surface accepts locator]
  {:role :record :label label :surface {:kind surface :accepts accepts} :locator locator})

(defn- card-locator [{:keys [root oid]} path]
  (if oid (str root "@" oid ":" path) (str root "/" path)))

(defn- read-refs [version text]
  (when text
    (let [targets (if (re-find #"[\[`]" text)
                    (keep (fn [[_ link quoted]] (or link quoted))
                          (re-seq #"\[[^\]\r\n]+\]\(([^()\s]+)\)|`([^`]+)`" text))
                    (str/split text #"[,\s]+"))]
      (->> targets
           (keep (fn [target]
                   (let [[path anchor] (str/split target #"#" 2)]
                     (cond
                       (re-matches #"https://[a-zA-Z0-9.-]+(?::[0-9]+)?(?:/[a-zA-Z0-9_./%~+-]*)?(?:#[a-zA-Z0-9_./~-]+)?" target)
                       (record-ref "Open linked reading" "Linked source" "HTTPS URL" target)

                       (and (re-matches (re-pattern (second git/path-schema)) path)
                            (re-find #"[/\.]" path)
                            (or (nil? anchor) (re-matches #"[a-zA-Z0-9_./~-]+" anchor)))
                       (record-ref "Open linked reading" "Repository file"
                                   (if (:oid version) "repository, revision and path" "local path")
                                   (str (card-locator version path) (when anchor (str "#" anchor))))))))
           distinct))))

(defn- card-title [{:keys [heading doc-title]}]
  (clip (or (some #(when-not (str/blank? %) %) [heading doc-title]) "Repository handoff") 200))

(defn- recorded-times [at] (cond-> {} at (assoc :source at)))

(defn- reading-key [path] [:next/read path])

(defn- reading [{:keys [text at path] :as version}]
  (let [links (read-refs version (get (now-fields (:now (sections text))) "Read"))]
    (mapv (fn [link]
            {:key [:next/read path (:locator link)] :title (:label link)
             :times (recorded-times at) :refs [link]}) links)))

(defn- finding [{:keys [text at path root repo git-channel agents] :as version}]
  (let [{:keys [rail now] :as card} (sections text)
        {:strs [Current Read State Awaits]} (now-fields now)
        rail-state (current-state rail)
        state (or State rail-state)
        who (get {"human" :human "owner" :owner "external" :external} Awaits)
        doc-id (owner-doc-id agents)
        reads (count (read-refs version Read))]
    (cond-> {:key path :title (card-title card) :times (recorded-times at)
             :refs [(record-ref "Open the handoff card" "NEXT handoff card"
                                (if (:oid version) "repository, revision and path" "local path")
                                (card-locator version path))]}
      Current (assoc :gist (cond-> {:text (clip Current 200) :asserted-by attester} at (assoc :at at)))
      state (assoc :state {:text (clip state 80) :asserted-by attester})
      who (assoc :awaits {:who who :asserted-by attester})
      (and git-channel at) (update :refs conj {:role :history :label "Commits after this handoff"
                                               :channel git-channel :key (git/history-since-key at)})
      (pos? reads) (update :refs conj {:role :more :label (str "읽을 곳 " reads) :key (reading-key path)})
      true (update :refs conj {:role :contact :label "Ask the repository owner"
                               :contact {:address (cond-> {:kind :repo-owner :cwd root}
                                                    repo (assoc :repo repo)
                                                    doc-id (assoc :doc-id doc-id))
                                         :scope "Repository handoff"}}))))

(defn- version [{:keys [root path repo git-channel] :or {path "NEXT.md"} :as opts} {:keys [live as-of]}]
  (let [ctx (git/context opts)
        root (.getCanonicalPath (io/file root))
        common {:root root :path path :repo repo :git-channel git-channel}]
    (git/require-working-root! ctx)
    (if live
      (let [text (or (live-file root path) (adapter/fail! :unreadable "NEXT working file is missing"))
            tip (:tip ctx)
            ;; Only a byte-for-byte matching committed version can attest a commit time.
            at (when (= text (git/file-at ctx tip path)) (git/file-change-at ctx tip path))]
        (when (and at (not (git/before? at as-of)))
          (adapter/fail! :unsupported-time "NEXT recording time does not precede the knowledge cut"))
        (assoc common :text text :at at :agents (live-file root "AGENTS.md")))
      (let [{:keys [oid]} (git/first-parent-at ctx as-of)
            text (or (git/file-at ctx oid path)
                     (adapter/fail! :unsupported-time "NEXT was not recorded in the selected past tree"))]
        (assoc common :oid oid :text text :at (git/file-change-at ctx oid path)
               :agents (git/file-at ctx oid "AGENTS.md"))))))

(defmethod adapter/describe :next-card [{:keys [title opts]}]
  (git/require-working-root! (git/context opts))
  {:kind "next-card" :title (or title "Repository handoff card")
   :as-of-support :replay :question :keyword
   :attests #{:source :gist :state :awaits} :contact-liveness :none})

(defmethod adapter/answer :next-card [{:keys [opts]} {:keys [mode question live] :as req}]
  (let [item (finding (version opts req))
        search? (= :question mode)
        f (fn [^String s] (.toLowerCase s Locale/ROOT))]
    {:matched-by (str (when search? "title or gist keyword; ") (if live card-origin history-origin))
     :findings (if (or (not search?) (some #(str/includes? (f %) (f question))
                                         (keep identity [(:title item) (get-in item [:gist :text])])))
                 [item] [])}))

(defmethod adapter/expand :next-card [{:keys [opts]} key {:keys [live] :as req}]
  (let [{:keys [path] :as v} (version opts req)
        origin (if live card-origin history-origin)]
    (cond
      (= key path) {:matched-by origin :findings [(finding v)]}
      (= key (reading-key path))
      (let [rows (reading v)]
        (when-not (seq rows) (adapter/fail! :ref-gone "NEXT reading links are no longer declared"))
        {:matched-by origin :findings rows})
      :else (adapter/fail! :ref-gone "NEXT card is not visible at the requested cut"))))
