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
                      {:i i :depth (count marks) :heading heading})) lines)
        named (fn [name] (filter #(re-find (re-pattern (str "^" name "(?:\\s|$)")) (:heading %)) headings))
        rails (named "RAIL") nows (named "NOW")
        rail (first rails) now (first nows)]
    (when-not (and (= 1 (count rails) (count nows)) (< (:i rail) (:i now)))
      (adapter/fail! :contract "unrecognized NEXT template: expected one RAIL before one NOW"))
    (letfn [(body [{:keys [i depth]}]
              (let [end (or (some #(when (and (> (:i %) i) (<= (:depth %) depth)) (:i %)) headings) (count lines))]
                (subvec lines (inc i) end)))]
      {:rail (body rail) :now (body now) :heading (:heading now)})))

(defn- now-fields [lines]
  (reduce
   (fn [out line]
     (if-let [[_ k v] (re-matches #"^\s*[-*]\s+(Current|Read|State|Awaits):\s*(.*)$" line)]
       (do (when (contains? out k)
             (adapter/fail! :contract "NEXT has duplicate declared card fields"))
           (assoc out k (str/trim v)))
       out))
   {} lines))

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

(defn- finding [{:keys [text at path root repo agents] :as version}]
  (let [{:keys [rail now heading]} (sections text)
        {:strs [Current Read State Awaits]} (now-fields now)
        title (str/replace heading #"^NOW\s*(?:[—–:-]\s*)?" "")
        state (or State (when (some #(re-find #"^\s*[-*]\s+\[[ xX]\].*\bCURRENT\b" %) rail) "CURRENT"))
        who (get {"human" :human "owner" :owner "external" :external} Awaits)
        doc-id (owner-doc-id agents)]
    (when (or (str/blank? Current) (> (count Current) 200)
              (> (count State) 80) (> (count title) 200) (and Awaits (nil? who)))
      (adapter/fail! :contract "unrecognized or oversized declared NEXT field"))
    (cond-> {:key path :title (if (str/blank? title) "Repository handoff" title)
             :times (cond-> {} at (assoc :source at))
             :gist (cond-> {:text Current :asserted-by attester} at (assoc :at at))
             :refs (into [(record-ref "Open the handoff card" "NEXT handoff card"
                                      (if (:oid version) "repository, revision and path" "local path")
                                      (card-locator version path))]
                         (read-refs version Read))}
      (not (str/blank? state)) (assoc :state {:text state :asserted-by attester})
      who (assoc :awaits {:who who :asserted-by attester})
      doc-id (update :refs conj {:role :contact :label "Ask the repository owner"
                                 :contact {:address (cond-> {:kind :repo-owner :cwd root :doc-id doc-id}
                                                      repo (assoc :repo repo))
                                           :scope "Repository handoff and owner document"}}))))

(defn- version [{:keys [root path repo] :or {path "NEXT.md"} :as opts} {:keys [live as-of]}]
  (let [ctx (git/context opts)
        root (.getCanonicalPath (io/file root))
        common {:root root :path path :repo repo}]
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
                                         [(:title item) (get-in item [:gist :text])]))
                 [item] [])}))

(defmethod adapter/expand :next-card [cfg key req]
  (let [result (adapter/answer cfg (assoc req :mode :follow))]
    (when-not (= key (get-in result [:findings 0 :key]))
      (adapter/fail! :ref-gone "NEXT card is not visible at the requested cut"))
    result))
