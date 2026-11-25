(ns wci.cli
  "clojure -M:run <op> … → JSON 한 줄 (stdout).

    channels
    overview [--as-of T] [--include a,b] [--exclude a,b] [--max-items N] [--max-bytes N]
    ask (--question TEXT | --where EDN) [같은 scope·budget 플래그]
    follow REF [--max-items N] [--max-bytes N]
    공통: --config PATH (기본 ./channels.local.edn)

  exit (spec 머리 주석): 0 success · 1 에러 봉투 · 2 호출 위반(인자, :bad-request) · 3 설정/IO ·
  4 코어 불변식 위반 — 봉투가 아니라 코어 버그다 (stderr)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [wci.core :as core]
            [wci.spec :as spec]
            [wci.time :as t]))

(defn- usage! [msg] (throw (ex-info msg {::usage true})))

(defn- int-arg [flag v]
  (or (try (Long/parseLong (str v)) (catch NumberFormatException _ nil))
      (usage! (str flag " needs an integer"))))

(defn- ids-arg [v] (vec (remove str/blank? (str/split (str v) #","))))

(defn parse-args
  "argv → {:config path :request map}. 모르는 플래그·빠진 값은 호출 위반."
  [args]
  (let [[op & more] args]
    (when-not (#{"channels" "overview" "ask" "follow"} op)
      (usage! "usage: channels | overview | ask (--question T | --where EDN) | follow REF [--config PATH]"))
    (loop [xs more acc {:op (keyword op)}]
      (if (empty? xs)
        (let [{:keys [op config as-of include exclude max-items max-bytes question where ref]} acc
              scope  (cond-> {} as-of (assoc :as-of as-of) include (assoc :include include)
                             exclude (assoc :exclude exclude))
              budget (cond-> {} max-items (assoc :max-items max-items) max-bytes (assoc :max-bytes max-bytes))
              req    (cond-> {:op op}
                       (and (#{:overview :ask} op) (seq scope)) (assoc :scope scope)
                       (and (not= :channels op) (seq budget)) (assoc :budget budget)
                       question (assoc :question question)
                       where    (assoc :where where)
                       ref      (assoc :ref ref))]
          (when (and (= :follow op) (nil? ref)) (usage! "follow needs a REF"))
          (when (and (not= :follow op) ref) (usage! (str "unexpected argument " ref)))
          {:config (or config "channels.local.edn") :request req})
        (let [[f v & tail] xs
              need (fn [] (when (nil? v) (usage! (str f " needs a value"))) v)]
          (case f
            "--config"    (recur tail (assoc acc :config (need)))
            "--as-of"     (recur tail (assoc acc :as-of (need)))
            "--include"   (recur tail (assoc acc :include (ids-arg (need))))
            "--exclude"   (recur tail (assoc acc :exclude (ids-arg (need))))
            "--max-items" (recur tail (assoc acc :max-items (int-arg f (need))))
            "--max-bytes" (recur tail (assoc acc :max-bytes (int-arg f (need))))
            "--question"  (recur tail (assoc acc :question (need)))
            "--where"     (recur tail (assoc acc :where
                                             (try (edn/read-string (need))
                                                  (catch Exception _ (usage! "--where needs an EDN vector")))))
            (if (and (= :follow (:op acc)) (nil? (:ref acc)) (not (str/starts-with? f "--")))
              (recur (rest xs) (assoc acc :ref f))
              (usage! (str "unknown argument " f)))))))))

(defn- bad-request-envelope [msg]
  {:spec-version spec/version
   :outcome      :error
   :observed-at  (t/format-kst (t/now-ms))
   :failures     [{:kind :bad-request :message (subs msg 0 (min 300 (count msg)))}]})

(defn exit-code [resp]
  (cond
    (= :success (:outcome resp)) 0
    (every? #(= :bad-request (:kind %)) (:failures resp)) 2
    :else 1))

(defn main*
  "argv → [exit-code stdout-json-or-nil stderr-or-nil]. 부수효과 없이 테스트할 수 있게."
  [args]
  (try
    (let [{:keys [config request]} (parse-args args)
          cfg  ((requiring-resolve 'wci.config/load-config) config)
          resp (core/run cfg request)]
      [(exit-code resp) (core/->json resp) nil])
    (catch clojure.lang.ExceptionInfo e
      (let [d (ex-data e)]
        (cond
          (::usage d)          [2 (core/->json (bad-request-envelope (ex-message e))) nil]
          (:wci/exit-code d)   [(:wci/exit-code d) nil (str "wci: " (ex-message e))]
          (:wci/internal d)    [4 nil (str "wci internal invariant " (:wci/internal d) ": " (ex-message e))]
          :else                [3 nil (str "wci: " (ex-message e))])))))

(defn -main [& args]
  (let [[code out err] (main* args)]
    (when out (println out))
    (when err (binding [*out* *err*] (println err)))
    (shutdown-agents)
    (System/exit code)))
