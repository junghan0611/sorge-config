(ns wci.spec
  "SSOT spec/wci.edn 을 읽어 malli registry 로 올린다.

  스키마의 모양은 EDN 이 정답이다. 이 ns 는 로드·컴파일·검증만 한다.
  코드는 여기 키워드만 참조한다. 검증 실패는 경고가 아니라 예외다."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [malli.core :as m]
            [malli.error :as me]))

(defn load-spec
  "spec EDN 을 읽는다. 기본은 classpath 의 wci.edn (deps :paths 에 spec/ 이 있다)."
  ([]
   (load-spec (or (io/resource "wci.edn")
                  (throw (ex-info "spec/wci.edn is not on the classpath"
                                  {:wci/exit-code 3})))))
  ([src]
   (with-open [r (java.io.PushbackReader. (io/reader src))]
     (edn/read r))))

(def spec-map (load-spec))

(def version
  "응답마다 정확히 싣는 :spec-version."
  (:spec-version spec-map))

(def constants (:constants spec-map))

(def registry
  (merge (m/default-schemas) (:registry spec-map)))

(def ^:private schema*
  (memoize (fn [k] (m/schema k {:registry registry}))))

(def ^:private validator*
  (memoize (fn [k] (m/validator (schema* k)))))

(defn schema [k] (schema* k))

(defn valid? [k x] ((validator* k) x))

(defn explain
  "사람이 읽는 위반 내역. 통과하면 nil."
  [k x]
  (some-> (m/explain (schema* k) x) me/humanize))

(defn validate!
  "통과하면 x, 아니면 ex-info {:wci/schema k :wci/explain …}."
  [k x]
  (if (valid? k x)
    x
    (let [why (explain k x)]
      (throw (ex-info (str "violates " k ": " (pr-str why))
                      {:wci/schema k :wci/explain why})))))

;; ─────────────── 모양 자체를 읽는 도구 (정합 테스트용) ───────────────

(defn form
  "registry 에 적힌 원래 EDN 형태."
  [k]
  (get-in spec-map [:registry k]))

(defn multi-keys
  "[:multi {...} [k schema] ...] 의 dispatch 값 집합."
  [k]
  (let [f (form k)]
    (when-not (= :multi (first f))
      (throw (ex-info (str k " is not a :multi schema") {:form f})))
    (set (map first (filter vector? (drop 2 f))))))

(defn enum-values
  "[:enum a b ...] 의 값 집합."
  [k]
  (let [f (form k)]
    (when-not (= :enum (first f))
      (throw (ex-info (str k " is not an :enum schema") {:form f})))
    (set (remove map? (rest f)))))
