(ns wci.adapter
  "채널 계약 — spec/wci.edn 의 :channel/* 를 구현하는 multimethod 셋.

  어댑터는 (:kind cfg) 로 dispatch 한다. cfg 는 channels.local.edn 의 채널 한 줄
  ({:id :kind :title :opts}). kind 구현은 `wci.adapter.<kind>` namespace 에 두고,
  `load-kind!` 가 관례대로 require 한다. 코어는 이 namespace 만 안다.

  어댑터의 몫: 원천을 읽어 :channel/answer 를 돌려준다.
  코어의 몫: 토큰·정렬·예산·상태 유도·술어 판정·봉투.

  실패는 (fail! kind msg) 로 던진다. 실패를 빈 답으로 돌려주면 계약 위반이다."
  (:require [clojure.java.io :as io]))

(defmulti describe
  "cfg → :channel/describe"
  (fn [cfg] (:kind cfg)))

(defmulti answer
  "cfg, :channel/request → :channel/answer (mode :overview / :question / :where)"
  (fn [cfg _req] (:kind cfg)))

(defmulti expand
  "cfg, 어댑터가 준 ref :key, :channel/request(mode :follow) → :channel/answer"
  (fn [cfg _key _req] (:kind cfg)))

(defmulti ref-label
  "Optional label for an already selected cross-channel reference; nil keeps its original label."
  (fn [cfg _seed _cut] (:kind cfg)))

(defmethod ref-label :default [_ _ _] nil)

(defn fail!
  "채널 실패를 계약대로 던진다. kind 는 :wci/error-kind 중 하나."
  ([kind msg] (fail! kind msg nil))
  ([kind msg data]
   (throw (ex-info msg (merge data {:wci/error-kind kind})))))

(defn load-kind!
  "kind 키워드에 해당하는 구현 namespace 를 require 한다. 없으면 :bad-request 로 터진다."
  [kind]
  (let [ns-sym (symbol (str "wci.adapter." (name kind)))
        path   (str "wci/adapter/" (.replace (name kind) "-" "_") ".clj")]
    (when-not (io/resource path)
      (fail! :bad-request (str "unknown channel kind " kind)))
    (require ns-sym)))
