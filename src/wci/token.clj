(ns wci.token
  "불투명 참조 토큰. 서버 상태 없이 다시 풀린다.

  형식 v2: [버전 2][압축 플래그][IV 12][AES-256-GCM 암호문 + 96비트 태그] → base64url.
  평문은 [만료(epoch 초) body] EDN 이고, 압축이 더 작을 때만 zlib 으로 줄인다.
  머리 두 바이트는 AAD 로 인증된다. 위조·변조·다른 키·이전 형식(v1)은 전부 :unknown-ref 다.
  토큰은 아무 권한도 주지 않는다(따라갈 때 현재 설정으로 다시 판정) — 96비트 태그로 충분하다.
  만료는 암호문 안에 있고 :ref-expired 로 구분된다. 사라짐(:ref-gone)은 원천이
  판정한다 — 토큰이 아니라 어댑터 몫.

  키는 환경변수에서만 온다. 없거나 짧으면 기동 에러(:wci/exit-code 3).
  프로세스마다 키를 지어내지 않는다: code mode 는 호출마다 새 프로세스라
  그러면 다음 호출의 follow 가 깨진다."
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.io ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest SecureRandom]
           [java.util Arrays Base64]
           [java.util.zip Deflater Inflater]
           [javax.crypto Cipher]
           [javax.crypto.spec GCMParameterSpec SecretKeySpec]))

(def default-key-env "WCI_TOKEN_KEY")
(def default-ttl-hours 72)

(def ^:private aad-prefix "wci-ref/2:")
(def ^:private version-byte (unchecked-byte 2))
(def ^:private iv-len 12)
(def ^:private tag-bits 96)
(def ^:private tag-len (quot tag-bits 8))
(def ^:private max-plain 65536)
(def ^:private rng (SecureRandom.))

(defn derive-key
  "비밀 문자열 → AES-256 키 (SHA-256). 16자 미만은 거절한다."
  ^SecretKeySpec [^String secret]
  (when (or (nil? secret) (< (count secret) 16))
    (throw (ex-info "token key must be at least 16 characters"
                    {:wci/exit-code 3 :wci/error-kind :bad-request})))
  (SecretKeySpec. (.digest (MessageDigest/getInstance "SHA-256")
                           (.getBytes secret StandardCharsets/UTF_8))
                  "AES"))

(defn key-from-env
  "환경변수에서 키를 만든다. 없으면 기동 에러."
  [env-name]
  (let [v (System/getenv ^String env-name)]
    (when (str/blank? v)
      (throw (ex-info (str "token key env " env-name " is not set")
                      {:wci/exit-code 3 :wci/error-kind :bad-request})))
    (derive-key v)))

(defn- deflate ^"[B" [^bytes in]
  (let [d   (Deflater. Deflater/BEST_COMPRESSION)
        out (ByteArrayOutputStream.)
        buf (byte-array 512)]
    (.setInput d in)
    (.finish d)
    (while (not (.finished d))
      (.write out buf 0 (.deflate d buf)))
    (.end d)
    (.toByteArray out)))

(defn- inflate ^"[B" [^bytes in]
  (let [i   (Inflater.)
        out (ByteArrayOutputStream.)
        buf (byte-array 512)]
    (.setInput i in)
    (loop []
      (let [n (.inflate i buf)]
        (.write out buf 0 n)
        (when (> (.size out) max-plain)
          (throw (ex-info "token payload too large" {})))
        (cond
          (.finished i) nil
          (and (zero? n) (or (.needsInput i) (.needsDictionary i)))
          (throw (ex-info "truncated token payload" {}))
          :else (recur))))
    (.end i)
    (.toByteArray out)))

(defn- cipher ^Cipher [mode ^SecretKeySpec k ^bytes iv flag]
  (doto (Cipher/getInstance "AES/GCM/NoPadding")
    (.init (int mode) k (GCMParameterSpec. (int tag-bits) iv))
    (.updateAAD (.getBytes (str aad-prefix flag) StandardCharsets/UTF_8))))

(defn seal
  "body(EDN 값) 를 exp-ms 까지 유효한 토큰으로 감싼다. 만료는 초 단위로 담긴다."
  ^String [^SecretKeySpec k body exp-ms]
  (let [raw   (.getBytes (pr-str [(quot (long exp-ms) 1000) body]) StandardCharsets/UTF_8)
        z     (deflate raw)
        [flag plain] (if (< (alength ^bytes z) (alength ^bytes raw)) [1 z] [0 raw])
        iv    (let [b (byte-array iv-len)] (.nextBytes ^SecureRandom rng b) b)
        ct    ^bytes (.doFinal (cipher Cipher/ENCRYPT_MODE k iv flag) ^bytes plain)
        out   (byte-array (+ 2 iv-len (alength ct)))]
    (aset-byte out 0 version-byte)
    (aset-byte out 1 (unchecked-byte flag))
    (System/arraycopy iv 0 out 2 iv-len)
    (System/arraycopy ct 0 out (+ 2 iv-len) (alength ct))
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) out)))

(defn- unknown! [why]
  (throw (ex-info (str "unknown reference: " why) {:wci/error-kind :unknown-ref})))

(defn open
  "토큰 → body. 위조·변조·형식 오류·이전 형식은 :unknown-ref, 만료는 :ref-expired."
  [^SecretKeySpec k ^String token now-ms]
  (let [raw ^bytes (try (.decode (Base64/getUrlDecoder) token)
                        (catch IllegalArgumentException _ (unknown! "not base64url")))]
    (when (< (alength raw) (+ 2 iv-len tag-len))
      (unknown! "bad framing"))
    (when-not (= version-byte (aget raw 0))
      (unknown! "this reference format is no longer issued; start again from overview or ask"))
    (let [flag  (aget raw 1)
          _     (when-not (#{0 1} (long flag)) (unknown! "bad framing"))
          iv    (Arrays/copyOfRange raw 2 (int (+ 2 iv-len)))
          ct    (Arrays/copyOfRange raw (int (+ 2 iv-len)) (alength raw))
          plain (try (.doFinal (cipher Cipher/DECRYPT_MODE k iv (long flag)) ct)
                     (catch Exception _ (unknown! "authentication failed")))
          [e b :as v]
          (try (edn/read-string (String. ^bytes (if (= 1 (long flag)) (inflate plain) plain)
                                         StandardCharsets/UTF_8))
               (catch Exception _ (unknown! "undecodable payload")))]
      (when-not (and (vector? v) (= 2 (count v)) (int? e)) (unknown! "bad payload"))
      (when (<= (* 1000 (long e)) (long now-ms))
        (throw (ex-info "reference has expired; start again from overview or ask"
                        {:wci/error-kind :ref-expired})))
      b)))
