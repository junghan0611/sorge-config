(ns wci.token
  "불투명 참조 토큰. 서버 상태 없이 다시 풀린다.

  body(EDN) → zlib 압축 → AES-256-GCM(무작위 12바이트 IV, AAD 고정) → base64url.
  GCM 이 인증을 겸하므로 위조·변조·다른 키의 토큰은 전부 :unknown-ref 다.
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

(def ^:private ^{:tag "[B"} aad (.getBytes "wci-ref/1" StandardCharsets/UTF_8))
(def ^:private version-byte (unchecked-byte 1))
(def ^:private iv-len 12)
(def ^:private tag-bits 128)
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

(defn- cipher ^Cipher [mode ^SecretKeySpec k ^bytes iv]
  (doto (Cipher/getInstance "AES/GCM/NoPadding")
    (.init (int mode) k (GCMParameterSpec. tag-bits iv))
    (.updateAAD aad)))

(defn seal
  "body(EDN 값) 를 exp-ms 까지 유효한 토큰으로 감싼다."
  ^String [^SecretKeySpec k body exp-ms]
  (let [plain (deflate (.getBytes (pr-str {:e (long exp-ms) :b body})
                                  StandardCharsets/UTF_8))
        iv    (let [b (byte-array iv-len)] (.nextBytes ^SecureRandom rng b) b)
        ct    (.doFinal (cipher Cipher/ENCRYPT_MODE k iv) plain)
        out   (byte-array (+ 1 iv-len (alength ct)))]
    (aset-byte out 0 version-byte)
    (System/arraycopy iv 0 out 1 iv-len)
    (System/arraycopy ct 0 out (inc iv-len) (alength ct))
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) out)))

(defn- unknown! [why]
  (throw (ex-info (str "unknown reference: " why) {:wci/error-kind :unknown-ref})))

(defn open
  "토큰 → body. 위조·변조·형식 오류는 :unknown-ref, 만료는 :ref-expired."
  [^SecretKeySpec k ^String token now-ms]
  (let [raw (try (.decode (Base64/getUrlDecoder) token)
                 (catch IllegalArgumentException _ (unknown! "not base64url")))]
    (when (or (< (alength ^bytes raw) (+ 1 iv-len 16))
              (not= version-byte (aget ^bytes raw 0)))
      (unknown! "bad framing"))
    (let [iv    (Arrays/copyOfRange ^bytes raw 1 (int (inc iv-len)))
          ct    (Arrays/copyOfRange ^bytes raw (int (inc iv-len)) (alength ^bytes raw))
          plain (try (.doFinal (cipher Cipher/DECRYPT_MODE k iv) ct)
                     (catch Exception _ (unknown! "authentication failed")))
          {:keys [e b] :as m}
          (try (edn/read-string (String. (inflate plain) StandardCharsets/UTF_8))
               (catch Exception _ (unknown! "undecodable payload")))]
      (when-not (and (map? m) (int? e)) (unknown! "bad payload"))
      (when (<= (long e) (long now-ms))
        (throw (ex-info "reference has expired; start again from overview or ask"
                        {:wci/error-kind :ref-expired})))
      b)))
