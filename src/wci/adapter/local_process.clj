(ns wci.adapter.local-process
  "Bounded local process execution. Never passes text to a shell."
  (:require [wci.adapter :as adapter])
  (:import [java.io ByteArrayOutputStream]
           [java.lang ProcessBuilder$Redirect]
           [java.nio ByteBuffer]
           [java.nio.charset CharacterCodingException StandardCharsets]
           [java.util.concurrent TimeUnit]))

(defn- read-bounded [process limit]
  (with-open [in (.getInputStream process)]
    (let [out (ByteArrayOutputStream.)
          buf (byte-array 8192)]
      (loop []
        (let [n (.read in buf)]
          (if (neg? n)
            (.toByteArray out)
            (if (> (+ (.size out) n) limit)
              (do (.destroyForcibly process) ::too-large)
              (do (.write out buf 0 n) (recur)))))))))

(defn decode-utf8 [bytes]
  (try
    (str (.decode (.newDecoder StandardCharsets/UTF_8) (ByteBuffer/wrap bytes)))
    (catch CharacterCodingException _
      (adapter/fail! :contract "local source output is not UTF-8"))))

(defn execute!
  "Execute a fixed argv vector. Return success or a nonzero exit without logging raw output."
  [argv strip-env? env]
  (let [pb (doto (ProcessBuilder. ^java.util.List argv)
             (.redirectError ProcessBuilder$Redirect/DISCARD))
        pe (.environment pb)
        _ (doseq [k (filter strip-env? (vec (.keySet pe)))] (.remove pe k))
        _ (.putAll pe env)
        process (try (.start pb)
                     (catch java.io.IOException _ (adapter/fail! :unreadable "cannot start local reader")))
        result (promise)
        reader (doto (Thread.
                      #(deliver result
                                (try (read-bounded process (* 8 1024 1024))
                                     (catch java.io.IOException _ ::read-error))))
                 (.setDaemon true)
                 (.start))]
    (try
      (.close (.getOutputStream process))
      (when-not (.waitFor process 5 TimeUnit/SECONDS)
        (adapter/fail! :unreadable "local reader timed out"))
      (let [bytes (deref result 1000 ::read-error)]
        (when (= ::too-large bytes)
          (adapter/fail! :unreadable "local reader output exceeds its limit"))
        (if (zero? (.exitValue process))
          (if (= ::read-error bytes)
            (adapter/fail! :unreadable "cannot read local reader output")
            {:exit 0 :stdout (decode-utf8 bytes)})
          {:exit (.exitValue process)}))
      (finally
        (when (.isAlive process) (.destroyForcibly process))
        (.interrupt reader)))))
