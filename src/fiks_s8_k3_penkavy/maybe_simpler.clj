(ns fiks-s8-k3-penkavy.maybe-simpler
  (:import (java.io ByteArrayOutputStream)))

(def ascii-num-diff (- (int \0) 0))

(defn integer-as-byte->integer [n]
  (- n ascii-num-diff))

;; https://clojuredocs.org/clojure.core/slurp
(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [x]
  (with-open [out (ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn dec->bin [n]
  (loop [ret ()
         n n]
    (if (pos? n)
      (recur (conj ret (rem n 2))
             (quot n 2))
      ret)))

(defn fill-zeros [byte-seq]
  (loop [byte-seq byte-seq
         filled (rem (count byte-seq) 4)]
    (if (= 4 filled)
      byte-seq
      (recur (cons 0 byte-seq)
             (inc filled)))))

(defn decode-bytecode [byte-seq]
  (loop [[firstb & more-bytes] byte-seq
         ret (transient [])
         prefix nil]
    (cond (not firstb)
          (persistent! ret)

          (nil? prefix)
          (if (#{-61 -62} firstb)
            (recur more-bytes
                   ret
                   firstb)
            (recur more-bytes
                   (conj! ret firstb)
                   nil))

          (= prefix -61)
          (recur more-bytes
                 (conj! ret (+ 320 firstb))
                 nil)

          :else #_(= prefix -62)
          (recur more-bytes
                 (conj! ret (+ 256 firstb))
                 nil))))

(defn read-and-process-input []
  (let [input-as-bytes (slurp-bytes "example.txt")]
    (map (comp (partial apply str) fill-zeros dec->bin) input-as-bytes)))