(ns fiks-s8-k3-penkavy.core
  (:import (java.io ByteArrayOutputStream)))

(def byte->number (->> (range 10) (map (fn [b]
                                         [(+ b 48) b]))
                       (into {})))

(def zero 48)
(def nine 57)
(defn number-byte? [b]
  (<= zero b nine))

;; https://clojuredocs.org/clojure.core/slurp
(defn slurp-bytes
  "Reads a file as unsigned bytes."
  [x]
  (with-open [out (ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (->> (.toByteArray out)
         (map #(Byte/toUnsignedInt ^Byte %)))))

(defn is-newline? [ch]
  (= \newline ch))

(defn number-builder [byte-seq]
  (loop [[firstb & more] byte-seq
         int-digits (transient [])]
    (if (number-byte? firstb)
      (recur more
             (conj! int-digits (get byte->number firstb)))
      [(->> int-digits persistent!
            (apply str)
            read-string)
       more])))

(defn finch-dna-builder [dna-length input]
  (let [need-bytes (Math/ceil (/ dna-length 4))]
    (loop [remaining-input input
           DNA (transient [])
           need-bytes need-bytes]
      (if (zero? need-bytes)
        [(persistent! DNA) (next remaining-input)]
        (recur (next remaining-input)
               (conj! DNA (first remaining-input))
               (dec need-bytes))))))

(defn process-input [input-as-bytes]
  (let [[num-inputs pure-input] (number-builder input-as-bytes)
        process-finches-sequence (fn [byte-seq]
                                   (let [[incoming-finches rest-input] (number-builder byte-seq)
                                         [dna-diff-tolerance rest-input] (number-builder rest-input)]
                                     (loop [num-remaining-finches incoming-finches
                                            remaining-input rest-input
                                            ret (transient [])]
                                       (if (pos? num-remaining-finches)
                                         (let [[dna-length input-rest] (number-builder remaining-input)
                                               [dna input-rest] (finch-dna-builder dna-length input-rest)]
                                           (recur (dec num-remaining-finches)
                                                  input-rest
                                                  (conj! ret {:dna-length         dna-length
                                                              :dna                dna
                                                              :dna-diff-tolerance dna-diff-tolerance})))
                                         [(persistent! ret) remaining-input]))))]
    (loop [remaining-inputs num-inputs
           ret (transient [])
           remaining-input pure-input]
      (if (zero? remaining-inputs)
        (persistent! ret)
        (let [[input-ret remaining-input] (process-finches-sequence remaining-input)]
          (recur (dec remaining-inputs)
                 (conj! ret input-ret)
                 remaining-input))))))
(defn read-and-process-input [filename]
  (let [input-as-bytes (slurp-bytes filename)]
    (process-input input-as-bytes)))

(defn -main [filename]
  (read-and-process-input filename))
