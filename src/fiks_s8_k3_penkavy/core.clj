(ns fiks-s8-k3-penkavy.core
  (:import (java.io ByteArrayOutputStream)))

(defn dec->bin [n]
  (loop [ret ()
         n n]
    (if (pos? n)
      (recur (conj ret (rem n 2))
             (quot n 2))
      ret)))

(def nucleus-basis {'(0 0) "A"
                    '(0 1) "C"
                    '(1 0) "G"
                    '(1 1) "T"})

(defn fill-zeros [byte-seq]
  (loop [byte-seq byte-seq
         filled (rem (count byte-seq) 4)]
    (if (= 4 filled)
      byte-seq
      (recur (cons 0 byte-seq)
             (inc filled)))))

(defn dec->8-bit-binary-pairs [n]
  (->> n dec->bin
       fill-zeros
       (partition 2)))

(def zero 48)
(def nine 57)
(def byte->number (->> (range 10) (map (fn [b] [(+ b 48) b])) (into {})))
(defn number-byte? [b] (<= zero b nine))

;; https://clojuredocs.org/clojure.core/slurp
(defn slurp-bytes
  "Reads a file as unsigned bytes."
  [x]
  (with-open [out (ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (->> (.toByteArray out)
         (map #(Byte/toUnsignedInt ^Byte %)))))

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
                                                  (conj! ret {:dna-length dna-length
                                                              :dna        dna})))
                                         [{:dna-diff-tolerance dna-diff-tolerance
                                           :finches            (persistent! ret)} remaining-input]))))]
    (loop [remaining-inputs num-inputs
           ret (transient [])
           remaining-input pure-input]
      (if (zero? remaining-inputs)
        (persistent! ret)
        (let [[input-ret remaining-input]
              (process-finches-sequence remaining-input)]
          (recur (dec remaining-inputs)
                 (conj! ret input-ret)
                 remaining-input))))))

(defn analyze-finches-seq [{:keys [dna-diff-tolerance
                                   finches] :as _finches-seq}]
  (let [analyzed-finches-seq (for [{:keys [dna-length dna]} finches]
                               (let [nucleus-basis (->> dna (mapcat #(->> % dec->8-bit-binary-pairs
                                                                          (map nucleus-basis)))
                                                        (take dna-length)
                                                        (apply str))]
                                 nucleus-basis))]
    {:dna-diff-tolerance dna-diff-tolerance
     :finches            analyzed-finches-seq}))

(defn read-and-process-input [filename]
  (let [input-as-bytes (slurp-bytes filename)
        processed-input (process-input input-as-bytes)]
    (map analyze-finches-seq processed-input)))

(defn -main [filename]
  (read-and-process-input filename))
