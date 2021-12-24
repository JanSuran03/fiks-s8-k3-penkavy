(ns fiks-s8-k3-penkavy.core
  (:require [clj-diff.core :as clj-diff]
            [clojure.string :as str]
            [fiks-s8-k3-penkavy.str-comp :as str-comp])
  (:import (java.io ByteArrayOutputStream)))

(defn dec->bin [n]
  (loop [ret ()
         n n]
    (if (pos? n)
      (recur (conj ret (rem n 2))
             (quot n 2))
      ret)))

#_(dotimes [i 5] (let [s (apply str (repeat 60 "abcdabcabacabbbabacba"))
                       s-rev (apply str (repeat 60 "bcbacbbababacbbabcadc"))]
                   (println (time (clj-diff/edit-distance s s-rev)))
                   (println (time (dec (count (diff s s-rev)))))
                   (println (time (metrics/levenshtein s s-rev)))))

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

(defn ret-indices-2-away [seq]
  (let [len (count seq)]
    (loop [i 0
           j 0
           ret (transient [])]
      (cond (>= i (dec len))
            (persistent! ret)

            (>= j len)
            (recur (inc i)
                   (inc (inc i))
                   ret)

            :else
            (recur i
                   (inc j)
                   (if (>= (- j i) 2)
                     (conj! ret [i j])
                     ret))))))

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
  (let [analyzed-finches-seq (mapv (fn [{:keys [dna-length dna]}]
                                     (->> dna (mapcat #(->> % dec->8-bit-binary-pairs
                                                            (map nucleus-basis)))
                                          (take dna-length)
                                          (apply str)))
                                   finches)]
    {:dna-diff-tolerance dna-diff-tolerance
     :finches            analyzed-finches-seq}))

(defn find-same-species-finches [dna-diff-tolerance finches]
  (let [finches (vec finches)
        len (count finches)]
    (loop [i 0
           j 1
           ret (transient {})
           interesting-finches (transient #{})]
      (cond (= i (dec len))
            [(persistent! ret)
             (persistent! interesting-finches)]

            (>= j len)
            (recur (inc i)
                   (+ i 2)
                   ret
                   interesting-finches)

            :else
            (let [diff (if (> (Math/abs (- (count (nth finches i))
                                           (count (nth finches j))))
                              dna-diff-tolerance)
                         999999999
                         (clj-diff/edit-distance (nth finches i) (nth finches j)))
                  same-species (<= diff dna-diff-tolerance)]
              (recur i
                     (inc j)
                     (if same-species
                       (assoc! ret i ((fnil conj #{}) (get ret i) j))
                       ret)
                     (if same-species
                       (-> interesting-finches (conj! i) (conj! j))
                       interesting-finches)))))))

(defn find-interesting-trinities [{:keys [dna-diff-tolerance finches]}]
  (let [[same-species-finches interesting-set] (find-same-species-finches dna-diff-tolerance finches)
        sorted-interesting-seq (vec (sort interesting-set))
        len (count sorted-interesting-seq)]
    (loop [i 0
           j 1
           k 2
           ret (transient [])]
      (cond (>= i (- len 2))
            (persistent! ret)

            (>= j (dec len))
            (recur (inc i)
                   (+ i 2)
                   (+ i 3)
                   ret)

            (>= k len)
            (recur i
                   (inc j)
                   (+ j 2)
                   ret)

            :else
            (let [[nth-i nth-j nth-k] (map #(get sorted-interesting-seq %) [i j k])
                  i-j-ss? (contains? (get same-species-finches nth-i) nth-j)
                  j-k-ss? (contains? (get same-species-finches nth-j) nth-k)
                  i-k-ss? (contains? (get same-species-finches nth-i) nth-k)]
              (recur i
                     j
                     (inc k)
                     (if (and i-j-ss? j-k-ss? (not i-k-ss?))
                       (conj! ret [i j k])
                       ret)))))))

(defn read-and-process-input [filename]
  (let [input-as-bytes (slurp-bytes filename)
        processed-input (process-input input-as-bytes)]
    (map analyze-finches-seq processed-input)))

(defn -main [filename]
  (->> filename read-and-process-input
       (map find-interesting-trinities)
       (map (fn [interesting-trinities]
              (->> interesting-trinities (map #(str/join " " %))
                   (cons (count interesting-trinities))
                   (str/join "\n"))))
       (str/join "\n")
       (spit "output.txt")))
