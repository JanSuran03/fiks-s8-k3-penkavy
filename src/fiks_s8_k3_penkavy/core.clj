(ns fiks-s8-k3-penkavy.core
  (:require [clojure.string :as str]
            [fiks-s8-k3-penkavy.str-diff :as str-diff]
            [clojure.java.io :as io])
  (:import (java.io ByteArrayOutputStream)
           (levenshtein Levenshtein)))

(defn ^String newline-join [strs]
  (str/join "\n" strs))

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

(defn dna->similar-to-bytecode-format [finches]
  (->> (for [{:keys [dna-diff-tolerance finches]} finches]
         (->> finches (map (fn [s]
                             (str (count s) " " s)))
              newline-join
              (apply str (count finches) " " dna-diff-tolerance "\n")))
       newline-join))

(defn fill-zeros [byte-seq]
  (if (= (count byte-seq) 8)
    byte-seq
    (loop [byte-seq byte-seq
           filled (rem (count byte-seq) 8)]
      (if (= 8 filled)
        byte-seq
        (recur (cons 0 byte-seq)
               (inc filled))))))

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
  (let [ret (loop [[firstb & more] byte-seq
                   int-digits (transient [])]
              (if (number-byte? firstb)
                (recur more
                       (conj! int-digits (get byte->number firstb)))
                [(->> int-digits persistent!
                      (apply str)
                      read-string)
                 more]))]
    ret))

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

(defn analyze-finches-seq [{:keys [dna-diff-tolerance finches]}]
  (let [analyzed-finches-seq (mapv (fn [{:keys [dna-length dna]}]
                                     (->> dna (mapcat #(let [ret (->> % dec->8-bit-binary-pairs
                                                                      (map nucleus-basis))]
                                                         ret))
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
           ret {}
           interesting-finches (transient #{})]
      (cond (= i (dec len))
            [ret
             (persistent! interesting-finches)]

            (>= j len)
            (recur (inc i)
                   (+ i 2)
                   ret
                   interesting-finches)

            :else
            (let [same-species? (and (<= (Math/abs (- (count (nth finches i))
                                                      (count (nth finches j))))
                                         dna-diff-tolerance)
                                     (Levenshtein/compute_levenshtein (nth finches i)
                                                                      (nth finches j)
                                                                      dna-diff-tolerance))]
              (recur i
                     (inc j)
                     (if same-species?
                       (assoc ret i ((fnil conj #{}) (get ret i) j)
                                  j ((fnil conj #{}) (get ret j) i))
                       ret)
                     (if same-species?
                       (-> interesting-finches (conj! i) (conj! j))
                       interesting-finches)))))))

(defn find-interesting-trinities [{:keys [dna-diff-tolerance finches]}]
  (let [[same-species-finches interesting-set] (find-same-species-finches dna-diff-tolerance finches)
        sorted-interesting-seq (vec (sort interesting-set))
        len (count sorted-interesting-seq)]
    (println "done")
    (loop [i 0
           j 1
           ret (transient [])]
      (cond (>= i (dec len))
            (persistent! ret)

            (>= j len)
            (recur (inc i)
                   (+ i 2)
                   ret)

            :else
            (let [[nth-i nth-j] (map #(nth sorted-interesting-seq %) [i j])
                  ss-nth-i (get same-species-finches nth-i)]
              (if (contains? ss-nth-i nth-j)
                (recur i (inc j) ret)
                (let [interesting-trinities (for [finch-idx sorted-interesting-seq
                                                  :when (and (not (contains? #{nth-i nth-j} finch-idx))
                                                             (contains? ss-nth-i finch-idx)
                                                             (contains? (get same-species-finches nth-j) finch-idx))]
                                              [nth-i finch-idx nth-j])]
                  (recur i
                         (inc j)
                         (reduce conj! ret interesting-trinities)))))))))

(defn read-and-process-input [filename]
  (let [input-as-bytes (slurp-bytes filename)
        processed-input (lazy-seq (process-input input-as-bytes))]
    (map analyze-finches-seq processed-input)))

(defn -main [filename]
  (->> filename read-and-process-input
       (take 8)
       (map #(find-interesting-trinities %))
       (map (fn [interesting-trinities]
              (->> interesting-trinities (map #(str/join " " %))
                   (cons (count interesting-trinities))
                   newline-join)))
       newline-join
       (spit "output.txt")))

(defn main* [filename]
  (with-open [writer (io/writer "output.txt")]
    (->> filename read-and-process-input
         (take 4)
         (map find-interesting-trinities)
         (map (fn [interesting-trinities]
                (->> interesting-trinities (map #(str/join " " %))
                     (cons (count interesting-trinities))
                     newline-join)))
         newline-join
         (.write writer))))
