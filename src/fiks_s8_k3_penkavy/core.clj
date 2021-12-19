(ns fiks-s8-k3-penkavy.core
  (:require [clojure.string :as str]))

(def ascii-num-diff (- (int \0) 0))

(def ascii->number (->> (range 10) (map #(vector (char (+ % ascii-num-diff))
                                                 %))
                        (into {})))
(def number-chars (set (map first ascii->number)))

(defn is-return?
  "In \"ISO-8859-1\" encoding, there ALWAYS is \return and \newline right after."
  [ch]
  (= \return ch))

(defn process-pure-input [pure-input])

(defn number-builder [char-sequence]
  (loop [[^Character firstch & input] char-sequence
         number ""]
    (cond (contains? number-chars firstch)
          (recur input
                 (str number firstch))

          (and (str/blank? number)
               (Character/isWhitespace firstch))
          (recur input
                 number)

          :else
          [(read-string number)
           input])))

(defn byte-builder
  "Returns: [byte-length byte input-rest]"
  [input]
  (let [[byte-length input] (number-builder input)
        need-chars (Math/ceil (/ byte-length 4))]
    (loop [need-chars need-chars
           [firstch & remaining-input] input
           byte-ret []]
      (if (zero? need-chars)
        [byte-length byte-ret (rest remaining-input)]
        (recur (dec need-chars)
               remaining-input
               (conj byte-ret firstch))))))

(defn read-and-process-input []
  (let [pure-input (slurp "sample.txt" :encoding "ISO-8859-1")
        [num-inputs pure-input] (number-builder pure-input)
        process-input (fn [input]
                        (let [[incoming-lines rest-input] (number-builder input)
                              [dna-diff-tolerance rest-input] (number-builder rest-input)]
                          (loop [num-remaining-inputs incoming-lines
                                 ret []
                                 remaining-input rest-input]
                            (if (pos? num-remaining-inputs)
                              (let [[byte-length byte input-rest] (byte-builder remaining-input)]
                                (recur (dec num-remaining-inputs)
                                       (conj ret {:byte-length byte-length
                                                  :byte        byte})
                                       input-rest))
                              [{:dna-diff-tolerance dna-diff-tolerance
                                :input              ret}
                               remaining-input]))))]
    (loop [remaining-inputs num-inputs
           ret []
           remaining-input pure-input]
      (if (zero? remaining-inputs)
        ret
        (let [[input-ret remaining-input] (process-input remaining-input)]
          (recur (dec remaining-inputs)
                 (conj ret input-ret)
                 remaining-input))))))

(defn -main [& _args]
  (read-and-process-input))
