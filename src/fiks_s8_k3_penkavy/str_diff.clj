(ns fiks-s8-k3-penkavy.str-diff
  (:import (penkavy LevenshteinDistanceRecursive)))

(defn init-2d-vector [rows cols]
  (vec (repeat rows (vec (repeat cols 0)))))

(def testing-file "test.txt")

(defmacro with-testing [& body]
  `(let [file-content# (slurp testing-file)]
     (as-> (with-out-str ~@body) body#
           (str file-content# body# "\n")
           (spit testing-file body#))))

(defmacro get-in-v [v i j]
  `(nth (nth ~v ~i) ~j))

(defmacro assoc-in-v [v i j val]
  `(assoc ~v ~i (assoc (nth ~v ~i) ~j ~val)))

(defn levenshtein [^String a ^String b max-diff]
  (let [inc-cnt-a (inc (count a))
        inc-cnt-b (inc (count b))
        vec-2d (init-2d-vector inc-cnt-a inc-cnt-b)
        with-first-row (loop [v vec-2d
                              i (int 0)]
                         (if (< i inc-cnt-a)
                           (recur (assoc-in-v v i 0 i)
                                  (inc i))
                           v))
        with-first-col (loop [v with-first-row
                              i (int 0)]
                         (if (< i inc-cnt-b)
                           (recur (assoc-in-v v 0 i i)
                                  (inc i))
                           v))
        diff (loop [v (transient with-first-col)
                    i 1
                    j 1]
               (cond (= i inc-cnt-a)
                     (get-in-v v (count a) (count b))

                     (= j inc-cnt-b)
                     (recur v
                            (inc i)
                            1)

                     :else
                     (let [dec-i (dec i)
                           dec-j (dec j)
                           a-i-dec (.charAt a dec-i)
                           b-j-dec (.charAt b dec-j)]
                       (if (identical? a-i-dec b-j-dec)
                         (recur (assoc! v i (assoc (get v i) j (get-in-v v dec-i dec-j)))
                                i
                                (inc j))
                         (let [insertion (inc (get-in-v v i dec-j))
                               deletion (inc (get-in-v v dec-i j))
                               replacement (inc (get-in-v v dec-i dec-j))]

                           (recur (assoc! v i (assoc (get v i) j (min insertion deletion replacement)))
                                  i
                                  (inc j)))))))]
    (<= diff max-diff)))

(defn random-str-testing [str-len times]
  (let [rand-strs (repeatedly times (fn []
                                      (let [s1 (apply str (repeatedly str-len #(rand-int 10)))
                                            s2 (apply str (repeatedly str-len #(rand-int 10)))]
                                        [s1 s2])))]
    (println (str "String length: " str-len ", num string pairs: " times))
    (doseq [[s1 s2] rand-strs]
      (time (levenshtein s1
                         s2
                         str-len)))))