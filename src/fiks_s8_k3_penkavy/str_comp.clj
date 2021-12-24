(ns fiks-s8-k3-penkavy.str-comp
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn init-2d-vector [rows cols]
  (vec (repeat rows (vec (repeat cols 0)))))

(defn init-2d-array [rows cols]
  (to-array (repeat rows (int-array (repeat cols 0)))))

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
  (cond (> (Math/abs (- (count a)
                        (count b)))
           max-diff)
        false

        :else
        (let [inc-cnt-a (inc (count a))
              inc-cnt-b (inc (count b))
              vec-2d (init-2d-vector inc-cnt-a inc-cnt-b)
              with-first-row (loop [v vec-2d
                                    i 0]
                               (if (< i inc-cnt-a)
                                 (recur (assoc-in-v v i 0 i)
                                        (inc i))
                                 v))
              with-first-col (loop [v with-first-row
                                    i 0]
                               (if (< i inc-cnt-b)
                                 (recur (assoc-in-v v 0 i i)
                                        (inc i))
                                 v))
              diff (loop [v (transient with-first-col)
                          i (int 1)
                          j (int 1)]
                     (cond (= i inc-cnt-a)
                           (get-in-v v (count a) (count b))

                           (= j inc-cnt-b)
                           (recur v
                                  (unchecked-inc-int i)
                                  1)

                           :else
                           (let [dec-i (unchecked-dec-int i)
                                 dec-j (unchecked-dec-int j)
                                 a-i-dec (.charAt a dec-i)
                                 b-j-dec (.charAt b dec-j)]
                             (if (= a-i-dec b-j-dec)
                               (recur (assoc! v i (assoc (get v i) j (get-in-v v dec-i dec-j)))
                                      i
                                      (unchecked-inc-int j))
                               (let [insertion (unchecked-inc-int (get-in-v v i dec-j))
                                     deletion (unchecked-inc-int (get-in-v v dec-i j))
                                     replacement (unchecked-inc-int (get-in-v v dec-i dec-j))]
                                 (recur (assoc! v i (assoc (get v i) j (min insertion deletion replacement)))
                                        i (unchecked-inc-int j)))))))]
          (<= diff max-diff))))

(defn random-str-testing [str-len times]
  (with-testing
    (let [rand-strs (repeatedly times (fn []
                                        (let [s1 (apply str (repeatedly str-len #(rand-int 10)))
                                              s2 (apply str (repeatedly str-len #(rand-int 10)))]
                                          [s1 s2])))]
      (println (str "String length: " str-len ", num string pairs: " times))
      (doseq [[s1 s2] rand-strs]
        (time (levenshtein s1
                           s2
                           str-len))))))

#_(defn levenshtein [^String a ^String b max-diff]
    (cond (> (Math/abs (- (count a)
                          (count b)))
             max-diff)
          false

          :else
          (let [inc-cnt-a (inc (count a))
                inc-cnt-b (inc (count b))
                arr-2d (time (init-2d-array inc-cnt-a inc-cnt-b))
                init-first-row! (loop [i (int 0)]
                                  (when (< i inc-cnt-a)
                                    (aset arr-2d i 0 i)
                                    (recur (unchecked-inc-int i))))
                init-first-col! (loop [i (int 0)]
                                  (when (< i inc-cnt-b)
                                    (aset arr-2d 0 i i)
                                    (recur (unchecked-inc-int i))))
                diff (loop [i (int 1)
                            j (int 1)]
                       (cond (= i inc-cnt-a)
                             (aget arr-2d (count a) (count b))

                             (= j inc-cnt-b)
                             (recur (unchecked-inc-int i)
                                    1)

                             :else
                             (let [dec-i (unchecked-dec-int i)
                                   dec-j (unchecked-dec-int j)
                                   a-i-dec (.charAt a dec-i)
                                   b-j-dec (.charAt b dec-j)]
                               (if (= a-i-dec b-j-dec)
                                 (do (aset-int arr-2d i j (aget arr-2d dec-i dec-j))
                                     (recur i
                                            (unchecked-inc-int j)))
                                 (let [insertion (unchecked-inc-int (aget arr-2d i dec-j))
                                       deletion (unchecked-inc-int (aget arr-2d dec-i j))
                                       replacement (unchecked-inc-int (aget arr-2d dec-i dec-j))]
                                   (aset-int arr-2d i j (min insertion deletion replacement))
                                   (recur i
                                          (unchecked-inc-int j)))))))]
            (<= diff max-diff))))