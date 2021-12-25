(ns fiks-s8-k3-penkavy.test)

(defn compute []
  (let [data [[0 3] [0 5] [1 2] [3 4] [3 5] [3 6] [5 6]]
        same-species-finches (loop [ret {}
                                    [[v1 v2] & remaining] data]
                               (if v1
                                 (let [new-ret (-> ret (assoc v1 ((fnil conj #{}) (get ret v1) v2))
                                                   (assoc v2 ((fnil conj #{}) (get ret v2) v1)))]
                                   (recur new-ret
                                          remaining))
                                 ret))
        interesting-set (set (keys same-species-finches))
        sorted-interesting-seq (vec (sort interesting-set))
        len (count sorted-interesting-seq)]
    (println same-species-finches)
    (println sorted-interesting-seq)
    (println len)
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
            (let [[finch-A finch-C] (map #(nth sorted-interesting-seq %) [i j])
                  same-species-for-finch-A (get same-species-finches finch-A)]
              (println same-species-for-finch-A)
              (if (contains? same-species-for-finch-A finch-C)
                (recur i (inc j) ret)
                (let [interesting-trinities (for [finch-idx sorted-interesting-seq
                                                  :when (and (not (contains? #{finch-A finch-C} finch-idx))
                                                             (contains? same-species-for-finch-A finch-idx)
                                                             (contains? (get same-species-finches finch-C) finch-idx))]
                                              [finch-A finch-idx finch-C])]
                  (recur i
                         (inc j)
                         (reduce conj! ret interesting-trinities)))))))))