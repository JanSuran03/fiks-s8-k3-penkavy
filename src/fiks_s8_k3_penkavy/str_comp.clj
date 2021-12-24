(ns fiks-s8-k3-penkavy.str-comp)

(defn- compute-next-row
  [prev-row current-element other-seq]
  (reduce
    (fn [row [diagonal above other-element]]
      (let [update-val
            (if (= other-element current-element)
              diagonal
              (inc (min diagonal above (peek row)))
              )]
        (conj row update-val)))
    [(inc (first prev-row))]
    (map vector prev-row (next prev-row) other-seq)))

(defn levenshtein-distance [a b]
  (peek
    (reduce
      (fn [prev-row current-element]
        (compute-next-row prev-row current-element b))
      (range (inc (count b)))
      a)))

(defn init-2d-vector [rows cols]
  (vec (repeat rows (vec (repeat cols 0)))))

(defn levenshtein [^String a ^String b]
  (let [inc-cnt-a (inc (count a))
        inc-cnt-b (inc (count b))
        vec-2d (init-2d-vector inc-cnt-a inc-cnt-b)
        with-first-row (loop [v vec-2d
                              i 0]
                         (if (< i inc-cnt-a)
                           (recur (assoc-in v [i 0] i)
                                  (inc i))
                           v))
        with-first-col (loop [v with-first-row
                              i 0]
                         (if (< i inc-cnt-b)
                           (recur (assoc-in v [0 i] i)
                                  (inc i))
                           v))]
    (loop [v with-first-col
           i 1
           j 1]
      (cond (= i inc-cnt-a)
            (get-in v [(count a) (count b)])

            (= j inc-cnt-b)
            (recur v
                   (inc i)
                   1)

            :else
            (let [dec-i (dec i)
                  dec-j (dec j)
                  a-i-dec (.charAt a dec-i)
                  b-j-dec (.charAt b dec-j)]
              (if (= a-i-dec b-j-dec)
                (recur (assoc-in v [i j] (get-in v [dec-i dec-j]))
                       i
                       (inc j))
                (let [insertion ^Integer (inc (get-in v [i dec-j]))
                      deletion ^Integer (inc (get-in v [dec-i j]))
                      replacement ^Integer (inc (get-in v [dec-i dec-j]))]
                  (recur (assoc-in v [i j] (Math/min (Math/min insertion deletion) replacement))
                         i (inc j)))))))))