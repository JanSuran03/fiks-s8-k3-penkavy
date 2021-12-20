(ns fiks-s8-k3-penkavy.maybe-simpler)

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