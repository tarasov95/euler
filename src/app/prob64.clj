(ns app.prob64
  (:require [clojure.test :as t]
            [lib.polygonal :as pn]
            [lib.numb :as numb]))


;;https://en.wikipedia.org/wiki/Periodic_continued_fraction
(defn next-a [p]
  (let [[a m d S a0 per ix-2a0] p
        mn (- (* d a) m)
        dn (/ (- S (* mn mn)) d)
        an (long (Math/floor (/  (+ a0 mn) dn)))
        ixn (if (= (* 2 a0) an) (inc ix-2a0) ix-2a0)
        pern (if (= 1 ixn) (inc per) per)]
    [an mn dn S a0 pern ixn]))

(defn a-seq [S]
  (->> (iterate next-a [(numb/lsqrt S) 0 1 S (numb/lsqrt S) 0 0])
       (take-while (fn [r] (<= (last r) 2)))
       (map #(last (butlast %)))
       (take-last 1)))

(defn list-fractions [N]
  (->> (range 2 (inc N))
      (filter #(-> %  Math/sqrt numb/natural? not))
      (mapcat a-seq)
      (filter odd?)))

(count (list-fractions 10000))
