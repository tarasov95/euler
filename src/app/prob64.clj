(ns app.prob64
  (:require [clojure.test :as t]
            [lib.polygonal :as pn]
            [lib.numb :as numb]))


;;https://en.wikipedia.org/wiki/Periodic_continued_fraction
(defn next-a [p]
  (let [[a m d S a0 len ix] p
        mn (- (* d a) m)
        dn (/ (- S (* mn mn)) d)
        an (long (Math/floor (/ (+ a0 mn) dn)))
        ixn (cond (< ix 0) (inc (- ix))
                  (= (* 2 a0) an) (- ix)
                  :else ix)
        lenn (if (or (= -1 ixn) (= 2 ixn)) (inc len) len)]
    [an mn dn S a0 lenn ixn]))

(defn intl-seq [S]
  (->> (iterate next-a [(numb/lsqrt S) 0 1 S (numb/lsqrt S) 0 1])
       (take-while (fn [r] (<= (last r) 2)))))

(defn a-seq [S]
  (map first (intl-seq S)))

(defn period [S]
  (last (butlast (last (intl-seq S)))))

(defn list-fractions [N]
  (->> (range 2 (inc N))
       (filter #(-> %  Math/sqrt numb/natural? not))
       (map period)
       (filter odd?)))

(time (count (list-fractions 10000)))

