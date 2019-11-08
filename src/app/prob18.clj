(ns app.prob18
  (:require [clojure.pprint :as pp]))
;; https://projecteuler.net/problem=18

(def t0
  {:r [3
       7 4
       2 4 6
       8 5 9 3]
   :w 4})

(def t1
  {:r [75
       95 64
       17 47 82
       18 35 87 10
       20 4 82 47 65
       19 1 23 75 3 34
       88 2 77 73 7 63 67
       99 65 4 28 6 16 70 92
       41 41 26 56 83 40 80 70 33
       41 48 72 33 47 32 37 16 94 29
       53 71 44 65 25 43 91 52 97 51 14
       70 11 33 28 77 73 17 78 39 68 17 57
       91 71 52 38 17 14 91 43 58 50 27 29 48
       63 66 4 68 89 53 67 30 73 16 69 87 40 31
       4 62 98 27 23 9 70 98 73 93 38 53 60 4 23]
   :w 15})

(defn dec-w [t]
  {:r (drop-last (:w t) (:r t))
   :w (dec (:w t))})

(defn row [t ix]
  (take ix (drop (apply + (range 1 ix)) t)))

(defn to-2d
  ([t] (to-2d t 1 []))
  ([t ix r]
   (if (empty? t)
     r
     (recur (drop ix t)
            (inc ix)
            (conj r (into [] (take ix t)))))))

(defn print-t [t]
  (let [m (to-2d t)
        w (count (last m))
        spaces (apply str (for [_ (range  w)] " "))]
    (doall
     (map-indexed
      (fn [i r] (println (subs spaces 1 (- w i)) r))
      m))))

(defn max-sum [m]
  (let [l (first m)
        p (second m)]
    (if (nil? p)
      l
      (letfn
         [(emax [i e] (max
                       (+ e (nth l i))
                       (+ e (nth l (inc i)))))]
        (recur (conj
                (drop 2 m )
                (into [] (map-indexed emax p))))))))

(defn solve [v]
  (let [m (to-2d v)]
    (max-sum (reverse m))))

(let []
  ;; (println "===========")
  ;; (print-t v)
  {:res0 (solve (:r t0))
   :res1 (solve (:r t1))})
