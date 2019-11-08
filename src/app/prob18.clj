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
  {:r []
   :w })

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
        spaces (apply str (for [_ (range 0 w)] " "))]
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

(let [v (:r t0)
      m (to-2d v)]
  (println "===========")
  (print-t v)
  {:result (max-sum (reverse m))
   })
