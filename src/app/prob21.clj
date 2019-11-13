(ns app.prob21
  (:require [app.utils :as u]))

;; https://projecteuler.net/problem=21

(defn dividers
  ([n] (dividers n (inc (int (Math/sqrt n))) []))
  ([n u r]
   (if (< u 2)
       (conj r 1)
     (recur
      n
      (dec u)
      (if (= 0 (mod n u))
        (conj  r u (/ n u))
        r)))))

(def dividers-mem (memoize dividers))

(defn d [n]
  (reduce + 0 (dividers-mem n)))

(let [rg (for [x (range 2 11000)
               :let [y (d x)]
               :when (and (not= y x) (= (d y) x))]
           (list x y))
      ]
  (reduce + (into #{} (take-while #(< % 10000)  (mapcat identity rg)))))
