(ns app.prob71
  (:require [clojure.test :as t]
            [lib.prime :as prime]
            [clojure.core.reducers :as r]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=71

;; Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

;; If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:

;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

;; It can be seen that 2/5 is the fraction immediately to the left of 3/7.

;; By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.

(defn min-to-goal
  ([] nil)
  ([e] e)
  ([e1 e2]
   (cond
     (nil? e1) e2
     (nil? e2) e1
     (< (- 3/7 e1) (- 3/7 e2)) e1
     :else e2)))

;;the idea is to make the denominator d from 7 ~> numerator => [3*(d/7)]

(defn solve []
  (->> (vec (range 2 (long 1e6)))
       (r/map #(/ (Math/round (* 3.0 (/ % 7)))
                  %))
       (r/filter #(< % 3/7))
       (r/fold min-to-goal)))

(time (println (solve)))
