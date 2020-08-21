(ns app.prob72
  (:require [clojure.test :as t]
            [lib.prime :as prime]
            [app.prob70 :as p70]
            [clojure.core.reducers :as r]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=72
;; https://en.wikipedia.org/wiki/Farey_sequence

;; Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.
;; If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;; It can be seen that there are 21 elements in this set.
;; How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?

(def pi 3.14159265358979323846)

(defn norm-approx [n]
  (/ (* 3 (* n n)) (* pi pi)))

(defn norm-phi [n]
  (inc
   (->> (range 1 (inc n))
        (map p70/phi)
        (reduce +))))

(def norm-recur
  (memoize
   (fn [n]
     (- (/ (* (+ n 3) n) 2)
        (->> (range 2 (inc n))
             (map #(norm-recur (Math/floor (/ n %))))
             (reduce +))))))

(t/deftest norm-test
  (t/is (= (long (norm-recur 8)) (long (norm-phi 8)))))

(defn solve []
  (- (long (norm-recur (long 1e6))) 2))

(time (println (solve)))
