(ns app.prob60
  (:require [lib.prime :as prime]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=60
;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
;; Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

(defn cat-left [x y]
  (+ y
     (* x (numb/pow-int 10 (inc (numb/log-int 10 y))))))

(defn cat-right [x y]
  (cat-left y x))

(defn in-family? [rg p]
  (->> rg
       (mapcat #(vector (cat-left % p) (cat-right % p)))
       (every? prime/is-prime?)))

(let [pr [3 7 109 673]
      ps (drop-while
          (partial >= (reduce max pr))
          (prime/primes-below 10000000))]
  (->> ps
       (filter (partial in-family? pr))
       (take 1)))
