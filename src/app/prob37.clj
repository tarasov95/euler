(ns app.prob37
  (:require [lib.numb :as numb]
            [lib.prime-data :as data]
            [lib.prime :as prime]
            [clojure.string :as s]
            [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=37

;; The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

;; n itself is a prime
;; the 1st digit is a prime ~> (2 3 5 7)
;; the last digit is a prime ~> 3 or 7 (can't be 2 or 5)

(defn subnumbs [n]
  (->> (iterate #(quot % 10) (numb/pow-int 10 (numb/log-int 10 n)))
       (take-while #(> % 1))
       (mapcat #(vector (quot n %) (mod n %)))))

(defn truncatable? [prev-prime p]
  (and
   (> p 10)
   (#{3 7} (mod p 10))
   (every? prev-prime (subnumbs p))))

;;TODO: to avoid explicit internal state (:pr) try to use lazy-seq
(defn solution [y e]
  (if (= 11 (count (:z y)))
    (reduced (:z y))
    {:z (if (truncatable? (:pr y) e)
          (conj (:z y) e)
          (:z y))
     :pr (conj (:pr y) e)}))

(time
 (->> (prime/primes-all)
      (reduce solution {:z [] :pr #{}})
      (reduce +)))
