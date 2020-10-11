(ns app.prob87
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [lib.prime :as prime]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=87

(defn eligible-only [rg U]
  (->> rg
       (filter #(< % (- U 4 8)))))

(defn list-powers [U]
  (let [p1 (prime/primes-below (numb/lsqrt (- U 4 8)))
        p2 (eligible-only (map * p1 p1) U)
        p3 (eligible-only (map * p2 p1) U)
        p4 (eligible-only (map * p3 p1) U)]
    (->> (for [x p2  y p3 z p4]
           (+ x y z))
         (filter #(< % U)))))

(defn solve []
  (->> (list-powers 50000000)
       (distinct)
       (count)))

(t/deftest solve-test
  (t/is (= 1097343 (solve))))
