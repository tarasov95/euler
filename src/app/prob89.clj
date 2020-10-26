(ns app.prob89
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.string :as st]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=89

(def denom
  {\I  1
   \V  5
   \X  10
   \L  50
   \C  100
   \D  500
   \M  1000})

(defn denoms-to10 [rg]
  (loop [z 0
         cur (first rg)
         y cur
         r (rest rg)]
    (if (empty? r)
      (+ z y)
      (let [n (first r)]
       (cond
         (= n cur) (recur z n (+ y n) (rest r))
         (> n (or cur n)) (recur (+ z (- n y)) nil 0 (rest r))
         :else (recur (+ z y) n n (rest r)))))))

(t/deftest denoms-to10-test
  (t/is (= 4672 (denoms-to10 [1000 1000 1000 1000 500 100 50 10 10 1 1])))
  (t/is (= 19 (denoms-to10 [10 1 10])))
  (t/is (= 28 (denoms-to10 [10 10 5 1 1 1])))
  (t/is (= 8 (denoms-to10 [5 1 1 1])))
  (t/is (= 3 (denoms-to10 [1 1 1]))))

(defn to10 [rom]
  (->> (seq rom)
       (map denom)
       denoms-to10))

(t/deftest to10-test
  (t/is (= 4672 (to10 "MMMMDCLXXII"))))

(let [data (slurp "resources/p089_roman.txt")
      r    (st/split data #"\n")]
  (->> r
       (map to10)
       (reduce max)))
