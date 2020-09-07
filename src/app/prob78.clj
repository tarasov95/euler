(ns app.prob78
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=78
;; https://en.wikipedia.org/wiki/Partition_function_(number_theory)

(defn K [n]
  (Math/round
   (/ (dec (Math/sqrt (inc (* 24 n)))) 6)))

(defn p [n]
  (letfn [(sign [k v] (if (even? k) (- v) v))]
    (loop [x 1
          rg [1]]
     (if (> x n)
       (rg (dec x))
       (recur (inc x)
              (->>  (range (- (K x)) (inc (K x)))
                    (filter (comp not zero?))
                    (map (fn [k]
                           (let [a (- x (/ (* k (dec (* 3 k))) 2))]
                             (sign k (if (< a 0) 0 (rg a))))))
                    (reduce +)
                    (conj rg)))))))

(t/deftest p-test
  (t/is (->> [1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77, 101, 135, 176, 231, 297, 385, 490]
        (map-indexed (fn [ix e] (= (p ix) e)))
        (every? true?))))
