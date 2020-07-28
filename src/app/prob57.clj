(ns app.prob57
  (:require
   [clojure.test :as t]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=57

(->>
 (iterate #(+ 1 (/ 1 (inc %)))
          (+ 1 (/ 1 2)))
 (map #(vector (numerator %) (denominator %)))
 (map #(map (partial numb/num2dig 10) %))
 (map #(map count %))
 (take 1000)
 (filter #(> (first %) (second %)))
 (count))
