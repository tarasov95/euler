(ns app.prob57
  (:require
   [clojure.test :as t]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=57

(defn src []
  (iterate #(+ 1 (/ 1 (inc %)))
           (+ 1 (/ 1 2))))

(defn solve-lazy []
  (->> (src)
       (map #(vector (numerator %) (denominator %)))
       (map #(map (partial numb/num2dig 10) %))
       (map #(map count %))
       (take 1000)
       (filter #(> (first %) (second %)))
       (count)))

(defn solve-eager []
  (transduce (comp
              (map #(vector (numerator %) (denominator %)))
              (map #(map (partial numb/num2dig 10) %))
              (map #(map count %))
              (map #(if (> (first %) (second %)) 1 0))
              (take 1000))
             +
             (src)))

(time (println "solve-lazy" (solve-lazy)))
(time (println "solve-eager" (solve-eager)))
