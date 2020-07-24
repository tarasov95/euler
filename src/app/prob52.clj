(ns app.prob52
  (:require
   [lib.prime :as prime]
   [lib.seq :as sq]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=52


;; It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.


(defn candidates-of-len [N]
  (let [pw10 (numb/pow-int 10 N)]
   (range (quot pw10 10) (quot pw10 6))))

(def ix (range 3 7))

(defn check [x]
  (let [t (numb/dig-mask (* x 2))]
    (every? (partial = t)
            (map #(numb/dig-mask (* x %)) ix))))

(defn solve []
  (let [rg (drop 2 (range))]
   (->> rg
        (mapcat candidates-of-len)
        (filter check)
        (take 1))))

(time (doall (solve)))
