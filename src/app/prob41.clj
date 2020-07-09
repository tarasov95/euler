(ns app.prob41
  (:require
   [lib.prime :as prime]
   [lib.seq :as sq]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=41

;; (defn count-panding []
;;   (->> (range 2 10)
;;        (map numb/fact)
;;        (reduce +)))

(defn all-pandig []
  (->> (range 9 1 -1)
       (mapcat #(sq/permut (range % 0 -1)))
       (map (partial numb/dig2num 10))))

(defn solve []
  (->> (all-pandig)
       (filter prime/is-prime?)
       (take 1)))

(time (solve))
