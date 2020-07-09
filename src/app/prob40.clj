(ns app.prob40
  (:require [lib.numb :as numb]))

;; https://projecteuler.net/problem=40


(defn Champernowne
  ([] (Champernowne 1 (list)))
  ([n surplus]
   (lazy-seq
    (if (empty? surplus)
      (let [s (seq (str n))]
        (cons (first s) (Champernowne (inc n) (rest s))))
      (cons (first surplus) (Champernowne n (rest surplus)))))))

(take 12 (Champernowne))
