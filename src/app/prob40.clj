(ns app.prob40
  (:require
   [clojure.core.reducers :as r]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=40
;; An irrational decimal fraction is created by concatenating the positive integers:
;; 0.123456789101112131415161718192021...
;; It can be seen that the 12th digit of the fractional part is 1.
;; If dn represents the nth digit of the fractional part, find the value of the following expression.
;; d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

(defn char2int [c]
  (- (int c) (int \0)))

(defn Champernowne
  ([] (Champernowne 1 (list)))
  ([n surplus]
   (lazy-seq
    (if (empty? surplus)
      (let [s (seq (str n))]
        (cons (char2int (first s)) (Champernowne (inc n) (rest s))))
      (cons (char2int (first surplus)) (Champernowne n (rest surplus)))))))

(defn brute-force []
  (let  [pw10 (into #{} (take 7 (iterate #(* 10 %) 1)))]
    (->> (take (reduce max pw10) (Champernowne))
         (map-indexed (fn [ix e] (if (= nil (pw10 (inc ix))) 1 e)))
         (reduce *))))

(time (brute-force))
