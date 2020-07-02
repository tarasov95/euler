(ns app.prob33
  (:require [lib.numb :as numb]))

;; https://projecteuler.net/problem=33

(defn candidates []
  (for [n (range 10 100)
        d (range 10 100)
        :let [mn (mod n 10)
              md (mod d 10)
              qn (quot n 10)
              qd (quot d 10)]
        :when (and (> mn 0)
                   (> md 0)
                   (< n d))]
    [[n d]
     (cond (= mn md) [qn qd]
           (= mn qd) [qn md]
           (= qn md) [mn qd]
           (= qd qn) [mn md]
           :else nil)]))

(defn norm-rat [r]
  (let [gcd (numb/gcd (first r) (second r))]
    (map #(/ % gcd) r)))

(defn list-the-rats []
  (->> (candidates)
       (filter #(not-empty (second %)))
       (filter #(= (norm-rat (second %)) (norm-rat (first %))))))

(defn mult-rat [r1 r2]
  [(* (first r2) (first r1))
   (* (second r2) (second r1))])

(norm-rat
 (->> (list-the-rats)
      (map #(norm-rat (second %)))
      (reduce mult-rat)))
