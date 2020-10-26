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

(def rdenom
  (->> denom
      (map identity)
      (sort-by (comp - second))
      (into [])))

(def rdig
  (->> denom
       (map reverse)
       (map (partial into []))
       (into {})))

(defn toR-denoms
  ([n] (toR-denoms {} (map second rdenom) n))
  ([z rd n]
   (if (or (< n 1)
           (empty? rd))
     z
     (let [d (first rd)
           q (quot n d)]
       (cond
         (= 0 q) (recur z (rest rd) n)
         :else (recur (assoc z d q) (rest rd) (mod n d)))))))

(t/deftest toR-denoms-test
  (t/is (= {1000 7, 500 1, 100 1, 10 4, 5 1, 1 3}
           (toR-denoms 7648))))

;; Only one I, X, and C can be used as the leading numeral in part of a subtractive pair.
;; I can only be placed before V and X.
;; X can only be placed before L and C.
;; C can only be placed before D and M.
(defmacro recur1 [md z R D]
  `(recur (into ~z (repeat (~md ~D) ~R)) (dissoc ~md ~D)))

(defmacro recur2 [md z R DD]
  `(recur (conj ~z ~R) (apply dissoc ~md ~DD)))

(defn toR-from-denoms [z md]
  (if (empty? md)
    z
    (cond
      (md 1000) (recur1 md z "M" 1000)
      (and (= 1 (md 500))
           (= 4 (md 100))) (recur2 md z "CM" [500 100])
      (md 500) (recur1 md z "D" 500)
      (= 4 (md 100)) (recur2 md z "CD" [100])
      (md 100) (recur1 md z "C" 100)
      (and (= 1 (md 50))
           (= 4 (md 10))) (recur2 md z "XC" [50 10])
      (md 50) (recur1 md z "L" 50)
      (= 4 (md 10)) (recur2 md z "XL" [10])
      (md 10) (recur1 md z "X" 10)
      (and (= 1 (md 5))
           (= 4 (md 1))) (recur2 md z "IX" [5 1])
      (md 5) (recur1 md z "V" 5)
      (= 4 (md 1)) (recur2 md z "IV" [1])
      (md 1) (recur1 md z "I" 1)
      :else [md z])))

(defn toR [n]
  (reduce
   str
   (toR-from-denoms [] (toR-denoms n))))

(t/deftest toR-test
  (t/is (= "VII" (toR 7)))
  (t/is (= "II" (toR 2)))
  (t/is (= "IV" (toR 4)))
  (t/is (= "IX" (toR 9)))
  (t/is (= "XIX" (toR 19))))

(defn solve []
  (let [data (slurp "resources/p089_roman.txt")
        r    (st/split data #"\n")]
    (->> r
         (map #(- (count %) (count (-> % to10 toR))))
         (reduce +))))

(t/deftest solve-test
  (t/is (= 743 (solve))))
