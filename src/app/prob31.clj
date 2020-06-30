(ns app.prob31
  (:require [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=31

(defn range-of-coin-* [amount c]
  (range 0 (inc (quot amount c))))

(defn try-one-coin [amount c]
  (->> (range-of-coin-* amount c)
       (r/map (fn [m] {:$ c :* m :v (* c m)}))))

(defn change
  ([amount coins] (change [] [] amount coins))
  ([z y amount coins]
   (if (or (empty? coins) (= 0 amount))
     (if (= 0 amount) (conj z y) z)
     (let [c (first coins)]
       (->> (try-one-coin amount c)
            (r/map (fn [e] (change z (conj y e) (- amount (:v e)) (rest coins))))
            (r/filter not-empty)
            (r/fold concat))))))

;; change returns all solutions for it's parameters as an array of arrays of $* structures
;; when adding a coin to the solution of n-1 complexity have to join all $* for that coin with all of the soluions in the above array

;; (count (change 100 [1 5 10 25 50]))
;; 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).
;; 

(defn pchange [amount coins]
  (let [c1 (first coins)]
    (->> (into (list) (try-one-coin amount c1))
         (pmap (fn [e] (change [] [e] (- amount (:v e)) (rest coins))))
         (filter not-empty)
         (reduce concat))))

(let [coins  [1 2 5 10 20 50 100 200]
      amount 200]
  (time (pmap identity
              [(time (count (change amount coins)))
               (time (count (pchange amount coins)))])))
