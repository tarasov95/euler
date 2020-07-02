(ns app.prob32
  (:require [clojure.core.reducers :as r]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=32
;; The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
;; Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
;; HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

(def code0 (int \0))

(defn num-to-set [num]
  (->> (seq (str num))
       (map (fn [ch] (- (int ch) code0)))
       (into #{})))

(def all-dig (num-to-set 123456789))

;; 123456789 (count (seq "123456789")) ~> 9 digits
;; we have to have 9 digits in the  "identity"
;; distributed somehow on the left and right sides of the product equation
;; (* 99 99) ~> 9801 <= 10000 (mininum of 5 digits on the right hand side) ~> have to add more digit to the left
;; (* 99 999) ~> 98901 > 1000 (min of 4 digits on the right hand side) ~> there is some gap to search for the numbers in question
;; (max of the left) ~> 98901 and (max of the right) ~> 9999 ~> (range 1000 10000)
;; x*y=z ~> z>=x && z>=y ~> the right hand size has to be 4 digits

(defn list-num-distinct-dig [rg]
  (->> rg
       (map (fn [n] {:num n :dig (num-to-set n)}))
       (filter (fn [nd] (and (not (get (:dig nd) 0))
                             (= (count (:dig nd)) (count (str (:num nd)))))))))

(defn list-products []
  (list-num-distinct-dig (range 1000 10000)))

(defn list-all-mult-of [nu]
  (for [dv     (range 2 nu)
        :let   [qu (quot nu dv)]
        :while (> qu  dv) ;;12/3 ~> 4; 12/4 ~> 3
        :when  (= 0 (mod nu dv))]
    [dv qu nu]))

(defn is-pandig [N s]
  (let [d (into #{} (seq s))]
    (and (not (d \0))
         (= N (count s) (count d)))))

(defn list-panding-mult-of [nu]
  (->> (list-all-mult-of nu)
       (filter #(is-pandig 9 (apply str %)))))

(defn list-all-pandig9-mult []
  (->> (list-products)
      (map :num)
      (mapcat list-panding-mult-of)
      (filter not-empty)))

(defn solve []
  (->> (list-all-pandig9-mult)
       (map last)
       (into #{})
       (reduce +)))

(time (solve))
