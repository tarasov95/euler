(ns app.prob36
  (:require [lib.numb :as numb]
            [clojure.string :as s]
            [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=36
;; The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
;; Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

;; 1 000 000 ~> max palindrome is 999 999 (6 digits)

;;NOTE while it's possible to check for a number being a palindrome in b2 directly, here it's done via converting it into array of digits just for the sake of having to use arrays

(defn lgpw10 [n]
  (numb/gen-lgpw10 6 n))

(defn pali-splice [head mid tail pad0]
  (let [pm (if (= pad0 nil) 1 (* pad0 pad0))]
    (if (= mid nil)
      (+ tail (* head 10 (lgpw10 tail) pm))
      (+ tail
         (* head 100 (lgpw10 tail) pm)
         (* mid 10 (lgpw10 tail) (or pad0 1))))))

(defn pali-gen
  ([n] (pali-gen n nil nil))
  ([n mid] (pali-gen n mid nil))
  ([n mid pad0]
   (loop [y (quot n 10)
          z (mod n 10)]
     (if (= 0 y)
       (pali-splice z mid n pad0)
       (recur (quot y 10)
              (+ (* z 10) (mod y 10)))))))

(defn pali-odd
  ([n] (pali-odd n 1))
  ([n pad0]
   (->> (range 0 10)
        (map #(pali-gen n % pad0)))))

(defn pali-pad [n]
  (take-while #(<= % (quot 100 (lgpw10 n)))
              (iterate #(* 10 %) 1)))

(defn pali [n]
  (concat
   (->> (pali-pad n)
        (map #(pali-gen n nil %)))
   (->> (butlast (pali-pad n))
        (mapcat #(pali-odd n %)))))

(defn palindrome? [rg]
  (let [cnt (count rg)]
    (case cnt
      (0 1) true
      (2 3) (= (first rg) (last rg))
      (and (= (first rg) (last rg))
           (recur (subvec rg 1 (dec cnt)))))))

(defn list-pali-b10 []
  (concat
   (range 0 10) ;;https://en.wikipedia.org/wiki/Palindromic_number
   (->> (range 1 1000)
        (filter #(> (mod % 10) 0))
        (mapcat pali))))

(defn solve []
  (->> (list-pali-b10)
       (filter #(palindrome? (numb/num2dig 2 %)))
       (reduce +)))

(time (solve))
