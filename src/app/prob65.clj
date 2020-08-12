(ns app.prob65
  (:require [clojure.test :as t]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=65

;; (defn e-conv []
;;   (iterate #(+ 1 (/ 1 (inc %)))
;;            (+ 1 (/ 1 2))))

;;https://en.wikipedia.org/wiki/List_of_representations_of_e#As_an_infinite_series
;;(sum 1/k!)

(def ^:dynamic P 200)
(def ^:private EPS (read-string (str "1e-" P "M")))

(defn eM []
  (loop [z 0M
         y 1M
         k 1M]
    (if (< y EPS)
      z
      (recur (+ z y) (/ y k) (inc k)))))

(t/deftest eM-test
  (t/is (clojure.string/starts-with?
         (str (with-precision P (eM)))
         "2.71828182845904523536028747135266249775724709369995")))

(defn next-frac [w]
  (with-precision P
    (let [[z y] w
         a (long y)]
    [a (/ 1M (- y a))])))

(defn cont-frac [N]
  (with-precision P
   (let [e (eM)]
     (take N (drop 1 (map first (iterate next-frac [nil e])))))))


;;https://en.wikipedia.org/wiki/Continued_fraction#Calculating_continued_fraction_representations
;;NOTE: The pattern repeats indefinitely with a period of 3 except that 2 is added to one of the terms in each cycle.
(t/deftest cont-frac-test
  (t/is (= [2, 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, 1, 10, 1, 1, 12, 1, 1, 14, 1, 1, 16, 1, 1, 18, 1, 1, 20, 1, 1, 22, 1, 1, 24, 1, 1, 26, 1, 1, 28, 1, 1, 30, 1, 1, 32, 1, 1, 34, 1, 1, 36, 1, 1, 38, 1, 1, 40, 1, 1, 42, 1, 1, 44, 1, 1, 46, 1, 1, 48, 1, 1, 50, 1, 1, 52, 1, 1, 54, 1, 1, 56, 1, 1, 58, 1, 1, 60, 1, 1, 62, 1, 1, 64, 1, 1, 66 ] (cont-frac 99))))

;;https://en.wikipedia.org/wiki/Continued_fraction#Infinite_continued_fractions_and_convergents
;;NOTE: convergents have a formula, but we'll try with blunt recursion

(defn e-conv [rf]
  (let [a (first rf)]
    (if (empty? (rest rf))
      a
      (+ a (/ 1 (e-conv (rest rf)))))))

(let [c (e-conv (cont-frac 100))]
  (reduce + (map #(- (int %)(int \0)) (seq (str (numerator c))))))
