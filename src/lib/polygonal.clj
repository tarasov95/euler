(ns lib.polygonal
  (:require [clojure.test :as t]
            [lib.numb :as numb]))

;; https://projecteuler.net/best_posts=061
;; BTW, the common formula for figure numbers:
;; p(k, n) = n * (2 + (k-2)*(n-1)) /2;

(defn p3 [n]
  (/ (* n (inc n)) 2))

(defn p4 [n]
  (* n n))

(defn p5 [n]
  (/ (* n (dec (* 3 n))) 2))

(defn p6 [n]
  (* n (dec (* 2 n))))

(defn p7 [n]
  (/ (* n (- (* 5 n) 3)) 2))

(t/deftest p7-test
  (t/is (= [1, 7, 18, 34, 55] (map p7 (range 1 6)))))

(defn p8 [n]
  (* n (- (* 3 n) 2)))

(t/deftest p8-test
  (t/is (= [1, 8, 21, 40, 65] (map p8 (range 1 6)))))

(defn p3-index [x]
  (/ (dec (Math/sqrt (inc (* 8 x)))) 2))

(defn p3? [x]
  (numb/natural? (p3-index x)))

(defn p4-index [x]
  (Math/sqrt x))

(defn p4? [x]
  (numb/natural? (p4-index x)))

(defn p5-index [x]
  (/ (inc (Math/sqrt (inc (* 24 x)))) 6))

(defn p5? [x]
  (numb/natural? (p5-index x)))

(defn p6-index [x]
  (/ (inc (Math/sqrt (inc (* 8 x)))) 4))

(t/deftest p6-index-test
  (t/is (= (range 1 6) (map (comp long p6-index) [1, 6, 15, 28, 45]))))

(defn p6? [x]
  (numb/natural? (p6-index x)))

(defn p7-index [x]
  (/ (+ 3 (Math/sqrt (+ 9 (* 40 x)))) 10))

(t/deftest p7-index-test
  (t/is (= (range 1 6) (map (comp long p7-index) [1, 7, 18, 34, 55]))))

(defn p7? [x]
  (numb/natural? (p7-index x)))

(defn p8-index [x]
  (/ (+ 1 (Math/sqrt (+ 1 (* 3 x)))) 3))

(t/deftest p8-index-test
  (t/is (= (range 1 6) (map (comp long p8-index) [1, 8, 21, 40, 65]))))

(defn p8? [x]
  (numb/natural? (p8-index x)))

