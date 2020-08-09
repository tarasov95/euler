(ns lib.polygonal
  (:require [clojure.test :as t]
            [lib.numb :as numb]))

(defn p3 [n]
  (/ (* n (inc n)) 2))

(defn p4 [n]
  (* n n))

(defn p5 [n]
  (/ (* n (dec (* 3 n))) 2))

(defn p6 [n]
  (* n (dec (* 2 n))))

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

