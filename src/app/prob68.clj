(ns app.prob68
  (:require [clojure.test :as t]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=68

;;3 triplets of numbers [1..6]

;; x1+x2+x3=S
;; y1+y2+y3=S
;; z1+z2+z3=S
;; y2 = x3
;; z2 = y3
;; z3 = x2
;; x1 < y1 < z1

;; x1+x2+x3=S
;; y1+x3+y3=S
;; z1+y3+x2=S
;; x1 < y1 < z1

(defn match3? [x1 x2 x3 y1 y3 z1]
  (let [S (+ x1 x2 x3)]
    (and (= S (+ y1 x3 y3))
         (= S (+ z1 y3 x2))
         (and (< x1 y1)
              (< x1 z1)))))

(defn solve3
  ([] (solve3 (range 1 7)))
  ([rg]
   (for [x1 rg
         x2 rg
         x3 rg
         y1 rg
         y3 rg
         z1 rg
         :when (and
                (= 6 (count (into #{} [x1 x2 x3 y1 y3 z1])))
                (match3? x1 x2 x3 y1 y3 z1))]
     [[x1 x2 x3] [y1 x3 y3] [z1 y3 x2]])))

;; x1+x2+x3 = S
;; y1+x3+y3 = S
;; z1+y3+z3 = S
;; a1+z3+a3 = S
;; b1+a3+x2 = S
;; x1<y1 && x1<z1 && x1<a1 && x1<b1

;; x1+x2 = y1+y3 ~> x1 = y1+y3-x2
;; y1+x3 = z1+z3 ~> z3 = y1+x3-z1
;; z1+y3 = a1+a3 ~> a3 = z1+y3-a1
;; a1+z3 = b1+x2 ~> b1 = a1+z3-x2

(defn match5? [x1 x2 x3 y1 y3 z1 z3 a1 a3 b1]
  (let [S (+ x1 x2 x3)]
    (and
     (= S (+ y1 x3 y3))
     (= S (+ z1 y3 z3))
     (= S (+ a1 z3 a3))
     (= S (+ b1 a3 x2))
     (= 10 (count (into #{} [x1 x2 x3 y1 y3 z1 z3 a1 a3 b1]))))))

(defn solve5
  ([] (solve5 (range 1 11)))
  ([rg]
   (for [x2 rg
         x3 rg
         y1 rg
         y3 rg
         z1 rg
         a1 rg
         :let [x1 (- (+ y1 y3) x2)
               z3 (- (+ y1 x3) z1)
               a3 (- (+ z1 y3) a1)
               b1 (- (+ a1 z3) x2)]
         :when (and
                (< x1 11)
                (< z3 11)
                (< a3 11)
                (< b1 11)
                (> x1 0)
                (> z3 0)
                (> a3 0)
                (> b1 0)
                (< x1 y1)
                (< x1 z1)
                (< x1 a1)
                (< x1 b1)
                (match5? x1 x2 x3 y1 y3 z1 z3 a1 a3 b1))]
     [[x1 x2 x3] [y1 x3 y3] [z1 y3 z3] [a1 z3 a3] [b1 a3 x2]])))

(defn max-str [z]
  (->> z
       (map (partial reduce concat))
       (map (partial reduce str))
       (sort)
       (last)))

(list (time (max-str (solve3)))
      (time (max-str (solve5))))

