(ns app.prob28
  (:require [lib.prime :refer :all]))

;; https://projecteuler.net/problem=28

;; 1x1 ~> nil
;; 3x3 ~> starts 1, step 2, ends at 9
;; 4x4 ~> nil
;; 5x5 ~> starts 9 step 4, ends at 25
;; 6x6 ~> nil
;; 7x7 ~> start 25, step 6, ends at 49
;; 8x8 ~> nil
;; 9x9 ~> start 49, step 8, ends at 81

;; 43 44 45 46 47 48 49 50
;; 42 21 22 23 24 25 26 51
;; 41 20  7  8  9 10 27 52
;; 40 19  6  1  2 11 28 53
;; 39 18  5  4  3 12 29 54
;; 38 17 16 15 14 13 30 55
;; 37 36 35 34 33 32 31 56
;;                      57

(defn link [N]
  (let [step (dec N)]
    (range (+ (if (> N 3) step 0)
              (* (- N 2) (- N 2)))
           (inc (* N N))
           step)))

(defn spiral [N]
  (mapcat link (range 3 (inc N) 2)))

(reduce + (spiral 1001))
