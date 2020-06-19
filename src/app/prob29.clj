(ns app.prob29
  (:require [lib.prime :refer :all]
            [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=29

;; Consider all integer combinations of ab for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:

(def primes4all (primes-all))
(def is-prime? (partial prime? primes4all))

;; (let [N 5
;;       U (inc N)]
;;   (for [a (range 2 U)
;;         b (range 2 U)]
;;     [a b]))

(defn log-xy
  "floor of pow <~> x^pow==y"
  [x y]
  (int (Math/floor (/ (Math/log y) (Math/log x)))))

(defn count-of-prime-pows [pr N]
  (->> pr
       (r/map #(* N (log-xy % N)))
       (r/reduce +)))

(let [N 10
      pr (take-while #(< % N) primes4all)]
  pr)

;; (log-xy 2 10)

;; (pows 2 10)
;; (pows 3 10)
