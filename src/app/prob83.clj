(ns app.prob83
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.spec.alpha :as c]
            [clojure.pprint :as pp]
            [app.prob81 :as p81]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=83

(defn edges [m]
  (let [M (count m)]
    (letfn [(put [z e]
              (let [[x y] e
                    x1 (inc x)
                    x2 (dec x)
                    y1 (inc y)
                    y2 (dec y)]
                (sq/conj-if
                 (sq/conj-if
                  (sq/conj-if
                   (sq/conj-if z (when (< x1 M) {:v1 [y x] :v2 [y x1] :w (p81/el m y x1)}))
                   (when (< y1 M) {:v1 [y x] :v2 [y1 x] :w (p81/el m y1 x)}))
                  (when (> y2 -1) {:v1 [y x] :v2 [y2 x] :w (p81/el m y2 x)}))
                 (when (> x2 -1) {:v1 [y x] :v2 [y x2] :w (p81/el m y x2)}))))]
      (->> (for [x (range 0 M)
                 y (range 0 M)]
             [x y])
           (reduce put [])))))

(t/deftest solve-test
  (t/is (= 2297 (p81/solve p81/M0 (edges p81/M0)))))

(t/deftest solve-prob83
  (t/is (= 425185 (let [M1 (p81/load-data "resources/p083_matrix.txt")]
                    (p81/solve M1 (edges M1))))))
