(ns app.prob82
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.spec.alpha :as c]
            [clojure.pprint :as pp]
            [app.prob81 :as p81]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=82

(defn edges [m]
  (let [M (count m)]
    (letfn [(put [z e]
              (let [[x y] e
                    x1 (inc x)
                    y1 (inc y)
                    y2 (dec y)]
                (sq/conj-if
                 (sq/conj-if
                  (sq/conj-if z (when (< x1 M) {:v1 [y x] :v2 [y x1] :w (p81/el m y x1)}))
                  (when (< y1 M) {:v1 [y x] :v2 [y1 x] :w (p81/el m y1 x)}))
                 (when (> y2 -1) {:v1 [y x] :v2 [y2 x] :w (p81/el m y2 x)}))))]
      (->> (for [x (range 0 M)
                 y (range 0 M)]
             [x y])
           (reduce put [])))))

(defn solve-from-vertex
  ([m v-start] (solve-from-vertex m (edges m) v-start))
  ([m edg v-start]
   (let [M (dec (count m))]
     (->> (p81/BellmanFord m edg v-start)
          (filter #(= M (-> % first second)))
          (map second)
          (reduce min)))))

(t/deftest solve-from-vertex-test
  (t/is (= 994 (solve-from-vertex p81/M0 [1 0]))))

(defn solve [m]
  (let [edg (edges m)]
    (->> (range 0 (count m))
        (pmap #(solve-from-vertex m edg [% 0]))
        (reduce min))))

(t/deftest solve-test
  (t/is (= 994 (solve p81/M0))))

(t/deftest solve-slow
  (t/is (= 260324
         (let [M1 (p81/load-data "resources/p082_matrix.txt")]
           (time (solve M1))))))
