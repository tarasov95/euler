(ns app.prob85
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as st]
            [app.prob45 :as p45]
            [clojure.pprint :as pp]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=85

;; a(a+1)b(b+1)/4

;; (let [a 50 b 50]
;;   (* 3 (/ (* a (inc a) b (inc b)) 4)))

;; (+ (* 50 50) (* 50 50) 50 50 25 25)
;; (let [N 50](+ (* 3 (* N N)) N))

(defn list-psquare [N]
  (->> (range 1 (inc (quot N 2)))
       (map #(vector % (- N %)))
       (filter #(numb/natural? (Math/sqrt (apply * %))))))

(comment)

(comment
  (let [N 50]
    (->> (range 1 (inc N))
         (mapcat #(list-psquare %))
         ;; (map #(apply + %))
         ;; (count)
         )))

(defn gen-rotations [N]
  (for [x (range 1 N)
        p (range 1 N)
        :let [y (- N x)
              k (- N p)
              a2 (+ (* N N) (* x x))
              b2 (+ (* y y) (* p p))
              c2 (+ (* N N) (* k k))]]
    [a2 b2 c2]))

(defn right2? [t]
  (let [[a2 b2 c2] t]
    (or (= a2 (+ b2 c2))
        (= b2 (+ a2 c2))
        (= c2 (+ a2 b2)))))

(defn count-rotations [N]
  (->> (range 1 (inc N))
       (mapcat gen-rotations)
       (filter right2?)
       (count)))

(comment (let [N 2
               R (count-rotations N)
               Q (->> (range 1 (inc N))
                      (mapcat #(list-psquare %))
                      (count))]
           (+ (* 3 (* N N)) (* 2 Q) R)))

(defn gen-vert [N]
  (for [x (range 0 (inc N))
        y (range 0 (inc N))]
    [x y]))

(defn gen-vpairs [N]
  (for [v1 (drop 1 (gen-vert N))
        v2 (drop 1 (gen-vert N))
        :when (some true? (map not= v1 v2))]
    [v1 v2]))

(defn len2 [v]
  (let [[p1 p2] v]
    (+ (* p2 p2) (* p1 p1))))

(defn vec [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    [(- x2 x1) (- y2 y1)]))

(defn solve [N]
  (/
   (->> (gen-vpairs N)
        (map #(let [[v1 v2] %
                    v3 (vec v1 v2)]
                (with-meta
                  [(len2 v1)
                   (len2 v2)
                   (len2 v3)]
                  {:vert [v1 v2 v3]})))
        (filter right2?)
        (count))
   2))

(t/deftest solve-test
  (t/is (= 14234 (solve 50))))
