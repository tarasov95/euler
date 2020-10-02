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

(s/def ::x number?)
(s/def ::y number?)
(s/def ::shape (s/tuple ::x ::y))
(s/def ::grid ::shape)
(s/def ::rect ::shape)
(s/def ::count number?)
(s/def ::wrapped-rect (s/keys :req-un [::grid ::rect ::count]))
(s/def ::search-space (s/coll-of ::wrapped-rect :kind vector?))

(def state-start {:grid [1 1] :rect [1 1] :count 1})

(t/deftest wrapped-rect-test
  (t/is (not (s/invalid? (s/conform ::search-space [state-start]))))
  (t/is (not (s/invalid? (s/conform ::wrapped-rect state-start)))))

(defn count-rect [grid rect]
  (let [[gx gy] grid
        [rx ry] rect]
    (* (+ 1 (- gx rx))
       (+ 1 (- gy ry)))))

(s/fdef count-rect
  :args (s/cat :grid ::shape :rect ::shape)
  :ret number?)

(stest/instrument)

(t/deftest count-rect-test
  (t/is (= 1 (count-rect [3 2] [3 2])))
  (t/is (= 2 (count-rect [3 2] [2 2])))
  (t/is (= 2 (count-rect [3 2] [3 1])))
  (t/is (= 3 (count-rect [3 2] [1 2])))
  (t/is (= 6 (count-rect [4 2] [2 1])))
  (t/is (= 4 (count-rect [3 2] [2 1])))
  (t/is (= 6 (count-rect [3 2] [1 1]))))

(defn prev-rect [rect ix]
  (assoc rect ix (dec (rect ix))))

(s/fdef prev-rect
  :args (s/cat :rect ::shape :ix #{0 1}))

(t/deftest prev-rect-test
  (t/is (= [3 1] (prev-rect [3 2] 1)))
  (t/is (= [2 2] (prev-rect [3 2] 0))))

(defn count-rect-all
  ([grid] (count-rect-all grid grid))
  ([grid rect]
   (if (some (partial = 0) rect)
     0
     (+ (count-rect grid rect)
        (count-rect-all grid (prev-rect rect 0))
        (count-rect-all grid (prev-rect rect 1))))))

(defn count-rect-grid [grid]
  (->>
   (for [x (range 1 (inc (first grid)))
         y (range 1 (inc (second grid)))]
     [x y])
   (map #(count-rect grid %))
   (reduce +)))

(t/deftest count-rect-grid-test
  (t/is (= 9 (count-rect-grid [2 2])))
  (t/is (= 18 (count-rect-grid [2 3])))
  (t/is (= 18 (count-rect-grid [3 2])))
  (t/is (= 21 (count-rect-grid [1 6]))))

(defn min-z [z0 z1]
  (if (> (Math/abs (- 2000000 (:c z0)))
         (Math/abs (- 2000000 (:c z1))))
    z1
    z0))

(defn find-min-for-x
  ([x y] (min-z (find-min-for-x x y -1)
                (find-min-for-x x y 1)))
  ([x y-start step]
   (loop [y y-start
          z (count-rect-grid [x y])]
     (let [yn (+ y step)
           zn (count-rect-grid [x yn])]
       (if (> (Math/abs (- 2000000 zn))
              (Math/abs (- 2000000 z)))
         {:g [x y] :c z}
         (recur yn zn))))))

(t/deftest find-min-for-x-test
  (t/is (= {:g [2 1154] :c 1999305} (find-min-for-x 2 1000))))

;; consider a grid 1xN
;; number of rectangles in such a grid is sum of i where i in (range 1 N)
;; the sum is a triangular number https://en.wikipedia.org/wiki/1_%2B_2_%2B_3_%2B_4_%2B_%E2%8B%AF
;; calculating index of a triangular number /see prob45/ close to 2e6 we get N=2000

(defn solve []
  (loop [x 1
         y 2000
         z {:g [x y] :c (count-rect-grid [x y])}]
    (if (>= x y)
      z
      (recur (inc x) (quot 2000 (inc x)) (min-z z
                                                (find-min-for-x x y))))))

(t/deftest solve-test
  (t/is (= 2772 (apply * (:g (solve))))))
