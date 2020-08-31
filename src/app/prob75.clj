(ns app.prob75
  (:require [clojure.test :as t]
            [lib.prime :as prime]
            [clojure.core.matrix :as m]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=75

(m/set-current-implementation :vectorz)

(def A (m/matrix [[1 -2 2] [2 -1 2] [2 -2 3]]))
(def B (m/matrix [[1 2 2] [2 1 2] [2 2 3]]))
(def C (m/matrix [[-1 2 2] [-2 1 2] [-2 2 3]]))

(t/deftest ABC-test
  (t/is (= (m/array [15 8 17]) (m/mmul C [3 4 5])))
  (t/is (= (m/array [21 20 29]) (m/mmul B [3 4 5])))
  (t/is (= (m/array [5 12 13]) (m/mmul A [3 4 5]))))

(def N 1500000)

(defn conj-if [pred? el coll]
  (if (pred? el)
    (conj coll el)
    coll))

(defn bfs-pyth
  ([z acc pred?] (bfs-pyth [(m/array [3 4 5])] z acc pred?))
  ([s z acc pred?]
   (if (empty? s)
     z
     (let [p (first s)]
       (recur (conj-if pred? (m/mmul A p)
                       (conj-if pred? (m/mmul B p)
                                (conj-if pred? (m/mmul C p)
                                         (rest s))))
              (acc z p)
              acc pred?)))))

(defn perim-fits? [N v]
  (<= (m/ereduce + v) N))

(t/deftest bfs-pyth-test
  (t/is (= [(m/array [3.0,4.0,5.0]) (m/array [5.0,12.0,13.0]) (m/array [15.0,8.0,17.0])]
           (bfs-pyth [] conj (partial perim-fits? 40)))))

(defn into-map-all-below [N t]
  (let [p (long (m/ereduce + t))]
    (->> (range p (inc N) p)
         (into {} (map #(vector % 1))))))

(t/deftest into-map-all-below-test
  (t/is (= {12 1, 24 1, 36 1} (into-map-all-below 40 [3 4 5]))))

(defn solve [N]
  (bfs-pyth {}
            (fn [z e] (merge-with + z (into-map-all-below N e)))
            (partial perim-fits? N)))

(t/deftest solve-test
  (t/is (= 3 ((solve 120) 120))))

(time (->> (solve N)
           (filter #(= 1 (second %)))
           (count)))
