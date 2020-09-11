(ns app.prob81
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.spec.alpha :as c]
            [clojure.string :as s]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=81

(def  m0 ^{:cols 5}
  [131  673  234  103  18
   201  96  342  965  150
   630  803  746  422  111
   537  699  497  121  956
   805  732  524  37  331])

(defn arr2mat [rg]
  (let [{:keys [cols]} (meta rg)]
    (into []
          (map (partial into []))
          (partition cols rg))))

(def M0 (arr2mat m0))

(defn el [m row col]
  ((m row) col))

(defn conj-if [coll & xs]
  (reduce conj coll (filter (comp not nil?) xs)))

(defn if-goal [m p]
  (let [N (dec (count m))
        c (last p)]
    (if (and (= N (:x c))
             (= N (:y c)))
      p
      nil)))

(defn path-step [m x y]
  (let [w (el m x y)]
    {:x x :y y :l w :w w}))

(defn go-next [m p coord]
  (let [c (last p)
        ix (inc (get c coord))]
    (if (< ix (count m))
      (let [cn (merge c (assoc c coord ix))
            w (el m (:y cn) (:x cn))
            ln (+ (:l c) w)]
        (conj p (assoc cn :l ln :w w)))
      nil)))

(t/deftest go-next-test
  (t/is (= nil
           (go-next M0 [(path-step M0 4 4)] :x)))
  (t/is (= [{:x 0, :y 0, :l 131 :w 131} {:x 0, :y 1, :l 332 :w 201}]
           (go-next M0 [(path-step M0 0 0)] :y))))

(defn bfs-path
  ;; state ~> [[{:x x-coordinate :y y-coordinate :l current-path-length}]]
  ([m] (bfs-path []
                 (vector [(path-step m 0 0)]) m))
  ([z s m]
   (if (empty? s)
     z
     (let [c (first s)]
       (recur (conj-if z (if-goal m c))
              (conj-if (subvec s 1)
                       (go-next m c :y)
                       (go-next m c :x))
              m)))))

(t/deftest bfs-path-test
  (t/is (= [131 201 96 342 746 422 121 37 331]
           (->> (bfs-path M0)
              (map (partial map :w))
              (sort-by (partial reduce +))
              (first)))))

