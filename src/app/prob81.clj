(ns app.prob81
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.spec.alpha :as c]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as s]
            [clojure.pprint :as pp]
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

(def conj-if sq/conj-if)

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

(defn list-indexes [M]
  (->> (bfs-path M)
      (map #(map (fn [e] (str (:x e) (:y e))) %))
      (map #(interpose "," %))
      (map (partial reduce str))
      (sort)
      (pp/pprint)))

(defn is-square-matrix [m]
  (let [M (count m)]
    (->> m
        (map count)
        (every? (partial = M)))))

(c/def ::vertex (c/tuple number? number?))
(c/def ::v1 ::vertex)
(c/def ::v2 ::vertex)
(c/def ::w number?)
(c/def ::edge (c/keys :req-un [::v1 ::v2 ::w]))
(c/def ::edges (c/coll-of ::edge :kind vector?))
(c/def ::matrix-row (c/coll-of number? :kind vector?))
(c/def ::square-matrix (c/and
                        (c/coll-of ::matrix-row :kind vector?)
                        is-square-matrix))

(defn edges [m]
  (let [M (count m)]
    (letfn [(put [z e]
              (let [[x y] e
                    x1 (inc x)
                    y1 (inc y)]
                (conj-if
                 (conj-if z (when (< x1 M) {:v1 [y x] :v2 [y x1] :w (el m y x1)}))
                 (when (< y1 M) {:v1 [y x] :v2 [y1 x] :w (el m y1 x)}))))]
      (->> (for [x (range 0 M)
                 y (range 0 M)]
             [x y])
           (reduce put [])))))

(c/fdef edges
  :args (c/cat :m ::square-matrix))

(t/deftest edges-test
  (t/is (= 40 (count (edges M0)))))

(defmacro dist [d v]
  `(or (~d ~v) ~(Integer/MAX_VALUE)))

(defmacro inc-meta-ver [obj]
  `(let [m# (meta ~obj)]
     (with-meta ~obj {:ver (-> m# :ver inc)})))

(defn update-dist [dst edg]
  (let [{:keys [v1 v2 w]} edg
        dn (+ (dist dst v1) w)]
    (if (< dn (dist dst v2))
      (inc-meta-ver (assoc dst v2 dn))
      dst)))

(defn BellmanFord
  ([m edg] (BellmanFord m edg [0 0]))
  ([m edg v-start]
   (let [dst (assoc {} v-start (apply el m v-start))]
     (loop [z (with-meta dst {:ver 1})]
       (let [zn (reduce update-dist z edg)]
         (if (= (meta zn) (meta z))
           z
           (recur zn)))))))

(c/fdef BellmanFord
  :args (c/alt
         :2arg (c/cat :m ::square-matrix :edg ::edges)
         :3arg (c/cat :m ::square-matrix :edg ::edges :v-start ::vertex))
  :ret (c/map-of ::vertex number?))

(defn solve
  ([m] (solve m (edges m)))
  ([m edg]
   (c/assert ::square-matrix m)
   (c/assert ::edges edg)
   (let [M (dec (count m))]
     ((BellmanFord m edg) [M M]))))

(t/deftest solve-test
  (t/is (= 2427 (solve M0))))

(defn load-data [path]
  (let [data (slurp path)
        r    (s/split data #"\n")
        M (count r)]
    (into []
          (map #(into [] (map (fn [e] (read-string e)))
                      (s/split % #",")))
          r)))

(c/fdef load-data
  :args string?
  :ret ::square-matrix)

(t/deftest solve-prob81
  (t/is (= 427337
           (let [M1 (load-data "resources/p081_matrix.txt")]
             (solve M1)))))
