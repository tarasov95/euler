(ns app.prob90
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.string :as st]
            [clojure.set :as se]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=90

;; In fact, by carefully choosing the digits on both cubes it is possible to display all of the square numbers below one-hundred: 01, 04, 09, 16, 25, 36, 49, 64, and 81.

(def squares
  [1, 4, 9, 16, 25, 36, 49, 64, 81])

(def edges
  (->> squares
       (map #(vector (quot % 10) (mod % 10)))))

(defn r96 [rg]
  (->> rg
       (map #(map (fn [e] (if (= 9 e) 6 e)) %))))

(def colors #{:r :b :g})
;; (defn valid-coloring? [edges coloring]
;;   (->> edges
;;        (every? (fn [e] (not= (or (coloring (first e)) -1)
;;                              (or (coloring (second e)) -2))))))

(defn vertices [edges]
  (->> (concat (map first edges)
               (map second edges))
       (distinct)))

(defn gen-colorings [vertices]
  (let [p (first vertices)]
    (if (empty? (rest vertices))
      (map (fn [c] [[p c]]) colors)
      (->>
       (map (fn [c] [p c]) colors)
       (mapcat (fn [pc]
                 (sq/conj-each
                  (gen-colorings (rest vertices))
                  pc)))
       (map (partial into {}))))))

(defn sets-from-coloring [c]
  (let [groups (->> c
                    (group-by second)
                    (map (fn [g] (map first (second g))))
                    (map (partial into (sorted-set))))]
    (when (= 3 (count groups))
      (let [[c1 c2 c3] groups]
        (with-meta
          (->> [[(into c1 c3) (into c2 c3)]
                [(into c1 c2) (into c3 c2)]
                [(into c2 c1) (into c3 c1)]]
              ;; (map #(sort-by (fn [e] (str e)) %))
               (filter #(and (< (count (first %)) 7)
                             (< (count (second %)) 7))))
          c)))))

(defn valid-sets? [edges s]
  (letfn [(ch [s d]
            (if (or (= 6 d) (= 9 d))
              (or (s 6) (s 9))
              (s d)))]
    (let [[s1 s2] s]
     (every? (fn [e]
               (let [[e1 e2] e]
                 (or (and (ch s1 e1) (ch s2 e2))
                     (and (ch s2 e1) (ch s1 e2))))) edges))))

(def full-set (into #{} (range 0 10)))

(defn ccomb [n k]
  (/ (numb/fact n) (* (numb/fact k) (numb/fact (- n k)))))

(t/deftest ccomb-test
  (t/is (= 2598960 (ccomb 52N 5))))

(defn comb [s k]
  (letfn [(comb-impl [s k]
            (if (>= 1 k)
              (map sorted-set s)
              (->> s
                   (mapcat #(sq/conj-each (comb-impl s (dec k)) %)))))]
    (->> (comb-impl s k)
         (filter #(= k (count %)))
         (distinct))))

(defn join-comb [s rg-comb]
  (if (empty? rg-comb)
    (vector s)
    (sq/each concat s rg-comb)))

(defn gen-padded-sets [tuple]
  (let [[s1 s2] (->> tuple
                     (map (fn [s]
                            (let [diff (se/difference full-set s)
                                  n (count diff)
                                  k (- 6 (count s))]
                              {:s s :n n :k k :comb (comb diff k)})))
                     (map (fn [e]
                            (join-comb (:s e) (:comb e))))
                     ;; (map #(into (sorted-set) %))
                     )]
    (for [x s1
          y s2
          :let [sx (into (sorted-set) x)
                sy (into (sorted-set) y)]]
      (if (>= (compare (str sy)(str sx)) 0)
        [sx sy]
        [sy sx]))))

(defn gen-9sets [tuple]
  (letfn [(set9 [s] (if (s 6) (disj (conj s 9) 6) nil))]
    (let [[s1 s2] tuple
          s19 (set9 s1)
          s29 (set9 s2)]
      (sq/conj-if
       []
       [s1 s2]
       (when (not-empty s19) [s19 s2])
       (when (not-empty s29) [s1 s29])
       (when (and (not-empty s19) (not-empty s29)) [s19 s29])))))

(defn solve []
  (let [e96 (r96 edges)
       vrt (vertices e96)]
   (->> (gen-colorings vrt)
        (mapcat sets-from-coloring)
        (filter (partial valid-sets? e96))
        (mapcat gen-9sets)
        (mapcat gen-padded-sets)
        (distinct)
        (count)
        )))

(t/deftest solve-test
  (t/is (= 1217 (solve))))
