(ns app.prob61
  (:require [clojure.test :as t]
            [lib.polygonal :as pn]
            [lib.numb :as numb]))

;; The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.
;; The set is cyclic, in that the last two digits of each number is the first two digits of the next number (including the last number with the first).
;; Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and pentagonal (P5,44=2882), is represented by a different number in the set.
;; This is the only set of 4-digit numbers with this property.
;; Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and octagonal, is represented by a different number in the set.

;; (take-while #(< (second %) 1000) (map #(vector % (P5 %)) (range 1 1000)))

(defn p-range [pn fn-p fn-index]
  (map (fn [e] {:n (fn-p e) :p pn}) (range (inc (long (fn-index 1000))) (long (fn-index 10000)))))

(defn l2d [n]
  (mod n 100))

(defn f2d [n]
  (quot n 100))

(defn tail2head-pairs [r1 r2]
  (for [x1 r1
        x2 r2
        :when (= (l2d (:n x1)) (f2d (:n x2)))]
    [x1 x2]))

(defn tail2head-perm [r1 r2]
  (concat (tail2head-pairs r1 r2)
          (tail2head-pairs r2 r1)))

;; (defn all-t2h-pairs []
;;   (let [r3 (p-range pn/p3 pn/p3-index)
;;         r4 (p-range pn/p4 pn/p4-index)
;;         r5 (p-range pn/p5 pn/p5-index)]
;;     (concat
;;      (tail2head-perm r3 r4)
;;      (tail2head-perm r3 r5)
;;      (tail2head-perm r4 r5))))

(defmacro gen-t2h-pairs [S E]
  (let [ix (range S (inc E))
        v (into [] (map #(gensym (str "r" % "_")) ix))
        h (mapcat (fn [ix v] `(~v (p-range ~ix ~(symbol (str "pn/p" ix)) ~(symbol (str "pn/p" ix "-index"))))) ix v)
        b (for [l (range 0 (inc (- E S)))
                r (range 0 (inc (- E S)))
                :when (> r l)]
            `(tail2head-perm ~(v l) ~(v r)))]
    `(let [~@h]
       (concat ~@b))))

(defn map-of-edges [rp]
  (letfn [(array-of-adjacent-v [z v r]
            (assoc z v
                   (into [] (apply concat (map rest r)))))]
    (->> (group-by first rp)
         (reduce-kv array-of-adjacent-v {}))))

(defn find-loops-from-point
  ([moe max-loop-len from-v] (find-loops-from-point [] [] moe max-loop-len from-v))
  ([z y moe N v]
   (if (and (= v (first y)) (= N (count y)))
     (conj z (conj y v))
     (if (> (count y) N)
       z
       (let [v-next (moe v)
             y-next (conj y v)
             cnt-y (= N (count y-next))
             fst-v (first y-next)
             flt (fn [e] (or (not-any? #(= (:p %) (:p e)) y-next)
                             (and cnt-y (= fst-v e))))]
         (->> v-next
              (filter flt)
              (mapcat (partial find-loops-from-point z y-next moe N))))))))

(defn find-all-loops [moe max-loop-len]
  (->> (map first moe)
       (mapcat (partial find-loops-from-point moe max-loop-len))))


(defn solve []
  (->> (find-all-loops (map-of-edges (gen-t2h-pairs 3 8)) 6)
       (map butlast)
       (map (partial map :n))
       (map (partial reduce +))))

(time (println (solve)))
