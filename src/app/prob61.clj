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
(defn p-range [fn-p fn-index]
  (map fn-p (range (inc (long (fn-index 1000))) (long (fn-index 10000)))))

(defn l2d [n]
  (mod n 100))

(defn f2d [n]
  (quot n 100))

(defn tail2head-pairs [r1 r2]
  (for [x1 r1
        x2 r2
        :when (= (l2d x1) (f2d x2))]
    [x1 x2]))

(defn tail2head-perm [r1 r2]
  (concat (tail2head-pairs r1 r2)
          (tail2head-pairs r2 r1)))

(defn all-t2h-pairs []
  (let [r3 (p-range pn/p3 pn/p3-index)
        r4 (p-range pn/p4 pn/p4-index)
        r5 (p-range pn/p5 pn/p5-index)]
    (concat
     (tail2head-perm r3 r4)
     (tail2head-perm r3 r5)
     (tail2head-perm r4 r5))))

(defn map-of-edges
  ([] (map-of-edges (all-t2h-pairs)))
  ([rp]
   (letfn [(array-of-adjacent-v [z v r]
             (assoc z v
                    (into [] (apply concat (map rest r)))))]
     (->> (group-by first rp)
          (reduce-kv array-of-adjacent-v {})))))

;; (let [e (map-of-edges)]
;;   ;; (take 10 e)
;;   (map e [ 8128, 2882, 8281 ])
;;   )

(defn find-loops
  ([moe max-loop-len from-v] (find-loops [] [] moe max-loop-len from-v))
  ([z y moe N v]
   (if (= v (first y))
     (conj z (conj y v))
     (if (> (count y) N)
       z
       (let [v-next (moe v)
             y-next (conj y v)]
         (->> v-next
              (mapcat (partial find-loops z y-next moe N))))))))

(let [moe (map-of-edges)]
  (->> (map first moe)
       (mapcat (partial find-loops moe 4))
       (filter #(= 4 (count %)))
       ;; (reduce concat)
       ))
