(ns app.prob93
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [lib.seq :as seq]
            [clojure.spec.test.alpha :as stest]))

;; https://projecteuler.net/problem=93

(defn ccomb [n k]
  (/ (numb/fact n) (* (numb/fact k) (numb/fact (- n k)))))

(def op [+ * - /])

(defn gen-ops []
  (for [o1 op
        o2 op
        o3 op]
    (fn [x1 x2 x3 x4]
      (o3
       (o2
        (o1 x1 x2) x3) x4))))

(defn apply-ops [ops arg]
  (->> ops
       (map #(apply % arg))
       (map long)
       (filter (partial < 0))
       (filter numb/natural?)))

(defn gen-ints [digs]
  (let [ops (gen-ops)
        args (seq/permut digs)]
    (->> args
         (mapcat (partial apply-ops ops))
         (distinct)
         (sort))))

(defn select-conseq [rg]
  (->> rg
       (map vector (drop 1 (range)))
       (take-while (partial apply =))))

(select-conseq (gen-ints [1 2 3 4]))
