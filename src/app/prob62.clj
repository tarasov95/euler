(ns app.prob62
  (:require [clojure.core.reducers :as r]
            [clojure.test :as t]
            [lib.numb :as numb]))


;; The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

;; Find the smallest cube for which exactly five permutations of its digits are cube.


(defn dig-perm [fn-sort n]
  (numb/dig2num 10 (sort-by fn-sort (numb/num2dig 10 n))))

(def min-perm (partial dig-perm +))
(def max-perm (partial dig-perm -))

(defn cbrt [n]
  (Math/round (Math/pow n 0.3333333333333333)))

(defn cube [n]
  (* n n n))

(defn list-cube-perms [n]
  (let [minp (min-perm n)
        maxp (max-perm n)]
    (into []
          (comp
           (map cube)
           (filter #(= maxp (max-perm %))))
          (range (cbrt minp) (inc (cbrt maxp))))))


(defn solve [N]
  (into []
        (comp
         (map cube)
         (map list-cube-perms)
         (filter #(= N (count %)))
         (take 1))
        (drop 5 (range))))

(time (solve 5))
