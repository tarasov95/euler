(ns app.prob49
  (:require
   [lib.prime :as prime]
   [lib.seq :as sq]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=49
;; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
;; What 12-digit number do you form by concatenating the three terms in this sequence?

(defn candidates []
  (->> (prime/primes-below 10000)
       (drop-while (partial > 1000))
       (group-by numb/dig-mask)
       (map second)))

(defn map-diff [rg]
  (letfn [(join-tail [i e] (->> (drop (inc i) rg)
                                (map #(vector e %))))]
    (->> rg
         (map-indexed join-tail) ;;all permutations of the items to calc the diff
         (reduce concat) ;;flatten the seq
         (group-by #(Math/abs (- (first %) (second %)))) ;; group-by the diff
         (map second);;leave onlty the grouped lists
         (filter #(= (count %) 2));; those that have 2 diffs
         (filter #(= (-> % first second) (-> % second first)));;to be a series last el of the 1st diff must be the same as the 1st el of the 2nd diff
         )))

(defn solve []
  (let [y (->> (candidates)
               (mapcat map-diff)
               (filter (comp not empty?)))]
    [y
     (->> y
          (map (partial apply concat))
          (map distinct)
          (map (partial apply str)))]))

(time (solve))
