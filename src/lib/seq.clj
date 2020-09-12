(ns lib.seq
  (:require [clojure.test :as t]))

(defn each [fun e col]
  (map #(fun % e)
       col))

(defn conj-each [col e]
  (map #(conj % e)
       col))

(defn if-conj [coll el]
  (if (nil? el)
    coll
    (conj coll el)))

(defn conj-if [coll & xs]
  (reduce conj coll (filter (comp not nil?) xs)))

(defn permut [rg]
  (if (empty? (rest rg))
    (list rg)
    (->> rg
         (mapcat (fn [e]
                   (conj-each
                    (permut (filter #(not= % e) rg))
                    e))))))

(defn palindrome? [rg]
  (let [cnt (count rg)]
    (case cnt
      (0 1) true
      (2 3) (= (first rg) (last rg))
      (and (= (first rg) (last rg))
           (recur (subvec rg 1 (dec cnt)))))))

(defn transduce-and
  ([] nil)
  ([b] b)
  ([b1 b2]
   (cond (and (= nil b1) (= nil b2)) true
         (= nil b1) b2
         (= nil b2) b1
         :else (and b1 b2))))

(defn match-len
  ([rg1 rg2] (match-len 0 rg1 rg2))
  ([z rg1 rg2]
   (cond
     (or (empty? rg1) (empty? rg2)) z
     (= (first rg1) (first rg2)) (recur (inc z) (rest rg1) (rest rg2))
     :else z)))

(t/deftest match-len-test
  (t/is (= 2 (match-len [1 2 3 4] [1 2 5 7 8]))))

