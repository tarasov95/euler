(ns app.prob22
  (:require [clojure.string :as s]
            [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=22

(def nA (dec (byte \A)))

(defn to-codes [s]
  (map #(- (byte %) nA)
       (filter #(not= % \") s)))

(defn list-merge
  ([l1 l2] (list-merge < l1 l2 (vector)))
  ([f< l1 l2] (list-merge f< l1 l2 (vector)))
  ([f< l1 l2 z]
   (cond
     (empty? l1) (concat z l2)
     (empty? l2) (concat z l1)
     :else       (let [f1 (first l1)
                       f2 (first l2)]
                   (if (f< f1 f2)
                     (recur f< (rest l1) l2 (conj z f1))
                     (recur f< l1 (rest l2) (conj z f2)))))))


(defn list-sort
  ([l] (list-sort < l (vector)))
  ([f< l] (list-sort f< l (vector)))
  ([f l z]
   (if (empty? l)
     z
     (recur f
            (drop 1 l)
            (list-merge f (take 1 l) z)))))


(defn list< [l1 l2]
  (cond
    (empty? l1) true
    (empty? l2) false
    :else       (let [f1 (first l1)
                      f2 (first l2)]
                  (cond
                    (< f1 f2) true
                    (> f1 f2) false
                    :else     (recur (rest l1) (rest l2))))))

(defn solve []
  (let [data (slurp "resources/p022_names.txt")
        r    (s/split data #",")
        ch   (map to-codes r)
        rs   (list-sort list< ch)]
    [
     (reduce + (map-indexed
                (fn [i e] (* (inc i) (reduce + e)))
                rs))
     ]))


(time (solve))
;; [(list-merge [6 10 11 12 16] [11 12 13])
