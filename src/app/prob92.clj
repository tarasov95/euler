(ns app.prob92
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.core.reducers :as r]
            [lib.numb :as numb]
            [clojure.spec.test.alpha :as stest]))

;; https://projecteuler.net/problem=92

(defn fold-num [fun start n]
  (loop [nn n
         z start]
    (if (= 0 nn)
      z
      (recur
       (quot nn 10)
       (fun z (mod nn 10))))))


(defn gen-seq
  ([n] (gen-seq [n] n))
  ([z n]
   (let [nn (fold-num #(+ %1 (* %2 %2)) 0 n)]
     (if (#{1 89} nn)
       (conj z nn)
       (recur (conj z nn) nn)))))

(def ends89?
  (fn [start]
    (if (#{1 89} start)
      (= 89 start)
      (recur (fold-num #(+ %1 (* %2 %2)) 0 start)))))

(defn solve []
  (->> (range 2 (long 1e7))
       (r/filter ends89?)
       (r/map (constantly 1))
       (r/fold +)))

(time (solve))
