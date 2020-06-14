(ns app.prob27
  (:require [lib.prime :refer :all]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; https://projecteuler.net/problem=27


(def primes4all (primes-all))
(def is-prime? (partial prime? primes4all))

(defn list-of-b []
  (take-while #(<= % 1000) primes4all))

(defn list-of-a
  ([b] (concat
        (list-of-a find-prev-prime b b '())
        (list-of-a find-next-prime b b '())))
  ([find b p z]
   (if-let [p (find primes4all p)]
     (let [a (dec (- p b))]
       (if (< (Math/abs a) 1000)
         (recur find b p (conj z a))
         z))
     z)))

(defn quad [a b]
  (fn [n] (+ (+ (* n n) (* a n)) b)))

(defn max-quad-n [a b]
  (let [qd (quad a b)]
    (loop [n 0]
      (if (is-prime? (qd n))
        (recur (inc n))
        n))))

(let [ab     (mapcat (fn [b] (map #(vector % b) (list-of-a b)))
                     (list-of-b))
      maxq   (fn [vec] (apply max-quad-n vec))
      max-ab (reduce (partial max-key maxq) ab)]
  (println "[a,b]=" max-ab)
  (println "rez=" (apply * max-ab)))
