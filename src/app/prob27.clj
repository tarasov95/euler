(ns app.prob27
  (:require [lib.prime :refer :all]
            [clojure.core.reducers :as r]))

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

(defn pmapcat [fun coll]
  (apply concat (pmap fun coll)))

(defn list-of-ab []
  (pmapcat
   (fn [b] (map #(vector % b)
                (list-of-a b)))
   (list-of-b)))

(defn solve-seq []
  (let [ab     (list-of-ab)
        maxq   (fn [vec] (apply max-quad-n vec))
        max-ab (reduce (partial max-key maxq) ab)]
    (println "[a,b]=" max-ab)
    (println "max-n=", (maxq max-ab))
    (println "rez=" (apply * max-ab))))

(defn r-list-of-ab []
  (r/mapcat
   (fn [b] (r/map #(vector % b)
                  (vec (list-of-a b))))
   (vec (list-of-b))))

(def r-max (r/monoid max (constantly Long/MIN_VALUE)))

(defn max-quad-n-vec [ab-vec]
  (apply max-quad-n ab-vec))

(defn r-solve []
  (->> (r-list-of-ab)
       (r/map max-quad-n-vec)
       (r/fold r-max)))

;; (time (r-solve))
