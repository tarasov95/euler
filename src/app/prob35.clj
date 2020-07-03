(ns app.prob35
  (:require [lib.prime :as prim]
            [lib.numb :as numb]
            [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=35
;; The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
;; How many circular primes are there below one million?

(defmacro gen-fast-log10 [n]
  (let [cnd (mapcat
             #(list (list '< n %) (/ % 10))
             (map #(numb/pow-int 10 %) (range 1 7)))]
    (concat (list `cond) cnd)))

(defn fast-log10 [n]
  (gen-fast-log10 n))

(defn rotate [n]
  (if (< n 10)
    n
    (let [lg (fast-log10 n)
          h  (quot n lg) ;;head
          m  (- n (* lg h))] ;;middle and tail
      (+ (* m 10) h))))

(defn all-rotations [n]
  (loop [z [n]
         r (rotate n)]
    (if (> (count z) 100)
      (throw (new Exception (str "all-rotations blew for " n)))
      (if (= r n)
        z
        (recur (conj z r) (rotate r))))))

(time
 (let [ps (into #{} (prim/primes-below 1000000))]
   (time
    (count
     (->> ps
          (filter #(not (clojure.string/index-of (str %) \0))) ;;numbers that have 0 are going to have a rotation ending with 0 ~> even ~> not prime
          (map all-rotations)
          (filter #(every? ps %)))))))

