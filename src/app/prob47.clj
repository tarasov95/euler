(ns app.prob47
  (:require
   [lib.prime :as prime]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=47

;; The first two consecutive numbers to have two distinct prime factors are:
;; 14 = 2 × 7
;; 15 = 3 × 5
;; The first three consecutive numbers to have three distinct prime factors are:
;; 644 = 2² × 7 × 23
;; 645 = 3 × 5 × 43
;; 646 = 2 × 17 × 19.
;; Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?

(defn candidates [cFactors]
  (->> (drop 6 (range))
       (filter #(= cFactors
                   (->> (prime/prime-fact %) (into #{}) (count))))))

(defmacro gen-conseq-eq [N]
  (let [arg (map #(symbol (str "v" %)) (range 1 (inc N)))]
    `(fn [~@arg]
       (= ~@(map-indexed (fn [i a] `(- ~a ~i)) arg)))))

(defn solve []
  (let [r1 (candidates 4)
        r2 (drop 1 r1)
        r3 (drop 2 r1)
        r4 (drop 3 r1)
        conseq= (gen-conseq-eq 4)]
    (->>
     (map vector r1 r2 r3 r4)
     (filter (partial apply conseq=))
     (take 1))))

(time (doall (solve)))

