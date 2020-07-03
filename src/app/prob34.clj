(ns app.prob34
  (:require [lib.numb :as numb]
            [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=34

;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
;; Find the sum of all numbers which are equal to the sum of the factorial of their digits.

;; => ([0 1] [1 1] [2 2] [3 6] [4 24] [5 120] [6 720] [7 5040] [8 40320] [9 362880])
;; if n contains 5 ~> n >= 120
;; if n contains 7 ~> n >= 5040
;; etc.
;; if n contains 9 ~> n >= 362880


;; (def mem-fact (memoize numb/fact))

(def map-fact
  ;; (into {} (map #(vector % (numb/fact %)) (range 0 10) ))
  (into [] (map numb/fact (range 0 10) )))

(defmacro map-to-case [e]
  (let [clauses (mapcat #(vector (char (+ % (int \0)))
                                 (numb/fact %))
                        (range 0 10))]
    (concat (list `case e) clauses)))

(defn case-fact [n]
  (map-to-case n))

(defn case-fact2 [ch]
  (case ch 0 1 1 1 2 2 3 6 4 24 5 120 6 720 7 5040 8 40320 9 362880))

(defn sum-fact [n]
  (->> (-> n str seq)
       ;; (r/map #(- (int %) (int \0)))
       (r/map case-fact)
       (r/reduce +)))

(defn sum-fact2 [n]
  (loop [q n
         z 0]
    (cond
      (= 0 q) z
      (> z n) 0
      :else (recur (quot q 10) (+ z (case-fact2 (mod q 10)))))))

(defn get-max-range [n]
  (if (> n (sum-fact n))
    n
    (recur (+ 9 (* n 10)))))

(defn candidates []
  (range 10 (sum-fact (get-max-range 9))))

(defn solve [sum-fact]
  (->> (vec (candidates))
       (r/filter #(= % (sum-fact %)))
       (r/fold +)))

(time (println "sum-fact" (solve sum-fact)))
(time (println "sun-fact2" (solve sum-fact2)))



