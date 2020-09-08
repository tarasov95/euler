(ns app.prob78
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=78
;; https://en.wikipedia.org/wiki/Partition_function_(number_theory)

(defn K [n]
  (Math/round
   (/ (dec (Math/sqrt (inc (* 24 n)))) 6)))

(defn p [n]
  (letfn [(sign [k v] (if (even? k) (- v) v))]
    (loop [x 1
           rg [1]]
      (if (> x n)
        (rg (dec x))
        (recur (inc x)
               (->>  (range (- (K x)) (inc (K x)))
                     (filter (comp not zero?))
                     (map (fn [k]
                            (let [a (- x (/ (* k (dec (* 3 k))) 2))]
                              (sign k (if (< a 0) 0 (rg a))))))
                     (reduce +)
                     (conj rg)))))))

(t/deftest p-test
  (t/is (->> [1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77, 101, 135, 176, 231, 297, 385, 490]
             (map-indexed (fn [ix e] (= (p ix) e)))
             (every? true?))))

(defn p-reduce
  "(fun [z n p(n)])"
  ([fun] (p-reduce fun nil))
  ([fun val]
   (letfn [(sign [k v] (if (even? k) (- v) v))]
     (loop [x 1N
            rg [1N]
            z (fun val 0 (rg 0))]
       (if (reduced? z)
         (deref z)
         (let [pn (transduce
                   (comp (filter (comp not zero?))
                         (map (fn [k]
                                (let [a (- x (/ (* k (dec (* 3 k))) 2))]
                                  (sign k (if (< a 0) 0 (rg a)))))))
                   + (range (- (K x)) (inc (K x))))
               rgn (conj rg pn)]
           (recur (inc x) rgn (fun z x (rgn x)))))))))

(t/deftest p-reduce-test
  (letfn [(p_ [N] (p-reduce (fn [z n pn] (when (>= n N) (reduced pn)))))]
    (t/is (->> [1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77, 101, 135, 176, 231, 297, 385, 490]
               (map-indexed (fn [ix e] (= (p_ ix) e)))
               (every? true?)))))

(t/deftest solve
  (t/is (= 55374
           (time (p-reduce
                  (fn [z n pn]
                    (when (zero? (mod pn 1000000))
                      (reduced n))))))))
