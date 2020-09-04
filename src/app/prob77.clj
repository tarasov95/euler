(ns app.prob77
  (:require [clojure.test :as t]
            [lib.prime :as prime]
            [lib.seq :as sq]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=77

;; It is possible to write ten as the sum of primes in exactly five different ways:
;; What is the first value which can be written as the sum of primes in over five thousand different ways?

;; (def count-sums
;;   (fn [N c]
;;     (if (or (= 1 N) (= 1 c))
;;       1
;;       (let [cn (dec c)]
;;         (+ (count-sums N cn)
;;            (->> (range c (inc N) c)
;;                 (map #(count-sums (- N %) cn))
;;                 (reduce +)))))))

(defn change-one [n p]
  (if (and (not= 0 n) (zero? (mod n p)))
    (do
      ;; (println "change-one" n p)
      (repeat (quot n p) p))
    nil))

(t/deftest change-one-test
  (t/is (= [2 2 2 2] (change-one 8 2)))
  (t/is (= nil (change-one 8 3))))

(defn prime-ways
  ([n] (prime-ways [] n (prime/primes-below (- n 2))))
  ([z n rp]
   ;; (println z n rp)
   (cond
     (< n 2) z
     (empty? (rest rp)) (sq/if-conj z (change-one n (first rp)))
     :else (let [fp (first rp)
                 rpn (rest rp)]
             (into []
                   (->> (range 0 (quot n fp))
                        (mapcat #(sq/each concat
                                          (repeat % fp)
                                          (prime-ways [] (- n (* % fp)) rpn)))
                        (concat (prime-ways [] n [fp]))
                        (concat z)))))))

(t/deftest prime-ways-test
  (t/is (= 5 (count (prime-ways 10)))))

(defn pways-count
  ([n] (pways-count n (prime/primes-below (- n 2))))
  ([n rp]
   (let [fp (first rp)
         rpn (rest rp)]
     (cond
       (= 0 (quot n fp)) 0
       (>= fp n) (if (zero? (mod n fp)) 1 0)
       (empty? rpn) (if (zero? (mod n fp)) 1 0)
       :else (+ (pways-count n [fp])
                (->> (range 0 (quot n fp))
                     (map #(pways-count (- n (* % fp)) rpn))
                     (reduce +)))))))

(t/deftest pways-count-test
  (t/is (= 5 (pways-count 10))))
