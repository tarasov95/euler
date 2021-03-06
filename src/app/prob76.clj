(ns app.prob76
  (:require [clojure.test :as t]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=76

(def count-sums
  (memoize
   (fn [N c]
     (if (or (= 1 N) (= 1 c))
       1
       (let [cn (dec c)]
         (+ (count-sums N cn)
            (->> (range c (inc N) c)
                 (map #(count-sums (- N %) cn))
                 (reduce +))))))))

(t/deftest count-sums-test
  (t/is (= 6 (count-sums 5 4))))

(t/deftest solve
  (t/is (= 190569291 (time (count-sums 100 99)))))

(def count-sums2
  "https://projecteuler.net/overview=031"
  (memoize
   (fn [N c]
     (cond
       (or (= 1 N) (= 1 c)) 1
       (and (> c 1) (< N c)) (count-sums2 N (dec c))
       (and (> c 1) (>= N c)) (+ (count-sums2 N (dec c))
                                 (count-sums2 (- N c) c))))))

(t/deftest solve2
  (t/is (= 190569291 (time (count-sums2 100 99)))))
