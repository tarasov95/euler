(ns app.prob56
  (:require
   [clojure.test :as t]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=56

;; A googol (10^100) is a massive number: one followed by one-hundred zeros; 100^100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.

;; Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?

(defn gen []
  (for [a (range 2N 100)
        b (range 2N 100)]
    (with-meta
      (numb/num2dig 10 (numb/pow-int a b))
      {:a a :b b})))

(defn solve []
  (let [z (->> (gen)
               (reduce (partial max-key (fn [dig] (reduce + dig)))))]
    (binding [*print-meta* true]
      (println "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      (pr z)
      (println "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      (pr (reduce + z)))))


(time (solve))
