(ns app.prob29
  (:require [lib.numb :as n]
            [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=30

(def pwN (memoize (fn [N x] (n/pow-int x N))))

(defn sum-pw [rg pw]
  (->> rg
       (r/map (partial pwN pw))
       (r/reduce + )))

(defn max-range [N]
  (sum-pw (repeat N 9) N))

(defn list-num [P]
  (let [U (inc (max-range P))]
    (r/filter (fn [e] (= e (sum-pw (n/num2dig 10 e) P)))
              (range 9 U))))

(reduce + (list-num 5))
