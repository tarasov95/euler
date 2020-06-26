(ns app.prob30
  (:require [lib.numb :as n]
            [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=30

(def pwN (memoize (fn [N x] (n/pow-int x N))))

(defn sum-pw [rg pw]
  (->> rg
       (r/map (partial pwN pw))
       (r/reduce + )))

(defn max-range [N]
  (sum-pw (repeat (inc N) 9) N)) ;; 10^4 ~> 5 digits, hence the inc

(defn list-num [P]
  (let [U (inc (max-range P))]
    (r/filter (fn [e] (= e (sum-pw (n/num2dig 10 e) P)))
              (vec (range 9 U))))) ;;vec is to support r/fold

(time (r/fold + (list-num 5)))
