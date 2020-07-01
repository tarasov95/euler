(ns app.prob32
  (:require [clojure.core.reducers :as r]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=32

(def code0 (int \0))

(defn num-to-set [num]
  (->> (seq (str num))
       (map (fn [ch] (- (int ch) code0)))
       (into #{})))

(defn list-products []
  (let [rg (range 1000 (inc (numb/pow-int 9 4)))]
    (->> rg
         (map (fn [n] {:num n :dig (num-to-set n)}))
         (filter (fn [nd] (and (not (get (:dig nd) 0))
                               (= (count (:dig nd)) 4)))))))

(time (count (list-products)))
