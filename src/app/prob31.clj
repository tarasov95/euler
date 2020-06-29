(ns app.prob31
  (:require [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=31

(defn range-of-coin-* [amount c]
  (range 0 (inc (quot amount c))))

(defn try-one-coin [amount c]
  (->> (range-of-coin-* amount c)
       (map (fn [m] {:$ c :* m :v (* c m)}))))

(defn change
  ([amount coins] (change [] [] amount coins))
  ([z y amount coins]
   (if (or (empty? coins) (= 0 amount))
     (if (= 0 amount) (conj z y) z)
     (let [c (first coins)]
       (->> (try-one-coin amount c)
            (mapcat (fn [e] (change z (conj y e) (- amount (:v e)) (rest coins))))
            (filter not-empty))))))

(count (change 100 [1 5 10 25 50]))

