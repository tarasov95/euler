(ns app.prob31
  (:require [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=31

(defn range-of-coin-* [amount c]
  (range 0 (inc (quot amount c))))

(defn try-one-coin [amount c]
  (->> (range-of-coin-* amount c)
       (map (fn [m] {:$ c :* m :v (* c m)}))))

(defn join [l r]
  (->> l
       (map (fn [el] (map (fn [er] (conj el er)) r)))))

(defn change-intl [z amount coins]
  (if (or (empty? coins) (= 0 amount))
    (if (= 0 amount) z nil)
    (let [c (first coins)]
      (->> (try-one-coin amount c)
           (map
            (fn [e] (change-intl (conj z e) (- amount (:v e)) (rest coins)) ))
           (filter not-empty)))))

(defn change [z amount coins]
  (if (or (empty? coins) (= 0 amount))
    (if (= 0 amount) z nil)
    (let [c (first coins)]
      (->> (try-one-coin amount c)
           (mapcat
            (fn [e] (change-intl [e] (- amount (:v e)) (rest coins)) ))
           (filter not-empty)))))

(change (vector) 100 [25 15 50])
;; (join [[1 2] [3 4]] [])

