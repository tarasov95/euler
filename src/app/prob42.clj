(ns app.prob42
  (:require
   [clojure.string :as s]))

;; https://projecteuler.net/problem=42

(defn str2num [s]
  (->> (seq s)
       (map #(- (inc (int %)) (int \A)))
       (reduce +)))

(defn trian? [n]
  (let [q (int (Math/floor (Math/sqrt (* 2 n))))]
    (= (* 2 n)
       (* q (inc q)))))

(defn solve []
  (let [data (slurp "resources/p042_words.txt")]
    (->> (s/split data #",")
         (map #(s/replace % "\"" ""))
         (map str2num)
         (filter trian?)
         (count))))

(time (solve))
