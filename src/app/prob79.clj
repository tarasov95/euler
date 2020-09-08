(ns app.prob79
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.string :as s]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=79

(defn load-data []
  (let [data (slurp "resources/p079_keylog.txt")]
    (s/split data #"\n")))

(defn group-by-pos [rg]
  (->> rg
      (map seq)
      (mapcat #(map-indexed (fn [i e] [e i]) %))
      (group-by first)
      (reduce-kv (fn [z k v] (assoc z k (into #{} (map second v)))) {})))

(defn tsort [z digs grph]
  (if (or (empty? digs)
          (empty? grph))
    (concat z digs)
    (let [d (->> digs
                 (filter #(not (some (fn [e] (= % (second e))) grph)))
                 (first))]
      (recur (conj z d)
             (disj digs d)
             (filter #(not= d (first %)) grph)))))

(defn solve []
  (let [rg (load-data)
        digs (->> rg
                  (mapcat seq)
                  (into #{}))
       grph (->> rg
                 (map seq)
                 (mapcat #(map vector % (drop 1 %)))
                 (distinct))]
   (reduce str (tsort [] digs grph))))

(t/deftest solve-test
  (t/is (= "73162890" (time (solve)))))
