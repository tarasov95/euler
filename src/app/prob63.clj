(ns app.prob63
  (:require [clojure.core.reducers :as r]
            [clojure.test :as t]
            [lib.numb :as numb]))

;;10^N ~> N+1 digits
;;9^N ~> N digits, but only until N gets sufficiently large ~> N-k digits
;; for every base below 9 it's the same as for 9 only stops faster
;; for ever base above then it's N+k and grows with the base
(defn count-dig-pw [base pw]
  (count (numb/num2dig 10 (numb/pow-int (bigint base) pw))))

(defn count-pw [base]
  (->> (drop 1 (range))
       (map #(vector % (count-dig-pw base %) (numb/pow-int (bigint base) %)))
       (take-while #(<= (first %) (second %)))
       (filter #(= (first %) (second %)))))

(let [base (range 1 10)]
  (count (->> base
              (mapcat count-pw))))
