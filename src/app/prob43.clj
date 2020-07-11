(ns app.prob43
  (:require
   [lib.numb :as numb]
   [clojure.string :as s]))

;; https://projecteuler.net/problem=43


(defn part-d3 [divisor]
  (->> (range 10 988)
       (filter #(not (and (< % 100) (= 0 (mod % 10)))))
       (filter #(= 0 (mod % divisor)))
       (filter numb/all-dig-diff)))

(defn part-d1 []
  (range 0 10))

(defn join [on select l r]
  (->> l
       (mapcat (fn [el]
                 (map (fn [er] (select el er))
                      (filter (on el) r))))))

(defn concat-d3 [el er]
  (+ er (* el 1000)))

(defn concat-d1 [el er]
  (+ er (* el 10)))

(defn filter-dig [el]
  (let [mask (numb/dig-mask el)]
    (fn [er]
      (not (numb/any-dig-in? mask er)))))

(defn filter-dig&div [div el]
  (let [fd (filter-dig el)
        dd (* 10 (mod el 100))]
    (fn [er]
      (and (fd er)
           (= 0 (mod (+ er dd) div))))))

(defn solve []
  (let [d1  (range 1 10)
        p2  (join filter-dig concat-d3 d1 (part-d3 2))
        p3  (join (partial filter-dig&div 3) concat-d1 p2 (part-d1))
        p5  (join (partial filter-dig&div 5) concat-d1 p3 (part-d1))
        p7  (join (partial filter-dig&div 7) concat-d1 p5 (part-d1))
        p11 (join (partial filter-dig&div 11) concat-d1 p7 (part-d1))
        p13 (join (partial filter-dig&div 13) concat-d1 p11 (part-d1))]
    (join (partial filter-dig&div 17) concat-d1 p13 (part-d1))))

(reduce + (solve))
;; (1406357289 1430952867 1460357289 4106357289 4130952867 4160357289)

;; (numb/num2dig 2 (numb/dig-mask 1024))
;; (numb/any-dig-in? (numb/dig-mask 1024) 3)
;; #(vector %1 %2)
;; (inner-join [1 2 ] [3 4])
