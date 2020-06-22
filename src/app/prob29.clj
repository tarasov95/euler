(ns app.prob29
  (:require [lib.prime :refer :all]
            [clojure.set :as hs]
            [clojure.core.reducers :as r]))

;; https://projecteuler.net/problem=29

;; Consider all integer combinations of ab for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:

(defn log-xy
  "floor of pow <~> x^pow==y"
  [x y]
  (int (Math/floor (/ (Math/log y) (Math/log x)))))

(defn format-num [nums n]
  (if (get nums n)
    (format "%3d" (- 0 n))
    (format "%3d" n)))

(defn print-matrix
  ([m] (print-matrix m 1 #{}))
  ([m ix nums]
   (if (not-empty m)
     (let [r (first m)
           c (count (filter (partial get nums) r))]
       (println (str ix "~>" c) ) ;;(map #(format-num nums %) r)
       (print-matrix (rest m)
                     (inc ix)
                     (hs/union nums (set r)))))))

(defn no-of-repeats [N]
  (let [rg (range 2 (inc N))
        lg (log-xy 2 N)
        mul-pw (fn [e] (map #(* e %) rg))]
  ;; (println lg)
  (println "================================")
  (print-matrix
   (->>
    (range 1 (inc lg))
    (map mul-pw)))))

;; (no-of-repeats 5)
;; (no-of-repeats 10)
;; (no-of-repeats 16)
;; (no-of-repeats 32)
;; (no-of-repeats 100)
;; (no-of-repeats 200)

(defn list-num-fact [N]
  (->> (range (inc N))
       (map prime-fact)
       (filter not-empty)))

(defn pow-fac [pw fac]
  (map #(assoc % :pow (* pw (:pow %)))
       fac))

(let [N 100
      rg (list-num-fact N) ;;list of numbers in question as prime factorials
      pw (range 2 (inc N)) ;;range of the powers
      pow (fn [p] (map #(pow-fac p %) rg))] ;;rise factorized number to the power
  (count (set (mapcat #(pow %) pw))))

