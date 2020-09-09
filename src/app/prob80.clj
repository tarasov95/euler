(ns app.prob80
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.string :as s]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=80

;; x(n+1) = x(n) - y(n)/y'(n)
;; x*x = y ~> sqrt(y) = x

(def ^:dynamic P 102)
(def ^:private EPS (read-string (str "1e-" P "M")))

(defn abs [x]
  (if (< x 0) (- x) x))

(defn sqrtn [y]
  (loop [x 1M
         xp x]
    (let [xn (/ (+ y (* x x)) 2 x)]
      (if (< (abs (- xp xn)) EPS)
        (if (< (abs xn) EPS) 0 xn)
        (recur xn x)))))

(defn to-dig [N n]
  (->> (seq (str n))
       (map #(- (int %) (int \0)))
       (filter #(and (>= % 0) (<= % 9)))
       (take N)))

(t/deftest to-dig100-test
  (t/is (= [1 2 3 4] (to-dig 4 1.234567M))))

(t/deftest sqrtn-test
  (with-precision P
    (t/is (= 475 (reduce + (to-dig 100 (sqrtn 2M)))))))


(defn list-squares-below [N]
  (->> (range 1 (inc (numb/lsqrt N)))
       (map #(* % %))))

(t/deftest list-squares-below-test
  (t/is (= [1 4 9] (list-squares-below 10))))

(defn solve []
  (with-precision P
    (let [excl (into #{} (list-squares-below 100))]
      (->> (range 1 101)
           (filter #(not (excl %)))
           (map #(vector % (with-precision P (sqrtn (bigdec %))))) ;;keep the source data for debugging purposes
           (map #(conj % (reduce + (to-dig 100 (second %)))))
           (map last)
           (reduce +)))))

(t/deftest solve-test
  (t/is (= 40886 (solve))))
