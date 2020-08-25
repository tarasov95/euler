(ns app.prob73
  (:require [clojure.test :as t]
            [lib.prime :as prime]
            [app.prob72 :as p72]
            [clojure.core.reducers :as r]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=73
;; https://en.wikipedia.org/wiki/Farey_sequence


(defn norm [n]
  (long (p72/norm-recur n)))

(defn rat [x]
  (clojure.lang.Numbers/toRatio x))

(defn med [x y]
  (/ (+ (numerator x) (numerator y))
     (+ (denominator x) (denominator y))))

(t/deftest med-test
  (t/is (= 1/2 (med (rat 0/1) (rat 1/1)))))

(defn next-F
  ([n rg] (next-F [] n rg))
  ([z n rg]
   (let [rgn (rest rg)]
     (if (empty? rgn)
       (conj z (first rg))
       (let [x (first rg)
             y (first rgn)]
         (if (<= (+ (denominator x) (denominator y)) n)
           (recur (conj z x (med x y)) n rgn)
           (recur (conj z x) n rgn)))))))

(defn iter-F [prev]
  (let [n  (first prev)
        nn (inc n)
        rg (second prev)]
    [nn (next-F nn rg)]))

(defn F [n]
  (->> (iterate iter-F [1 [(rat 0/1) (rat 1/1)]])
       (drop (dec n))
       (first)
       (second)))

(t/deftest F-test
  (t/is (= (butlast (drop 1 (F 8)))
           [1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8])))

;; How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d ≤ 12,000?
(defn F32 [N]
  (->> (iterate iter-F [1 [(rat 0/1) (rat 1/1)]])
       (take N)
       (map second)
       (map #(drop-while (partial >= 1/3) %))
       (map #(take-while (partial > 1/2) %))))

(t/deftest F32-test
  (t/is (= (last (F32 8))
           [3/8, 2/5, 3/7])))

(comment (defn reduce-F [n fun val]
   (loop [p1 (rat 0/1)
          p2 (rat (/ 1 n))
          z (fun val p2)]
     (let [a (numerator p1)
           b (denominator p1)
           c (numerator p2)
           d (denominator p2)
           pn (/ (long (- (* (Math/floor (/ (+ n b) d)) c) a))
                 (long (- (* (Math/floor (/ (+ n b) d)) d) b)))]
       (if (< pn 1)
         (let [zn (fun z pn)]
           (if (reduced? zn)
             z
             (recur p2 pn zn)))
         z)))))

(defn reduce-F [n fun val]
  (loop [a 0
         b 1
         c 1
         d n
         z (fun val (/ c d))]
    (let [p (long (- (* (Math/floor (/ (+ n b) d)) c) a))
          q (long (- (* (Math/floor (/ (+ n b) d)) d) b))]
      (if (= p q)
        z
        (let [zn (fun z (/ p q))]
          (if (reduced? zn)
            z
            (recur c d p q zn)))))))

(t/deftest reduce-F-test
  (t/is (= [1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8]
         (reduce-F 8 conj []))))

(defn count23 [z e]
  (cond
    (>= e 1/2) (reduced z)
    (> e 1/3)  (inc z)
    :else      z))

(t/deftest count23-test
  (t/is (= (count [3/8, 2/5, 3/7]) (reduce-F 8 count23 0))))

(time (reduce-F 12000 count23 0))
