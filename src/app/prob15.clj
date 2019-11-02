(ns app.prob15
  (:require [clojure.pprint :as pp]))
;; https://projecteuler.net/problem=15

;; (defn print-path [mp n]
;;   (let [i (range 0 n)
;;         j (range 0 n)]
;;     ))

(def ^:dynamic ci 3)
(def ^:dynamic cj 4)

(defn visit [mp i j]
  (assoc mp (+ j (* i cj)) 1))

(defn render [mp]
  (map
   (fn [i] (map
            (fn [j] (or (mp (+ j (* i cj))) 0))
            (range 0 cj)))
   (range 0 ci)))

(defn print-path [m]
  (doseq [r m]
    (println r)))

(defn seek [mp i j r]
  (if (and (< i ci) (< j cj))
    (let [mn (visit mp i j)
          ui (dec ci)
          uj (dec cj)
          r0 (if (and (= i ui) (= j uj)) (conj r mn) r)
          r1 (seek mn (inc i) j r0)
          r2 (seek mn i (inc j) r1)]
      r2)
    r))

(def cnt
  (memoize
   (fn [m n]
     (cond
       (or (< m 2) (< n 2)) 1
       (and (= m 2) (= n 2)) 2
       :else (let [m1 (min m n)
                   n1 (max m n)]
               (+ (cnt (dec m1) n1)
                  (cnt m1 (dec n1))))))))

(defn prob15 [n]
  (let [n1 (inc n)]
    (cnt n1 n1)))

(defn solve [n m]
  (binding [ci n
            cj m]
    (let [rg (seek {} 0 0 [])]
      (doall (map-indexed
              (fn [i p]
                (println "------------" i "-----------------")
                (print-path (render p)))
              rg))
      (println (dec ci) "x" (dec cj) "=" (count rg)))))


(defn fac1 [x]
  (if (< x 2) 1
      (* x (fac1 (dec x)))))

(defn fac-str [x]
  (if (< x 2) "1"
      (str (fac-str (dec x)) "*" x)))

(defn fac2
  ([n] (fac2 n 1 1))
  ([n x f]
   (if (= n x) f
       (recur n (inc x) (* (inc x) f)))))

(defn fac2-str [n x f]
  (if (= n x) f
      (recur n (inc x) (str f "*" (inc x)))))

(defn binoc [m n]
  "https://en.wikipedia.org/wiki/Binomial_coefficient"
  (/ (fac2 m)
     (* (fac2 n) (fac2 (- m n)))))

(defn prob15-2 [n]
  ;; (binoc (+ n n) n))
  (/ (fac2 (+ n n) n 1)
     (fac2 n)))
