(ns app.prob23
  (:require [clojure.core.reducers :as r]))

(defn divisors
  "n|umber c|urrent s|tep re|z|ult"
  ([n] (divisors n
                 (if (even? n) 2 3)
                 (if (even? n) 1 2)
                 (list)))
  ([n c s z]
   (if (> (* c c) n)
     (conj z 1)
     (letfn [(conj-if-diff [r n1 n2]
               (if (not= n1 n2) (conj r n1 n2) (conj r n1)))]
       (recur n
              (+ c s)
              s
              (if (= 0 (mod n c))
                (conj-if-diff  z c (/ n c))
                z))))))

(defn d
  "proper divisors of n"
  [n] (reduce + (divisors n)))

(defn is-abundant [n]
  (> (d n) n))

(defn join-full [r1 r2]
  (letfn [(each-r2 [e] (map #(vector e %) r2))]
    (map each-r2 r1)))

(defn join-self-half
  ([fJ r] (join-self-half fJ r []))
  ([fJ r z]
   (if (empty? r)
     z
     (let [f (first r)]
       (recur fJ
              (rest r)
              (concat z (map #(fJ f %) r)))))))

(defn sum-pair? [ra ma n]
  (some identity (map #(get ma (- n %))
                      (take-while #(> (- n %) 11) ra))))

(let [r  (range 1 (inc 28123))
      ra (filter is-abundant r)
      ma (into #{} ra)
      nsp? (comp not (partial sum-pair? ra ma))]
  [
   (reduce + (filter nsp? r))
   ])
