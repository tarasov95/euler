(ns app.prob39
  (:require [lib.numb :as numb]))

;; https://projecteuler.net/problem=39

;; If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
;; {20,48,52}, {24,45,51}, {30,40,50}
;; For which value of p ≤ 1000, is the number of solutions maximised?

(defn triple?
  ([t] (apply triple? t))
  ([a b c]
   (= (* c c) (+ (* a a) (* b b)))))

(defn perim
  ([t] (apply perim t))
  ([a b c]
   (+ a b c)))

(defn triple [n m k]
  (let [m2 (* m m)
        n2 (* n n)
        a (* k (- m2 n2))
        b (* k (* 2 m n))
        c (* k (+ m2 n2))]
    (with-meta [(min a b) (max a b) c] {:nmk [n m k]})))

;;TODO: try to get rid of the duplicates by checking for m-n oddity and them being coprime (i.e. gcd = 1)
(defn gen-triples
  ([P] (gen-triples P []))
  ([P z] (gen-triples P z 1 2 1))
  ([P z n m k]
   (let [t (triple n m k)
         p (perim t)]
     ;; (println n m k t p)
     (if (<= p P)
       (recur P (conj z t) n m (inc k))
       (cond
         (> k 1) (recur P z n (inc m) 1)
         (> m (+ n 1)) (recur P z (inc n) (+ 2 n) 1)
         :else z)))))

(defn solve []
  (->> (gen-triples 1000 #{})
       (group-by perim)
       (map #(vector (first %) (count (second %))))
       ;; (sort-by second)
       ;; (take-last 1)
       (reduce (partial max-key second))))

(time (solve))
