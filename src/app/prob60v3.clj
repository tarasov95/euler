(ns app.prob60v3
  (:require [lib.prime :as prime]
            [lib.prime-data :as data]
            [lib.seq :as sq]
            [clojure.test :as t]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=60
;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
;; Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

;; https://projecteuler.net/overview=007
;; 1 is not a prime.All primes except 2 are odd.All primes greater than 3 can be written in the form  6k+/-1.Any number n can have only one primefactor greater than sqrt(n).The consequence for primality testing of a number n is: if we cannot find a numberf less than or equal sqrt(n) that divides n then n is prime: the only primefactor of n is n itself

(defn cat-left [x y]
  (read-string (str x y)))

(t/deftest cat-left-test
  (t/is (= 737 (cat-left 7 37)))
  (t/is (= 6737 (cat-left 673 7))))

(defn cat-right [x y]
  (cat-left y x))

(t/deftest cat-right-test
  (t/is (= 377 (cat-right 7 37)))
  (t/is (= 7673 (cat-right 673 7))))

(def check-prime (comp prime/is-prime? bigint))

(defn in-family? [rg p]
  (not (transduce
        (comp
         (filter #(not
                   (and (check-prime (cat-right % p))
                        (check-prime (cat-left % p)))))
         (take 1))
        sq/transduce-and rg)))

(t/deftest in-family?-test
  (t/is (not (in-family? [3 7] 17)))
  (t/is (in-family? [3 7] 109)))

(comment (defn prime-loop [S body]
   (loop [p S]
     (cond
       (and (check-prime p) (body p)) p
       (and (check-prime (+ 2 p)) (body (+ 2 p))) (+ 2 p)
       :else (recur (+ 6 p))))))

(comment (defn solve []
   (let [pr [3 7 109 673]
         ps (drop-while
             (partial >= (reduce max pr))
             prime-inf)]
     (->> ps
          (filter (partial in-family? pr))
          (take 1)))))

(comment (defn solve2 []
   (let [pr [3 7 109 673]]
     (prime-loop
      5N
      (fn [p]
        (in-family? pr p))))))

(defn join-if-family [p rg]
  (if (in-family? (second rg) p)
    [rg
     [(+ p (first rg)) (conj (second rg) p)]]
    [rg]))

(defn select-of-count [rg K]
  (->> rg
       (filter #(= K (count (second %))))))

(defn remove-greater [p N K rg]
  (->> rg
       (filter #(< (+ (* p (- K (count (second %)))) (first %))
                   N))))

(t/deftest remove-greater-test
  (t/is (= [[10 [3 7]]] (remove-greater 109 137 [[782 [109 673]] [10 [3 7]]]))))

(defn select-min [z candidates]
  (if (empty? candidates)
    z
    (let [y (if (next candidates)
             (reduce (partial min-key first) candidates)
             (first candidates))]
     (if (< (first y) (first z))
       y
       z))))

(defn solve-min
  ([K] (solve-min [(last data/prime-seed) []] ;; result ~> [sum [primes]]
                  [[3 [3]]] ;;current state ~> array of result
                  (drop 3 data/prime-seed)
                  K))
  ([z y primes K]
   (if (empty? primes)
     [nil z]
     (let [p (first primes)
           min-sum (first z)
           y0 (->> y (remove-greater p min-sum K))]
       (if (or (> p min-sum) (empty? y0))
         [p z]
         (let [y1 (->> y0
                       (mapcat (partial join-if-family p))
                       (group-by #(= K (count (second %)))))
               z1 (select-min z (y1 true))]
           (recur z1 (conj (or (y1 false) []) [p [p]]) (next primes) K)))))))

(t/deftest solve3-test
  (t/is (= [113N [119N [3N 7N 109N]]] (solve3 3))))

(defn solve-many
  ([K N] (solve-many [] ;; result ~> [sum [primes]]
               [[3 [3]]] ;;current state ~> array of result
               (drop 3 data/prime-seed)
               K N))
  ([z y primes K N]
   (if (empty? primes)
     [nil z]
     (let [p (first primes)]
       (if (= N (count z))
         [p
          (count y)
          (sort-by #(first %) z)]
         (let [y1 (->> y
                       (mapcat (partial join-if-family p))
                       (group-by #(= K (count (second %)))))
               z1 (into [] (concat z (y1 true)))
               y2 (conj (or (y1 false) []) [p [p]])]
           (recur z1 y2 (next primes) K N)))))))

(time (println (reduce + (solve-min 5))))
