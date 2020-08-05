(ns app.prob60
  (:require [lib.prime-data :as data]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=60
;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
;; Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

;; https://projecteuler.net/overview=007
;; 1 is not a prime.All primes except 2 are odd.All primes greater than 3 can be written in the form  6k+/-1.Any number n can have only one primefactor greater than sqrt(n).The consequence for primality testing of a number n is: if we cannot find a numberf less than or equal sqrt(n) that divides n then n is prime: the only primefactor of n is n itself

(def pw10 (memoize (partial numb/pow-int 10)))
(def lg10 (memoize (partial numb/log-int 10)))

(defn prime? [prev n]
  (let [last-prime (prev (dec (count prev)))]
    (if (< n last-prime)
      (-> prev meta :pset n)
      (let [q (long (Math/sqrt n))]
        (->> (take-while (partial >= q) prev)
             (every? #(not= 0 (mod n %))))))))

(defn update-pset [new old n]
  (with-meta new {:pset (conj (-> old meta :pset) n)}))

(defn find-next-prime [prev]
  ;; (when (not (counted? prev))
  ;;   (throw (new Exception "prev must be counted")))
  (let [cnt (count prev)
        last-prime (prev (dec cnt))]
    (when (= 0 (mod cnt 100000))
      (println cnt last-prime))
    (loop [step 2
           n (+ step last-prime)]
      (when (> (- n last-prime) 1000) (throw (new Exception (str "find-next-prime stuck at " n " started from " last-prime))))
      (if (prime? prev n)
        (update-pset
         (cons n (lazy-seq (find-next-prime (update-pset (conj prev n) prev n))))
         prev n)
        (recur step (+ step n))))))

;; (def prime-seed [2 3 5 7 11])

(def ^:private prime-inf
  (let [seed data/prime-seed
        pset {:pset (into #{} seed)}]
    (with-meta
      (concat seed (find-next-prime (with-meta seed pset)))
      pset)))

(defn cat-left [x y]
  (+ y
     (* x (pw10 (inc (lg10 y))))))

(defn cat-right [x y]
  (cat-left y x))

(defn in-family? [rg p]
  (->> rg
       (mapcat #(vector (cat-left % p) (cat-right % p)))
       (every? check-prime)))

(defn solve []
  (let [pr [3 7 109 673]
        ps (drop-while
            (partial >= (reduce max pr))
            prime-inf)]
    (->> ps
         (filter (partial in-family? pr))
         (take 1))))

;; (solve)

;; (find-next-prime prime-seed (last prime-seed))
;; (prime? prime-inf 15)
;; (->> (map #(vector (= %1 %2) %1 %2)
;;       (take 1200 prime-inf)
;;       (take 1200 data/prime-seed))
;;      (filter #(not (first %))))
