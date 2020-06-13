(ns lib.prime
  (:require [lib.prime-data :refer :all]))

(defn prime? [rg n]
  (not (some #(zero? (mod n %)) rg)))


(defn primes-below
  ([N] (primes-below prime-seed (+ 2 (last prime-seed)) N))
  ([z ix N]
   (if (> ix N)
     z
     (if (prime? z ix)
       (recur (conj z ix) (+ ix 2) N)
       (recur z (+ ix 2) N)))))

(defn find-next-prime
  ([z] (find-next-prime z (last z)))
  ([z p]
   (loop [x (+ 2 p)]
     (if (prime? z x)
       x
       (recur (+ 2 x))))))

(defn primes-all
  ([] (primes-all prime-seed prime-seed))
  ([z-full z-rest]
   (lazy-seq
    (let [p  (find-next-prime z-full)]
      (cons
       (first z-rest)
       (primes-all (conj z-full p) (conj (subvec z-rest 1) p)))))))

