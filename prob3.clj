(ns prob3
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

(defn prime? [n rg]
  (let [b (Math/sqrt n)]
    (not-any? #(= 0 (mod n %)) (take-while (partial < b) rg))))

(defn conj-prime [r x]
  (if (p :conj-prime-if (prime? x r))
    (p :conj-prime-conj (conj r x))
    r))

(defn list-primes [N]
  (reduce conj-prime [2 3 5 7 11] (filter (comp not even?) (range 13 N))))

(defn prime-pow "Number poWer Prime" [n w p]
  (if (or (<= n 1) (not= 0 (mod n p)))
    [n w p]
    (recur (/ n p) (inc w) p)))

(defn conj-prime-pow "Result Element" [r e]
  (let [[n _ _] (last r)]
    (if (<= n 1)
      (reduced r)
      (conj r (p :conj-prime-pow (prime-pow n 0 e))))))

(defn list-prime-factors [N]
  (filter
   #(not= 0 (second %))
   (reduce
    conj-prime-pow
    [(prime-pow N 0 2)]
    (rest (list-primes N)))))

(def N1 13195)
(def N2 (long 600851475143))

(tufte/add-basic-println-handler! {})
(profile {} (map list-prime-factors [N1, N2]))
