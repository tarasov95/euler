(ns prob3)

(def N 13195)

(defn not-divider-of [x]
  (comp (partial not= 0) (partial mod x)))

(defn prime? [x rg]
  (every? (not-divider-of x) rg))

(defn conj-prime [r x]
  (if (prime? x r)
    (conj r x)
    r))

(defn list-primes [N]
  (reduce conj-prime [2 3 5 7 11] (filter (comp not even?) (range 13 N))))

(list-primes (Math/sqrt N))

