(ns prob3)

(def N 13195)

(defn not-divider-of [x]
  (comp (partial not= 0) (partial mod x)))

(defn prime?
  "x:value to test; rg:array of primes to test against"
  [x rg]
  (every? (not-divider-of x) rg)) ;TODO: optimize to check only up to (Math/sqrt)

(defn conj-prime [r x]
  (if (prime? x r)
    (conj r x)
    r))

(defn list-primes [N]
  (reduce conj-prime [2 3 5 7 11] (filter (comp not even?) (range 13 N))))

(defn prime-pow "Number poWer Prime" [n w p]
  (if (or (<= n 1) (not= 0 (mod n p)))
    [n w p]
    (prime-pow (/ n p) (inc w) p)))

(defn conj-prime-pow "Result Element" [r e]
  (let [[n _ _] (last r)]
    (if (<= n 1)
      (reduced r)
      (conj r (prime-pow n 0 e)))))

(reduce
 conj-prime-pow
 [(prime-pow N 0 2)]
 (filter #(> % 2) (list-primes N)))
