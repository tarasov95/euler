(ns prob2)

(defn fib-next [n r c]
  (let [s (+ (last r) (-> r butlast last))]
    (if (> s n)
      (reduced r)
      (conj r s))))

(defn fib[n]
  (reduce (partial fib-next n) [1 2] (range)))

(reduce + (filter even? (fib 4000000)))
