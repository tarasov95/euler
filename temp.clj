(ns temp)

(defn fib-seq
  "Returns a lazy sequence of Fibonacci numbers"
  ([]
   (fib-seq 0 1))
  ([a b]
   (lazy-seq
    (cons b (fib-seq b (+ a b))))))

(defn prime? [n rg]
  (not-any? #(= 0 (mod n %)) rg))

(defn find-next-prime [rg]
  (loop [p (+ 2 (last rg))]
    (if (prime? p rg)
      p
      (recur (+ 2 p)))))

(defn prime-seq
  ([] (let [s [2 3 5 7 11 13 17 19]]
        (concat s (prime-seq s))))
  ([rg]
   (lazy-seq
    (let [n (find-next-prime rg)]
      (cons n (prime-seq (cons n rg)))))))

;; (take 15 (prime-seq))
