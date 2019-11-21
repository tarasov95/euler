(ns app.prob25)

;; https://projecteuler.net/problem=25

(defn fib-raw [n]
  (cond (< n 2) n
        :else (+' (fib (- n 1)) (fib (- n 2)))))

(def fib (memoize fib-raw))

(loop [n1 1 n2 1 ix 1]
  (if (>= (-> n1 str count) 1000)
    (println ix "=>" n1)
    (recur n2 (+' n1 n2) (inc ix))))
