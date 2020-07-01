(ns lib.numb)

(defn pow-int
  "x to the power of n; n must be integer"
  ([x n] (pow-int x n 1))
  ([x n z]
   (cond (<= n 1) (* x z)
         (even? n) (recur (* x x) (quot n 2) z)
         :else (recur x (dec n) (* x z)))))

(defn num2dig
  "returns array of digits of n in numberal system with base B"
  [B n]
  (if (= n 0)
    []
    (conj
     (num2dig B (quot n B))
     (mod n B))))