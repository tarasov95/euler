(ns lib.numb)

(defn pow-int
  "x to the power of n; n must be an integer"
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

(defn gcd
  "greates common divider"
  [n1 n2]
  (cond
    (< n1 0)  1
    (= n1 n2) n1
    (> n1 n2) (recur (- n1 n2) n2)
    :else (recur (- n2 n1) n1)))

(defn fact
  "factorial of the argument"
  ([n] (if (= n 0)
         1
         (fact 1 n)))
  ([z n]
   (if (= 1 n)
     z
     (recur (* z n) (dec n)))))

(defn log-int
  "floor of pow <~> base^pow==order of n"
  [base n]
  (int (Math/floor (/ (Math/log n) (Math/log base)))))

(defmacro gen-lgpw10
  "generates a cond to return (pow-int 10 (log-int 10 arg)) supports up to max-pow of 10"
  [max-pow arg]
  (let [cnd (mapcat
             (fn [pw] (list `(< ~arg ~pw) (/ pw 10)))
             (map #(pow-int 10 %) (range 1 (inc max-pow))))]
    `(cond ~@cnd)))
