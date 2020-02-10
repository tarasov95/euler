(ns sicp.ch1)

(defn square [x] (* x x))

(defn pow [x n]
  (cond (= 1 n) x
        (odd? n) (* x (pow x (dec n)))
        :else (square (pow x (quot n 2)))))

(defn pow-iter
  ([x n] (pow-iter x n 1))
  ([x n z]
   (cond (<= n 1) (* x z)
         (even? n) (recur (* x x) (quot n 2) z)
         :else (recur x (dec n) (* x z)))))

[(pow 2 10)
 [ (pow 4 6) (pow-iter 4 6) ]
 [ (pow 2 11) (pow-iter 2 11) ]
 [ (pow 2 12) (pow-iter 2 12) ]
 [ (pow 3 13) (pow-iter 3 13) ]
 (pow-iter 2 8)
 (pow-iter 2 5)
 (pow-iter 2 6)
 (pow-iter 2 16)
 (pow-iter 2 3)
 (pow-iter 2 4)
 ]
