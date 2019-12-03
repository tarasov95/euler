(ns app.prob26b)

;; https://projecteuler.net/problem=26

(defn dec-fn [d]
  (fn [a]
    (let [c (first a)]
      (vector (* 10 (mod c d))
              (quot c d)))))

(defn print-f [f]
  (loop [ix 1
         r (f [10])]
    (print r)
    (when (< ix 30)
      (recur (inc ix) (f r)))))

(defn brent-lam
  ([f] (brent-lam f [10] (f [10]) 1 1))
  ([f t h po2 lam]
   "`f`unction `t`ortoise `h`are `p`ower_`o`f_`2` `lam`bda"
   (cond (= po2 lam) (brent-lam f h (f h) (* po2 2) 1)
         (= t h) lam
         :else (brent-lam f t (f h) po2 (inc lam)))))

(let [r (range 1 1000)]
  (apply max-key #(brent-lam (dec-fn %)) r))
