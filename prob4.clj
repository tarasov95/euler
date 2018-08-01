(ns prob4)

(defn is-palin
  ([n m]
   (let [d (mod n 10)]
     (cond (= n m) true
           (and (= m 0) (= 0 d)) false
           (> m n) false
           :else (recur (/ (- n d) 10) (+ (* 10 m) d)))))
  ([n] (is-palin n 0)))

(defn gen-prod [c]
  (let [up (long (Math/pow 10 c))
        rg (range (/ up 10) up)]
    (for [x rg y rg
          :while (<= y x)]
      (* x y))))

(map
 #(reduce max (filter is-palin (gen-prod %)))
 [2 3])
