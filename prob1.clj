(ns prob1)

(def >0 (partial < 0))

(defn f5 [x]
  (and (>0 x)
       (not= (mod x 3) 0)))

(defn src-range [n]
  (concat
   (filter >0 (range 0 n 3))
   (filter f5 (range 0 n 5))))

(reduce + (src-range 1000))


