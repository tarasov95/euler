(ns prob5)

(defn square [x] (* x x))

(def sum (partial reduce +))

;; (let [rg (range 1 11)]
;;   {:s (sum (map square rg))
;;    :q (square (sum rg))})


(defn prob6 [up]
  (let [rg (range 1 (inc up))]
   (sum (for [x rg y rg :when (not= x y)]
          (* x y)))))

{:test (prob6 10)
 :prob (prob6 100)}

