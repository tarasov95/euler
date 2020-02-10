(ns sicp.ch2)

(defn tuple [x y]
  (fn [ix]
    (if (= ix 0)
      x
      y)))

(defn car [p] (p 0))

(defn cdr [p] (p 1))

(defn tuple2 [x y]
  (fn [f] (f x y)))

(defn car2 [p]
  (p (fn [x y] x)))

(defn cdr2 [p]
  (p (fn [x y] y)))

(let [p2 (tuple2 7 8)
      p  (tuple 9 10)]
  [[(car p) (cdr p)]
   [(car2 p2) (cdr2 p2)]])

;; (cons 10 (list 1 2 3 4))
