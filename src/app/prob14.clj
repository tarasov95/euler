(ns app.prob14)
;; https://projecteuler.net/problem=14

(defn next [n]
  (if (even? n)
    (/ n 2)
    (inc (* 3 n))))


(defn collatz [n]
  (take-while #(not= % 1) (iterate next n)))

(def cnt (comp count collatz))
;; (inc (count (collatz 13)))
;; (map (comp count collatz) [12 13 27])
;; (let [ix (range 1 5)]
;;   (apply (partial max-key cnt) ix))

;; (let [ix (range 1 11)]
;;   (map (fn [n] {:n n :rg (collatz n)}) ix))

(def countz
  (memoize
   (fn [n]
     (if (= 1 n)
       1
       (if (even? n)
         (inc (countz (/ n 2)))
         (inc (countz (inc (* 3 n)))))))))

;; (let [ix (range 1 11)]
;;   (map (fn [n] {:n n :rg (collatz n) :cnt (countz n)} ) ix))

(let [ix (range 1 1000000)]
  (apply (partial max-key countz) ix))
