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

(defn test-tuple []
  (let [p2 (tuple2 7 8)
       p  (tuple 9 10)]
   [[(car p) (cdr p)]
    [(car2 p2) (cdr2 p2)]]))

;; (cons 10 (list 1 2 3 4))
(defn subsets [l]
  (if (or (empty? l) (nil? l))
    (list '())
    (let [r (subsets (rest l))]
      (concat r (map #(conj % (first l)) r)))))

(defn test-subsets []
  (subsets (list 1 2 3)))

;; (define (accumulate op initial sequence)
;;   (if (null? sequence)
;;     initial
;;     (op (car sequence)
;;         (accumulate op 
;;                     initial 
;;                     (cdr sequence)))))

(defn accumulate [op initial sequence]
  (if (empty? sequence)
    initial
    (op (first sequence)
        (accumulate op initial (rest sequence)))))

(defn horner-eval [x a]
  (letfn [(step [e z]
            (+ e (* z x)))]
    (accumulate step 0 a)))

;; (horner-eval 2 (list 1 3 0 5 0 1))
