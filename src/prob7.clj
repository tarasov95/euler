(ns prob7
  (:require
   [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; (defn prime? [rg n]
;;   (let [b (p :sqrt-in-prime? (Math/sqrt n))]
;;     (p :not-any-in-prime? (not-any? #(p :mod-test-in-prime? (zero? (mod n %))) (p :take-while-in-prime? (take-while (partial < b) rg))))))

;; (defn find-next-prime [l rg]
;;   (loop [p (+ 2 l)]
;;     (if (tufte/p :prime?-in-find (prime? p rg))
;;       p
;;       (recur (+ 2 p)))))

(defn prime? [rg n]
  (not (some #(zero? (mod n %)) rg)))

(defn find-next-prime [n rg]
  (first (filter (fn [e] (prime? (take-while #(<= (* % %) e) rg) e)) (iterate inc (inc n)))))

;; (take-while #(<= (* % %) 4) '(2 3 5))
;; (prime? '(2) 4)
(find-next-prime 3 '(2 3 5))
(find-next-prime 2 '(2))

(defn prime-seq
  ([] (prime-seq [2] 2 1))
  ([rg l ix]
   (lazy-seq
    (let [n (find-next-prime l rg)]
      ;; (println rg)
      (when (zero? (mod ix 100)) (println "l= " l " realized #" ix "=" n))
      (p :cons1 (cons n (prime-seq (p :conj2 (conj rg n)) n (inc ix))))))))

(tufte/add-basic-println-handler! {})
(profile {} (last (take 10001 (prime-seq))))

;; (defn sieve [s]
;;   (cons (first s)
;;         (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
;;                                  (rest s))))))


;; (last (take 10000 (sieve (iterate inc 2))))


;; (def prime-numbers
;;   ((fn f [x]
;;      (cons x
;;            (lazy-seq
;;             (f (first
;;                 (drop-while
;;                  (fn [n]
;;                    (some #(zero? (mod n %))
;;                          (take-while #(<= (* % %) n) prime-numbers)))
;;                  (iterate inc (inc x))))))))
;;    2))

;; (last (take 10000 prime-numbers))

;; {:l (cons 2 '(1))
;;  :a (cons 2 [1])}
