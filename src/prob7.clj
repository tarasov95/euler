(ns prob7
  (:require
   [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

(defn prime? [rg n]
  (not (some #(zero? (mod n %)) rg)))

(defn find-next-prime [n rg]
  (first (filter (fn [e] (prime? (take-while #(<= (* % %) e) rg) e)) (iterate inc (inc n)))))

(defn prime-seq
  ([] (prime-seq [2] 2 1))
  ([rg l ix]
   (lazy-seq
    (let [n (find-next-prime l rg)]
      ;; (println rg)
      ;; (when (zero? (mod ix 100)) (println "l= " l " realized #" ix "=" n))
      (p :cons1 (cons n (prime-seq (p :conj2 (conj rg n)) n (inc ix))))))))

(tufte/add-basic-println-handler! {})
(profile {} (last (take 10000 (prime-seq))))

;; https://clojuredocs.org/clojure.core/lazy-seq
;; (defn sieve [s]
;;   (cons (first s)
;;         (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
;;                                  (rest s))))))

;; https://www.google.com/url?q=https%3A%2F%2Fgist.github.com%2Fkohyama%2F8e599b2e765ad4256f32&sa=D&sntz=1&usg=AFQjCNEFpYFTyEo6PbVtAkCWSmE8k8muvw;; (def prime-numbers
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
