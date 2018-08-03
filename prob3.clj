(ns prob3
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

(defn prime? [n rg]
  (not-any? #(= 0 (mod n %)) rg))

;; (defn prime? [n rg]
;;      (let [b (Math/sqrt n)]
;;             (not-any? #(= 0 (mod n %)) (take-while (partial < b) rg))))

(defn find-next-prime [rg]
  (loop [p (+ 2 (last rg))]
    (if (prime? p rg)
      p
      (recur (+ 2 p)))))

(defn prime-seq
  ([] (let [s [2 3 5 7 11 13 17 19]]
        (concat s (prime-seq s))))
  ([rg]
   (lazy-seq
    (let [n (find-next-prime rg)]
      (println "realized " n)
      (cons n (prime-seq (cons n rg)))))))

;; (defn conj-prime [r x]
;;   (if (p :conj-prime-if (prime? x r))
;;     (p :conj-prime-conj (conj r x))
;;     r))

;; (defn list-primes [N]
;;   (reduce conj-prime [2 3 5 7 11] (filter (comp not even?) (range 13 N))))

(defn prime-pow "Number poWer Prime" [n w p]
  (if (or (<= n 1) (not= 0 (mod n p)))
    [n w p]
    (recur (/ n p) (inc w) p)))

(defn conj-prime-pow "Result Element" [r e]
  (let [[n _ _] (last r)]
    (if (<= n 1)
      (reduced r)
      (conj r (p :conj-prime-pow (prime-pow n 0 e))))))

(defn list-prime-factors
  ([N] (list-prime-factors (prime-seq) N))
  ([rg N]
   (filter
    #(not= 0 (second %))
    (reduce
     conj-prime-pow
     [(prime-pow N 0 2)]
     (rest rg)))))

(def N1 13195)
(def N2 (long 600851475143))

;; (list-prime-factors N2)

(tufte/add-basic-println-handler! {})
(profile {} (map (partial list-prime-factors (prime-seq)) [N1, N2]))
