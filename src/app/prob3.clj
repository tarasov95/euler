(ns app.prob3
  (:require [taoensso.tufte :as tufte :refer (defnp p profiled profile)]))

;; (defn prime? [n rg]
;;   (not-any? #(= 0 (mod n %)) rg))

(defn prime? [n rg]
  (let [b (p :sqrt-in-prime? (Math/sqrt n))]
    (p :not-any-in-prime? (not-any? #(p :mod-test-in-prime? (zero? (mod n %))) (p :take-while-in-prime? (take-while (partial < b) rg))))))

(defn find-next-prime [l rg]
  (loop [p (+ 2 l)]
    (if (tufte/p :prime?-in-find (prime? p rg))
      p
      (recur (+ 2 p)))))

(defn prime-seq
  ([] (prime-seq nil 2 1))
  ([rg l ix]
   (lazy-seq
    (let [n (p :find-next-prime-in-lazy-seq (find-next-prime l rg))]
      ;; (println "realized #" ix "=" n)
      (p :cons1 (cons n (prime-seq (p :cons2 (cons n rg)) n (inc ix))))))))

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

(def list-of-dividers (cons 2 (rest (filter odd? (range)))))

(def N1 13195)
(def N2 (long 600851475143))

;; (list-prime-factors  list-of-dividers N2)


(tufte/add-basic-println-handler! {})
(profile {} (map (partial list-prime-factors list-of-dividers) [N1, N2]))
