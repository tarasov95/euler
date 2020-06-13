(ns app.prob27
  (:require [prob7 :as p7]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; https://projecteuler.net/problem=27

;; (defn gen-primes []
;;   (let [fl (io/file "resources/prime10000.edn")]
;;    (if (.exists fl)
;;      (with-open [r (io/reader fl)]
;;        (edn/read (java.io.PushbackReader. r)))
;;      (with-open [w (io/writer fl)]
;;        (binding [*out* w]
;;          (let [q (take 100 (p3/prime-seq))]
;;            (prn q)
;;            q))))))

;; (let [primes (cons 1 (take-while (fn [e] (< e 20)) (p7/prime-seq)))]
;;   (concat (map - primes) primes))

;; (take 200 (p7/prime-seq))

(defn prime? [rg n]
  (not (some #(zero? (mod n %)) rg)))

(def prime-seed [2 3 5 7 11 13 19])

(defn primes-below
  ([N] (sieve prime-seed (+ 2 (last prime-seed)) N))
  ([z ix N]
   (if (> ix N)
     z
     (if (prime? z ix)
       (recur (conj z ix) (+ ix 2) N)
       (recur z (+ ix 2) N)))))

(defn find-next-prime
  ([z] (find-next-prime z (last z)))
  ([z p]
   (loop [x (+ 2 p)]
     (if (prime? z x)
       x
       (recur (+ 2 x))))))

(defn primes-all
  ([] (primes prime-seed prime-seed))
  ([z-full z-rest]
   (lazy-seq
    (let [p  (find-next-prime z-full)]
      (cons
       (first z-rest)
       (primes (conj z-full p) (conj (subvec z-rest 1) p)))))))

(take 100 (primes-all))
;; (find-next-prime prime-seed)
;; (sieve 100)
;; (prime? [2 3 5 7 11 13 19] 21)
