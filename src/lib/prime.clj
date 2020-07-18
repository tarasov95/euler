(ns lib.prime
  (:require [lib.prime-data :as data]))

(defn prime? [rg n]
  (and (>= n 2)
       (not (some
             #(zero? (mod n %))
             (take-while #(<= (* % %) n) rg)))))

(defn primes-below
  ([N] (let [last-known (last data/prime-seed)]
         (if (<= N last-known)
           (take-while #(<= % N) data/prime-seed)
           (primes-below data/prime-seed (+ 2 last-known) N))))
  ([z ix N]
   (if (> ix N)
     z
     (if (prime? z ix)
       (recur (conj z ix) (+ ix 2) N)
       (recur z (+ ix 2) N)))))

(defn find-prime
  ([dir z] (find-prime dir z (last z)))
  ([dir z p]
   (cond
     (= p 3) (if (= dir -) 2 5)
     (= p 2) (if (= dir -) nil 3)
     (= p 1) (if (= dir -) nil 2)
     :else (loop [x (dir p 2)]
             (if (prime? z x)
               x
               (recur (dir x 2)))))))

(defn find-next-prime
  ([z] (find-prime + z (last z)))
  ([z p] (find-prime + z p)))

(defn find-prev-prime
  ([z] (find-prime - z (last z)))
  ([z p] (find-prime - z p)))

(defn primes-all
  ([] (primes-all data/prime-seed data/prime-seed))
  ([z-full z-rest]
   (lazy-seq
    (if (empty? z-rest)
      (let [p (find-next-prime z-full)]
        (cons p
              (primes-all (conj z-full p) nil)))
      (cons (first z-rest)
            (primes-all z-full (subvec z-rest 1)))))))

(def ^:dynamic *prime-feed* (primes-all))
(def is-prime? (partial prime? *prime-feed*))

(defn fac-pow
  "caclulates power of the prime factor p in the number n"
  [n p]
  (loop [pow 0 ;;power of the factor
         ppow 1 ;;factor to the power
         cur n]
    (if (= 0 (mod cur p))
      (recur (inc pow) (* ppow p) (quot cur p))
      {:pow pow :ppow ppow})))

(defn prime-fact1
  ;;TODO: refactor to use a simple (guess-next-factor) instead of the *prime-feed*
  ([n] (prime-fact1 *prime-feed* n))
  ([prime-feed n]
   (let [p (first prime-feed)]
     (if (<= n 1)
       []
       (let [fac (fac-pow n p)]
         (if (> (:pow fac) 0)
           (conj (prime-fact1 (rest prime-feed) (quot n (:ppow fac)))
                 {:fac p :pow (:pow fac)})
           (prime-fact1 (rest prime-feed) n)))))))

(defn prime-fact
  ([n] (prime-fact [] 2 n))
  ([z fact-guess n]
   (cond
     (<= n 1) z
     (> (* fact-guess fact-guess) n) (conj z {:fac n :pow 1}) ;;it's a prime
     :else
     (let [next-guess (if (< fact-guess 3) 3 (+ fact-guess 2))
           fac (fac-pow n fact-guess)]
       (if (> (:pow fac) 0)
         (recur (conj z {:fac fact-guess :pow (:pow fac)})
                next-guess
                (quot n (:ppow fac)))
         (recur z next-guess n))))))

;; test
;; (->> (range 1 10000)
;;      (map (fn [e] (= (sort-by #(:fac %) (prime-fact1 e))
;;                      (sort-by #(:fac %) (prime-fact e)))))
;;      (filter not))
