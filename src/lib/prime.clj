(ns lib.prime
  (:require [lib.prime-data :as data]
            [lib.numb :as numb]))

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

(defn rand-mr [x1 x2]
  (+ x1 (rand-int (- x2 x1))))

(defn ^:private prime-mr-ds [n]
  (loop [s 0
         d (dec n)]
    (if (= 0 (mod d 2))
      (recur (inc s) (quot d 2))
      [d s])))

(defn ^:private loop-j-mr [x n s]
  (loop [j (dec s)
         y (mod (* x x) n)]
    (cond (= j 0) false
          (= y 1) false
          (= y (dec n)) nil
          :else (recur (dec j) (mod (* y y) n)))))

(def n2047 (list 2))
(def n1373653 (list 2 3))
(def n9090191 (list 31 73))
(def n25326001 (list 2 3 5))
(def n3215031751 (list 2 3 5 7))
(def n1122004669633 (list 2 13 23 1662803))
(def n2152302898747 (list 2 3 5 7 11))
(def n3474749660383 (list 2 3 5 7 11 13))
(def n341550071728321 (list 2 3 5 7 11 13 17))
(def n3825123056546413051 (list 2 3 5 7 11 13 17 19 23))

(defn ^:private mr-a [n]
  ;; (rand-mr 2 (dec n))
  (cond
    (< n 2047) n2047
    (< n 1373653) n1373653
    (< n 9090191) n9090191
    (< n 25326001) n25326001
    (< n 3215031751) n3215031751
    (< n 1122004669633) n1122004669633
    (< n 2152302898747) n2152302898747
    (< n 3474749660383) n3474749660383
    (< n 341550071728321) n341550071728321
    (< n 3825123056546413051) n3825123056546413051
    :else (throw (new Exception "n is too large."))))

(defn prime-mr? [n]
  (cond
    (< n 2) false
    (< n 4) true
    (= 0 (mod n 2)) false
    :else (let [[d s] (prime-mr-ds n)]
            (loop [ra (mr-a n)]
              (if (empty? ra)
                true
                (let [a (first ra)
                      x (numb/pow-mod n a d)]
                  (if (or (= x 1) (= x (dec n)))
                    (recur (rest ra))
                    (if (= nil (loop-j-mr x n s))
                      (recur (rest ra))
                      false))))))))
