(ns lib.prime
  (:require [lib.prime-data :as data]))

(defn prime? [rg n]
  (and (>= n 2)
       (not (some
             #(zero? (mod n %))
             (take-while #(<= (* % %) n) rg)))))

(defn primes-below
  ([N] (primes-below data/prime-seed (+ 2 (last data/prime-seed)) N))
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
  ([] (primes-all prime-seed prime-seed))
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

(defn prime-fact
  ([n] (prime-fac *prime-feed* n))
  ([prime-feed n]
   (let [p (first prime-feed)]
     (if (<= n 1)
       []
       (let [fac (fac-pow n p)]
        (if (> (:pow fac) 0)
          (conj (prime-fac (rest prime-feed) (quot n (:ppow fac)))
                {:fac p :pow (:pow fac)})
          (prime-fac (rest prime-feed) n)))))))

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
