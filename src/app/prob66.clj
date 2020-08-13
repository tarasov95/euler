(ns app.prob66
  (:require [clojure.test :as t]
            [app.prob64 :as p64]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=66
;; https://en.wikipedia.org/wiki/Pell%27s_equation

(defn tseq [tail]
  (concat tail (lazy-seq (tseq tail))))

(defn a-seq-inf [S]
  (let [h (p64/a-seq S)
        P (p64/period S)
        t (take-last P h)]
    (concat  h (tseq t))))

(defn next-conv [c0 c1 ar]
  (let [an (first ar)
        hn (+ (* an (c1 0)) (c0 0))
        kn (+ (* an (c1 1)) (c0 1))
        cn [(bigint hn) (bigint kn)]] ;;[numerator denominator]
    (cons cn (lazy-seq (next-conv c1 cn (rest ar))))))

(defn convergents [S]
  (let [ar (a-seq-inf S)
        [a0 a1] (take 2 ar)
        c0 [a0 1];; [numerator denominator]
        c1 [(+ (* a1 a0) 1) a1]]
    (concat [c0 c1]
            (lazy-seq (next-conv c0 c1 (drop 2 ar))))))

(defmacro sqr [x]
  `(* ~x ~x))

(defn solves-for [D c]
  (= 1
     (- (sqr (c 0))
        (* D (sqr (c 1))))))

(defn find-min-sol [D]
  (->> (convergents D)
       (filter (partial solves-for D))
       (first)))

(defn find-maxx [U]
  (->> (range 2 (inc U))
       (filter #(-> %  Math/sqrt numb/natural? not))
       (map #(conj (find-min-sol %) %)) ;;[x y D]
       (reduce (partial max-key first))))

(time (find-maxx 1000))
