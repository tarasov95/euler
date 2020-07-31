(ns app.prob59
  (:require [clojure.string :as s]))

;; https://projecteuler.net/problem=59
;; https://crypto.stackexchange.com/questions/333/how-does-the-index-of-coincidence-work-in-the-kasiki-test
;; https://en.wikipedia.org/wiki/Index_of_coincidence
;; https://idafchev.github.io/crypto/2017/04/13/crypto_part1.html

(def letter-freq-base
  {
  \a 	8.497
  \b 	1.492
  \c 	2.202
  \d 	4.253
  \e 	11.162
  \f 	2.228
  \g 	2.015
  \h 	6.094
  \i 	7.546
  \j 	0.153
  \k 	1.292
  \l 	4.025
  \m 	2.406
  \n 	6.749
  \o 	7.507
  \p 	1.929
  \q 	0.095
  \r 	7.587
  \s 	6.327
  \t 	9.356
  \u 	2.758
  \v 	0.978
  \w 	2.560
  \x 	0.150
  \y 	1.994
  \z 	0.077 })

(defn letter-freq []
  (->> letter-freq-base
       (map #(vector (int (first %))  (second %)))
       (into {})))

(defn load-data []
  (let [data (slurp "resources/p059_cipher.txt")
        rg    (s/split data #",")]
    (into [] (map read-string) rg)))

(def ^:private data-promise (promise))
(defn data []
  (deliver data-promise (load-data))
  @data-promise)

(defn split-msg [rg key-len]
  (let [cch (count rg)]
    (->> (range 0 key-len)
         (map #(range % cch key-len))
         (map (partial map rg))
         (map (partial into [])))))

(defn ioc [freqs text-len]
  (let [denom (* text-len (dec text-len))]
     (reduce + (map #(/ (* % (dec %)) denom) freqs))))

(defn ioc-for-keylen [data key-len]
  (let [col (split-msg data key-len)]
    (let [fq (->> col
                  (map frequencies)
                  (map (partial map (fn [k] (float (second k))))))
          cnt (map count col)
          ioc (map ioc fq cnt)]
      (/ (reduce + ioc) key-len))))

(defn guess-ley-len [ciph]
  (let [iocs (->> (range 1 1000)
                 (map (partial ioc-for-keylen ciph)))]
   (->> (map-indexed vector iocs)
        (filter #(> (second %) 0.06))
        (first)
        (first)
        (inc))))

(defn xor-col [col x]
  (map (partial bit-xor x) col))

(defn eval-col-freqs [lfq col]
  (->> (frequencies col)
       (map #(* (or (lfq (first %)) 0) (second %)))
       (reduce +)))

(defn guess-xor-for-col [col]
  (let [lfq (letter-freq)]
    (->> (range (int \a) (inc (int \z)))
         (map (partial xor-col col))
         (map (partial eval-col-freqs lfq))
         (map-indexed vector)
         (reduce (partial max-key second)))))

(defn guess-key [ciph]
  (let [key-len (guess-ley-len ciph)
       cols (split-msg ciph key-len)]
   (->> (map guess-xor-for-col cols)
        (map #(+ (int \a) (first %)))
        (into []))))

(defn apply-ckey
  ""
  [ckey ix ch]
  (bit-xor ch (ckey (mod ix (count ckey)))))

(defn decrypt [ciph]
  (let [ckey (guess-key ciph)]
   (->> ciph
        (map-indexed (partial apply-ckey ckey))
        )))

(defn solve []
  (->> (decrypt (data))
       (reduce +)))

(defn reveal []
  (->> (decrypt (data))
       (map char)
       (reduce str)))

(time (solve))

