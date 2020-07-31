(ns app.prob59
  (:require [clojure.string :as s]))

;; https://projecteuler.net/problem=59


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


(guess-ley-len (data))
