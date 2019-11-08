(ns app.prob19)
;; https://projecteuler.net/problem=19

(defn is-leap [y]
  (and
   (= 0 (mod y 4))
   (or (not= 0 (mod y 100))
       (= 0 (mod y 400)))))

;;;;;;;;;; Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
(def mlen [31  28  31  30  31  30  31  31  30  31  30  31])
(def mlen-leap (map #(if (= % 28) 29 %) mlen))

(def yrs (range 1901 2001))

(def clen (mapcat (fn [y] (if (is-leap y) mlen-leap mlen)) yrs))

;; (map (fn [l] (apply + l)) clen)

;; January 1, 1901 (Tuesday) => 1
;; January 6, 1901 (Sunday) => 1 + 7 - 2
;; to start from 6th Jan subtract 5 from the 1st month length
;; (map is-leap [1900 2000 1980])

(defn sunc [r e]
  (let [s (:s r)
        c (:c r)
        s0 (+ s e)]
    {:s s0
     :c (if (= 0 (mod s0 7)) (inc c) c)}))

(let [l (map-indexed
         (fn [i e] (if (= i 0) (- e 5) e))
         clen)]
  (reduce sunc {:s 0 :c 0} l))
