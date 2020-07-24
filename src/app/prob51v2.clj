(ns app.prob51v2
  (:require
   [lib.prime :as prime]
   [lib.seq :as sq]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=51

;; By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

;; By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

;; Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

(defn for-mask [N]
  (range 1 (dec (numb/pow-int 2 N))))

(defn get-dig-pw [N mask]
  (->> (range 0 N)
       (map #(if (bit-test mask %) 1 0))
       (reduce +)))

(defn for-dig
  "simulate (for [d1 (range 0 10) ... dn (range 0 10)] [d1...dn]) with numbers
  when (bit-test mask 'eldest')=>1 (i.e. use a-dig there) generate a range without the leading 0
  else generate a range with one extra digit on the left hand side to have 0 in the digit next to it"
  [N mask]
  (let [pw10 (numb/pow-int 10 (get-dig-pw N mask))]
    (if (bit-test mask (dec N))
      (range (quot pw10 10) pw10)
      (range pw10 (* 2 pw10)))))

(comment (defn for-dig [N mask]
   (let [pw10 (numb/pow-int 10 (get-dig-pw N mask))]
     (range (quot pw10 10) pw10))))

(defn mask-dig
  "when (bit-test mask)=>1 use one from digs(a-dig) else use the-dig"
  [N mask digs the-dig]
  (loop [z 0
         ix 0
         pw10 1
         n digs]
    (if (>= ix N)
      z
      (let [f (bit-test mask ix)]
        (recur (+ z (* pw10 (if f (mod n 10) the-dig)))
               (inc ix)
               (* 10 pw10)
               (if f (quot n 10) n))))))

(defn pat-fam [N mask digs]
  (with-meta
    (->> (if (and (bit-test mask (dec N))
                  (bit-test mask 0)) ;;use a-dig both in the lowest and the highest digits
          (range 0 10)
          (range 1 10))
         (map (partial mask-dig N mask digs)))
    {:m mask :d digs}))

;; 101
;; (mask-dig 3 0x5 123 9)
;; (mask-dig 3 0x3 142 9)

(defn solve [N]
  (->> (for-mask N)
       (map (fn [m]
              (map (fn [d] (pat-fam N m d)) (for-dig N m))))))


;; (count (for-dig 2))

;; (pat-fam 3 0x5 123)

(binding [*print-meta* true] (pr (solve 2)))
(comment (map (partial take 10)
      (list (for-dig 4 0x7)
            (for-dig 4 0xD)
            (list (numb/pow-int 10 (get-dig-pw 4 0x8))))))
