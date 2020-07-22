(ns app.prob51
  (:require
   [lib.prime :as prime]
   [lib.seq :as sq]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=51

;; By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

;; By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.

;; Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.

(defn gen-pat-seq
  ([N] (let [s (into [] (repeat N 0))]
         (filter #(and (some (partial = 0) %)
                       (some (partial = 1) %))
                 (gen-pat [s] s (range N)))))
  ([z seed ctl]
   (if (empty? ctl)
     z
     (->> ctl
          (mapcat #(let [y (assoc seed % 1)]
                     (gen-pat [y] y (drop-while (partial >= %)  ctl))))
          (concat z)))))

(defn gen-pat-mask [N]
  (range 1 (dec (numb/pow-int 2 N))))

(defn apply-pat [mask dig x]
  (loop [ix  0
         z   0
         cur x
         pw10 1]
    (if (= cur 0) z
        (recur (inc ix)
               (+ z (* pw10 (if (bit-test mask ix) dig (mod cur 10))))
               (quot cur 10)
               (* 10 pw10)))))

(defn prime-family [N p mask]
  (let [z (into []
                (comp
                 (filter #(not (and (bit-test mask 0) (#{0 2 5} %))))
                 (filter #(not (and (= 0 %) (bit-test mask (dec N)))))
                 (map #(apply-pat mask % p))
                 (filter prime/is-prime?))
                (range 0 10))]
    (with-meta [p z] {:p p :m (Integer/toBinaryString mask)})))

(defn all-prime-pats [N p]
  (->> (gen-pat-mask N)
       (map (partial prime-family N p))))

(defn solve4primes [N]
  (let [pw10 (numb/pow-int 10 (dec N))
        rg (drop-while (partial > pw10) (prime/primes-below (* 10 pw10)))]
    (mapcat (partial all-prime-pats N) rg)))

(defn brute1 []
  (->> (solve4primes 6)
       (filter #(>= (count (second %)) 8))
       (first)))

(defmacro gen-num-vec [rg]
  (loop [z []
         pw10 1
         v (reverse rg)]
    (if (empty? v)
      `(+ ~@z)
      (recur
       (conj z (list '* pw10 (first v)))
       (* 10 pw10)
       (rest v)))))

(defmacro gen-for-pat [N mask]
  (let [rg   (fn [i]
               (if (or (= i 0) (= i (dec N)))
                 '(range 1 10)
                 '(range 0 10)))
        ix   (range 0 N)
        dig  (gensym "dig")
        vars (into [] (map #(gensym (str "v" % "_")) ix))
        forh (mapcat #(when (not (bit-test mask %))
                        (list (vars %) (rg %))) ix)
        forb (map #(if (bit-test mask %) dig (vars %)) ix)]
    `(for [~@forh
           ~dig (range -1 10)]
       (if (= ~dig -1)
         nil
         (gen-num-vec [~@forb])))))

(defmacro all-pats [N len]
  (let [pats (gen-pat-mask N)
        pw10 (numb/pow-int 10 (dec N))]
    `(->> (concat ~@(map (fn [p] `(gen-for-pat ~N ~p)) pats))
          (into [] (comp
                    (filter #(or (= nil %) (>= % ~pw10)))
                    (partition-by (partial = nil))
                    (filter #(not= nil (first %)))
                    (map #(filter prime/is-prime? %))
                    (filter (comp not empty?))
                    (filter #(>= (count %) ~len))
                    (take 1))))))

(time (doall (all-pats 6 8)))
;; (macroexpand-1 '(all-pats 3 4))
;; (time (binding [*print-meta* true]
;;    (pr (brute1))))
;; (macroexpand-1 '(gen-for-pat 5 0x2A))
;; (macroexpand-1 '(gen-num-vec [1 2 3 4]))

