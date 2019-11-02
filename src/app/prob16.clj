(ns app.prob16)
;; https://projecteuler.net/problem=16

(defn as-bin
  ([n] (as-bin n (list)))
  ([n r]
   (if (<= n 1) (conj r n)
       (let [d (mod n 2)]
         (recur (/ (- n d) 2) (conj r d))))))

(defn as-dec
  ([b] (as-dec b 0))
  ([b n]
   (let [f (first b)
         r (rest b)]
     (if (empty? r)
      (+ n f)
      (recur r (* 2 (+ n f)))))))

(defn p2 [n]
  (for [x (range (inc n))]
   (if (= 0 x) 1 0)))

(def b10 (list 1 0 1 0))

(defn abs [x]
  (if (< x 0) (- x) x))

(defn spy [r]
  (println r)
  r)

(defn badd [n1 n2]
  (let [b1 (first n1)
        b2 (first n2)
        r1 (rest n1)
        r2 (rest n2)
        s0 (+ b1 b2)]
    (if (or (empty? r1) (empty? r2))
      {:n (list (mod s0 2))
       :c (quot s0 2)}
      (let [a0 (badd r1 r2)
            s1 (+ (mod s0 2) (:c a0))]
        {:n (conj (:n a0) (mod s1 2))
         :c (max (quot s0 2) (quot s1 2))}))))

(defn bnorm [m]
  (conj (:n m) (:c m)))

(defn comp1 [n]
  (map (fn [e] (if (= e 0) 1 0)) n))

;; (as-dec (bnorm (badd '(1 1 1 1) '(1 1 1 1))))

(defn m2-intl [n r c]
  (let [f0 (first n)
        r0 (rest n)
        m0 (+ (* 2 f0) c)]
    (if (empty? r0)
      ((fn [r1 c1] (if (> c1 0) (conj r1 c1) r1))
       (conj r (mod m0 10))
       (quot m0 10))
      (recur r0
             (conj r (mod m0 10))
             (quot m0 10)))))

(defn m2 [n]
  (reverse (m2-intl (reverse n) [] 0)))

(defn p2 [n r]
  (if (<= n 0)
    r
    (recur (dec n) (m2-intl r [] 0))))

(let [rg (p2 1000 [1])]
  {:rg rg
   :sum (apply + rg)})
