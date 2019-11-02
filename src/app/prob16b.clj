(ns app.prob16)
;; https://projecteuler.net/problem=16

(defn as-bin
  ([n] (as-bin n (list)))
  ([n r]
   (if (<= n 1) (conj r n)
       (let [d (mod n 2)]
         (recur (/ (- n d) 2) (conj r d))))))

(defn as-dec
  "msb -> lsb"
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

(defn bsub1 [n1 n2]
  )

(defn to-bin
  "lsb -> msb"
  ([n] (to-bin n []))
  ([n r]
   (if (< n 2)
     (conj r n)
     (recur (quot n 2) (conj r (mod n 2))))))

(defn from-bin
  "lsb -> msb"
  ([r] (from-bin r 0 1))
  ([r n m]
   (let [f (first r)]
     (if (nil? f)
       n
       (recur (rest r)
              (+ n (* m f))
              (* 2 m))))))

(defn add2
  "lsb -> msb"
  ([b1 b2] (add2 b1 b2 [] 0))
  ([b1 b2 r c]
   (let [f1 (first b1)
         f2 (first b2)]
     (cond
       (and (nil? f1) (nil? f2)) {:n r :c c}
       :else (let [d0 (+ (or f1 0) (or f2 0))
                   c0 (quot d0 2)
                   d1 (+ c (mod d0 2))
                   c1 (quot d1 2)]
               (recur (rest b1)
                      (rest b2)
                      (conj r (mod d1 2))
                      (max c1 c0)))))))

(let [b7 '(0 1 1 1)]
  {:add1 (add2 [0 0 1] [1 1 1] [] 0)
   :add2 (add2 [1] [1 1 1] [] 0)
   :test1 (to-bin 8)
   :test2 (from-bin [1 0 1])})
