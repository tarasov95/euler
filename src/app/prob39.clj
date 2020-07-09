(ns app.prob39
  (:require [lib.numb :as numb]))

;; https://projecteuler.net/problem=39

;; If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
;; {20,48,52}, {24,45,51}, {30,40,50}
;; For which value of p ≤ 1000, is the number of solutions maximised?

(defn triple?
  ([t] (apply triple? t))
  ([a b c]
   (= (* c c) (+ (* a a) (* b b)))))

(defn perim
  ([t] (apply perim t))
  ([a b c]
   (+ a b c)))

(defn triple [n m k]
  (let [m2 (* m m)
        n2 (* n n)
        a (* k (- m2 n2))
        b (* k (* 2 m n))
        c (* k (+ m2 n2))]
    (with-meta [(min a b) (max a b) c] {:nmk [n m k]})))

(defn gen-triples []
  (for [n (range 1 5)
        m (range (inc n) 10)
        k (range 1 10)
        :let [m2 (* m m)
              n2 (* n n)
              a (* k (- m2 n2))
              b (* k (* 2 m n))
              c (* k (+ m2 n2))]]
    (with-meta [(min a b) (max a b) c] {:nmk [n m k]})))

(defn test1 []
  (binding [*print-meta* true]
    (pr (->> (gen-triples)
            ;; (map (partial apply perim))
            ;; (into #{})
             (group-by (partial apply perim))
            ;; (map #(vector (first %) (count (second %))))
             (filter #(= 120 (first %)))))))

(defn check []
  (filter #(not (apply triple? %)) (gen-triples)))

(defn gen-loop [z param next-param]
  (let [[n m k] param]
    (let [t (triple n m k)
          p (perim t)]
      (if (<= p 1000)
        (recur (conj z t) (next-param param) next-param)
        z))))

(defmacro inc-ix [ix]
  `(fn [v#] (assoc v#  ~ix (inc (v# ~ix)))))

(def inc-n (inc-ix 0))
(def inc-m (inc-ix 1))
(def inc-k (inc-ix 2))

(defn explore1 []
  (into #{} (concat
             (->> (range 1 100)
                  (map #(triple % (inc %) 1))
                  (take-while #(<= (apply perim %) 1000)))
             (->> (range 1 100)
                  (map #(triple 1 (inc %) 1))
                  (take-while #(<= (apply perim %) 1000))))))


;; (gen-loop [] [1 2 1] inc-k)
;; (macroexpand '(inc-ix 2))

;; if(p>=1000 and k=1) inc m
;; if(p>=1000 and m=n+1) inc n
;; 

(defn gen2
  ([P] (gen2 P [] 1 2 1))
  ([P z n m k]
   (let [t (triple n m k)
         p (perim t)]
     ;; (println n m k t p)
     (if (<= p P)
       (recur P (conj z t) n m (inc k))
       (cond
         (> k 1) (recur P z n (inc m) 1)
         (> m (+ n 1)) (recur P z (inc n) (+ 2 n) 1)
         :else z)))))

;; (time (binding [*print-meta* true]
;;    (pr (->> (gen2 1000)
;;             (into #{})
;;             (filter #(= 120 (perim %)))))))

(->> (gen2 1000)
     (into #{})
     (group-by perim)
     (map #(vector (first %) (count (second %))))
     (sort-by second)
     (take-last 1))
