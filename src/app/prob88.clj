(ns app.prob88
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [lib.prime :as prime]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=88


;; (defn list-facts [n]
;;   (->> (prime/prime-fact n)
;;        (mapcat (fn [e]
;;                  (repeat (:pow e) (:fac e))))))

;; (defn list-prodsum [n]
;;   (let [f (list-facts n)]
;;     (concat f
;;             (repeat (- n (reduce + f)) 1))))

(defn expand-factor [f]
  (let [{:keys [:pow :fac]} f
        z0 (take
            (inc pow)
            (iterate
             (fn [e]
               [(* fac (first e))
                (subvec (second e) 1)])
             [1 (into [] (repeat pow fac))]))]
    (->> z0
         (mapcat #(vector %
                          [(reduce * (second %)) [(first %)]])))))

(s/fdef expand-factor
  :args (s/cat
         :factor :lib.prime/factor))

;; (defn permut-factors [z m rg]
;;   (let [[h & r] rg]
;;     (if (empty? h)
;;       nil
;;       (->> (expand-factor m h)
;;            (map (fn [ef]
;;                   (conj (second ef)
;;                         (permut-factors [] (first ef) (into [] r)))))))))

;; (s/fdef permut-factors
;;   :args (s/cat
;;          :current-result (s/coll-of number? :kind vector)
;;          :multiplier number?
;;          :array-of-factors :lib.prime/factors))

(defn nullif [val null]
  (if (= val null) nil
      val))

(defn join-exfact [f1 f2]
  (let [[m1 r1] f1
        [m2 r2] f2]
    [(* m1 m2) (into r1 r2)]))

(defn join2 [rg1 rg2]
  (->> rg1
       (mapcat (fn [e1]
                 (map #(join-exfact e1 %) rg2)))))

(defn join
  ([facts] (join (first facts) (rest facts)))
  ([z facts]
   (if (empty? facts)
     z
     (join (join2 z (first facts))
           (rest facts)))))

(defn wrap-join [coll]
  (->> coll
       (map #(sq/if-conj (second %) (nullif (first %) 1)))
       (map sort)
       (filter #(not= 1 (first %)))
       (distinct)))

(defn fact-set [n]
  (->> (prime/prime-fact n)
       (map expand-factor)
       (reduce join2)
       (wrap-join)))

(defn facts-to-prod-sum [n rg]
  (let [s (reduce + rg)]
    (into [] (concat (repeat (- n s) 1)
                     rg))))

(defn list-prod-sum [n]
  (->> (fact-set n)
       (map (partial facts-to-prod-sum n))
       (filter #(> (count %) 1))))

(defn has-keys-upto [z N]
  (loop [ix N]
    (cond
      (< ix 2) true
      (not (z ix)) false
      :else (recur (dec ix)))))

(defn record-len [N]
  (fn [z e]
    (if (has-keys-upto z N)
      (reduced z)
      (let [[n rg] e]
        (->> rg
             (reduce (fn [z1 c] (if (< (or (z1 c) n) n)
                                  z1
                                  (assoc z1 c n))) z))))))

(defn solve [N]
      z (->> (drop 2 (range))
             (pmap #(vector % (list-prod-sum %)))
             (map #(vector (first %) (map count (second %))))
             (reduce (record-len N) (sorted-map)))]
  (->> z
       (filter #(<= (first %) N))
       (map second)
       (distinct)
       (reduce +)))

;; (map count (list-prod-sum 16))
;; (list-prod-sum 16)
;; (fact-set 16)


;; 2 2 3 3
;; 1, 2 2
;; 2, 2
;; 4, nil

;; 1, 3 3
;; 3, 3
;; 9, nil
