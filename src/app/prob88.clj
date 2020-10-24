(ns app.prob88
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [lib.prime :as prime]
            [clojure.spec.alpha :as s]
            [clojure.set :as st]
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
         (mapcat #(vector % ;;[original reversed]
                          [(reduce * (second %)) [(first %)]])))))

(t/deftest expand-factor-test
  (expand-factor {:fac 2 :pow 3}))

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
  (let [z (->> (drop 2 (range))
          (pmap #(vector % (list-prod-sum %)))
          (map #(vector (first %) (map count (second %))))
          (reduce (record-len N) (sorted-map)))]
 (->> z
      (filter #(<= (first %) N))
      ;; (map second)
      ;; (distinct)
      ;; (reduce +)
      )))


(defn inflate-fact [f]
  (repeat (:pow f) (:fac f)))

(t/deftest inflate-fact-test
  (t/is (= [2 2 2] (inflate-fact {:fac 2 :pow 3}))))

(defn mask [dig m]
  (letfn [(select [pred]
            (->> dig
                 (map-indexed #(* (if (pred (bit-test m %1)) 1 0) %2))
                 (filter (partial not= 0))))]
    (let [z0 (select identity)
          z1 (select not)]
      (->> [(conj z0 (reduce * z1))
            (conj z1 (reduce * z0))
            [(reduce * z0) (reduce * z1)]]
           (map #(filter (partial not= 1) %))
           (map #(into [] (sort %)))
           (distinct))
      )))

(t/deftest mask-test
  (mask [2 2 3] 5))

(defn list-all-multiples [N]
  (let [fact (prime/prime-fact N)
        pw (->> fact
                (map :pow)
                (reduce +))
        dig (reduce concat (map inflate-fact fact))]
    (->> (range 0 (bit-shift-left 1 pw))
         (mapcat #(mask dig %))
         (map sort)
         (distinct)
         )))

(defn comb2
  ([rg] (comb2 [] 0 (into [] rg)))
  ([z ix rg]
   (if (>= ix (count rg))
     (->> z
          (map #(->> % sort (into []))))
     (let [n (rg ix)
           r (into (subvec rg 0 ix) (subvec rg (inc ix)))]
       (recur
        (into z
              (map-indexed
               #(conj (into (subvec r 0 %1) (subvec r (inc %1)))
                      (* n %2)) r))
        (inc ix)
        rg)))))

(defn comb3 [rg]
  (let [z0 (distinct (comb2 rg))
        z1 (distinct (mapcat #(comb2 (into [] %)) z0))
        z2 (distinct (mapcat #(comb2 (into [] %)) z1))]
    (->> (concat z0
                 z1
                 z2)
         (sort)
         (cons rg))))

(defn permult-facts [rg]
  (loop [y (distinct (comb2 rg))
         z (into [rg] y)]
    (if (every? #(= 1 (count %)) y)
      z
      (let [yn (distinct (mapcat #(comb2 %) y))]
        (recur yn
               (into z yn))))))

(defn permult [N]
  (->> (prime/prime-fact N)
       (mapcat inflate-fact)
       (into [])
       (permult-facts)))

(defn test-multiples [N]
  (let [v1 (into #{} (fact-set N))
        v2 (into #{} (list-all-multiples N))
        v3 (into #{} (permult N))]
    {:v1v2 (st/difference v1 v2)
     :v2v1 (st/difference v2 v1)
     :v2v3 (st/difference v2 v3)
     :v3v2 (st/difference v3 v2)
     :v3v1 (st/difference v3 v1)
     :v1v3 (st/difference v1 v3)}))

(test-multiples 120)
