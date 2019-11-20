(ns app.prob24)

;; https://projecteuler.net/problem=24


(defn perm [d]
  (if (empty? (rest d))
    (list d)
    (mapcat (fn [e]
              (let [d2 (filter #(not= % e) d)]
                (map #(conj % e) (perm d2))))
            d)))

(defn print-perm [d]
  (doseq [r (map-indexed #(vector (inc %1) %2) (perm d))]
    (println "#" (first r) ": " (second r))))

;; (print-perm [0 1 2 3])

(defn conj-each
  ([l e] (conj-each l e []))
  ([l e z]
   (if (empty? l)
     z
     (recur (rest l) e (conj z (conj (first l) e))))))

(defn defdbg [name f]
  (fn [& args]
    (println name args "...")
    (let [z (apply f args)]
      (println name args "=>" z)
      z)))

(defn perm3
  ([l] (perm3 l [] []))
  ([l r z]
   (let [h (first l)
         t (rest l)]
     (if (nil? h)
       z
       (if (and (empty? t) (empty? r))
         (conj z l)
         (let [p3 (perm3 (concat r t))
               z3 (conj-each p3 h)]
           (recur t
                  (conj r h)
                  (concat z z3))))))))

;; 0 1 2 3 4 5 6 9 8 7
;; 999999
;; 9! = 362880 = (nth p (dec 362880)) => 0 9 8 7 6 5 4 3 2 1 

(def xs [0 1 2 3 4 5 6 7 8 9])
(def sm [0 1 2 3])

(with-redefs [;;perm3 (defdbg "perm3" perm3)
              ;; conj-each (defdbg "conj-each" conj-each)
              ]
  (let [p (map #(apply str %) (perm3 xs))]
    (println (nth p (dec 1000000)))))

;; (println (sort p))
;; [(nth p 5)
;;  (nth p (dec 362880))
;;  (nth p (dec 725760))
;;  (nth p (dec 1000000))
;;  (nth p 1000000)
;;  (nth p (inc 1000000))
;;  (count p)]

