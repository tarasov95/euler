(ns sicp.queen-breadth)

(defn render-row [p N]
  (map #(if (= % p) "Q" "*") (range N)))

(defn print-board
  ([r] (print-board r (count r)))
  ([r N]
   (println "  " (range N))
   (dotimes [ix N]
     (println (str ix ")") (render-row (nth r ix) N)))))

(defn is-pos-ok
  ([q x] (is-pos-ok q 0 x (count q)))
  ([q qy x y]
   (if (empty? q) true
       (let [q0 (first q)
             p (fn [ix f]
                 (when false (println {:ix ix :q0 q0 :qy qy :x x :y y}))
                 f)]
         (cond
           (p 1 (= q0 x)) false
           (p 3 (= (+ q0 (- y qy)) x)) false
           (p 4 (= (- q0 (- y qy)) x)) false
           :else (recur (rest q) (inc qy) x y))))))

(defn find-next-pos
  ([q] (find-next-pos q 0 8))
  ([q N] (find-next-pos q 0 N))
  ([q ix N]
   (when (< ix N)
     (if (is-pos-ok q ix)
       ix
       (find-next-pos q (inc ix) N)))))

(defn find-games-at-pos [ix fun-find-all z q N]
  (if (is-pos-ok q ix)
    (fun-find-all 0 z (conj q ix) N)
    z))

(defn find-all-games
  ([q N] (find-all-games 0 '() q N))
  ([ix z q N]
   "_ix_-of-the-column-to-check re_z_ult _q_ueens _N_umber-of-rows"
   (if (= N (count q))
     (cons q z)
     (if (< ix N)
       (let [z1 (find-games-at-pos ix find-all-games z q N)]
         (recur (inc ix) z1 q N))
       z))))

(count (find-all-games [] 8))
