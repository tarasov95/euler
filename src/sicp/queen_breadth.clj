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

(defn find-game [fun q ix N]
  (when (is-pos-ok q ix)
    (fun (conj q ix) N)))

(defn find-all-games
  ([q N] (find-all-games '() q N))
  ([z q N]
   "re_z_ult q_ueens N_umber-of-rows"
   (if (= (count q) N)
     (cons q z)
     (loop [y (list)
            ix 0]
       (if (< ix N)
         (recur (concat y (find-game find-all-games q ix N))
                (inc ix))
         y)))))

(count (find-all-games [] 8))
