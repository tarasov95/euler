(ns sicp.queen-col)

;; (loop [ix 0]
;;   (println ix)
;;   (when (< ix N)
;;     (recur (inc ix))))

(defn render-row [p N]
  (map #(if (= % p) "Q" "*") (range N)))

(defn render-board [r N]
  (loop [ix 0
         z []]
    (if (= ix N)
      z
      (recur
       (inc ix)
       (conj z (render-row (nth r ix -1) N))))))

(defn print-board
  ([r] (print-board r 8))
  ([r N]
   (let [b (render-board r N)]
     (println "    A B C D E F G H")
     (println "    0 1 2 3 4 5 6 7")
     (dotimes [ix N]
       (println (str ix ".") (nth b ix))))))

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

(defn cut-tail [r]
  (into [] (butlast r)))

(defn swap-tail [r t]
  (conj (cut-tail r) t))

(defn backtrack [fun q N]
  (if (empty? q)
    nil
    (let [p (find-next-pos (cut-tail q) (inc (last q)) N)]
      (if (and p (< p N))
        (fun (swap-tail q p) N)
        (backtrack fun (cut-tail q) N)))))

(defn find-game [q N]
  (if (= N (count q))
    q
    (let [p (find-next-pos q N)]
      (if (nil? p)
        (backtrack find-game q N)
        (find-game (conj q p) N)))))


(defn find-all-games
  ([N] (let [g (find-game [] N)]
         (find-all-games (list g) g N)))
  ([z q N]
   (let [g (backtrack find-game q N)]
     (if (nil? g)
       z
       (find-all-games (conj z g) g N)))))

(let [z (find-all-games 8)]
  (doseq [g z]
    (print-board g))
  (println "count " (count z)))

