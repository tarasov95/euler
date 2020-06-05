(ns sicp.queen-col)

;; (loop [ix 0]
;;   (println ix)
;;   (when (< ix N)
;;     (recur (inc ix))))

(defn render-row [p N]
  (map #(if (= % p) 1 0) (range N)))

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

;; (print-board [0 2 7] 8)
(let [q [0 2 7]]
  ;; (print-board (conj q 5))
  (println (map (fn [e] {:x e :z (is-pos-ok q e)}) (range 8))))

;; p => 3 row#4
;; 0,0 => !0 && !3
;; 1,2 => !2 && !4 && !0 (+ 2 (- 3 1))
;; 2,7 => !7 && !6
