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

(defn print-board [r N]
  (let [b (render-board r N)]
    (println "    A B C D E F G H")
    (dotimes [ix N]
      (println (str (inc ix) ".") (nth b ix)))))

(print-board [0 2 7] 8)
