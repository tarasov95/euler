(ns misc.queen)

(def ^:dynamic N 8)

(defn visit [mp i j q]
  (if (and
       (and (> i -1) (< i N))
       (and (> j -1) (< j N)))
    (assoc mp (+ j (* i N)) q)
    mp))

(defn render [mp]
  (map
   (fn [i] (map
            (fn [j] (or (mp (+ j (* i N))) 0))
            (range 0 N)))
   (range 0 N)))

(defn print-board [m]
  (println (repeat N "-"))
  (doseq [r (render m)]
    (println r)))

(defn putq [mp x y q]
  (letfn [(mark [z i]
            (-> (visit z x i q)
                (visit , i y q)
                (visit , (+ x i) (- y i) q)
                (visit , (- x i) (+ y i) q)
                (visit , (+ x i) (+ y i) q)
                (visit , (- x i) (- y i) q)))]
    (reduce mark mp (range 0 N))))

(print-board (putq {} 1 3 1))
(print-board (putq {} 0 0 2))
