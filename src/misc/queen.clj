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
  (println (apply str (repeat (* 2 N) "-")))
  (doseq [r (render m)]
    (println)
    (doseq [c r]
      (print (str (if (< c 0) "*" " ") (java.lang.Math/abs c) " ")))
    (println)))

(defn putq [mp x y q]
  (letfn [(mark [z i]
            (-> (visit z x i q)
                (visit , i y q)
                (visit , (+ x i) (- y i) q)
                (visit , (- x i) (+ y i) q)
                (visit , (+ x i) (+ y i) q)
                (visit , (- x i) (- y i) q)))]
    (let [z (reduce mark mp (range 0 N))]
      (visit z x y (- q)))))

;; (print-board (putq {} 1 3 1))
;; (print-board )
;; (for [x rg
;;       y rg]
;;   (println x y))

(defn find-pos
  ([mp] (find-pos mp 0))
  ([mp k]
   (cond
     (>= k (* N N)) nil
     (nil? (mp k)) [(quot k N) (mod k N) k]
     :else (recur mp (inc k)))))

(defn spy [mp q]
  (println "q=" q)
  (print-board mp)
  mp)

(defn apply-p [mp p q]
  (putq mp (nth p 0) (nth p 1) q))

(defn solve [mp q k]
  ;; (spy mp q k)
  (cond (>= q  N)       (spy mp q)
        (>= k (* N N)) nil
        :else          (let [p  (find-pos mp k)
                             q1 (inc q)
                             z0 (if (nil? p) nil (solve (apply-p mp p q1) q1 0))]
                         (if (nil? z0)
                           ;; (spy mp q [p k])
                           (if (nil? p)
                             nil
                             (recur mp q (inc (nth p 2))))
                           z0))))

(solve {} 0 0)

;; (let [rg (range N)
;;       mp (putq {} 0 0 1)
;;       p  (find-pos mp)]
;;   (print-board mp)
;;   (println ">>>>>>>>>>>>>>>>>")
;;   (println "pos=" p)
;;   (print-board (putq mp (nth p 0) (nth p 1) 2)))
