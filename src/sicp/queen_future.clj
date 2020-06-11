(ns sicp.queen-future
  (:require
   [clojure.core.async :as async :refer [chan thread >!! <!!]]
   [sicp.queen-puzzle :as qp]))

(defmacro logtime
  [label expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (prn (str ~label " in " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " ms."))
     ret#))

(defn find-games-starting-at [N p]
  (qp/find-all-games [p] N))

(defn count-sequent [N]
  (logtime "Sequent" (count (mapcat (partial find-games-starting-at N) (range N)))))

(defn count-concur [N]
  (let [rq (map
            (fn [p] (future (find-games-starting-at N p)))
            (range N))]
    (logtime "Concur" (count (mapcat deref rq)))))

(defn count-pmap [N]
  (let [rq (pmap
            (partial find-games-starting-at N)
            (range N))]
   (logtime "Pmap" (count (mapcat identity rq)))))

(defn solve-concur [N]
  (let [c1 (future (count-sequent N))
        c2 (future (count-concur N))]
   (logtime "Total" (println "c1=" @c1 "; c2=" @c2))
   (println "c3=" (count-pmap N))))

(defn solve-async [N]
  (logtime "Total-async"
   (let [a (chan)
         z (chan)
         rez (fn [_] (<!! z))]
     ;;prepare calculators
     (dotimes [ix N] 
       (thread
         (let [g (logtime
                  (str "Calc" ix)
                  (find-games-starting-at N (<!! a )))]
           (>!! z g))))
     ;;start calculations
     (dotimes [ix N] (>!! a ix))
     ;;mustn't block because the number of calculators is N
     (println "c=" (count (mapcat rez (range N)))))))
