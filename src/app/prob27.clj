(ns app.prob27
  (:require [app.prob3 :as p3]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; https://projecteuler.net/problem=27

;; (take 10 )

;; (slurp "x")
(let [fl (io/file "resources/prime10000.edn")]
  (if (.exists fl)
    (with-open [r (io/reader fl)]
      (edn/read (java.io.PushbackReader. r)))
    (with-open [w (io/writer fl)]
      (binding [*out* w]
        (let [q (take 100 (p3/prime-seq))]
          (prn q)
          q)))))

