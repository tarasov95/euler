(boot.core/set-env! :source-paths #(conj % "."))
(boot.core/set-env! :dependencies #(into % '[[com.taoensso/tufte "2.0.1"]]))
