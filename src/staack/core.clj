(ns staack.core)

(def queue (clojure.lang.PersistentQueue/EMPTY))
(def stack ())
; ????

(defn thrush [init & fns]
  (reduce #(%2 %1) init fns))

(defn lit [v]
  #(conj % v))

(defn block [& words]
  #(apply thrush % words))

(defmacro defblock [name & words]
  `(def ~name (block ~@words)))
