(ns staack.parallel
  (:require [clojure.core.reducers :as r]))

(deftype NegStack [popfn stack]
  clojure.lang.ISeq
  (first [_] nil)
  (more [_] ())
  (next [_] nil)
  (cons [_ o] (popfn o stack))
  clojure.lang.Seqable
  (seq [_] nil)
  clojure.lang.Counted
  (count [_] -1))

(defn negative-stack? [s]
  (instance? NegStack s)) 

(defn mpeek [stack cont]
  (if (seq stack)
    (cont (first stack) (next stack))
    (NegStack. cont stack)))

(defn conjcat [x y]
  (let [s (seq x)]
    (if s
      (conj (conjcat (rest s) y) (first s))
      y)))

(defn consume [x y]
  (fn [f stack]
    (let [nx (conj x f)]
      (if (negative-stack? nx)
        (NegStack. (consume nx y) stack)
        (conjcat nx y)))))

(defn mconcat
  ([] ())
  ([base add]
    (println "combine")
    (if (negative-stack? base)
      (if (negative-stack? add)
        (NegStack. (consume base add) ())
        (recur (conj base (first add)) (rest add)))
      (conjcat base add))))

(defn concall [stack f n]
  (if (> n 0)
    (mpeek stack #(concall %2 (partial f %1) (dec n)))
    (conj stack (f))))

(defn pthrush [fns]
  (r/fold 20 mconcat  #(%2 %1) fns))
