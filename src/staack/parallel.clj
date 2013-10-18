(ns staack.parallel
  (:require [clojure.core.reducers :as r]))

(defn safe-print [& stuff]
  (locking *out*
    (apply println (.getId (Thread/currentThread)) stuff)))

(deftype NegStack [popfn stack]
  clojure.lang.ISeq
  (first [_] nil)
  (more [_] ())
  (next [_] nil)
  (cons [_ o] (safe-print "added" o) (popfn o stack))
  clojure.lang.Seqable
  (seq [_] nil)
  clojure.lang.Counted
  (count [_] (dec (count stack))))

(defn negative-stack? [s]
  (instance? NegStack s)) 

(defn mpeek [stack cont]
  (if (seq stack)
    (cont (first stack) (next stack))
    (NegStack. cont stack)))

(defn ^:private conjcat [x y]
  (let [s (seq x)]
    (if s
      (conj (conjcat (rest s) y) (first s))
      y)))

(defn ^:private consume [x y]
  (fn [f stack]
    (let [nx (conj x f)]
      (if (negative-stack? nx)
        (NegStack. (consume nx y) stack)
        (conjcat nx y)))))

(defn mconcat
  ([] (safe-print "init") ())
  ([base add]
    (safe-print "combine" (count base) base (count add) add)
    (if (negative-stack? base)
      (if (negative-stack? add)
        (NegStack. (consume base add) ())
        (recur (conj base (first add)) (rest add)))
      (conjcat base add))))

(defn concall [stack f n]
  (if (> n 0)
    (mpeek stack #(concall %2 (partial f %1) (dec n)))
    (conj stack (f))))

(defn thrush [fns]
  (reduce (fn [a b] (safe-print (count a) a) (b a)) () fns))

(defn pthrush [fns]
  (r/fold 20 mconcat (fn [a b] (safe-print (count a) a) (b a)) fns))
