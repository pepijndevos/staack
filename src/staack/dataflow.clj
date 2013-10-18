(ns staack.dataflow)

(deftype NegStack [popfn stack]
  clojure.lang.ISeq
  (first [_] nil)
  (more [_] ())
  (next [_] nil)
  (cons [_ o] (popfn o stack))
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

(defn concall [stack f n]
  (if (> n 0)
    (mpeek stack #(concall %2 (partial f %1) (dec n)))
    (conj stack (f))))
