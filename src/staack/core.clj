(ns staack.core)

(def queue (clojure.lang.PersistentQueue/EMPTY))
(def stack (list))
; ????

(defn ctake
  "Like take, but generic"
  [n coll]
   (if (or (= 0 n) (empty? coll))
     (empty coll)
     (conj (ctake (dec n) (pop coll)) (peek coll))))

(defn cdrop
  "Like drop, but generic"
  [n coll]
  (if (or (= 0 n) (empty? coll))
    coll
    (cdrop (dec n) (pop coll))))

(def csplit (juxt ctake cdrop))

(defn confn
  "Turn a clojure function into a concatenative one"
  ([f arity]
    #(let [[h t] (csplit arity %)]
      (conj t (apply f h))))
  ([stack f arity]
    ((confn f arity) stack)))

(defn lit
  ([v]
    #(conj % v))
  ([stack v]
    (conj stack v)))

(defn block* [& words]
  (fn [stack] (reduce #(%2 %1) stack words)))

(defn block [stack & words]
  (conj stack (apply block* words)))

(defmacro defblock [name & words]
  `(def ~name (block* ~@words)))

(defn cwhen [stack]
  (let [[[bl con] tail] (csplit 2 stack)]
    (if con
      (bl tail)
      tail)))

(defn cif [stack]
  (let [[[bl2 bl con] tail] (csplit 3 stack)]
    (if con
      (bl  tail)
      (bl2 tail))))

(defn dup [stack]
  (conj stack (peek stack)))

(def c+ (confn + 2))
(def c- (confn - 2))
(def c* (confn * 2))
(def cnot (confn not 1))
(def c= (confn = 2))
(def c< (confn < 2))