(ns staack.core)

(def queue (clojure.lang.PersistentQueue/EMPTY))
(def stack (list))
; ????

(defn thrush [& fns]
  (reduce #(%2 %1) fns))

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

(defn nth-stack [n f]
  #(apply update-in % [n] f %&))

(defn confn
  "Turn a clojure function into a concatenative one"
  [f arity]
  (nth-stack 0
    #(let [[h t] (csplit arity %)]
      (conj t (apply f h)))))

(defn lit [v]
  (nth-stack 0 #(conj % v)))

(defn block [& words]
  #(apply trush % words))

(defmacro defblock [name & words]
  `(def ~name (block ~@words)))

; exapmle functions
; these use a data stack, a control stack and a queue??

(defn >r [[data control & other]]
  (apply vector
    (pop data)
    (conj control (peek data))
    other))

(defn <r [[data control & other]]
  (apply vector
    (conj data (peek control))
    (pop control)
    other))

(def dup
  (nth-stack 0
    (fn [stack]
      (conj stack (peek stack)))))

(def c+ (confn + 2))
(def c- (confn - 2))
(def c* (confn * 2))
(def cnot (confn not 1))
(def c= (confn = 2))
(def c< (confn < 2))
