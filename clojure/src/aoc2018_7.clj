(ns aoc2018-7
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def lines
  (-> "day7.txt"
      io/resource
      io/reader
      line-seq))

(defn- lines->deps-tuples
  "list of tuple, tuple [x y] means x depends on y"
  [lines]
  (for [s lines]
    (vec (reverse (drop 1 (re-seq #"[A-Z]" s))))))

(defn ->deps [a-deps-tuples]
  (reduce (fn [m [x y]]
            (update m x #((fnil conj []) % y)))
          {}
          a-deps-tuples))

(defn all-nodes [deps]
  (let [nodes-in-key (keys deps)
        nodes-in-val (apply concat (vals deps))]
    (set (concat nodes-in-key nodes-in-val))))

(defn nodes-with-no-incoming-edges [deps]
  (set/difference
   (all-nodes deps)
   (set (keys deps))))

(defn keys-with-empty-val [deps]
  (reduce-kv (fn [result k vs]
               (if (empty? vs)
                 (conj result k)
                 result))
             []
             deps))

(defn ammend-deps [deps]
  (reduce (fn [deps n] (assoc deps n []))
          deps
          (nodes-with-no-incoming-edges deps)))

(defn remove-node-with-no-incoming-edges [deps n]
  (assert (map? deps))
  (assert (empty? (deps n)))
  (reduce-kv (fn [m k nodes]
               (let [nodes' (seq (remove #{n} nodes))]
                 (assoc m k (vec nodes'))))
             {}
             deps))

(def deps (->deps (lines->deps-tuples lines)))

(-> deps
    ammend-deps
    (remove-node-with-no-incoming-edges "G")
    (remove-node-with-no-incoming-edges "K")
    (remove-node-with-no-incoming-edges "P")
    (remove-node-with-no-incoming-edges "T")
    #_(nodes-with-no-incoming-edges)
    #_(remove-node-with-no-incoming-edges "S")
    #_(remove-node-with-no-incoming-edges "X")
    #_(nodes-with-no-incoming-edges)
    #_(remove-node-with-no-incoming-edges "B")
    #_(remove-node-with-no-incoming-edges "L")
    #_(remove-node-with-no-incoming-edges "I")
    #_(nodes-with-no-incoming-edges)
    )


(loop [,,,]
  (if ,,,
    ,,,
    (recur state-1 state-2 next)))

; while deps is not empty?
;   for each node in sorted (nodes with no-incoming-edge)
;     remove node in deps

(defn remove-nodes [deps nodes]
  (reduce (fn [deps n]
            (remove-node-with-no-incoming-edges deps n))
          deps
          nodes))

(defn solve
  ([deps]
   (solve deps
          (sort (keys-with-empty-val (ammend-deps deps)))))
  ([deps nodes-to-remove]
   (let [nodes' nodes-to-remove]
     (lazy-seq
      (cons
       nodes'
       (let [deps (remove-nodes deps nodes')]
         (if (empty? deps)
           nil
           (solve deps
                  (sort (keys-with-empty-val deps))))))))))

(-> deps
    solve
    flatten)


(count (all-nodes deps))

(let [deps deps
      ns (sort (nodes-with-no-incoming-edges deps))]

  (remove-nodes deps ns))

(defn step [deps ])

(comment

 lines

 (set! *print-length* 30)

 (find-root rev-deps "X")

 (first (rev-deps "X"))

 (deps "X")

 lines

 )
