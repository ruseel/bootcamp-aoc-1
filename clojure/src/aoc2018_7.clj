(ns aoc2018-7
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

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

(defn remove-n-in-val [deps n]
  (assert (map? deps))
  (assert (empty? (deps n)))
  (reduce-kv (fn [m k nodes]
               (let [nodes' (seq (remove #{n} nodes))]
                 (assoc m k (vec nodes'))))
             {}
             deps))

(defn solve
  ([deps]
   (solve deps (keys-with-empty-val deps)))
  ([deps remove-candidates]
   (let [[n & _] (sort remove-candidates)]
     (lazy-seq
      (cons
       n
       (let [deps (-> (remove-n-in-val deps n)
                      (dissoc n))]
         (if (empty? deps)
           nil
           (solve deps))))))))

(comment

  (set! *print-length* 30)

  (def lines
    (-> "day7.txt"
        io/resource
        io/reader
        line-seq))

  (def deps (->deps (lines->deps-tuples lines)))

  (->> deps
       ammend-deps
       solve
       clojure.string/join)


  (-> deps
      ammend-deps
      (remove-n-in-val "G")
      #_(dissoc "G")
      #_(remove-n-in-val "K")
      #_(remove-n-in-val "P")
      #_(remove-n-in-val "T")
      )

  )
