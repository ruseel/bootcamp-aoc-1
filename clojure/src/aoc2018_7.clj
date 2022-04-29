(ns aoc2018-7
  (:require [clojure.java.io :as io]))

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

(defn ->revdeps [a-deps-tuples]
  (reduce (fn [m [y x]]
            (update m x #((fnil conj []) % y)))
          {}
          a-deps-tuples))

(def deps (->deps (lines->deps-tuples lines)))
(def rev-deps (->revdeps (lines->deps-tuples lines)))

(defn find-root [rev-deps n]
  (loop [n n
         parents (rev-deps n)]
    (if (seq parents)
      (let [n' (first parents)]
        (recur n' (rev-deps n')))
      n)))

(defn dfs-print [deps depth node]
  (doall
   (for [child (deps node)]
     (dfs-print deps (inc depth) child)))
  (println depth node))

;;
;;
(defn dfs-seq-preorder [deps depth node]
  (lazy-seq
   (cons
    [depth node]
    (mapcat (partial dfs-seq-preorder deps (inc depth))
            (deps node)))))

(dedupe
 (map second
      (dedupe
       (sort-by (juxt (comp - first) second)
                (dfs-seq-preorder deps 0 "Z")))))

deps

(comment

 (set! *print-length* 20)

 (find-root rev-deps "X")

 (first (rev-deps "X"))

 (deps "X")

 lines

 )
