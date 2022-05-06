(ns aoc2018-7
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn- ->deps-tuples
  "list of tuple, tuple [x y] means x depends on y"
  [lines]
  (for [s lines]
    (vec (reverse (drop 1 (re-seq #"[A-Z]" s))))))

(defn ->deps
  "deps-tuple 을 의존관계를 담은 map 으로 변환합니다.

  key 에 node 를, val 에 key 가 의존하는 node 목록을 담은 map 을 리턴합니다.
  'x 가 y 에 의존한다'는 y가 끝난 후에 x를 시작할 수 있다는 의미로 사용합니다."
  [a-deps-tuples]
  (reduce (fn [m [x y]]
            (update m x #((fnil conj []) % y)))
          {}
          a-deps-tuples))

(defn all-nodes
  "deps 안에 존재하는 모든 nodes."
  [deps]
  (let [nodes-in-key (keys deps)
        nodes-in-val (apply concat (vals deps))]
    (concat nodes-in-key nodes-in-val)))

(defn nodes-only-exists-in-val
  "key 에 존재하지 않고 val (== list of nodes) 에만 존재하는 nodes."
  [deps]
  (set/difference
   (set (all-nodes deps))
   (set (keys deps))))

(defn keys-with-empty-val
  "empty list of nodes 를 가진 key 목록."
  [deps]
  (reduce-kv (fn [result k vs]
               (if (empty? vs)
                 (conj result k)
                 result))
             []
             deps))

(defn ammend-deps
  "deps 를, val 에만 존재하는 node 도 key 로
   -- empty vector 를 가지도록 해서 -- 넣습니다."
  [deps]
  (reduce (fn [deps n] (assoc deps n []))
          deps
          (nodes-only-exists-in-val deps)))

(defn remove-n-in-val [deps n]
  {:pre [(map? deps)
         (empty? (deps n))]}
  (reduce-kv (fn [m k nodes]
               (let [nodes' (seq (remove #{n} nodes))]
                 (assoc m k (vec nodes'))))
             {}
             deps))

(defn remove-nodes-in-val [deps nodes]
  (reduce #(remove-n-in-val %1 %2)
          deps
          nodes))

;; part2

(defn available-worker-count [{:keys [capacity workers]}]
  (- capacity (count workers)))

(defn next-ends-at [{:keys [workers]}]
  (->> workers (map :ends-at) seq (apply min)))

(defn emulate-time-to-next-ends-at [state]
  (emulate-time-to state (next-ends-at state)))

(defn processing-duration [node]
  {:pre [(= 1 (count node))]}
  (+ 60 (- (int (first node)) 64)))

(defn worker-available? [state]
  (pos? (available-worker-count state)))

(defn assign
  [{:keys [cur-time] :as state} n]
  {:pre [(worker-available? state)]}
  (if (nil? n)
    state
    (-> state
        (update :workers
                #(conj %
                       {:node n
                        :ends-at (+ cur-time (processing-duration n))}))
        (update :deps
                #(dissoc % n))
        (update :history
                #(conj % n)))))

(defn emulate-time-to
  "cur-time 을 time 으로 옮기고,
   deps 에서 완료된 nodes 를 지우고,
   workers 에 진행중인 worker 만 남깁니다."
  [{:keys [cur-time deps workers] :as state} time]
  (let [ended? (fn [worker] (<= (:ends-at worker) time))
        finished-nodes (->> workers
                            (filter ended?)
                            (map :node))]
    (merge state
           {:cur-time time
            :deps (remove-nodes-in-val deps finished-nodes)
            :workers (filter (complement ended?) workers)})))

(defn- step [{:keys [deps history] :as state}]
  (let [n (-> deps
              keys-with-empty-val
              sort
              first)]
    (if (and (worker-available? state) n)
      (assign state n)
      (emulate-time-to-next-ends-at state))))

(defn solve-part2 [state]
  (->> state
       (iterate step)
       (drop-while (fn [{:keys [workers deps]}]
                     (or (not-empty workers)
                         (not-empty deps))))
       first
       :cur-time))

(defn solve-part1-rev2 [state]
  (->> state
       (iterate step)
       (drop-while (fn [{:keys [workers deps]}]
                     (or (not-empty workers)
                         (not-empty deps))))
       first
       :history
       (apply str)))

;; step 을 좀 더 읽기 쉽게,
;;   (assign, emulate-time-to-next-ends-at) 으로 나눔.
;; defrecord 삭제.
;; worker-pool 개념을 없앰.

(comment

  (do
    (in-ns 'user)
    (remove-ns 'aoc2018-7)
    (load-file "src/aoc2018_7.clj")
    (in-ns 'aoc2018-7))

  ;; part2

  (set! *print-length* 30)

  (def lines (-> "day7.txt"
                 io/resource
                 io/reader
                 line-seq))

  (def deps (-> lines
                ->deps-tuples
                ->deps
                ammend-deps))

  (solve-part2 {:capacity 5
                :deps deps
                :cur-time 0
                :workers []
                :history []})

  ;; part1-rev2
  (solve-part1-rev2 {:capacity 1
                     :deps deps
                     :cur-time 0
                     :workers []
                     :history []})

  )
