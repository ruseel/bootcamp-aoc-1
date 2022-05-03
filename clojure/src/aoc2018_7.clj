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
    (concat nodes-in-key nodes-in-val)))

(defn nodes-only-exists-in-val [deps]
  (set/difference
   (set (all-nodes deps))
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
          (nodes-only-exists-in-val deps)))

(defn remove-n-in-val [deps n]
  (assert (map? deps))
  (assert (empty? (deps n)))
  (reduce-kv (fn [m k nodes]
               (let [nodes' (seq (remove #{n} nodes))]
                 (assoc m k (vec nodes'))))
             {}
             deps))

(defn solve-part1
  ([deps]
   (solve-part1 deps (keys-with-empty-val deps)))
  ([deps remove-candidates]
   (let [n (first (sort remove-candidates))]
     (lazy-seq
      (cons
       n
       (let [deps (-> (remove-n-in-val deps n)
                      (dissoc n))]
         (if (empty? deps)
           nil
           (solve-part1 deps))))))))

(defrecord WorkerPool [capacity cur-time workers])

(defrecord Worker [node ends-at])

(map->Worker {:node "A" :ends-at 61})


(def wp (map->WorkerPool {:capacity 5
                          :cur-time 0
                          :workers []}))

(defn available-worker-count [worker-pool]
  (let [{:keys [capacity workers]} worker-pool]
    (- capacity (count workers))))

(defn emulate-time-to
  "worker-pool 의 cur-time 을 time 으로 옮기고, :finished-queue 에 끝난 worker 를
   :workers 에 안끝난 worker 를 넣습니다"
  [worker-pool time]
  (let [{:keys [cur-time workers]} worker-pool
        ended? (fn [worker] (<= (:ends-at worker) time))]
    (-> worker-pool
        (assoc :cur-time time)
        (assoc :finished-queue (filter ended? workers))
        (update :workers
                (fn [workers]
                  (filter (complement ended?) workers))))))

(defn next-available-time [worker-pool]
  (some->> worker-pool :workers (map :ends-at) seq (apply min)))

(defn processing-duration [node]
  (assert (= (count node) 1))
  (+ 60 (- (int (first node)) 64)))

(defn start-processing [worker-pool n]
  (assert (> (available-worker-count worker-pool) 0))
  (let [{:keys [worker cur-time]} worker-pool
        new-worker (->Worker n (+ cur-time (processing-duration n)))]
    (update worker-pool
            :workers
            conj new-worker)))

(defn try-start-processing [worker-pool n]
  (if (and n (> (available-worker-count worker-pool) 0))
    (start-processing worker-pool n)
    (emulate-time-to worker-pool (next-available-time worker-pool))))


(defn solve-part2
  ([worker-pool deps]
   (let [remove-candicates (keys-with-empty-val deps)
         n ((comp first sort) remove-candidates)
         start-processing? (and n (> (available-worker-count worker-pool) 0))
         worker-pool' (if start-processing?
                        (start-processing worker-pool n)
                        (emulate-time-to worker-pool
                                         (next-available-time worker-pool)))
         deps' (if start-processing?
                 (dissoc deps n)
                 deps)]
     (lazy-seq
      (cons
       worker-pool
       (let [finished-queue (:finished-queue worker-pool')
             worker-pool'' (dissoc worker-pool' :finished-queue)
             deps (reduce #(remove-n-in-val %1 %2)
                          deps'
                          finished-queue)]
         (if (empty? deps)
           worker-pool''
           (solve-part2 worker-pool'' deps))))))))

(comment

  ;; part2

  (def wp
    (map->WorkerPool
     {:capacity 5
      :cur-time 0
      :workers [(map->Worker {:node "A" :ends-at 61})
                (map->Worker {:node "B" :ends-at 120})
                (map->Worker {:node "C" :ends-at 180})]}))

  (def wp
    (map->WorkerPool {:capacity 5
                      :cur-time 0
                      :workers []}))

  (solve-part2
   wp
   (-> deps ammend-deps))

  (-> wp
      (advance-time-to 120)
      (next-available-time))

  (-> wp
      (start-processing "D"))

  ;; part1

  (set! *print-length* 30)

  (def lines
    (-> "day7.txt"
        io/resource
        io/reader
        line-seq))

  (def deps (->deps (lines->deps-tuples lines)))

  (->> deps
       ammend-deps
       solve-part1
       clojure.string/join)

  ;; (comp first sort) 를 min 으로 바꿔 보려고 시도는 했었는데
  ;; string 을 number 로 바꿀 수 없다고 에러가 발생합니다.
  ; (min "A" "B")

  )
