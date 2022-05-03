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

(defn remove-nodes-in-val [deps nodes]
  (reduce #(remove-n-in-val %1 %2)
          deps
          nodes))

(defn solve-part1
  ([deps]
   (solve-part1 [] deps (keys-with-empty-val deps)))
  ([result-vec deps remove-candidates]
   (let [n (first (sort remove-candidates))]
     (lazy-seq
      (cons
       n
       (let [deps (-> (remove-n-in-val deps n)
                      (dissoc n))]
         (if (empty? deps)
           result-vec
           (solve-part1 (conj result-vec n) deps))))))))


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
  "worker-pool 의 cur-time 을 time 으로 옮기고,
   :finished-queue 에 완료된 node 를
   :workers 에 안끝난 worker 를 넣습니다"
  [worker-pool time]
  (let [{:keys [cur-time workers]} worker-pool
        ended? (fn [worker] (<= (:ends-at worker) time))]
    (-> worker-pool
        (assoc :cur-time time)
        (assoc :finished-queue (->> workers
                                    (filter ended?)
                                    (map :node)))
        (update :workers
                (fn [workers]
                  (filter (complement ended?) workers))))))

(defn next-ends-at [worker-pool]
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
    (emulate-time-to worker-pool (next-ends-at worker-pool))))

(defn step [{:keys [deps worker-pool] :as state}]
  (let [n (-> deps
              keys-with-empty-val
              sort
              first)
        start-processing?
        (and n
             (> (available-worker-count worker-pool) 0))]
    (if start-processing?
      {:worker-pool (start-processing worker-pool n)
       :deps (remove-nodes-in-val
              (dissoc deps n)
              (:finished-queue worker-pool))}
      (let [worker-pool (emulate-time-to
                         worker-pool
                         (next-ends-at worker-pool))]
        {:worker-pool (dissoc worker-pool :finished-queue)
         :deps (remove-nodes-in-val
                deps
                (:finished-queue worker-pool))}))))

(defn solve-part2
  ([worker-pool deps]
   (->> {:worker-pool worker-pool :deps (ammend-deps deps)}
        (iterate step)
        (drop-while (fn [{deps :deps
                          {workers :workers} :worker-pool}]
                      (or (not (empty? workers))
                          (not (empty? deps)))))
        first
        :worker-pool
        :cur-time)))

;; step function 만들기
;; step function 안에서 하는 일을 한글로 순차적으로 써보기
;;
;; step (앞으로 할일, 처리된 finished-queue, cur-time) state
;;   step function 안에 들어있는 과정마다 함수로 나누었음.
;;
;; doc string 을 달면서 코딩하면 좀 더 쉬울 수도 있겠음.

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
      (next-ends-at))

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
       #_clojure.string/join)

  ;; (comp first sort) 를 min 으로 바꿔 보려고 시도는 했었는데
  ;; string 을 number 로 바꿀 수 없다고 에러가 발생합니다.
  ; (min "A" "B")

  )
