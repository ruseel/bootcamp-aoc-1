(ns aoc2018-7
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn- lines->deps-tuples
  "list of tuple, tuple [x y] means x depends on y"
  [lines]
  (for [s lines]
    (vec (reverse (drop 1 (re-seq #"[A-Z]" s))))))

(defn ->deps
  "deps-tuple 을 의존관계를 담은 map 으로 변환합니다.
  key 에 node 를, val 에 key 가 의존하는 노드 목록을 담은 map 을 리턴합니다.

  x 가 y 에 의존한다를 y가 끝난 후에 x를 시작할 수 있다는 의미로 사용합니다."
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

;; part2

(defn available-worker-count [worker-pool]
  (let [{:keys [capacity workers]} worker-pool]
    (- capacity (count workers))))

(defn emulate-time-to
  "worker-pool 의 cur-time 을 time 으로 옮기고,
   :finished-queue 에 완료된 node 를
   :workers 에 안끝난 worker 를 넣습니다."
  [worker-pool time]
  (let [{:keys [cur-time workers]} worker-pool
        ended? (fn [worker] (<= (:ends-at worker) time))]
    (-> worker-pool
        (assoc :cur-time time)
        (assoc :finished-queue (->> workers
                                    (filter ended?)
                                    (map :node)))
        (assoc :workers
               (filter (complement ended?) workers)))))

(defn next-ends-at [worker-pool]
  (some->> worker-pool :workers (map :ends-at) seq (apply min)))

(defn processing-duration [node]
  (assert (= (count node) 1))
  (+ 60 (- (int (first node)) 64)))

(defn worker-available? [worker-pool]
  (pos? (available-worker-count worker-pool)))

(defn start-processing
  [worker-pool n]
  (assert (worker-available? worker-pool))
  (let [{:keys [worker cur-time]} worker-pool
        new-worker {:node n
                    :ends-at (+ cur-time (processing-duration n))}]
    (update worker-pool
            :workers
            conj new-worker)))

(defn step [{:keys [deps worker-pool history] :as state}]
  (let [n (-> deps
              keys-with-empty-val
              sort
              first)
        start-processing?
        (and n (worker-available? worker-pool))]
    (if start-processing?
      (let [worker-pool (start-processing worker-pool n)]
        {:worker-pool worker-pool
         :deps (remove-nodes-in-val
                (dissoc deps n)
                (:finished-queue worker-pool))
         :history (conj history n)})
      (let [worker-pool (emulate-time-to
                         worker-pool
                         (next-ends-at worker-pool))]
        {:worker-pool (dissoc worker-pool :finished-queue)
         :deps (remove-nodes-in-val
                deps
                (:finished-queue worker-pool))
         :history history}))))

(defn solve-part2 [{:keys [deps worker-pool] :as state}]
  (->> state
       (iterate step)
       (drop-while (fn [{deps :deps
                         {workers :workers} :worker-pool}]
                     (or (not-empty workers)
                         (not-empty deps))))
       first
       :worker-pool
       :cur-time))


(defn solve-part1-rev2 [state]
  (->> state
       (iterate step)
       (drop-while (fn [{deps :deps
                         {workers :workers} :worker-pool}]
                     (or (not-empty workers)
                         (not-empty deps))))
       first
       :history
       (apply str)))

#_(defn solve-part1-rev1
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

;; solve-part2 도 state 를 받도록 변경 완료.
;; record 를 제거.
;; (comp not empty?) 를 not-emtpy 로 변경.
;; (> x 0) 를 pos? 로 바꿈.
;; part1 을 part2 의 코드로 바꿈
;;   언제 start-procssing 한 노드를 저장하나? history 에 남김.
;; solve-part1 함수도 iterate + drop-while 로 바꿈.
;;
;; step 을 좀 더 읽기 쉽게 바꾸자.
;;

(comment

  (do
    (in-ns 'user)
    (remove-ns 'aoc2018-7)
    (load-file "src/aoc2018_7.clj")
    (in-ns 'aoc2018-7))

  ;; part2

  (set! *print-length* 30)

  (def lines
    (-> "day7.txt"
        io/resource
        io/reader
        line-seq))

  (def deps (->deps (lines->deps-tuples lines)))

  (solve-part2 {:worker-pool {:capacity 5
                              :cur-time 0
                              :workers []}
                :deps (-> deps ammend-deps)
                :history []})

  ;; part1-rev2
  (solve-part1-rev2 {:worker-pool {:capacity 1
                                   :cur-time 0
                                   :workers []}
                     :deps (-> deps ammend-deps)
                     :history []})

  ;; part1-rev1

  (->> deps ammend-deps
       solve-part1-rev1
       clojure.string/join)

  )
