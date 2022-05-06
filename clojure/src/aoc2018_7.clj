(ns aoc2018-7
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn- ->jobs-tuples
  "list of tuple, tuple [x y] means x depends on y"
  [lines]
  (for [s lines]
    (vec (reverse (drop 1 (re-seq #"[A-Z]" s))))))

(defn ->jobs
  "jobs-tuple 을 의존관계를 담은 map 으로 변환합니다.

  key 에 node(==job) 를, val 에 key 가 의존하는 node(==job) 목록을
  담은 map 을 리턴합니다.
  'x 가 y 에 의존한다'는 job y가 끝난 후에 job x를 시작할 수 있다는 의미로 사용합니다."
  [a-jobs-tuples]
  (reduce (fn [m [x y]]
            (update m x #((fnil conj []) % y)))
          {}
          a-jobs-tuples))

(defn all-nodes
  "jobs 안에 존재하는 모든 nodes."
  [jobs]
  (let [nodes-in-key (keys jobs)
        nodes-in-val (apply concat (vals jobs))]
    (concat nodes-in-key nodes-in-val)))

(defn nodes-only-exists-in-val
  "key 에 존재하지 않고 val (== list of nodes) 에만 존재하는 nodes."
  [jobs]
  (set/difference
   (set (all-nodes jobs))
   (set (keys jobs))))

(defn keys-with-empty-val
  "empty list of nodes 를 가진 key 목록."
  [jobs]
  (reduce-kv (fn [result k vs]
               (if (empty? vs)
                 (conj result k)
                 result))
             []
             jobs))

(def find-jobs-ready keys-with-empty-val)

(defn ammend-jobs
  "jobs 를, val 에만 존재하는 node 도 key 로
   -- empty vector 를 val 로 -- 넣습니다."
  [jobs]
  (reduce (fn [jobs n] (assoc jobs n []))
          jobs
          (nodes-only-exists-in-val jobs)))

(defn remove-n-in-val [jobs n]
  {:pre [(map? jobs)
         (empty? (jobs n))]}
  (reduce-kv (fn [m k nodes]
               (let [nodes' (seq (remove #{n} nodes))]
                 (assoc m k (vec nodes'))))
             {}
             jobs))

(defn remove-nodes-in-val [jobs nodes]
  (reduce #(remove-n-in-val %1 %2)
          jobs
          nodes))

;; part2

(defn processing-seconds [node]
  {:pre [(= 1 (count node))]}
  (+ 60 (- (int (first node)) 64)))

(defn available-worker-count [{:keys [capacity workers]}]
  (- capacity (count workers)))

(defn next-ends-at [{:keys [workers]}]
  (->> workers (map :ends-at) seq (apply min)))

(defn worker-available? [state]
  (pos? (available-worker-count state)))

(defn assign
  "가능한 worker 가 있고 처리할 node 도 있으면
   worker 를 추가하고
   jobs 에서도 제거하고 (dissoc)
   history 에도 추가한다."
  [{:keys [cur-sec] :as state} n]
  {:pre [(worker-available? state)]}
  (if (nil? n)
    state
    (-> state
        (update :workers
                #(conj %
                       {:node n
                        :ends-at (+ cur-sec (processing-seconds n))}))
        (update :jobs
                #(dissoc % n))
        (update :history
                #(conj % n)))))

(defn emulate-time-to
  "시간이 진행한 후에 벌어지는 상태변화를 구현합니다.

   cur-sec 을 sec 으로 옮기고,
   jobs 에서 완료된 nodes 를 지우고,
   workers 에 진행중인 worker 만 남깁니다."
  [{:keys [cur-sec jobs workers] :as state} sec]
  (let [ended? (fn [worker] (<= (:ends-at worker) sec))
        finished-nodes (->> workers
                            (filter ended?)
                            (map :node))]
    (merge state
           {:cur-sec sec
            :jobs (remove-nodes-in-val jobs finished-nodes)
            :workers (filter (complement ended?) workers)})))

(defn emulate-time-to-next-ends-at
  "worker 가 작업을 끝낼 시간으로
  시간진행이 일어난 것 처럼 바꿉니다"
  [state]
  (emulate-time-to state (next-ends-at state)))

(defn- step [{:keys [jobs history] :as state}]
  (let [n (-> jobs
              find-jobs-ready
              sort
              first)]
    (if (and (worker-available? state) n)
      (assign state n)
      (emulate-time-to-next-ends-at state))))

(defn solve-part2 [state]
  (->> state
       (iterate step)
       (drop-while (fn [{:keys [workers jobs]}]
                     (or (not-empty workers)
                         (not-empty jobs))))
       first
       :cur-sec))

(defn solve-part1-rev2 [state]
  (->> state
       (iterate step)
       (drop-while (fn [{:keys [workers jobs]}]
                     (or (not-empty workers)
                         (not-empty jobs))))
       first
       :history
       (apply str)))

;; step 을 좀 더 읽기 쉽게,
;;   (assign, emulate-time-to-next-ends-at) 으로 나눔.
;; defrecord 삭제.
;; worker-pool 개념을 없앰.
;; 단어 deps 를 jobs 로 바꿈.
;;
;; XXX spec 적용?
;; XXX remove-nodes-in-val 구현을 set 끼리 하는 것이 더 적절한가?
;; XXX remove-n-in-val 을 없애는 것이 더 적절한가?
;; XXX state 대신 simulation?

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

  (def jobs (-> lines
                ->jobs-tuples
                ->jobs
                ammend-jobs))

  (solve-part2 {:capacity 5
                :jobs jobs
                :cur-sec 0
                :workers []
                :history []})

  ;; part1-rev2
  (solve-part1-rev2 {:capacity 1
                     :jobs jobs
                     :cur-sec 0
                     :workers []
                     :history []})

  )
