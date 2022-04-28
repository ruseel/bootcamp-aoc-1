(ns aoc2020_8
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]))

(defn- parse-int [s]
  (Integer/parseInt s))

(defn line->instruction [s]
  (let [[a b] (split s #"\s+" 2)]
    {:type :instruction
     :op (keyword a)
     :operand (parse-int b)}))

;; ip - instruction pointer
;; acc - accumulator register
(def env {:ip 0 :acc 0 :visited-addrs #{}})

;; eval-ins
;;   (env, instruction) -> env
(defmulti eval-ins (fn [env instruction] (:op instruction)) :default nil)

(defmethod eval-ins :acc [env instruction]
  (let [v (:operand instruction)]
    (-> env
        (update-in [:acc] (partial + v))
        (update-in [:ip] inc))))

(defmethod eval-ins :jmp [env instruction]
  (let [v (:operand instruction)]
    (-> env
        (update-in [:ip] (partial + v)))))

(defmethod eval-ins :nop [env _]
  (-> env (update-in [:ip] inc)))

(defn record-visited-addrs [env]
  (let [ip (:ip env)]
    (-> env
        (update-in [:visited-addrs] #(conj % ip)))))

(defn- infinite-loop? [env]
  (contains? (:visited-addrs env) (:ip env)))

;; loop-recur 를 썼습니다.
;; reduce 로 바꾸는 것이 영 어색해서
;; 그건 좀 더 작업하도록 해볼께요.

(defn- step [env ins]
  (-> env
      record-visited-addrs
      (eval-ins ins)))

(defn eval [env instructions]
  (loop [env env]
    (cond
      (infinite-loop? env)
      (assoc env :halt :detected-infinite-loop)

      (>= (:ip env) (count instructions))
      (assoc env :halt :finished)

      :else
      (recur (step env (nth instructions (:ip env)))))))

(comment

  (ns-unmap *ns* 'eval-ins)

  (eval-ins env {:op :acc :operand +3})
  (eval-ins env {:op :jmp :operand +4})
  (eval-ins env {:op :nop :operand +0})
  (eval-ins env {:op :nop :operand 0})

  (eval env
        (map line->instruction
             ["nop +1",
              "nop +1"
              "nop +1"
              "nop +1"
              "nop +1"]))

  (eval env (map line->instruction
                 ["nop +0"
                  "jmp -1"
                  "nop +0"]))

  (def sample-instructions (map line->instruction
                  ["nop +0"
                   "jmp -1"]))

  (-> env
      ((fn [env] (prn (infinite-loop? env)) env))
      (step (nth sample-instructions 0))
      ((fn [env] (prn (infinite-loop? env)) env))
      (step (nth sample-instructions 1))
      ((fn [env] (prn (infinite-loop? env)) env)))

  (eval env (map line->instruction
                 ["nop +0"
                  "acc +1"
                  "jmp +4"
                  "acc +3"
                  "jmp -3"
                  "acc -99"
                  "acc +1"
                  "jmp -4"
                  "acc +6"]))

  (def instructions (map line->instruction lines))

  (def lines (-> "day8.txt"
                 io/resource
                 io/reader
                 line-seq))

  (eval env instructions)
  )
