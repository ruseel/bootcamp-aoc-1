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

(def lines (-> "day8.txt"
               io/resource
               io/reader
               line-seq))

(def instructions (map line->instruction lines))

;; ip - instruction pointer
;; acc - accumulator register
(def env {:ip 0
          :acc 0
          :visited-addrs #{}
          :instructions instructions})

;; execute-ins
;;   (env, instruction) -> env
(defmulti execute-ins (fn [env instruction] (:op instruction)) :default nil)

(defmethod execute-ins :acc [env instruction]
  (let [v (:operand instruction)]
    (-> env
        (update-in [:acc] (partial + v))
        (update-in [:ip] inc))))

(defmethod execute-ins :jmp [env instruction]
  (let [v (:operand instruction)]
    (-> env
        (update-in [:ip] (partial + v)))))

(defmethod execute-ins :nop [env _]
  (-> env (update-in [:ip] inc)))

(defn record-visited-addrs [env ip]
  (-> env
      (update-in [:visited-addrs] #(conj % ip))))

(defn- infinite-loop? [env]
  (contains? (:visited-addrs env) (:ip env)))

;; loop-recur 를 썼습니다.
;; reduce 로 바꾸는 것이 영 어색해서
;; 그건 좀 더 작업하도록 해볼께요.

(defn- step [{ip :ip instructions :instructions :as env}]
  (cond
    (infinite-loop? env)
    (assoc env :halt :detected-infinite-loop)

    (>= ip (count instructions))
    (assoc env :halt :finished)

    :else
    (-> env
        (record-visited-addrs ip)
        (execute-ins (nth instructions ip)))))

(defn execute [env]
  (iterate step-1 env))

(defn solve [env]
  (first (drop-while #(nil? (:halt %)) (execute env))))

(comment

  (execute-ins env {:op :acc :operand +3})
  (execute-ins env {:op :jmp :operand +4})
  (execute-ins env {:op :nop :operand +0})
  (execute-ins env {:op :nop :operand 0})

  (execute (merge env
                  {:instructions (map line->instruction
                                      ["nop +1",
                                       "nop +1"
                                       "nop +1"
                                       "nop +1"
                                       "nop +1"])}))

  (execute (merge env
                  {:instructions (map line->instruction
                                          ["nop +0"
                                           "jmp -1"
                                           "nop +0"])}))

  (eval (merge env {:instructions instructions}))

  (first (drop-while #(nil? (:halt %)) (execute env)))

  )
