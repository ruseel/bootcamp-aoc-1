(ns aoc2020_8
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]))

(defn- parse-int [s]
  (Integer/parseInt s))

(def lines (-> "day8.txt"
               io/resource
               io/reader
               line-seq))

(defn line->instruction [s]
  (let [[a b] (split s #"\s+" 2)]
    {:type :instruction
     :op (keyword a)
     :operand b}))

;; ip - instruction pointer
;; acc - accumulator register
(def env {:ip 0 :acc 0 :visited-addrs #{}})

;; eval-ins
;;   (env, instruction) -> env
(defmulti eval-ins (fn [env instruction] (:op instruction)) :default nil)

(defmethod eval-ins :acc [env instruction]
  (let [v (parse-int (:operand instruction))]
    (-> env
        (update-in [:acc] (partial + v))
        (update-in [:ip] inc))))

(defmethod eval-ins :jmp [env instruction]
  (let [v (parse-int (:operand instruction))]
    (-> env
        (update-in [:ip] (partial + v)))))

(defmethod eval-ins :nop [env _] env)


(defn- infinite-loop? [env]
  (contains? (:visited-addrs env) (:ip env)))

(defn- update-visited-addrs [{ip :ip :as env}]
  (update-in env [:visitied-addrs] #(conj % ip)))

;; loop-recur 를 썼습니다.
;; reduce 로 바꾸는 것이 영 어색해서
;; 그건 좀 더 작업하도록 해볼께요.
(defn eval-print-loop [env instructions]
  (loop [env env
         [ins & more] instructions]
    (let [env (-> env
                  (eval-ins ins)
                  update-visited-addrs)]
      (prn [ins env])
      (if (infinite-loop? env)
        (throw (ex-info "infinite loop")))
      (if (seq? more)
        (recur env more)))))

(comment

  (ns-unmap *ns* 'eval)

  (def instructions
    (map line->instruction lines))

  (eval-ins env {:op :acc :operand "+3"})
  (eval-ins env {:op :jmp :operand "+4"})
  (eval-ins env {:op :nop :operand "+0"})
  (eval-ins env {:op :nop :operand "+x"})

  (eval-print-loop env instructions)

  )
