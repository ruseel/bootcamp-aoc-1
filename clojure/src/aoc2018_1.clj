(ns aoc2018-1
  (:require [clojure.java.io :as io]))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력

;; lines 를 let 으로 바꿉니다.
;; thread-first 와 thread-last 를 섞지 않도록 하기 위한 첫 시도.
(def vs
  (let [lines (-> (io/resource "day1.txt")
                  slurp
                  (clojure.string/split #"\n+"))]
    (map #(Integer/parseInt %) lines)))

(apply + vs)

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

;; destructuring, cycle, cons
;;   destructuring, cycle 은 확실히 사용하는 것이 더 좋은 것이다 싶은데요.
;;   cons 는 좀 애매하다 싶습니다.
;;
;; f & rest 라는 변수명을 사용하다가
;; review 후에 v & more 로 바꿨습니다.
(defn find-first-dup-1 [frequencies]
  (loop [seen #{}
         [v & more] frequencies]
    (if (seen v)
      v
      (recur (conj seen v) more))))

;; reduce 를 사용하자
(defn find-first-dup-2 [frequencies]
  (let [[_ first-dup] (reduce (fn [[seen first-dup] v]
                                (if (seen v)
                                  (reduced [seen v])
                                  [(conj seen v) nil]))
                              [#{} nil]
                              frequencies)]
    first-dup))

(defn cycled-freq [vs]
  (cons 0 (reductions + (cycle vs))))

(comment

  (def vs [+1 -2 +3 +1])
  (def vs [+1 -1])
  (def vs [+3, +3, +4, -2, -4])
  (def vs [-6, +3, +8, +5, -6])

  (find-first-dup-1 (cycled-freq vs))
  (find-first-dup-2 (cycled-freq vs))

  )
