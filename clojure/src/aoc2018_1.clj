(ns aoc2018-1
  (:require [clojure.java.io :as io]))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력

(def vs
  (-> (io/resource "day1.txt")
      slurp
      (clojure.string/split #"\n+")
      (->> (map #(Integer/parseInt %)))))

(reduce + vs)

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

(comment

  (def vs [+1 -2 +3 +1])
  (def vs [+1 -1])
  (def vs [+3, +3, +4, -2, -4])
  (def vs [-6, +3, +8, +5, -6])

)

;; first, next 를 사용한 버전
(loop [seen #{}
       vs (concat [0] (reductions + (flatten (repeat vs))))]
  (let [v (first vs)]
    (if (seen v)
      v
      (recur (conj seen v) (next vs)))))

;; destructuring, cycle, cons
;;   destructuring, cycle 은 확실히 사용하는 것이 더 좋은 것이다 싶은데요.
;;   cons 는 좀 애매하다 싶습니다.
(loop [seen #{}
       [f & rest] (cons 0 (reductions + (cycle vs)))]
  (if (seen f)
    f
    (recur (conj seen f) rest)))
