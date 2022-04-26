(ns user
  (:require [clojure.pprint]
            [clojure.main]))

(clojure.main/repl
 :init (fn []
         (set! *print-level* 7)
         (set! *print-length* 20))
 :print clojure.pprint/pprint)
