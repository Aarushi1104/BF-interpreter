(ns bf.interpreter
  (:require [clojure.string :as str]))

;; pointer means "memory/data pointer"
(def init-pointer 0)
(def tape-size 30)
(def init-tape (vec (repeat tape-size 0)))

(defn read-file [path]
  (-> path
      slurp
      str/trim-newline))

(defn inc-at [index v]
  (let [elem (get v index)]
    (assoc v index (inc elem))))

(defn dec-at [index v]
  (let [elem (get v index)]
    (assoc v index (dec elem))))

(defn interpret [code tape ptr]
  (let [cell (first tape)
        tokens (seq code)
        token (first tokens)
        rest-tokens (rest tokens)]
    (if (empty? tokens) tape
      (case token
        \+    (interpret rest-tokens (inc-at ptr tape) ptr)
        \-    (interpret rest-tokens (dec-at ptr tape) ptr)
        \>    (interpret rest-tokens tape (inc ptr))
        \<    (interpret rest-tokens tape (dec ptr))
        \.    nil
              ; otherwise
              (interpret rest-tokens tape ptr)))))

(defn bf [path]
  (let [bf-code (read-file path)]
    (interpret bf-code init-tape init-pointer)))

; testing
; (prn (inc-at 0 [1 2 3]))
; (prn (dec-at 2 [0 0 1]))
(-> "./test.bf" bf prn)
