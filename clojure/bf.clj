(ns bf.interpreter
  (:require [clojure.string :as str]))

(defn read-file [path]
  (slurp path))

(defn interpret [code tape ptr]
  (let [tokens (seq code)
        token (first tokens)
        rest-tokens (rest tokens)
        head (first tape)]
    (cond
      (empty? tokens) tape
      (empty? tape) (interpret code [0] ptr)
      (= \+ token) (interpret rest-tokens (cons (+ 1 head) '(rest tape)) ptr)
      (= \- token) (interpret rest-tokens (cons (- 1 head) '(rest tape)) ptr)
      (= \> token) (interpret rest-tokens tape (inc ptr))
      (= \< token) (interpret rest-tokens tape (dec ptr))
      :else        (interpret rest-tokens tape ptr)))
  )

(defn bf [path]
  (let [bf-code (str/trim-newline (read-file path))]
    (interpret bf-code [0] 0)))
