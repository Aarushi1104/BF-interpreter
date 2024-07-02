(ns ishi.bf
  (:require [clojure.string :as str]))

(defn read-file [path]
  (slurp path))

(defn interpret [code arr ptr]
  (let [tokens (seq code)]
    (cond
      (empty? tokens) arr
      (empty? arr) (interpret code [0] ptr)
      :else (do
              (cond
                (= \+ (first tokens)) (interpret (rest tokens) (cons (+ 1 (first arr)) '(rest arr)) ptr)
                (= \- (first tokens)) (interpret (rest tokens) (cons (- 1 (first arr)) '(rest arr)) ptr)
                (= \> (first tokens)) (interpret (rest tokens) arr (inc ptr))
                (= \< (first tokens)) (interpret (rest tokens) arr (dec ptr))
                :else (interpret (rest tokens) arr ptr)
                )
              )
    )
  )
)

(defn bf [path]
  (let [bf-code (str/trim-newline (read-file path))]
    (interpret bf-code [0] 0)))
