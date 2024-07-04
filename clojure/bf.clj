(ns bf.interpreter
  (:require [clojure.string :as str]))

; "pointer" here means "memory/data pointer"
(def init-pointer 0)
(def tape-size 30)
(def init-tape (vec (repeat tape-size 0)))

(defn read-file [path]
  (-> path
      slurp
      str/trim-newline))

(defn machine-constraints [tape ptr]
  (and (< -1 ptr tape-size)
       (every? (comp not neg?) tape)))

(defn apply-at [f]
  (fn [index v]
    (let [elem (get v index)]
      (assoc v index (f elem)))))

(def inc-at (apply-at inc))
(def dec-at (apply-at dec))

(defn valid-pairs [input]
    (loop [stack []
           idx 0
           [x & xs :as s] input
           match {}]
      (cond
        (empty? s) (if (empty? stack) match false)
        (= \[ x) (recur (conj stack [x idx]) (inc idx) xs match)
        (= \] x) (if (and (not (empty? stack)) (= (first (peek stack)) \[))
                      (recur (pop stack) (inc idx) xs (assoc match (second (peek stack)) idx))
                      false)
        :else (recur stack (inc idx) xs match))))

(defn interpret [code tape ptr]
  (assert (machine-constraints tape ptr))
  (let [tokens (seq code)
        token (first tokens)
        next-tokens (rest tokens)
        matching (valid-pairs code)]
    (if (empty? tokens) tape
      (case token
        \+ (recur next-tokens (inc-at ptr tape) ptr)
        \- (recur next-tokens (dec-at ptr tape) ptr)
        \> (recur next-tokens tape (inc ptr))
        \< (recur next-tokens tape (dec ptr))
        \[ (prn 0)
        \. (do (print (get tape ptr))
               (recur next-tokens tape ptr))
        ; otherwise
        (recur next-tokens tape ptr)))))

(defn bf [path]
  (let [bf-code (read-file path)]
    (interpret bf-code init-tape init-pointer)))

; testing
;(-> "../tests/test3.bf" bf prn)
