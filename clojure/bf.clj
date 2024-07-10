(ns bf.interpreter
  (:require [clojure.string :as str]))

; "pointer" here means "memory/data pointer"
(def init-pointer 0)
(def tape-size 30000)
(def init-tape (vec (repeat tape-size 0)))
(def init-code-ptr 0)

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

(defn read-byte []
  (int (.read System/in)))

(defn interpret [code tape ptr code-ptr]
  (assert (machine-constraints tape ptr))
  (let [token (get code code-ptr)
        matching (valid-pairs code)]
    (if (= token nil) tape
        (case token
          \+ (recur code (inc-at ptr tape) ptr (inc code-ptr))
          \- (recur code (dec-at ptr tape) ptr (inc code-ptr))
          \> (recur code tape (inc ptr) (inc code-ptr))
          \< (recur code tape (dec ptr) (inc code-ptr))
          \[ (if (= (get tape ptr) 0)
               (recur code tape ptr (inc (matching code-ptr)))
               (recur code tape ptr (inc code-ptr)))
          \] (if (= (get tape ptr) 0)
               (recur code tape ptr (inc code-ptr))
               (recur code tape ptr (inc (matching code-ptr))))
          \. (do (print (char (get tape ptr)))
                 (recur code tape ptr (inc code-ptr)))
          \, (recur code (assoc tape ptr (read-byte)) ptr (inc code-ptr))
          ; otherwise
          (recur code tape ptr (inc code-ptr))))))

(defn bf [path]
  (let [bf-code (read-file path)]
    (interpret bf-code init-tape init-pointer init-code-ptr)))

; testing
(-> "../tests/one_to_10.bf" bf prn)

; bracket matching tests
;; (prn (= (valid-pairs "[][[[[[[]]]]]]") {0 1, 7 8, 6 9, 5 10, 4 11, 3 12, 2 13}))
;; (prn (= (valid-pairs "[][][][]") {0 1, 2 3, 4 5, 6 7}))
;; (prn (= (valid-pairs "[a]") {0 2}))
;; (prn (= (valid-pairs "[a]asd[[ds[gf]hg]]") {0 2, 10 13, 7 16, 6 17}))
;; (prn (= (valid-pairs "sdfbg") {}))
;; (prn (= (valid-pairs "") {}))
;; (prn (= (valid-pairs "[[[[[[[[]") false))
