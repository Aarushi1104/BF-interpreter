(ns bf.interpreter
  (:require [clojure.string :as str])
  (:require clojure.set))

; "pointer" here means "memory/data pointer"
(def init-pointer 0)
(def tape-size 300)
(def init-tape (vec (repeat tape-size 0)))
(def init-code-ptr 0)
(def help-message "Usage:")

(defn read-file [path]
  (-> path
      slurp
      str/trim-newline))

(defn read-byte []
  (int (.read System/in)))

(defn machine-constraints [tape ptr]
  (and (< -1 ptr tape-size)
      ;;  (every? (comp not neg?) tape)
       true))

(defn apply-at [f]
  (fn [index v]
    (let [elem (get v index)]
      (assoc v index (f elem)))))

(def inc-at (apply-at inc))
(def dec-at (apply-at dec))

(defn bimap [a-map]
  (merge a-map (clojure.set/map-invert a-map)))

(defn init-state [input]
  {:stack []
   :idx 0
   :input input
   :match {}})

(defn handle-open-bracket [state]
  (let [{:keys [stack idx input match]} state
        [x & xs] input]
    {:stack (conj stack [x idx])
     :idx (inc idx)
     :input xs
     :match match}))

(defn handle-close-bracket [state]
  (let [{:keys [stack idx input match]} state
        [x & xs] input
        top (peek stack)]
    (if (and (not (empty? stack)) (= (first top) \[))
      {:stack (pop stack)
       :idx (inc idx)
       :input xs
       :match (assoc match (second top) idx)}
      false)))

(defn process-input [state]
  (let [{:keys [stack idx input match]} state
        [x & xs] input]
    (cond
      (empty? input)
      (if (empty? stack) match false)

      (= \[ x) (handle-open-bracket state)

      (= \] x) (let [new-state (handle-close-bracket state)]
                 (if new-state
                   new-state
                   false))

      :else
      {:stack stack
       :idx (inc idx)
       :input xs
       :match match})))

(defn valid-pairs [input]
  (loop [state (init-state input)]
    (if (or (empty? (:input state)) (false? state))
      (if (or (false? state) (not (empty? (:stack state))))
        false
        (:match state))
      (recur (process-input state)))))


(defn interpret [code tape ptr code-ptr]
  (assert (machine-constraints tape ptr))
  (let [token (get code code-ptr)
        matching (bimap (valid-pairs code))]
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

(defn main []
  (let [first-arg (first *command-line-args*)]
    (cond
      (nil? first-arg) (print help-message)
      :else            (print (bf first-arg)))))

(main)


; testing
; (-> "../tests/test3.bf" bf prn)

; bracket matching tests
;; (prn (= (valid-pairs "[][[[[[[]]]]]]") {0 1, 7 8, 6 9, 5 10, 4 11, 3 12, 2 13}))
;; (prn (= (valid-pairs "[][][][]") {0 1, 2 3, 4 5, 6 7}))
;; (prn (= (valid-pairs "[a]") {0 2}))
;; (prn (= (valid-pairs "[a]asd[[ds[gf]hg]]") {0 2, 10 13, 7 16, 6 17}))
;; (prn (= (valid-pairs "sdfbg") {}))
;; (prn (= (valid-pairs "") {}))
;; (prn (= (valid-pairs "[[[[[[[[]") false))
;; (prn (bimap (valid-pairs "[][[[[[[]]]]]]")))

;;;;;;;;;;;;;;;;;;;;;; FOR TESTING ;;;;;;;;;;;;;;;;;;;;;;
(def testing true)
(def test-dir "tests/")
(def tests {"test1.bf" [1]
            "test2.bf" [0 7 3]
            "test3.bf" [8 0 1 0 5]
            "helloworld.bf" [0 87 100 33 10]
            "one_to_10.bf" [48, 10]
            "print50.bf" [5]
            ;; "beer.bf" [0, 119, 118, 117, 116, 115, 114, 113, 112, 111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 101, 100, 99, 98, 97, 84, 46, 44, 32, 10]
            "square.bf" [0, 53, 0, 0, 0, 0, 2, 1, 0, 6, 1, 0, 7, 1, 0, 6, 1]
            "42.bf" [0, 8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120, 128]
            "triangles.bf" [0, 10, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
            })

(defn trim-tape [tape]
  "Takes the tape (as a vector) and removes all
   trailing zeros to the right of the tape."
  (loop [index (.length tape)]
    (if (zero? index)
      []
      (if (zero? (get tape (dec index)))
        (recur (dec index))
        (vec (take index tape)))))) ; convert list to vector

(defn run-test [test expected-tape]
  "Runs an individual test .bf file, prints the
   results to the terminal and then evaluates to nil.
   The final state of the tape is compared to the expected
   correct tape to determine whether the test passed."
  (println (str "Running test \"" test "\""))
  (let [result (trim-tape (bf (str test-dir test)))
        passed (= expected-tape result)]
    ; (println "Tape:\n" result)
    (println (if passed "passed" "failed") "\n")))

(defn run-all-tests []
  (doseq [[test tape] tests] (run-test test tape)))

(when testing (run-all-tests))
