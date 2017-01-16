(ns PEMDAS_REPL_Calculator.core
  (:require [clojure.string :refer [split]])
  (:gen-class :main true))

(defn -main
  "PEMDAS CALCULATOR REPL"
  [& args]

  (defn exp [x n]
    (reduce * (repeat n x)))

  (defn sin [x]
    (Math/sin x))

  (defn cos [x]
    (Math/cos x))

  (defn tan [x]
    (Math/tan x))

  (defn asin [x]
    (Math/asin x))

  (defn acos [x]
    (Math/acos x))

  (defn atan [x]
    (Math/atan x))

  (def ^{:private true} operators {'** 3
                                   'sin 2
                                   'cos 2
                                   'tan 2
                                   'arcsin 2
                                   'arccos 2
                                   'arctan 2
                                   '* 2
                                   '/ 2
                                   '+ 1
                                   '- 1})

  (def ^{:private true} unary-operators {'sin 2
                                         'cos 2
                                         'tan 2
                                         'arcsin 2
                                         'arccos 2
                                         'arctan 2})

  (def operators-func
    "supported operators and functions"
    {"+" +
     "-" -
     "*" *
     "/" /
     "**" exp
     "sin" sin
     "cos" cos
     "tan" tan
     "arcsin" asin
     "arccos" acos
     "arctan" atan})

  (def ^{:private true} operator? (set (keys operators)))
  (def ^{:private true} unary-operator? (set (keys unary-operators)))

  (defn- lower-precedence? [leftop rightop]
    (< (operators leftop) (operators rightop)))

  (defn- equal-precedence? [leftop rightop]
    (= (operators leftop) (operators rightop)))

  (defn- left-paren? [p]
    (= p (symbol "(")))

  (defn- right-paren? [p]
    (= p (symbol ")")))

  (def ^{:private true}
    operator-strs (set (map str (keys operators))))

  (def instructions
    (str "\n/***************PEMDAS REPL CALCULATOR***************/\nBy Elton Vinh\nPlease enter an arithmetic expression separated by spaces.\ni.e.  sin ( 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3 )\nSupports +, -, *, /, ^, sin, cos, tan, arcsin, arccos, arctan\n\nCalculator also supports the use of variable storage and assignment.
         \nType 'assignments' to see variables stored.\nType 'exit' to exit.\n"))

  (defn- error
    ([]    (error instructions))
    ([msg] (str "ERROR: " (if (nil? msg)
                           instructions
                           msg))))

  (defn parse [s]
    (cond
      (operator-strs s) (symbol s)
      (= s "(") (symbol s)
      (= s ")") (symbol s)
      (= s "^") (symbol "**")
      :else (Double/parseDouble s)))

  (defn parse-assignment [s]
    (cond
      (operator-strs s) (symbol s)
      (= s "(") (symbol s)
      (= s ")") (symbol s)
      (= s "^") (symbol "**")
      (nil? (re-find #"\d+" s)) (str s)
      :else (Double/parseDouble s)))


  (defn infix-to-RPN
    "converts infix expression to postfix(reverse polish notation)"
    [input]
    (try
      (loop [params (map parse-assignment (split input #"\s+"))
             numbers []
             stack '()]
        (let [p (first params)]
          (if (empty? params)
            (if (not (empty? stack))
              (concat numbers stack)
              numbers)
            (cond
              (left-paren? p) (recur (rest params) numbers (conj stack p))
              (right-paren? p) (let [ops (take-while (comp not left-paren?) stack)
                                        ret (drop-while (comp not left-paren?) stack)]
                                  (try
                                    (assert (= (first ret) (symbol "(")))
                                    (catch java.lang.AssertionError ar
                                      (error (.getMessage ar))))
                                  (recur (rest params) (into [] (concat numbers ops)) (rest ret)))
              (operator? p) (if (operator? (peek stack))
                              (if (or (and (= p (symbol "**"))
                                           (lower-precedence? p (peek stack)))
                                      (and (not= p (symbol "**")) (or (lower-precedence? p (peek stack)) (equal-precedence? p (peek stack)))))
                                (recur (rest params) (conj numbers (peek stack)) (conj (rest stack) p))
                                (recur (rest params) numbers (conj stack p)))
                              (recur (rest params) numbers (conj stack p)))
              (number? p) (recur (rest params) (conj numbers p) stack)))))
      (catch java.lang.RuntimeException ex
        (error (.getMessage ex)))))

  (defn eval-RPN
    "evalulates rpn/postfix expressions"
    [expr]
    (letfn [(rpn-reduce [stack item]
                        (if (operator? item)
                          (if (unary-operator? item)
                            (if (= (count stack) 1)
                              (let [operand (peek stack)]
                                (conj (pop stack)
                                      ((operators-func (str item)) operand)))
                              (println "Mismatched amount of numbers and operators"))
                            (if (>= (count stack) 2)
                              (let [operand (peek stack)
                                  stack-1 (pop stack)]
                                (conj (pop stack-1)
                                    ((operators-func (str item)) (peek stack-1) operand)))
                              (println "Mismatched amount of numbers and operators")))
                          (conj stack item)))]
      (reduce rpn-reduce [] expr)))

  (defn assignment-operator
    "converts infix expression to postfix(reverse polish notation)"
    [input]
    (try
      (let [exprs (split input #"\s+")]
        (if (or (not= (second exprs) "=")
                (not (re-matches #"^[a-zA-Z0-9_\-]+$" (first exprs)))
                (> (get (frequencies exprs) "=") 1))
          (do
            (println "ERROR: Invalid variable name. Variable names may only contain alphanumerics or '_'")
            nil)
          (let [var (first exprs)
                assignment (second exprs)
                params (nthrest exprs 2)]
            (println exprs)
            (conj (eval-RPN (infix-to-RPN (clojure.string/join " " params))) var))))
      (catch java.lang.RuntimeException ex
        (error (.getMessage ex)))))

  (defn parse-bindings
    [input bindings]
    (let [params (split input #"\s+")]
      (clojure.string/join " " (replace bindings params))))

  (defn- main-loop []
    (print instructions)
    (loop [bindings {}]
      (print "=> ") (flush)
      (let [inp (read-line)]
        (println inp)
        (when (= inp "exit") (do (println "Bye for now!")
                           (System/exit 0)))
        (cond
          (.contains inp "=") (let [parsed-inp (str (subs inp 0 (clojure.string/index-of inp "=")) (parse-bindings (subs inp (clojure.string/index-of inp "=")) bindings))]
                                (let [assignment (assignment-operator parsed-inp)]
                                  (if (not (nil? assignment))
                                    (recur (assoc bindings (second assignment) (first assignment)))
                                    (recur bindings))))
          (= inp "assignments") (do
                                 (println "assignments: "bindings)
                                 (recur bindings))
          :else (do
                    (println "\t= "(eval-RPN (infix-to-RPN (parse-bindings inp bindings))))
                    (recur bindings)))
      )
    )
  )

  (main-loop)

)
