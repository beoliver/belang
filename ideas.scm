(fun 
  (<pattern> -> <body>)
  ...
  (<pattern> -> <body>))

// a lambda function  
(<pattern> -> <body>)

// would be fun if all functions were curried

(def <symbol> <body>)

(def add-1 (+ 1))

(+ 1)

// what about functions that do not have a fixed arity?
// (+ 1 2 3 4 5 ... n) == (apply + '(1 2 3 4 5 ... n))


// in belang like scheme, functions can have fixed arity or arbitrary arity...

// functions that have fixed arity are curried until the point that they become callable
// functions that have arbitrary arity are curried indefinitely... they are then passed to a call function

(def add +)

(def add' (+ 1 2 3 4))
(def add'' (add' 5 6 7 8))
((add''))

(define 
  (call-applied-function f)
    ((f)))

// prolog|erlang style pattern matching?

(def factorial
  (0 -> 1)
  (N -> (* N (factorial (- N 1)))))

(def cons
  (X Y ->
    (SELECTOR -> (SELECTOR X Y))))

(def car
  (CELL ->
    (CELL (X Y -> X))))







