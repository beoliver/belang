(fun 
  (<pattern> -> <body>)
  ...
  (<pattern> -> <body>))
  
;; currently

(guard
  (<pattern> <body>)
  ...
  (<pattern> <body>))
  
(guard 
  ((x y 10) (+ x y))
  ((x y 20) (display "twenty") (* x y)))
  
;; this will become...  
  
(fun 
  (x y 10 -> (+ x y))
  (x y 20 -> (display "twenty") (* x y)))
  

(define (parse-fun-clause head body)
  (if (eq? (car body) '->)
    (cons (reverse head) (cdr body))
    (parse-fun-clause (cons (car body) head) (cdr body))))

(define (sf-fun x)
  (let* ((clauses (cdr x))
         (parsed-clauses (map (lambda (clause) (parse-fun-clause (list) clause)) clauses)))
    (sf-guard (cons 'guard parsed-clauses)))) ;; now eval and convert to lambdas


;; we will also alow for named lambdas
;; is there a nicer way of calling a lambda 

((fun (x) -> x) 1)

(fun (x) -> x) 1

(define example
  '(lambda (n)
     (display n) 
     (newline)
     (if (not (= n 0))
	 (this (- n 1)))))
	 
(define (sf-lambda lambda-exp env)
  (let* ((this-env (cons '() env))
	 (this-closure (list 'closure (cadr lambda-exp) (cddr lambda-exp) this-env)))
    (set-car! this-env (cons (cons 'this this-closure) (car this-env)))
    this-closure))




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







