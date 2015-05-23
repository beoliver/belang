(define primitives
  ;; loaded as the first frame of the environment
  `((eq? . ,eq?) (= . ,=) (+ . ,+) (- . ,-) (* . ,*) 
    (/ . ,/) (nil . nil) (newline . ,newline) 
    (display . ,display) (id . ,(lambda (x) x))))

(define (lookup k env)
  (if (null? env)
      #f
      (or (assoc k (car env))
	  (lookup k (cdr env)))))

(define (define-var! k v env)
  (let ((kv (assoc k (car env))))
    (if kv
	(set-cdr! kv v)
	(set-car! env (cons (cons k v) (car env))))))

;; ----------------------------------------------------------------
;; ----------------------------------------------------------------
(define (sf-define sf env)
  (let ((var (cadr sf))
	(val (caddr sf)))
    (define-var! var (meta-eval val env) env)))
;; ----------------------------------------------------------------

;; ((fun (0 -> (display 0))
;;       (n -> (display n)
;;             (newline)
;;             (loop (- n 1)))) 10)

(define (make-fun/n exp env)
  ;; define some helpers...
  ;; parse-fun-clause
  (define (parse-fun-clause head body)
    (if (eq? (car body) '->)
	(cons (reverse head) (cdr body))
	(parse-fun-clause (cons (car body) head) (cdr body))))
  ;; parse-fun-exp-body
  (define (parse-fun-exp-body xs)
    (map (lambda (x) (parse-fun-clause '() x)) xs))
  ;; make-fun-clause
  (define (make-fun-clause param-body-pair env)
    `(<fun-branch> ,(car param-body-pair) ,(cdr param-body-pair) ,env))
  ;; BODY OF MAKE-FUN/N
  (let* ((param-body-pairs (parse-fun-exp-body (cdr exp)))
	 (extended-env (cons (list) env))
	 (clauses (map (lambda (pbp) 
			 (make-fun-clause pbp extended-env)) 
		       param-body-pairs))
	 (fun (cons '<fun> clauses)))
    ;; now set a pointer called 'recur in each clause environment (head frame)
    ;; that points to the top level <fun>. this allows us to 
    ;; recurively match patterns without defining a named function
    ;; in the global space
    (for-each (lambda (clause)
		(set-car! (cadddr clause) 
			  (cons (cons 'loop fun)
				(car (cadddr clause)))))
	      clauses)
    fun))


;; ----------------------------------------------------------------
;; ----------------------------------------------------------------

(define (tagged? tag xs)
  (and (list? xs)
       (eq? tag (car xs))))

(define (self-evaluating? x)
  (or (number? x) (boolean? x) (string? x) (char? x) (tagged? 'quote x)))

(define (primitive-procedure? x)
  (procedure? x))

(define (fun-internal? x)
  ;; internal representation of lambdas
  (tagged? '<fun> x))

(define (callable? x)
  (tagged? 'callable x))


(define (fun-exp? x)
  ;; internal representation of lambdas
  (tagged? 'fun x))

(define (fun-internal-branch? x)
  ;; many internal lambdas under one definition
  (tagged? '<fun-branch> x))

(define (special-form? x)
  (and (list? x) (assoc (car x) special-forms)))

(define special-forms 
  `((fun . ,make-fun/n)
    (define . ,sf-define)))

(define (eval-special-form exp env)
  (let ((special-form (cdr (assoc (car exp) special-forms))))
    (special-form exp env)))

;; ----------------------------------------------------------------
;; ----------------------------------------------------------------	 

(define (meta-eval x env)
  ;; (display "meta-eval")(newline)
  (cond ((self-evaluating? x) x)
	((symbol? x) (cdr (lookup x env)))
	((special-form? x) (eval-special-form x env))
	(else ; application
	 (let ((evaluated-head (meta-eval (car x) env))
	       (evaluated-body (map (lambda (arg) (meta-eval arg env)) (cdr x))))
	   (meta-apply evaluated-head evaluated-body)))))

(define (meta-eval-sequence xs env)
  ;; (display "meta-eval-sequence")(newline)
  (if (null? (cdr xs))
      (meta-eval (car xs) env)
      (begin (meta-eval (car xs) env)
	     (meta-eval-sequence (cdr xs) env))))

(define (meta-apply x args)
  ;; (display "meta-apply")(newline)
  (cond ((primitive-procedure? x) (apply x args))
	((fun-internal? x)
	 (let ((branch-curry (curry-fun-branches x args)))
	   (if (callable? branch-curry) ; we found a match
	       (meta-eval-sequence 
		(branch-body branch-curry) 
		(branch-env branch-curry))
	       branch-curry)))))

(define (curry-fun-branches fun args)
  (let ((branches (cdr fun))
	(new-partial-branches (list)))
    (call-with-current-continuation
     (lambda (return)
       (for-each (lambda (branch)
		   (let ((new-branch (curry-fun-branch branch args)))
		     (cond ((eq? new-branch 'curry-pattern-err) 'do-nothing)
			   ((callable? new-branch)
			    (return new-branch)) ; return IMMEDIATELY as we are good
			   (else (set! new-partial-branches
				       (cons new-branch new-partial-branches))))))
		 branches)
       (if (null? new-partial-branches)
	   (display "NO PATTERN MATCHES EXPRESSION")
	   (cons '<fun> (reverse new-partial-branches)))))))

(define branch-params cadr)
(define branch-body caddr)
(define branch-env cadddr)

(define (curry-fun-branch branch args)
  (let ((params (branch-params branch))
	(body (branch-body branch))
	(env (branch-env branch))
	(new-frame (list)))
    (call-with-current-continuation
     (lambda (return)
       (for-each (lambda (param arg)
		   (cond ((and (self-evaluating? param) ; param is num,bool,char etc
			       (not (equal? param arg))); arg is NOT identical!
			  (return 'curry-pattern-err)); return error
			 ((and (self-evaluating? param) ; param is num,bool,char etc
			       (equal? param arg))      ; arg is identical
			  (set! params (cdr params)))   ; remove param as it matched
			 (else 
			  (set! params (cdr params))    ; remove param as it matched
			  (set! new-frame               ; add binding to new-frame
				(cons (cons param arg)
				      new-frame)))))
		 params args)
       (cond ((null? params) `(callable () ,body ,(cons new-frame env)))
	     (else `(<fun-branch> ,params ,body ,(cons new-frame env))))))))

;; ----------------------------------------------------------------
;; ----------------------------------------------------------------

(define (repl)
  (display "\n beLANG> ")
  (let ((x (meta-eval (read) global-env)))
    (if (list? x)
	(case (car x)
	  ((<fun>) (display '<fun>))
	  ((<clause> (display '<clause>))))
	(display x))
    (repl)))

;; ----------------------------------------------------------------	 
    
(define global-env (list '() primitives))

;; ----------------------------------------------------------------	 
;; tests    

;; (define a '(fun (-> 'constant)))

;; (define x '(fun (1 2 -> 1)
;; 		(x y -> (+ x y))))

;; (define f 
;;   '(fun (0 -> (display 0))
;; 	       (n -> (display n)
;;               (loop (- n 1)))))
