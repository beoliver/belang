
(define (lookup k env)
  ;; (display "lookup")(newline)
  (if (null? env)
      #f
      (or (assoc k (car env))
	  (lookup k (cdr env)))))

(define (define-var! k v env)
  ;; (display "define-var!")(newline)
  (let ((kv (assoc k (car env))))
    (if kv
	(set-cdr! kv v)
	(set-car! env (cons (cons k v) (car env))))))

;; predicates

(define (tagged? tag xs)
  ;; (display "tagged?")(newline)
  (and (list? xs)
       (eq? tag (car xs))))

(define (self-evaluating? x)
  (or (number? x) (boolean? x) (string? x) (char? x) (tagged? 'quote x)))

(define (primitive-procedure? x)
  (procedure? x))

(define (compound-procedure? x)
  ;; internal representation of lambdas
  (tagged? 'procedure x))

(define (guarded-procedures? x)
  ;; many internal lambdas under one definition
  (tagged? 'guarded x))

(define (special-form? x)
  ;; (display "special-form?")(newline)
  (and (list? x) (assoc (car x) special-forms)))

;; special forms -- only for parsing

(define (sf-define sf env)
  (let ((var (cadr sf))
	(val (caddr sf)))
    (define-var! var (meta-eval val env) env)))

(define (sf-lambda sf env)
  (let ((params (cadr sf))
	(body (cddr sf)))
    `(procedure ,params ,body ,env)))

(define (sf-guard sf env)
  (let* ((untagged-lambdas (cdr sf))
	 (tagged-lambdas 
	  (map (lambda (x) 
		 (let* ((params (car x))
			(body   (cdr x)))
		   `(procedure ,params ,body ,env))) untagged-lambdas)))
    ;; (display tagged-lambdas) (newline)
    (cons 'guarded tagged-lambdas)))

(define (sf-fun sf env)
  (let* ((clauses (cdr sf))
         (parsed-clauses 
	  (map (lambda (clause) (parse-fun-clause (list) clause)) clauses)))
    (sf-guard (cons 'guard parsed-clauses) env))) ;; now eval and convert to lambdas

(define (parse-fun-clause head body)
  (if (eq? (car body) '->)
    (cons (reverse head) (cdr body))
    (parse-fun-clause (cons (car body) head) (cdr body))))

(define special-forms 
  `((define . ,sf-define)
    (lambda . ,sf-lambda)
    (fun .    ,sf-fun)
    (guard  . ,sf-guard))
    )

(define (eval-special-form sf env)
  ;; (display "eval-special-form") (newline)
  (let ((special-form (cdr (assoc (car sf) special-forms))))
    (special-form sf env)))

;; accessors 

(define procedure-params cadr)
(define procedure-body caddr)
(define procedure-env cadddr)

;; eval 

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

;; apply

(define (meta-apply x args)
  ;; (display "meta-apply")(newline)
  (cond ((primitive-procedure? x) (apply x args))
	((compound-procedure? x)
	 (if (null? (procedure-params x))
	     (meta-eval-sequence (procedure-body x) (procedure-env x))
	     (let ((y (curry-procedure x args)))
	       (if (null? (procedure-params y))
		   (meta-eval-sequence (procedure-body y) (procedure-env y))
		   y))))
	((guarded-procedures? x)
	 (let ((y (curry-guarded-procedures x args)))
	   (if (compound-procedure? y) ; we found a match
	       (meta-eval-sequence (procedure-body y) (procedure-env y))
	       y)))))

;; handling partial application (the meat of it)
;; curry procedure seems to work for normal lambdas

(define (curry-procedure proc args)
  ;; (display "curry-procedure")(newline)
  (let ((params (procedure-params proc))
	(body (procedure-body proc))
	(env (procedure-env proc))
	(new-frame (list)))
    ;; (display params)
    ;; (newline)
    ;; (display body)
    ;; (newline)
    (call-with-current-continuation
     (lambda (return)
       (for-each (lambda (param arg)
		   (cond ((and (self-evaluating? param) ; param is num,bool,char etc
			       (not (equal? param arg))); arg is NOT identical!
			  ;; (display "error matching ")   ; DEBUG
			  ;; (display (list param arg))    ; DEBUG
			  ;; (newline)                     ; DEBUG
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
       (if (null? new-frame) ; empty is all params were self evaluating
	   `(procedure ,params ,body ,env)
	   `(procedure ,params ,body ,(cons new-frame env)))))))


(define (curry-guarded-procedures proc args)
  ;; (display "curry-guarded-procedures")(newline)  
  (let ((procedures (cdr proc))
	(matching-procedures (list)))
    ;; (for-each (lambda (p)
    ;; 		(display "PROCEDURE") (newline)
    ;; 		;; (display p) (newline)
    ;; 		(display (procedure-params p)) (newline)
    ;; 		(display (procedure-body p)) (newline)) procedures)
    (call-with-current-continuation
     (lambda (return)
       (for-each (lambda (proc)
		   (let ((new-proc (curry-procedure proc args)))
		     (cond ((eq? new-proc 'curry-pattern-err) 'do-nothing)
			   ((null? (procedure-params new-proc))
			    (return new-proc)) ; return IMMEDIATELY as we are good
			   (else (set! matching-procedures
				       (cons new-proc matching-procedures))))))
		 procedures)
       (if (null? matching-procedures)
	   (display "NO PATTERN FOUND FOR GUARD")
	   (cons 'guarded (reverse matching-procedures)))))))

;; repl (avoid infinite loops)

(define (repl)
  (display "\n ?> ")
  (let ((x (meta-eval (read) global-env)))
    (cond ((compound-procedure? x) (display '<compound-procedure>))
	  ((guarded-procedures? x) (display '<guarded-procedures>))
	  (else (display x)))
    (repl)))

;; global state
(define global-env (list (list)))
