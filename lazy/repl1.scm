
(define (self-evaluating? x)
  (or (number? x) 
      (boolean? x)
      (string? x)
      (char? x)))

(define (meta-eval exp env)
  (cond ((special-form? exp) (eval-special-form exp env))
	((symbol? exp) (cdr (lookup exp env)))
	((self-evaluating? exp) exp)
	(else (cons '<thunk> exp))))

(define (lookup k env)
  (if (null? env)
      #f
      (or (assoc k (car env))
	  (lookup k (cdr env)))))

(define (define-var! k v env)
  (let* ((frame (car env))
	 (kv (assoc k frame)))
    (if (or (null? frame) (not kv))
	(set-car! env (cons (cons k v) frame))
	(set-cdr! kv v))))

(define (special-form? x)
  (and (list? x)
       (assoc (car x) special-forms)))

(define (special-form-define exp env)
  (let ((evaluated-body (meta-eval (caddr exp) env)))
    (display evaluated-body)
    (newline)
    (define-var! (cadr exp) evaluated-body env)))
  
(define special-forms
  `((define . ,special-form-define)))

(define (eval-special-form exp env)
  (let ((form (cdr (assoc (car exp) special-forms))))
    (form exp env)))

(define global (list '()))


3> /Users/boliver/Desktop/lazy.scm
; no values returned
3> (meta-eval '(define foo 10) global)
10
#{Unspecific}
3> (meta-eval '(define bar foo) global)
10
#{Unspecific}
3> (meta-eval '(define foo 20) global)
20
#{Unspecific}
3> (meta-eval 'foo global)
20
3> (meta-eval 'bar global)
10
3> 
