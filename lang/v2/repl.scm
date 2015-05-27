;; a bit messy, and lots of debug messages, but we appear to be able to pattern match on lists

(define primitives
  `((list . ,list)
    (cons . ,cons)
    (nil  . ,(list))))

(define global-env (list primitives))

(define (lookup k env)
  (if (null? env) #f
      (or (assoc k (car env))
	  (lookup k (cdr env)))))

(define (update-frame! k v env)
  (let ((kv (assoc k (car env))))
    (if kv
	(set-cdr! kv v)
	(set-car! env (cons (cons k v) (car env))))))

(define (parse-fun-branch head body)
  ;; return (head . body)
  (if (eq? (car body) '->)
      (cons (reverse head) (cdr body))
      (parse-fun-branch (cons (car body) head) (cdr body))))

(define (parse-fun-branches branches)
  (map (lambda (b) (parse-fun-branch '() b)) branches))

(define (special-form-fun exp env)
  (let* ((branches (cdr exp))
	 (parsed-branches (parse-fun-branches branches))
	 (arity (length (car (car parsed-branches))))
	 (fun-object
	  ;; (<curried-fun> number <branches> passed-arguments environment)
	  (list '<curried-fun> arity parsed-branches (list) (cons (list) env))))
    (display "arity based on: ") (display (car (car parsed-branches))) (newline)
    ;; (for-each (lambda (branch) ;; allow for annonymous recursion
    ;; ONLY NEED TO DO THIS AFTER SELECTING A SUITABLE PATTERN
    ;; 		(set-cdr! branch (list (cons 'loop fun-object))))
    ;; 	      parsed-branches)
    fun-object))

(define (self-evaluating? x)
  (or (number? x) (boolean? x)))

(define (special-form-fun? x)
  (and (list? x) (eq? (car x) 'fun)))

(define (special-form-define? x)
  (and (list? x) (eq? (car x) 'define)))

(define (curried-fun? x)
  (and (list? x) (eq? (car x) '<curried-fun>)))

(define (callable-fun? x)
  (and (list? x) (eq? (car x) '<callable-fun>)))

(define curried-fun-arity cadr)
(define curried-fun-branches caddr)
(define curried-fun-args cadddr)
(define (curried-fun-env x) (car (cddddr x)))

(define (meta-eval exp env)
  (cond ((self-evaluating? exp) exp)
	;; ((eq? exp 'nil) (apply list '()))
	((symbol? exp) (cdr (lookup exp env)))
	((and (list? exp) (eq? 'quote (car exp)) (list? (car (cdr exp)))) (display "problems with quoted lists and pattern matching at the moment"))
	((curried-fun? exp) exp)
	((callable-fun? exp) exp)
	((special-form-fun? exp) (special-form-fun exp env))
	((special-form-define? exp)
	 (let ((evaluated-expression (meta-eval (caddr exp) env)))
	   (update-frame! (cadr exp) evaluated-expression env)))
	((pair? exp)
	 (let ((proc (meta-eval (car exp) env))
	       (args (map (lambda (x) (meta-eval x env)) (cdr exp))))
	   (meta-apply proc args)
	   ))))
	 
(define (meta-eval-sequence xs env)
  ;; (display "meta-eval-sequence")(newline)
  (if (null? (cdr xs))
      (meta-eval (car xs) env)
      (begin (meta-eval (car xs) env)
	     (meta-eval-sequence (cdr xs) env))))

(define (meta-apply proc args)
  (display "proc passed")
  (newline)  
  (cond ((curried-fun? proc) (display "proc is curried") (newline) (partially-apply proc args))
	((procedure? proc) (apply proc args))
	(else (display "proc is not curried"))))

(define (partially-apply proc args)
  (let ((arity (curried-fun-arity proc))
	(arg-count (length args)))
    (cond ((and (null? args) (> arity 0)) "error... missing arguments")
	  ((= arg-count arity)
	   (display "arg-count == arity")
	   (newline)
	   (let ((callable-fun
		  (list '<callable-fun>
			0
			(curried-fun-branches proc)
			(append (curried-fun-args proc) args)
			(curried-fun-env proc))))
	     (expand-and-eval callable-fun)))
	  (else 
	   (list '<curried-fun>
		 (- arity arg-count)
		 (curried-fun-branches proc)		 
		 (append (curried-fun-args proc) args)
		 (curried-fun-env proc))))))
      
(define (expand-and-eval callable-fun)
  (display "expand and eval") (newline)
  (let ((branches (curried-fun-branches callable-fun))
	(args (curried-fun-args callable-fun))
  	(env (curried-fun-env callable-fun)))
    (display args)
    (newline)
    (for-each (lambda (x) (display "branch: ") (display x) (newline)) branches)
    (display "MATCHING...")
    (display (match branches args))
    (newline)
    (display "valid match -> ")
    (display (test-and-reduce-matched-pairs (match branches args)))
    (newline)
    (let* ((matched-branch (test-and-reduce-matched-pairs (match branches args)))
	   (params (car matched-branch))
	   (args (cadr matched-branch))
	   (new-frame (map cons params args))
	   (body (caddr matched-branch)))
       	 (meta-eval-sequence body (cons new-frame env)))
      
    ;; now need to pattern match against branches
    ))

(define (match branches args)
  (call-with-current-continuation
   (lambda (return)
     (for-each (lambda (branch)
		 (let* ((params  (car branch))
			(body    (cdr branch))
			(matched (test-matched-pairs (match-branch-params params args))))
		   (if matched
		       (let ((lambda-params (map car matched))
			     (lambda-args   (map cdr matched)))
			 (return
			  (list lambda-params lambda-args body)
			  ;; (cons (cons 'lambda (cons lambda-params body)) lambda-args)
			  )))))
	       branches)
     #f)))

(define (match-branch-params params args)
  (if (and (null? params) (null? args))
      '()
      (let* ((param (car params))
	     (arg   (car args))
	     (match-list (match-elem param arg)))
	(append match-list (match-branch-params (cdr params) (cdr args))))))

(define (test-and-reduce-matched-pairs xs)
  (let ((ys '()))
    (call-with-current-continuation
     (lambda (return)
       (for-each (lambda (x)
		   (cond ((eq? x #f) (return #f))
			 ((not (eq? x #t)) (set! ys (cons x ys))))) xs)
       (reverse ys)))))

	 	  

(define (match-elem param arg)
  (cond ((or (number? param)(boolean? param)(char? param)) (list (eq? param arg)))
	((string? param) (list (equal? param arg)))
	((and (list? param) (null? param)) (list (eq? param arg)))
	((and (list? param) (eq? 'quote (car param))) (list (eq? arg (car (cdr param))))) ;; literal quote
	((symbol? param) (list (cons param arg))) ;; binable
	((and (pair? param) (pair? arg)) (list-binder param arg)) ;; lists
	(else (list #f))
	))

(define (list-binder xs ys)
  ;; xs can be dotted, ys can not
  (cond ((and (null? xs)  (null? ys)) (list)) ;; end of the lists
	((and (null? xs) (not (null? ys))) (list #f))
	((not (pair? xs)) (cons (cons xs (cons 'list ys)) (list)))
	(else (let ((x (car xs))
		    (y (car ys)))
		(append (match-elem x y) (list-binder (cdr xs) (cdr ys)))))))


(define (repl)
  (display "\n beLANG> ")
  (let ((x (meta-eval (read) global-env)))
    (if (and (list? x) (not (null? x)))
	(case (car x)
	  ((<curried-fun>) (display (list '<curried-fun> (curried-fun-arity x))))
	  ((<callable-fun>) (display '<callable-fun>))
	  (else (display x)))
	(display x))
    (repl)))

