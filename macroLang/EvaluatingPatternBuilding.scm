;; problems with eval, but in theory we can make the trnasformations
;; see bottom of file for examples

(define-syntax fun*
  (syntax-rules ()
    ((_ branch ...)
     (let* ((branches (map parse-branch `(branch ...)))
	    (arity (length (car (car branches)))))
       (fun-curry arity branches)))))

(define (parse-branch exp)
  (define (loop params rest)    
    (if (eq? (car rest) '->)
	(cons (reverse params) (cdr rest))
	(loop (cons (car rest) params) (cdr rest))))
  (loop '() exp))

(define (fun-curry arity unevaluated)
  (define (loop passed arity)
    (lambda args
      (let ((arity (- arity (length args)))
	    (args (append passed args)))
	(if (<= arity 0) (match unevaluated args) (loop args arity)))))
  (loop '() arity))

(define branch-params car)
(define branch-body cdr)

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
			  (cons (cons 'lambda (cons lambda-params body)) lambda-args)
			  )))))
	       branches)
     'error-in-match)))

(define (test-matched-pairs xs)
  (let ((ys '()))
    (call-with-current-continuation
     (lambda (return)
       (for-each (lambda (x)
		   (cond ((eq? x #f) (return #f))
			 ((not (eq? x #t)) (set! ys (cons x ys))))) xs)
       ys))))
	 

(define (match-branch-params params args)
  (if (and (null? params) (null? args))
      '()
      (let* ((param (car params))
	     (arg   (car args))
	     (match-list (match-elem param arg)))
	(append match-list (match-branch-params (cdr params) (cdr args))))))
	
(define (match-elem param arg)
  (cond ((or (number? param)(boolean? param)(char? param)) 
	 (list (eq? param arg)))
	((string? param) 
	 (list (equal? param arg)))
	((and (list? param) (null? param)) 
	 (list (eq? param arg)))
	((and (list? param) (eq? 'quote (car param)))
	 (list (eq? arg (car (cdr param))))) ;; literal quote
	((symbol? param) 
	 (list (cons param arg))) ;; binable
	((and (pair? param) (pair? arg))
	 (list-binder param arg)) ;; lists
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

;; -------------------------------------------------------


(define foo
  (fun* (() -> '())
	((x . xs) -> (foo xs))))

(define bar
  (fun* (0 n -> 0)
	(m n -> (+ m n))))


(define baz1 
  (fun* ('foo () xs -> "three")))


(define baz2 
  (fun* ('foo (x . xs) ys -> "three")))


(define test1
  (fun* ((f . xs) -> (apply f xs))))

