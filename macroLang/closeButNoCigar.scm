
;; getting very close, not quite though

;; 41> (bar 1 2)
;; ((lambda (n m) (+ m n)) 2 1)
;; 41> (foo '(1 2 3))
;; ((lambda ((_ . 1)) (foo xs)) ((xs 2 3)))
;; 41> 

;; (define (applicative proc)
;;     (define (loop state)
;;       (lambda xs
;; 	(if (null? xs)
;; 	    (apply proc state)
;; 	    (loop (append state xs)))))
;;     (loop '()))

;; (define (curry arity proc)
;;   (define (loop passed arity)
;;     (lambda args
;;       (let ((n (- arity (length args)))
;; 	    (xs (append passed args)))
;; 	(if (<= n 0) (apply proc xs) (loop xs n)))))
;;   (loop '() arity))

;; (define-syntax lambda*
;;   (syntax-rules ()
;;     ((_ params body)
;;      (if (symbol? `params)
;; 	 (applicative (lambda params body))
;; 	 (curry (length `params) (lambda params body))))))

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
	(if (<= arity 0) (evaluate unevaluated args) (loop args arity)))))
  (loop '() arity))

(define branch-params car)
(define branch-body cdr)

(define (evaluate branches args)
  (if (null? branches)
      'error
      (begin
	;; (display "ARGS...\n")
	;; (display args)
	;; (newline)
	;; (display "BRANCHES...\n")
	;; (for-each (lambda (b)
		    ;; (display b)
		    ;; (newline)) branches)
	(match branches args))))

(define (match branches args)
  (call-with-current-continuation
   (lambda (return)
     (for-each (lambda (branch)
		 (let* ((params  (car branch))
			(body    (cdr branch))
			(matched (map match-elem params args))
			(vmatched (valid-match matched)))
		   (if (valid-match matched)
		       (let ((body (branch-body branch))
			     (lambda-params (map car vmatched))
			     (lambda-args   (map cdr vmatched)))
			 (return (cons (cons 'lambda (cons lambda-params 
				       body))
				       lambda-args))))))
	       branches)
     'error-in-match)))

(define (valid-match xs)
  (let ((ys (list)))
    (call-with-current-continuation
     (lambda (return)
       (for-each (lambda (x)
		   (cond ((eq? x #f) (return #f))
			 ((not (eq? x #t)) (set! ys (cons x ys))))) xs)
       ys))))

(define (match-elem param arg)
  ;; match a single pram and arg
  ;; (display "param: ")(display param)
  ;; (newline)
  ;; (display "arg: ")(display arg)  
  ;; (newline)
  (cond ((or (number? param)(boolean? param)(char? param)) (eq? param arg))
	((string? param) (equal? param arg))
	((and (list? param) (null? param))(eq? param arg))
	((and (list? param) (eq? 'quote (car param)))
	 (eq? arg (car (cdr param)))) ;; literal quote
	((symbol? param) (cons param arg)) ;; binable
	((and (pair? param) (pair? arg))
	 (list-binder param arg)) ;; lists
	(else #f)
	))

(define (list-binder xs ys)
  ;; xs can be dotted, ys can not
  (cond ((and (null? xs)  (null? ys)) (list)) ;; end of the lists
	((and (null? xs) (not (null? ys))) (list #f))
	((not (pair? xs)) (cons (cons xs ys) (list)))
	(else (let ((x (car xs))
		    (y (car ys)))
		(cons (match-elem x y) (list-binder (cdr xs) (cdr ys)))))))

;; -------------------------------------------------------



(define foo
  (fun* (() -> ())
	((_ . xs) -> (foo xs))))

(define bar
  (fun* (0 n -> 0)
	(m n -> (+ m n))))


(define baz1 
  (fun* ('foo () xs -> "three")))


(define baz2 
  (fun* ('foo (1 . xs) ys -> "three")))


















