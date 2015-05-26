
(define-syntax fun*
  (syntax-rules ()
    ((_ branch ...)
     (let* ((branches (map parse-branch `(branch ...)))
            (arity (length (car (car branches)))))
       (curry-fun arity branches)))))

(define (parse-branch exp)
  (define (loop params rest)    
    (if (eq? (car rest) '->)
      (cons (reverse params) (cdr rest))
      (loop (cons (car rest) params) (cdr rest))))
  (loop '() exp))

(define (curry-fun arity unevaluated)
  (define (loop passed arity)
    (lambda args
      (let ((arity (- arity (length args)))
            (args (append passed args)))
        (if (<= arity 0) 
          (evaluate-fun-branches unevaluated args) 
          (loop args arity)))))
  (loop '() arity))

(define (evaluate-fun-branches branches args)
  (if (null? branches)
    'error-in-fun-branches
    (begin
      (display "ARGS...\n")
      (display args)
      (newline)
      (display "BRANCHES...\n")
      (for-each (lambda (b)
        (display b)
        (newline)) 
          branches)
      (match branches args))))

(define (match branches args)
  (call-with-current-continuation
    (lambda (return)
      (for-each (lambda (branch)
        (let* ((params  (car branch))
               (body    (cdr branch))
               (matched (map match-elem params args)))
          (if (valid-match matched)
            (return matched)))) branches)
            'error-in-match)))
