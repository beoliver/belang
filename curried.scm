

(define (curry arity proc)
  (define (curried proc arity args)
    (if (<= arity 0)
        (apply proc args)
        (lambda xs
          (curried proc (- arity (length xs)) (append args xs)))))
  (curried proc arity '()))

(define kons
  (curry 3 (lambda (a b selector) (selector a b))))

(define kar
  (lambda (kons-cell) (kons-cell (lambda (a b) a))))

(define kdr
  (lambda (kons-cell) (kons-cell (lambda (a b) b))))



(define binary-add
  (curry 2 (lambda (a b) (+ a b))))
  
(define add5 (binary-add 5))
