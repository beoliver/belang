

(define map*
  (let ((internal-values '()))
    (lambda (proc)
      (define (proc-mapper xs)
	(if (eq? xs 'show-internal-values)
	    internal-values
	    (begin
	      (set! internal-values (cons (map proc xs) internal-values))
	      proc-mapper)))
      proc-mapper)))
      

;; 4> (define adder (map* (lambda (x) (+ x 1))))
;; ; no values returned
;; 4> (adder '(1 2 3 4 5))
;; #{procedure 8610 (proc-mapper in map*)}
;; 4> (adder '(5 6 7 8 9))
;; #{procedure 8610 (proc-mapper in map*)}
;; 4> (adder 'show-internal-values)
;; ((6 7 8 9 10) (2 3 4 5 6))
;; 4> (adder '(10 11 12 13 14))
;; #{procedure 8610 (proc-mapper in map*)}
;; 4> (adder 'show-internal-values)
;; ((11 12 13 14 15) (6 7 8 9 10) (2 3 4 5 6))
;; 4> 
