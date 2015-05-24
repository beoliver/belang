
(define foo
  (fun (x -> 
    ((fun -> (display x)
             (newline))))))

(define bar
  (fun (x ->
    ((fun (x -> (display x)
                (newline))) (+ x x)))))

(define baz
  (fun (0 -> (display "the end"))
       (n -> (display n) 
             (newline) 
             (baz (- n 1)))))


((fun (0 -> (display "let's GO!"))
      (n -> (display n) 
            (newline) 
            (loop (- n 1)))) 3)


(define infiniteA
  (fun -> (display "infinite...")
          (loop)))

(define infiniteB
  (fun -> (display "infinite...")
          (infiniteB)))


;; for numbers from 10 to 0 
;; print '<number> is even" when even.

((fun (0 -> (display "0 is even")) ; base case
      (n -> ((fun (0 -> (display n) ; modulo n 2 == 0
                        (display " is even")
                        (newline))
                  (_ -> (display ""))) (modulo n 2))
            (loop (- n 1)))) 
 10)



