# belang

belang is currently built on top of a 'meta circular evaluator' in scheme. 

The current implementation stands at about 200 loc and allows for:

1. curried procedures
2. pattern matching 
3. a slightly less lispy syntax

for example

```
?> (define foo
      (fun (x 1 -> "one")
           (x 2 -> "two")
           (x n -> n)))
#{Unspecific}

?> (define bar (foo 100))                                                              
#{Unspecific}                                                                           

?> (bar 2)                                                                             
two

```

named recursion

```
 ?> (define baz
      (fun (0 -> (display "the end"))
           (n -> (display n) 
                 (newline) 
                 (baz (- n 1)))))
           
#{Unspecific}
 ?> (baz 5)
5
4
3
2
1
the end#{Unspecific}
```
annonymous recursion. At the moment it looks like this:

```
 ?> ((lambda* (n)
      (if (= 0 n)
        "finished"
        (this (- n 1))))
     10)
finished

```
Which works by storing a ref to 'this' in every unique lamnda environment.
Ideally it would allow for pattern matching against the recursive calls, 
