# belang

belang is currently built on top of a 'meta circular evaluator' in scheme. 

It is an exploration of how far one can push a 'lisp' before it is unreconisable (and suddenly a lot harder to parse)

The current implementation stands at about 200 loc and allows for:

1. curried procedures
2. pattern matching 
3. a slightly less lispy syntax

for example

`?> (define foo`                                                                      
      `(fun (x 1 -> "one")`
           `(x 2 -> "two")`
           `(x n -> n)))`
`#{Unspecific}`

?> (define bar (foo 100))                                                              
#{Unspecific}                                                                           

?> (bar 2)                                                                             
two
