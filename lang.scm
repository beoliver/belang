
-- this is a comment

-- below is is an example of an annonymous procedure

fun <pattern 1> <body 1>
  | <pattern 2> <body 2>
  | ...
  | <pattern N> <body N>

-- every <body> is a single expression, if we wish to do things in "sequence"
-- we can use a special term e.g 'begin' or 'do' or 'progn' etc...

fun ('hello 13) "wowowowow"
  | ('what x) (+ x x)
  | ('bye reason) 
        begin display "time to say good bye" >
              newline >
              display reason

-- as "begin" is a keyword, we know what what follows is a series expressions
-- instead of (begin (expr1 ...) (expr2 ...) (expr3 ...))
-- we can a "seperator symbol" eg.  $  or >
-- begin expr1 ... > expr2 ... > expr3 ...

-- what about it we wanted to nest procedures in the sequential expression?

(begin 
  (display (+ 1 2)) 
  (newline) 
  (display (* (+ 1 2) 3)) 
  (newline))

begin 
  display (+ 1 2)
  newline
  display (* (+ 1 2) 3)
  newline



  
  















