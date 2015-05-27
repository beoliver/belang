#version 2

```
(define map
  (fun (_ () -> '())
       (f (x . xs) -> (cons (f x) (map f xs)))))
```
