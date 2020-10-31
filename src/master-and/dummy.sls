#!r6rs

(library 
    (master-and dummy)
  (export
   foo)
  (import 
    (rnrs)
    (rnrs base))



  (define (foo x) (* 2 x)))
