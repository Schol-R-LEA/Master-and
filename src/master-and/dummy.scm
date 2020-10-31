#!r6rs
(library 
    (master-and dummy)
  (import 
    (rnrs)
    (rnrs base))
  (export
   foo)

  (define (foo x) (* 2 x)))
