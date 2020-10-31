#!r6rs

(library 
    (master-and opcodes-backup (1))
  (import 
    (rnrs (6))
    (rnrs base (6)))
  (export
   foo)


  (define (foo x) (* 2 x)))
