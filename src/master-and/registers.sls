#!r6rs

(library (leg assembly-record)
  (export
   register-8 register-8-set
   )
  
  (import (rnrs base (6))
          ;; composite standard library, imports most std libs
          (rnrs (6)))

  (define-enumeration register-8
    (a x y)
    register-8-set))
