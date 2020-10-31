#!r6rs
(import 
 (rnrs (6))
 (rnrs base (6))
 (rnrs io simple (6))
 (srfi srfi-64)
 (rnrs bytevectors (6))
 (master-and opcodes))


(define runner (test-runner-simple))


(test-with-runner runner 
		  (test-group "Test nullaries"
			      (test-group "Test make-nullary"
					  (let ((zero-test (make-nullary 0)))
					    (test-assert (list (make-bytevector 1 0)) (zero-test)))))
                  (test-group "Test general opcodes"
                              (test-group "Test make-opcode"
					  (let ((foo (make-opcode #b11100000 (ind-zp-x zp imm abs ind-zp-y zp-x abs-y abs-x) "no-op")))
					    (test-equal 
                                                (list (make-bytevector 1 #b11111100) 
                                                      (make-bytevector 1 3)) 
                                              (foo 'abs-x 3))))))
