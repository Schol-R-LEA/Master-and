#!r6rs
(import 
 (rnrs)
 (rnrs base)
 (srfi srfi-64)
 (master-and dummy))


(define runner (test-runner-simple))

(test-with-runner runner 
		  (test-group "Test module import-export"
			      (test-group "Test (foo)"
					  (test-equal 6 (foo 3)))))
