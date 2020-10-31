#!r6rs

(import (rnrs bytevectors (6))
        (srfi srfi-60)
        (master-and opcodes)

(define assembler-data-init
  '((output '())
    (line-count 0)
    (current-address 0)
    (labels '())
    (equates '())
    (listing '())
    (errors '())
    (warnings '())))


(define make-nullary
  (lambda (opcode)
    (lambda ()
      (list (make-bytevector 1 opcode)))))


(define-syntax define-nullary
  (lambda (macro)
    (syntax-case macro ()
      ((_ <name> <opcode>)
       #'(define <name> (make-nullary <opcode>)))
      ((_ ((<name-n> <opcode-n>) ...))
       #`(begin (define <name-n> (make-nullary <opcode-n>)) ...)))))


(define-syntax define-opcode
  (syntax-rules (arith logic jump extended)   
    ((_ <name> <opcode> arith)
     (define (<name> subop value)
       (let* ((computed-subop 
               (case subop
                 ((z-indirect-x) (inexact->exact #b00000))
                 ((z-page) (inexact->exact #b00100))
                 ((abs) (inexact->exact #b01100))
                 ((z-indirect-y) (inexact->exact #b10000))
                 ((z-indexed-x) (inexact->exact #b10100))
                 ((abs-y) (inexact->exact #b11000))
                 ((abs-x) (inexact->exact #b11100))
                 (else (error "Invalid arithmetic indexing operation"))))
              (computed-opcode (bitwise-ior <opcode> (bitwise-ior computed-subop 1))))
         (list (make-bytevector 1 computed-opcode)
               (make-bytevector 1 value)))))
    ((_ <name> <opcode> jump)
     (define (<name> subop value)
       (let* ((computed-subop 
               (case subop
                 ((imm) (inexact->exact #b00000))
                 ((z-page) (inexact->exact #b00100))
                 ((abs) (inexact->exact #b01100))
                 ((z-page-x) (inexact->exact #b10100))
                 ((abs-x) (inexact->exact #b11100))
                 (else (error "Invalid jump indexing operation"))))
              (computed-opcode (bitwise-ior <opcode> computed-subop)))
         (list (make-bytevector 1 computed-opcode)
               (make-bytevector 1 value)))))
    ((_ <name> <opcode> logic)
     (define (<name> subop value)
         (let* ((computed-subop
                 (case subop
                   ((imm) (inexact->exact #b00000))
                   ((z-page) (inexact->exact #b00100))
                   ((acc) (inexact->exact #b01000))
                   ((abs) (inexact->exact #b01100))
                   ((z-page-x) (inexact->exact #b10100))
                   ((abs-x) (inexact->exact #b11100))
                   (else (error "Invalid logic indexing operation"))))
                (computed-opcode (bitwise-ior <opcode> (bitwise-ior computed-subop 2))))
           (list (make-bytevector 1 computed-opcode)
                 (make-bytevector 1 value)))))))


;;;
;;;
;;;

(define-nullary ((brk 0) (nop #xEA) 
                 (rts #x60) (rti #x40) 
                 (clc #x18) (sec #x38) (cld #xD8) (sed #xF8) (cli #x58) (sei #x78) (clv #xB8) 
                 (pha #x48) (pla #x68) (php #x08) (plp #x28) 
                 (inx #xE8) (iny #xC8) (dex #xCA) (dey #x88) 
                 (tax #xAA) (txa #x8A) (tay #xA8) (tya #x98) (tsx #xBA) (txs #x9A)))


(define jsr 
  (lambda (addr)
    (list (make-bytevector 1 #x20)
          (make-bytevector 1 addr))))

(define-opcode ora #b00000000 arith)
(define-opcode and-a #b00100000 arith)
(define-opcode eor #b01000000 arith)
(define-opcode adc #b01100000 arith)
(define-opcode sta #b10000000 arith)
(define-opcode lda #b10100000 arith)
(define-opcode cmp #b11000000 arith)
(define-opcode sbc #b11100000 arith)

(define-opcode asl #b00000000 logic)
(define-opcode rol #b00100000 logic)
(define-opcode asr #b01000000 logic)
(define-opcode ror #b01100000 logic)
(define-opcode stx #b10000000 logic)
(define-opcode ldx #b10100000 logic)
(define-opcode dec #b11000000 logic)
(define-opcode inc #b11100000 logic)

(define-opcode bit #b00100000 jump)
(define-opcode jmp #b01000000 jump)
(define-opcode jmp-a #b01100000 jump)
(define-opcode sty #b10000000 jump)
(define-opcode ldy #b10100000 jump)
(define-opcode cpy #b11000000 jump)
(define-opcode cpx #b11100000 jump)

;;;
;;;
;;;

(define assembler
  (lambda (source-file output-file listing-file)
    (let* ()
      )))

(define assemble-expression
(lambda (expr starting-address)

  (define assemble
    (lambda (code)
      "assembles a mixed list of keywords, assembly directives,
data definitions, and expressions (lists consisting of zero
or more prefixes, an assembly mnemonic, and zero or more
operands) and returns a record consisting of a vector of
bytevectors, a hashtable of label-value pairs, the number of
lines of code (where each directive, definition, label, and
expression is considered one line), the total size of the
bytevector in bytes, a code listing with line numbers,
a list of warning reports, and a list of error reports.

The assembler will attempt to complete the assembly in spite
of errors, but instead proceed while returning an error report,
but a few errors will probably be immediately fatal."

      (letrec ((output '())
               (line-count 0)
               (current-address 0)
               (labels '())
               (equates '())
               (listing '())
               (errors '())
               (warnings '())
               ;;
               ;;
               (add-warning!
                (lambda (warning expr line)
                  (append! warnings (list warning expr line))))
               (add-error!
                (lambda (error expr line)
                  (append! errors (list error expr line))))
               ;;
               ;;
               (identify-exprs
                (lambda (expr)
                  (cond
                   ((label? expr)
                    (assoc-set! labels expr current-address))
                   ((list? expr)
                    ())
                   )))
               ;;
               (value-byte
                (lambda (value)
                  (cond
                   ((integer? value) value)
                   (else #f))))
               (define-const
                 (lambda (name value)
                   (if (assoc equates name)
                       (add-warning! 'Duplicate-constant name line-count)
                       (assoc-set! equates name value))))
               (define-byte
                 (lambda (value . values)
                   (map value->byte (cons value values))))
               (define-string
                 (lambda ()
                   0))
               (define-stringz
                 (lambda ()
                   0))
               ;;

               (nullaries ((brk 0) (nop #xEA) 
                           (rts #x60) (rti #x40) 
                           (clc #x18) (sec #x38) (cld #xD8) (sed #xF8) (cli #x58) (sei #x78) (clv #xB8) 
                           (pha #x48) (pla #x68) (php #x08) (plp #x28) 
                           (inx #xE8) (iny #xC8) (dex #xCA) (dey #x88) 
                           (tax #xAA) (txa #x8A) (tay #xA8) (tya #x98) (tsx #xBA) (txs #x9A)
                           (asl #x0A) (rol #x2A) (lsr #x4A) (ror #x6A)))
               (if (null? code)
                   (display "Fatal error - source list is empty.")

                   ;; step one - apply identify-labels to each entry in
                   ;; the code list. This is done by currying the
                   ;; intermediates record to the identify-expr call,
                   ;; returning the actual identifier function. This is
                   ;; then applied to a generated list of expression numbers
                   ;; and the expressions themselves.

                   (for-each
                    (identify-exprs intermediates)
                    code
                    ()
                    ;; step two -
                    ((delabeled-code
                      (map resolve-labels evaluated-exprs)))
                    ))))

;;;
;;;
;;;

      (define symbol->label
        (lambda (sym)
          (if (and (symbol? sym) (not (label? sym)))
              (string->symbol (string-append (symbol->string sym) ":"))
              sym)))

      (define label->string
        (lambda (lbl)
          (if (label? lbl)
              (let ((text (symbol->string lbl)))
                (substring text 0 (- (string-length text) 1)))
              lbl)))

      (define label->symbol
        (lambda (lbl)
          (string->symbol (label->string lbl))))

      (define label?
        (lambda (sym)
          (let ((text (symbol->string sym)))
            (char=? #\: (string-ref text (- (string-length text) 1))))))
