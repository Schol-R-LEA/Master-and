#!r6rs
(library 
    (master-and opcodes)
  (export 
   make-nullary define-nullary 
   make-opcode 
   make-conditional 
   nop 
   jsr
   brk rts rti clc sec cld sed cli 
   sei clv pha pla php plp inx iny
   bcc bcs beq bmi bne bpl bvc bvs
   dex dey tax txa tay tya tsx txs 
   ora ana eor adc sta lda cmp sbc 
   asl rol asr ror stx ldx dec inc 
   bit jmp jma sty ldy cpy cpx)
  (import 
    (rnrs (6))
    (rnrs base (6))
    (rnrs syntax-case (6))
    (rnrs bytevectors (6))
    (ice-9 format))

  (define ind-zp-x (inexact->exact #b00000))
  (define rls-imm  (inexact->exact #b00000))
  (define zp       (inexact->exact #b00100))
  (define al-imm   (inexact->exact #b01000))
  (define acc      (inexact->exact #b01000))
  (define abs      (inexact->exact #b01100))
  (define ind-zp-y (inexact->exact #b10000))
  (define al-zp-x  (inexact->exact #b10100))
  (define abs-y    (inexact->exact #b11000))
  (define zp-x     (inexact->exact #b11000))
  (define abs-x    (inexact->exact #b11100))


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


  (define-syntax make-opcode
    (syntax-rules ()
      ((_ <opcode>)
       (lambda (subop value)
         (let* ((computed-opcode (bitwise-ior <opcode> subop)))
           (list (make-bytevector 1 computed-opcode)
                 (make-bytevector 1 value)))))))
  

  (define-syntax make-conditional
    (syntax-rules ()   
      ((_ <name> <opcode>)
       (define name> 
         (lambda (operand dest)
           )))))
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


  (define ora (make-opcode #b00000001))
  (define ana (make-opcode #b00100001))
  (define eor (make-opcode #b01000001))
  (define adc (make-opcode #b01100000))
  (define sta (make-opcode #b10000000))
  (define lda (make-opcode #b10100000))
  (define cmp (make-opcode #b11000000))
  (define sbc (make-opcode #b11100000))

  (define asl (make-opcode #b00000000))
  (define rol (make-opcode #b00100000))
  (define asr (make-opcode #b01000000))
  (define ror (make-opcode #b01100000))
  (define stx (make-opcode #b10000000))
  (define ldx (make-opcode #b10100000))
  (define dec (make-opcode #b11000000))
  (define inc (make-opcode #b11100000))

  (define bit (make-opcode #b00100010))
  (define jmp (make-opcode #b01000010))
  (define jma (make-opcode #b01100010))
  (define sty (make-opcode #b10000010))
  (define ldy (make-opcode #b10100010))
  (define cpy (make-opcode #b11000010))
  (define cpx (make-opcode #b11100010)))
