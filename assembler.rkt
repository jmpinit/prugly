#lang racket

(require "architecture.rkt")

(struct reg (offset field)
        #:guard (lambda (offset field name)
                  (unless (and (>= offset 0) (< offset 32))
                    (error (format "~a is not a valid register offset." offset)))
                  (unless (and (>= field 0) (< field 8))
                    (error (format "~a is not a valid register field." field)))
                  (values offset field)))

(struct imm8 (value)
        #:guard (lambda (value name)
                  (unless (and (>= value 0) (< value (expt 2 8)))
                    (error (format "~a is not a valid imm8." value)))
                  value))

(struct imm16 (value)
        #:guard (lambda (value name)
                  (unless (and (>= value 0) (< value (expt 2 16)))
                    (error (format "~a is not a valid imm16." value)))
                  value))

;(define r0 (reg 0))
;(define r1 (reg 1))
;(define r2 (reg 2))

(define-syntax (define-instruction stx)
  (syntax-case stx ()
    [(_ (name arg ...) [(pred ...) expr] ...)
     (syntax
       (begin
       (define (name arg ...)
         (cond
           [(and (pred arg) ...) expr] ...
           [else
             (raise-type-error 'name
                               "No arguments matched opcode predicates"
                               (list arg ...))]))))]))

; encoding 0

(define-syntax (make-enc-0 stx)
  (syntax-case stx ()
    [(_ name alu-op)
     #'(define-instruction
         (name dest src operand)
         [(reg? reg? reg?)
          (let ([sub-op alu-op]
                [r2-sel (reg-field operand)]
                [r2     (reg-offset operand)]
                [r1-sel (reg-field src)]
                [r1     (reg-offset src)]
                [rd-sel (reg-field dest)]
                [rd     (reg-offset dest)])
            (merge-bitfields 32 `((3 1) (4 ,sub-op) (1 0) (3 ,r2-sel) (5 ,r2) (3 ,r1-sel) (5 ,r1) (3 ,rd-sel) (5 ,rd))))]
         [(reg? reg? imm8?)
          (let ([sub-op alu-op]
                [value  (imm8-value operand)]
                [r1-sel (reg-field src)]
                [r1     (reg-offset src)]
                [rd-sel (reg-field dest)]
                [rd     (reg-offset dest)])
            (merge-bitfields 32 `((3 1) (4 ,sub-op) (1 1) (8 ,value) (3 ,r1-sel) (3 ,rd-sel) (5 ,rd))))])]))

(make-enc-0 op-add 0)
(make-enc-0 op-adc 1)
(make-enc-0 op-sub 2)
(make-enc-0 op-suc 3)
(make-enc-0 op-lsl 4)
(make-enc-0 op-lsr 5)
(make-enc-0 op-rsb 6)
(make-enc-0 op-rsc 7)
(make-enc-0 op-and 8)
(make-enc-0 op-or  9)
(make-enc-0 op-xor 10)
(make-enc-0 op-not 11)
(make-enc-0 op-min 12)
(make-enc-0 op-max 13)
(make-enc-0 op-clr 14)
(make-enc-0 op-set 15)

; encoding 1

;    '([op 3] [sub-op 4] [io 1] [rsel2 3] [r2 5] [_ 16])
;    '([op 3] [sub-op 4] [io 1] [rsel2 3] [r2 5] [_ 8] [rdsel 3] [rd 5])
;    '([op 3] [sub-op 4] [io 1] [imm-16 16] [_ 8])
;    '([op 3] [sub-op 4] [io 1] [imm-16 16] [rdsel 3] [rd 5])
;    '([op 3] [sub-op 4] [io 1] [imm-8 8] [_ 8] [rdsel 3] [rd 5])
;    '([op 3] [sub-op 4] [_ 24])
;    '([op 3] [sub-op 4] [io 1] [slp 1])))

(define-instruction (op-ldi dest imm)
  [(reg? imm16?) (let ([value (imm16-value imm)]
                       [rd-sel (reg-field dest)]
                       [rd (reg-offset dest)])
                   (merge-bitfields 32 `((3 1) (4 2) (1 0) (16 ,value) (3 ,rd-sel) (5 ,rd))))])

(format "~x" (op-add (reg 0 7) (reg 1 7) (imm8 #xbe)))
(format "~x" (op-ldi (reg 0 7) (imm16 #xbeef)))
