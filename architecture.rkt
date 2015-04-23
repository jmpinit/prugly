#lang racket

(require macro-debugger/stepper-text)

; make a number from a bit range
(define (bit-slice num start len)
  (let ([mask (- (expt 2 len) 1)])
    (bitwise-and (arithmetic-shift num (- start)) mask)))

(define-syntax extract-bitfields
  (syntax-rules ()
    [(extract-bitfields size value ([name len]) body)
     (let ([name (bit-slice value (- size len) len)]) body)]
    [(extract-bitfields size value ([name len] name-len2 ...) body)
     (let ([name (bit-slice value (- size len) len)])
       (extract-bitfields (- size len) value (name-len2 ...) body))]))

(define-syntax let-instruction-fields
  (syntax-rules ()
    [(let-instruction-fields instruction (name-len ...) body)
     (extract-bitfields 32 instruction (name-len ...) body)]))

(provide bit-slice extract-bitfields let-instruction-fields)
