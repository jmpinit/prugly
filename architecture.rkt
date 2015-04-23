#lang racket

(require macro-debugger/stepper-text)

; make a number from a bit range
(define (bit-slice num start len)
  (let ([mask (- (expt 2 len) 1)])
    (bitwise-and (arithmetic-shift num (- start)) mask)))

(define-syntax extract-fields-from
  (syntax-rules ()
    [(extract-fields-from size value ([name len]) body)
     (let ([name (bit-slice value (- size len) len)]) body)]
    [(extract-fields-from size value ([name len] name-len2 ...) body)
     (let ([name (bit-slice value (- size len) len)])
       (extract-fields-from (- size len) value (name-len2 ...) body))]))

(extract-fields-from
  32 #xdeadbeef
  ([op 4] [reg 4] [reg1 8] [reg2 8] [reg3 8] [reg4 8])
  (displayln (format "~x ~x ~x ~x ~x" op reg reg1 reg2 reg3)))
