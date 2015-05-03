#lang racket

(require "assembler.rkt"
         "debugger.rkt")

(define (print-registers pru)
  (map (lambda (offset val)
         (displayln (format "r~a = ~x" offset val)))
       (range 32)
       (read-registers pru)))

(define program-reg-test
  (assemble-lines
    (list
      "ldi  r4, 4919"
      "ldi  r31.b0, 35"
      "halt")))

(pru-run 1 program-reg-test)

(void (print-registers 1))
