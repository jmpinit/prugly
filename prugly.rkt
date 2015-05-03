#lang racket

(require "assembler.rkt"
         "debugger.rkt"
         "utility.rkt")

(define (print-registers pru)
  (map (lambda (offset val)
         (displayln (format "r~a = ~s" offset (format-hex val))))
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
(write-register 1 3 #xdeadbeef)
(void (print-registers 1))
