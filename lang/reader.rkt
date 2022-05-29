#lang racket

(provide read-syntax)

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define instr-lines (filter non-empty-string? src-lines))
  (define module-datum `(module asm-mod racket
      (require "../assembler.rkt")
      (print-program-listing (list ,@instr-lines))))
  (datum->syntax #f module-datum))
