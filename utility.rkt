#lang racket

(require "types.rkt")

(provide field-to-number
         string->register)

(define (field-to-number field)
  (match field
    [(regexp #rx"b([0-3])" (list _ num)) (string->number num)]
    [(regexp #rx"w([0-2])" (list _ num)) (+ (string->number num) 4)]
    [_ (error (format "unrecognized field \"~a\"" field))]))
             
(define (string->register reg)
  (match reg
    [(regexp #rx"r([0-9][0-9]?).([wb][0-3])" (list _ offset field))
     (make-register (string->number offset) (field-to-number field))]
    [(regexp #rx"r([0-9][0-9]?)" (list _ offset))
     (make-register (string->number offset) 7)]
    [_ (error (format "could not make register from \"~a\"." reg))]))

