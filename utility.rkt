#lang racket

(require "types.rkt")

(provide field-to-number
         string->register
         split-by
         read-dwords)

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

; split list into lists of n elements
(define (split-by lst n)
  (if (not (empty? lst))
    (cons (take lst n) (split-by (drop lst n) n))
    '()))

(define (read-dwords port)
  (let ([instr-bytes (bytes->list (port->bytes port))])
    (if (= (modulo (length instr-bytes) 4) 0)
      (map (lambda (bs) (integer-bytes->integer (apply bytes bs) #f))
           (split-by instr-bytes 4))
      (error "File is not purely double words (not divisible by 4)."))))

