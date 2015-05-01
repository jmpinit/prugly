#lang racket

(provide (all-defined-out))

(define-struct directive (syntax))
(define-struct expression (syntax))
(define-struct symbol (name))
(define-struct instruction (name parameters))
(define-struct label (name))
(define-struct register (offset field))
(define-struct immediate (value))
