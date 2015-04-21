#lang racket/base

(define (assemble-line line)
  #xdeadbeef)

(define (assemble . lines)
  (map assemble-line lines))

(provide assemble)
