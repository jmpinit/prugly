#lang racket

(require parser-tools/yacc)

(require "lexer.rkt"
         "utility.rkt"
         "types.rkt")

(provide assembly-parser)

(define assembly-parser
  (parser
    (debug "debug.log")
    (start program)
    (end EOF)
    (error (lambda (tok-ok? tok-name tok-value) (displayln (format "~a, ~a, ~a" tok-ok? tok-name tok-value))))
    (tokens empty-tokens valued-tokens)
    (grammar
      (parameter
        [(ID) (make-symbol $1)]
        [(REG) (string->register $1)]
        [(EXPR) (make-expression $1)])
      (param-list
        [(parameter) (cons $1 null)]
        [(parameter param-list) (cons $1 $2)])
      (instruction
        [(INSTR param-list) (make-instruction $1 $2)]
        [(INSTR) (make-instruction $1 '())])
      (line
        [(instruction LEND) $1]
        [(LABEL LEND) (make-label $1)]
        [(DIRECTIVE LEND) (make-directive $1)] )
      (program
        [(line) (cons $1 null)]
        [(line program) (cons $1 $2)]))))

