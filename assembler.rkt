#lang racket

(require "instructions.rkt"
         "lexer.rkt"
         "parser.rkt"
         "utility.rkt"
         "types.rkt")

(provide assemble
         assemble-lines)

(define-struct assembler (address symbols dwords))

(define (assembler-forward prev bytes)
  (let ([new-address (+ (assembler-address prev) 4)])
    (struct-copy assembler prev [address new-address])))

(define (assembler-label prev label-name)
  (let* ([label-address (assembler-address prev)]
         [new-table (hash-set (assembler-symbols prev) label-name (make-immediate label-address))])
    (struct-copy assembler prev [symbols new-table])))

; produces a symbol table
(define (pass-1 nodes)
  ; TODO execute syntax? directives
  (assembler-symbols
    (foldl
      (lambda (node prev)
        (cond
          [(instruction? node)
           (assembler-forward prev 4)]
          [(label? node)
           (assembler-label prev (label-name node))]
          [else prev]))
      (make-assembler 0 (make-immutable-hash) '()) nodes)))

(define (resolve-expression expr)
  (define semantic-ns (make-base-namespace))
  (let ([result (eval `(,@(expression-syntax expr)) semantic-ns)])
    (cond
      [(number? result) (make-immediate result)]
      [(string? result) (string->register result)]
      [else (error (format "Result of expression (\"~a\") is not number or register." result))])))

(define (simplify-params symbols instr)
  (define (simplify param)
    (cond
      [(symbol? param) (hash-ref symbols (symbol-name param))]
      [(expression? param) (resolve-expression param)]
      [(number? param) (begin (displayln "NUMBER CONTAMINATION!") param)]
      [else param]))

  (let ([params (instruction-parameters instr)])
    (map simplify params)))

(define (instruction->dword symbols instr)
  (let ([params (simplify-params symbols instr)]
        [dword-maker (hash-ref instructions (instruction-name instr))])
    (with-handlers
      ([exn:fail?
         (lambda (exn)
           (error
             (format
               "parsing \"~a ~s\" (with ~a params) failed because ~s."
               (instruction-name instr)
               params
               (length params)
               (exn-message exn))))])
      (dword-maker params))))

; produces instruction dwords
(define (pass-2 symbols nodes)
  (map (lambda (instr)
         (instruction->dword symbols instr))
       (filter instruction? nodes)))

(define (assemble input)
  (let* ([nodes (assembly-parser (lex-this assembly-lexer input))]
         [symbols (pass-1 nodes)]
         [dwords (pass-2 symbols nodes)])
    dwords))

; convenience function
(define (assemble-lines lines)
  (let* ([formatted-lines
           (map (lambda (line)
                  (string-append line "\n"))
                lines)]
         [code (apply string-append formatted-lines)]
         [code-port (open-input-string code)])
    (assemble code-port)))

