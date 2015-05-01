#lang racket

(require parser-tools/yacc)

(require "assembler.rkt"
         "instructions.rkt"
         "utility.rkt"
         "types.rkt")

(define assembly-parser
  (parser
    (debug "debug.log")
    (start program)
    (end EOF)
    (error (lambda (tok-ok? tok-name tok-value) (displayln (format "~a, ~a, ~a" tok-ok? tok-name tok-value))))
    (tokens a b)
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

(define (lex-this lexer input)
  (lambda () (lexer input)))

(void
  (let* ([test-file "tests/asm/one-plus-one.asm"])
    (let ([tokens (lex-assembly (open-input-file test-file))])
      (displayln "\n-- lexed --\n")
      (map displayln tokens))

    (let ([ast (assembly-parser (lex-this assembly-lexer (open-input-file test-file)))])
      (displayln "\n-- parsed --\n")
      (map displayln ast))

    (displayln "\n-- assembled --\n")
    (map (lambda (dword) (displayln (format "~x" dword))) (assemble (open-input-file test-file)))))
