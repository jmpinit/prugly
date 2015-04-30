#lang racket
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens a (LABEL ID INSTR DIRECTIVE EXPR))
(define-empty-tokens b (EOF LEND))

(define-lex-trans number
  (syntax-rules ()
    ((_ digit)
     (re-: (re-? (re-or "-" "+")) (uinteger digit)))))

(define-lex-trans uinteger
  (syntax-rules ()
    ((_ digit) (re-+ digit))))

(define-lex-abbrevs
  [digit10 (char-range "0" "9")]
  [number10 (number digit10)]
  [identifier-characters (re-or (char-range "A" "z")
                                "?" "!" ":" "$" "%" "^" "&")]
  [identifier (re-+ identifier-characters)]
  [register (re-: "r" (re-+ number10) (re-? (re-: "." (re-or "b0" "b1" "b2" "b3" "w0" "w1" "w2"))))]
  [line-end (re-or "\r\n" "\n" "\r")]
  [comment (re-: "//" (complement (re-: any-string "\n" any-string)) line-end)]
  [multiline-comment (re-: "/*" (complement (re-: any-string "*/" any-string)) "*/" line-end)]
  [instruction (re-or "add" "adc" "sub" "suc" "lsl" "lsr" "rsb" "rsc" "and" "or" "xor" "not" "min" "max" "clr" "set"
                      "jmp" "jal" "ldi" "lmbd" "scan" "halt" "slp"
                      "qblt" "qbeq" "qble" "qbgt" "qbne" "qbge" "qba"
                      "qbbc" "qbbs"
                      "sbbo" "lbbo" "sbco" "lbco")])

(define assembly-lexer
  (lexer
    [(re-: identifier ":") (token-LABEL (regexp-match #rx".*(?=:)" lexeme))]
    [instruction (token-INSTR lexeme)]
    [identifier (token-ID lexeme)]
    [(re-+ number10) (token-EXPR `,(string->number lexeme))]
    [register (token-EXPR `(reg ,(split-reg lexeme)))]
    ["@" (token-EXPR (read input-port))]
    ["." (token-DIRECTIVE (read input-port))]
    [(re-+ (re-or "\r\n" "\n")) (token-LEND)]
    [(re-or "," whitespace comment multiline-comment)
     (assembly-lexer input-port)]
    [(eof) (token-EOF)]))

(define (lex-assembly port)
  (let ([token (assembly-lexer port)])
    (if (equal? (token-name token) 'EOF)
      null
      (cons token (lex-assembly port)))))

(define (field-to-value field)
  (match field
    [(regexp #rx"b([0-3])" (list _ num)) (string->number num)]
    [(regexp #rx"w([0-2])" (list _ num)) (+ (string->number num) 4)]
    [_ (error (format "unrecognized field \"~a\"" field))]))
             
(define (split-reg reg)
  (match reg
    [(regexp #rx"r([0-9][0-9]?).([wb][0-3])" (list _ offset field)) (list (string->number offset) (field-to-value field))]
    [(regexp #rx"r([0-9][0-9]?)" (list _ offset)) (list (string->number offset) 7)]
    [_ (error (format "could not split reg \"~a\"" reg))]))

; PARSER

(define assembly-parser
  (parser
    (debug "debug.log")
    (start program)
    (end EOF)
    (error (lambda (tok-ok? tok-name tok-value) (displayln (format "~a, ~a, ~a" tok-ok? tok-name tok-value))))
    (tokens a b)
    (grammar
      (parameter
        [(ID) $1]
        [(EXPR) $1])
      (param-list
        [(param-list parameter) (cons $1 $2)]
        [(parameter) $1])
      (instruction
        [(INSTR param-list) (cons $1 $2)]
        [(INSTR) $1])
      (line
        [(instruction LEND) $1]
        [(LABEL LEND) $1]
        [(DIRECTIVE LEND) $1] )
      (program
        [(line) $1]
        [(line program) (cons $1 $2)]))))

(define (lex-this lexer input)
  (lambda () (lexer input)))

(void
  (let* ([test-file "tests/asm/one-plus-one.asm"])
    (let ([tokens (lex-assembly (open-input-file test-file))])
      (displayln "\n-- lexed --\n")
      (map displayln tokens)
      (displayln "\n"))

    (let ([ast (assembly-parser (lex-this assembly-lexer (open-input-file test-file)))])
      (displayln "\n-- parsed --\n")
      (map displayln ast))))
