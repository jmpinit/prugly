#lang racket
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre))

(require "utility.rkt"
         "types.rkt")

(provide assembly-lexer lex-assembly a b)

(define-tokens a (LABEL ID INSTR DIRECTIVE REG EXPR))
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
  [reg (re-: "r" (re-+ number10) (re-? (re-: "." (re-or "b0" "b1" "b2" "b3" "w0" "w1" "w2"))))]
  [line-end (re-or "\r\n" "\n" "\r")]
  [comment (re-: "//" (complement (re-: any-string "\n" any-string)) line-end)]
  [multiline-comment (re-: "/*" (complement (re-: any-string "*/" any-string)) "*/" line-end)]
  [op (re-or "add" "adc" "sub" "suc" "lsl" "lsr" "rsb" "rsc" "and" "or" "xor" "not" "min" "max" "clr" "set"
                      "jmp" "jal" "ldi" "lmbd" "scan" "halt" "slp"
                      "qblt" "qbeq" "qble" "qbgt" "qbne" "qbge" "qba"
                      "qbbc" "qbbs"
                      "sbbo" "lbbo" "sbco" "lbco")])

(define assembly-lexer
  (lexer
    [(re-: identifier ":") (token-LABEL
                             (first
                               (regexp-match #rx".*(?=:)" lexeme)))]
    [op (token-INSTR lexeme)]
    [identifier (token-ID lexeme)]
    [(re-+ number10) (token-EXPR `,(string->number lexeme))]
    [reg (token-REG lexeme)]
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
