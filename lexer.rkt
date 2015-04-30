#lang racket
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(provide (all-defined-out))

(define-tokens a (LABEL ID NUM REG DIRECTIVE EXPR))
(define-empty-tokens b (EOF))

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
  [comment (re-: "//" (complement (re-: any-string "\n" any-string)))]
  [multiline-comment (re-: "/*" (complement (re-: any-string "*/" any-string)) "*/")])

(define assembly-lexer
  (lexer
    [(re-: identifier ":") (token-LABEL (regexp-match #rx".*(?=:)" lexeme))]
    [identifier (token-ID lexeme)]
    [(re-+ number10) (token-NUM (string->number lexeme))]
    [register (token-REG (split-reg lexeme))]
    ["@" (token-EXPR (read input-port))]
    ["." (token-DIRECTIVE (read input-port))]
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

(map displayln
     (lex-assembly (open-input-file "tests/asm/one-plus-one.asm")))

