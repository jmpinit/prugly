#lang racket

(require rackunit rackunit/text-ui)

(require "../assembler.rkt"
         "../utility.rkt")

(define (get-directory path)
  (let-values ([(base name ?) (split-path path)])
    base))

(define (get-filename path)
  (let-values ([(base name ?) (split-path path)])
    name))

; assembles some source code using pasm
(define (pasm origin-zero? sourceport)
  (let* ([code-string (port->string sourceport)]
         [sourcecode (string-append
                       (if origin-zero? ".origin 0\n" "")
                       code-string)]
         [pasm (find-executable-path "pasm")]
         [old-dir (current-directory)]
         [src-path (make-temporary-file "temp-~a.p")]
         [bin-path (path-replace-suffix src-path ".bin")])
    (current-directory (get-directory src-path))
    
    ; save assembly code to temp file
    (display-to-file sourcecode src-path #:exists 'replace)

    (subprocess-wait
      (let-values
        ([(process sout sin serr)
          (subprocess #f #f (current-output-port)
                      pasm "-V2" "-b" src-path)])
        process))

    (if (file-exists? bin-path)
      (read-dwords (open-input-file bin-path))
      (error "Assembled file not found."))))

(define assembler-tests
  (test-suite
    "Tests for assembler functionality."

    (test-case
      "One plus one."

      (let* ([source
               (apply 
                 string-append
                 (list
                   "start:\n"
                   "ldi    r0, 1\n"
                   "ldi    r1, 1\n"
                   "add    r2, r0, r1\n"))]
             [prugly-dwords (assemble (open-input-string source))]
             [pasm-dwords (pasm #t (open-input-string source))])
        (check-equal? prugly-dwords pasm-dwords)))))

(run-tests assembler-tests)
