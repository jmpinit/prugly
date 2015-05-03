#lang racket

(require rackunit rackunit/text-ui)

(require "../assembler.rkt"
         "../debugger.rkt"
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

; TODO extract code shared with assemble-lines
(define (pasm-lines origin-zero? lines)
  (let* ([formatted-lines
           (map (lambda (line)
                  (string-append line "\n"))
                lines)]
         [code (apply string-append formatted-lines)]
         [code-port (open-input-string code)])
    (pasm origin-zero? code-port)))

(define assembler-tests
  (test-suite
    "Assembler"

    (test-suite
      "Output"

      (test-case
        "One plus one."

        (let* ([source
                 (list
                   "start:"
                   "ldi    r0, 1"
                   "ldi    r1, 1"
                   "add    r2, r0, r1")]
               [prugly-dwords (assemble-lines source)]
               [pasm-dwords (pasm-lines #t source)])
          (check-equal? prugly-dwords pasm-dwords))))

    (test-suite
      "Behavior"

      (test-case
        "Set one register."

        (let* ([dwords
                 (assemble-lines
                   (list
                     "ldi    r4, 4919" ; 1337 in hex
                     "ldi    r31.b0, 35" ; trip interrupt
                     "halt"))])
          (pru-run 1 dwords)
          (check-equal? (read-register 1 4) #x1337))))))

(run-tests assembler-tests)
