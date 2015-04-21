#lang racket

(require rackunit
         "../assembler.rkt"
         "../pru-debug.rkt")

(require rackunit/text-ui)

(define opcode-tests
  (test-suite
    "Testing assembler's implementation of individual opcodes."
    
    (test-case
      "add"
      (let ([code (assemble "add r0, 124"
                            "add r1, 28"
                            "add r0, r1")])
        (pru-eval code)
        (check-equal? 152 (register-read 0))))))
 
(run-tests opcode-tests)
