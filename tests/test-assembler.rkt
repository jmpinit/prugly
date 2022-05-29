#lang racket

(require rackunit rackunit/text-ui)

(require "../assembler.rkt")

(define assembler-tests
  (test-suite
    "Assembler"

    (test-suite
      "Basic programs"

      (test-case
        "One plus one."

        (print-program-listing
        (list
        "ldi  r4, 4919"
        "ldi  r31.b0, 35"
        "halt"))))))

(run-tests assembler-tests)
