#lang racket

(require rackunit "../architecture.rkt")
(require rackunit/text-ui)

(define (hash-like-this? h1 h2)
  (and
    (= (length (hash-keys h1)) (length (hash-keys h2)))
    (foldl (lambda (key result)
             (if (equal? (hash-ref h1 key) (hash-ref h2 key))
               result
               #f))
           #t (hash-keys h1))))

(define architecture-tests
  (test-suite
    "Tests for architecture functionality."

    (test-case
      "Extracting bytes of a dword."
      
      (let ([field-hash (extract-bitfields 32 #xdeadbeef '((first 8) (second 8) (third 8) (fourth 8)))])
        (check hash-like-this? field-hash
               (hash 'first #xde 'second #xad 'third #xbe 'fourth #xef))))

    (test-case
      "Extracting fields of various sizes."

      (let ([field-hash (extract-bitfields 8 #xde '((first 1) (second 2) (third 3) (fourth 2)))])
        (check hash-like-this? field-hash
               (hash 'first 1 'second 2 'third 7 'fourth 2))))

    (test-case
      "Sum of field lengths bigger than bits in value."

      (check-exn exn:fail? (lambda ()
                             (extract-bitfields 8 #xde '((a 4) (b 5))))))

    (test-case
      "Merging bitfields to make a byte."

      (check-equal? (merge-bitfields 8 '((#xd 4) (#xe 4))) #xde))

    (test-case
      "Value too large for its bitfield."

      (check-exn exn:fail? (lambda ()
                             (merge-bitfields 8 '((#xff 4) (#xa 4))))))))


(run-tests architecture-tests)
