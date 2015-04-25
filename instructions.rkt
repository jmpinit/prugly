#lang racket

(require unstable/hash)
(require "architecture.rkt")

; format 0
(define enc-0
  (list
    '([op 3] [alu-op 4] [io 1] [rsel2 3] [r2 5] [rsel1 3] [r1 5] [rdsel 3] [rd 5])
    '([op 3] [alu-op 4] [io 1] [imm-8 8]        [rsel1 3] [r1 5] [rdsel 3] [rd 5])))

; format 1
(define enc-1
  (list
    '([op 3] [sub-op 4] [io 1] [rsel2 3] [r2 5] [_ 16])
    '([op 3] [sub-op 4] [io 1] [rsel2 3] [r2 5] [_ 8] [rdsel 3] [rd 5])
    '([op 3] [sub-op 4] [io 1] [imm-16 16] [_ 8])
    '([op 3] [sub-op 4] [io 1] [imm-16 16] [rdsel 3] [rd 5])
    '([op 3] [sub-op 4] [io 1] [imm-8 8] [_ 8] [rdsel 3] [rd 5])
    '([op 3] [sub-op 4] [_ 24])
    '([op 3] [sub-op 4] [io 1] [slp 1])))

; format 2
(define enc-2
  (list
    '([oph 2] [test 3] [broff-h 2] [io 1] [rsel2 3] [r2 5] [rsel1 3] [r1 5] [broff-l 8])
    '([oph 2] [test 3] [broff-h 2] [io 1] [imm-8 8]        [rsel1 3] [r1 5] [broff-l 8])))

; format 3
(define enc-3
  (list
    '([op 3] [test 2] [broff-h 2] [io 1] [rsel2 3] [r2 5] [rsel1 3] [r1 5] [broff-l 8])
    '([op 3] [test 2] [broff-h 2] [io 1] [_ 3] [imm-5 5]  [rsel1 3] [r1 5] [broff-l 8])))

; format 4
(define enc-4
  (list
    '([op 3] [l/s 1] [bl-h 3] [io 1] [rosel 3] [ro 5] [bl-m 3] [rb 5] [bl-l 1] [rxsel 2] [rx 5])
    '([op 3] [l/s 1] [bl-h 3] [io 1] [imm-8 8]        [bl-m 3] [rb 5] [bl-l 1] [rxsel 2] [rx 5])))

(define (fill-in-bitfields encoding values)
  (let ([field-names (map first encoding)]
        [field-sizes (map second encoding)])
    (if (equal? (apply set field-names) (apply set (hash-keys values)))
      (merge-bitfields
        32 (map (lambda (name size)
                  (let ([value (hash-ref values name)])
                    (list size value)))
                field-names field-sizes))
      (error "incorrect values supplied for encoding."))))

(define (build-instructions)
  (let ([instructions (make-hash)])
    ; build instructions of encoding 0
    (map (lambda (name op-num)
           (hash-set! instructions name
                      (lambda (params)
                        (match params
                          [(list (list 'reg rd rdsel) (list 'reg r1 rsel1) (list 'reg r2 rsel2))
                           (fill-in-bitfields
                             (first enc-0)
                             (hash-union
                               (hash 'op 0 'alu-op op-num)
                               (hash 'io 0
                                     'rsel2 rsel2 'r2 r2
                                     'rsel1 rsel1 'r1 r1
                                     'rdsel rdsel 'rd rd)))]
                          [(list (list 'reg rd rdsel) (list 'reg r1 rsel1) (list 'imm-8 value))
                           (fill-in-bitfields
                             (second enc-0)
                             (hash-union
                               (hash 'op 0 'alu-op op-num)
                               (hash 'io 1
                                     'imm-8 value
                                     'rsel1 rsel1 'r1 r1
                                     'rdsel rdsel 'rd rd)))]
                          [_ (error "unrecognized instruction signature.")]))))
         '(add adc sub suc lsl lsr rsb rsc and or xor not min max clr set)
         (range 16))

    ; build instructions of encoding 1

    instructions
    ))

(define instructions (build-instructions))

(displayln "example:")
(displayln
  (format "add r0, r1, 7 => ~x"
          ((hash-ref instructions 'add) (list '(reg 0 7) '(reg 1 7) '(imm-8 7)))))
