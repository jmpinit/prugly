#lang racket

(require unstable/hash)
(require "architecture.rkt")

; TODO macro for parameter signatures
; (sig rd r1 imm-8) to (list (list 'reg rd rdsel) (list 'reg r1 r1sel) (list 'imm-8 value))

; TODO macro for instruction handler
; (handler (sig encoding hash) ...)

; TODO selectors are just the register name with "-sel" appended
; like rd -> rd-sel

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
      (error (format "incorrect values supplied for encoding.\ngiven ~a\nbut need ~a."
                     (hash-keys values) field-names)))))

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
                               (hash 'op 0 'alu-op op-num 'io 0)
                               (hash 'rsel2 rsel2 'r2 r2
                                     'rsel1 rsel1 'r1 r1
                                     'rdsel rdsel 'rd rd)))]
                          [(list (list 'reg rd rdsel) (list 'reg r1 rsel1) (list 'imm-8 value))
                           (fill-in-bitfields
                             (second enc-0)
                             (hash-union
                               (hash 'op 0 'alu-op op-num 'io 1)
                               (hash 'imm-8 value
                                     'rsel1 rsel1 'r1 r1
                                     'rdsel rdsel 'rd rd)))]
                          [_ (error "unrecognized instruction signature.")]))))
         '(add adc sub suc lsl lsr rsb rsc and or xor not min max clr set)
         (range 16))

    ; build instructions of encoding 1

    (hash-set!
      instructions 'jmp
      (lambda (params)
        (match params
          [(list (list 'reg r2 rsel2))
           (fill-in-bitfields
             (first enc-1)
             (hash-union
               (hash 'op 1 'sub-op 0 'io 0 '_ 0)
               (hash 'rsel2 rsel2 'r2 r2)))]
          [(list (list 'imm-16 value))
           (fill-in-bitfields
             (third enc-1)
             (hash-union
               (hash 'op 1 'sub-op 0 'io 1 '_ 0)
               (hash 'imm-16 value)))])))

    (hash-set!
      instructions 'jal
      (lambda (params)
        (match params
          [(list (list 'reg r2 rsel2) (list 'reg rd rdsel))
           (fill-in-bitfields
             (second enc-1)
             (hash-union
               (hash 'op 1 'sub-op 1 'io 0 '_ 0)
               (hash 'rsel2 rsel2 'r2 r2 'rdsel rdsel 'rd rd)))]
          [(list (list 'reg rd rdsel) (list 'imm-16 value))
           (fill-in-bitfields
             (fourth enc-1)
             (hash-union
               (hash 'op 1 'sub-op 1 'io 1)
               (hash 'rdsel rdsel 'rd rd 'imm-16 value)))])))

    ; lmbd and scan
    (map (lambda (name sub-op)
           (hash-set!
             instructions name
             (lambda (params)
               (match params
                 [(list (list 'reg rd rdsel) (list 'reg r2 rsel2))
                  (fill-in-bitfields
                    (second enc-1)
                    (hash 'op 1 'sub-op sub-op 'io 0
                          'rsel2 rsel2 'r2 r2
                          'rdsel rdsel 'rd rd))]
                 [(list (list 'reg rd rdsel) (list 'imm-8 value))
                  (fill-in-bitfields
                    (fifth enc-1)
                    (hash 'op 1 'sub-op sub-op 'io 1
                          'imm-8 value
                          'rdsel rdsel 'rd rd))]))))
         '(lmbd scan) '(3 4))

    ; halt
    (hash-set!
      instructions 'halt
      (lambda (params)
        (match params
          [(list)
           (fill-in-bitfields
             (sixth enc-1)
             (hash 'op 1 'sub-op 5 '_ 0))])))

    ; slp
    (hash-set!
      instructions 'slp
      (lambda (params)
        (match params
          [(list (list 'imm-1 value))
           (fill-in-bitfields
             (seventh enc-1)
             (hash 'op 1 'sub-op 15 'slp value '_ 0))])))

    ; build instructions of encoding 2
    
    (map (lambda (name op-num test)
           (hash-set!
             instructions name
             (lambda (params)
               (match params
                 [(list (list 'imm-10 broff) (list 'reg r1 rsel1) (list 'reg r2 rsel2))
                  (fill-in-bitfields
                    (first enc-2)
                    (hash 'op op-num 'test test
                          'rsel2 rsel2 'r2 r2
                          'rsel1 rsel1 'r1 r1
                          'broff-h (bit-slice broff 8 2)
                          'broff-l (bit-slice broff 0 8)))]
                 [(list (list 'imm-10 broff) (list 'reg r1 rsel1) (list 'imm-8 value))
                  (fill-in-bitfields
                    (second enc-2)
                    (hash 'op op-num 'test test
                          'imm-8 value
                          'rsel1 rsel1 'r1 r1
                          'broff-h (bit-slice broff 8 2)
                          'broff-l (bit-slice broff 0 8)))]))))
         '(qblt qbeq qble qbgt qbne qbge qba)
         '(2    2    2    3    3    3    3)
         (range 1 8))
        
    ; build instructions of encoding 3

    (map (lambda (name test)
           (hash-set!
             instructions name
             (lambda (params)
               (match params
                 [(list (list 'imm-10 broff) (list 'reg r1 rsel1) (list 'reg r2 rsel2))
                  (fill-in-bitfields
                    (first enc-3)
                    (hash 'op 6 'test test 'io 0
                          'rsel2 rsel2 'r2 r2
                          'rsel1 rsel1 'r1 r1
                          'broff-h (bit-slice broff 8 2)
                          'broff-l (bit-slice broff 0 8)))]
                 [(list (list 'imm-10 broff) (list 'reg r1 rsel1) (list 'imm-5 value))
                  (fill-in-bitfields
                    (second enc-3)
                    (hash 'op 6 'test test 'io 1
                          'imm-5 value
                          'rsel1 rsel1 'r1 r1
                          'broff-h (bit-slice broff 8 2)
                          'broff-l (bit-slice broff 0 8)))]))))
         '(qbbc qbbs) '(1 2))

    ; build instructions of encoding 4

    (map (lambda (name op-num l/s)
           (hash-set!
             instructions name
             (lambda (params)
               (match params
                 [(list (list 'reg rb 7) (list 'reg rx rxsel) (list 'reg ro rosel) (list 'imm-7 burstlen))
                  (fill-in-bitfields
                    (first enc-4)
                    (hash 'op op-num 'l/s l/s 'io 0
                          'rosel rosel 'ro ro
                          'rb rb
                          'rxsel rxsel 'rx rx
                          'bl-h (bit-slice burstlen 4 3)
                          'bl-m (bit-slice burstlen 1 3)
                          'bl-l (bit-slice burstlen 0 1)))]
                 [(list (list 'reg rb 7) (list 'reg rx rxsel) (list 'imm-8 value) (list 'imm-7 burstlen))
                  (fill-in-bitfields
                    (second enc-4)
                    (hash 'op op-num 'l/s l/s 'io 1
                          'imm-8 value
                          'rb rb
                          'rxsel rxsel 'rx rx
                          'bl-h (bit-slice burstlen 4 3)
                          'bl-m (bit-slice burstlen 1 3)
                          'bl-l (bit-slice burstlen 0 1)))]))))
         '(sbbo lbbo sbco lbco)
         '(7    7    4    4)
         '(0    1    0    1))

    instructions
    ))

(define instructions (build-instructions))

(displayln "example:")
(displayln ((hash-ref instructions 'jal) (list `(reg 0 7) `(imm-16 ,#xdead))))
