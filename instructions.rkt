#lang racket

(require racket/hash)
(require "architecture.rkt"
         "types.rkt")

(provide instructions)

; TODO macro for parameter signatures
; (sig rd r1 imm-8) to (list (list 'reg rd rd-sel) (list 'reg r1 r1-sel) (list 'imm-8 value))

; TODO macro for instruction handler
; (handler (sig encoding hash) ...)

; TODO selectors are just the register name with "-sel" appended
; like rd -> rd-sel

; format 0
(define enc-0
  (list
    '([op 3] [alu-op 4] [io 1] [r2-sel 3] [r2 5] [r1-sel 3] [r1 5] [rd-sel 3] [rd 5])
    '([op 3] [alu-op 4] [io 1] [imm-8 8]        [r1-sel 3] [r1 5] [rd-sel 3] [rd 5])))

; format 1
(define enc-1
  (list
    '([op 3] [sub-op 4] [io 1] [r2-sel 3] [r2 5] [_ 16])
    '([op 3] [sub-op 4] [io 1] [r2-sel 3] [r2 5] [_ 8] [rd-sel 3] [rd 5])
    '([op 3] [sub-op 4] [io 1] [imm-16 16] [_ 8])
    '([op 3] [sub-op 4] [io 1] [imm-16 16] [rd-sel 3] [rd 5])
    '([op 3] [sub-op 4] [io 1] [imm-8 8] [_ 8] [rd-sel 3] [rd 5])
    '([op 3] [sub-op 4] [_ 24])
    '([op 3] [sub-op 4] [io 1] [slp 1])))

; format 2
(define enc-2
  (list
    '([oph 2] [test 3] [broff-h 2] [io 1] [r2-sel 3] [r2 5] [r1-sel 3] [r1 5] [broff-l 8])
    '([oph 2] [test 3] [broff-h 2] [io 1] [imm-8 8]        [r1-sel 3] [r1 5] [broff-l 8])))

; format 3
(define enc-3
  (list
    '([op 3] [test 2] [broff-h 2] [io 1] [r2-sel 3] [r2 5] [r1-sel 3] [r1 5] [broff-l 8])
    '([op 3] [test 2] [broff-h 2] [io 1] [_ 3] [imm-5 5]  [r1-sel 3] [r1 5] [broff-l 8])))

; format 4
(define enc-4
  (list
    '([op 3] [l/s 1] [bl-h 3] [io 1] [ro-sel 3] [ro 5] [bl-m 3] [rb 5] [bl-l 1] [rx-sel 2] [rx 5])
    '([op 3] [l/s 1] [bl-h 3] [io 1] [imm-8 8]        [bl-m 3] [rb 5] [bl-l 1] [rx-sel 2] [rx 5])))

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

; signature is of form (list number? number?)
(define (match-params params signature)
  (if (= (length params) (length signature))
    (foldl
      (lambda (check param result)
        (if (check param) result #f))
      #t signature params)
    #f))

(define (build-instructions)
  (let ([instructions (make-hash)]
        [decode-fail-msg "parameter signature unrecognized"])
    ; build instructions of encoding 0
    (map (lambda (name op-num)
           (hash-set!
             instructions name
             (lambda (params)
               (cond
                 [(match-params params (list register? register? register?))
                  (let ([rd (register-offset (first params))]
                        [rd-sel (register-field (first params))]
                        [r1 (register-offset (second params))]
                        [r1-sel (register-field (second params))]
                        [r2 (register-offset (third params))]
                        [r2-sel (register-field (third params))])
                    (fill-in-bitfields
                      (first enc-0)
                      (hash-union
                        (hash 'op 0 'alu-op op-num 'io 0)
                        (hash 'r2-sel r2-sel 'r2 r2
                              'r1-sel r2-sel 'r1 r1
                              'rd-sel rd-sel 'rd rd))))]
                 [(match-params params (list register? register? immediate?))
                  (let ([rd (register-offset (first params))]
                        [rd-sel (register-field (first params))]
                        [r1 (register-offset (second params))]
                        [r1-sel (register-field (second params))]
                        [value (immediate-value (third params))])
                    (fill-in-bitfields
                      (second enc-0)
                      (hash-union
                        (hash 'op 0 'alu-op op-num 'io 1)
                        (hash 'imm-8 value
                              'r1-sel r1-sel 'r1 r1
                              'rd-sel rd-sel 'rd rd))))]
                 [else (error decode-fail-msg)]))))
         (list "add" "adc" "sub" "suc" "lsl" "lsr" "rsb" "rsc" "and" "or" "xor" "not" "min" "max" "clr" "set")
         (range 16))

    ; build instructions of encoding 1

    (hash-set!
      instructions "jmp"
      (lambda (params)
        (cond
          [(match-params params (list register?))
           (let ([r2 (register-offset (first params))]
                 [r2-sel (register-field (first params))])
             (fill-in-bitfields
               (first enc-1)
               (hash-union
                 (hash 'op 1 'sub-op 0 'io 0 '_ 0)
                 (hash 'r2-sel r2-sel 'r2 r2))))]
          [(match-params params (list immediate?))
           (let ([value (immediate-value (first params))])
             (fill-in-bitfields
               (third enc-1)
               (hash-union
                 (hash 'op 1 'sub-op 0 'io 1 '_ 0)
                 (hash 'imm-16 value))))]
          [else (error decode-fail-msg)])))

    (hash-set!
      instructions "jal"
      (lambda (params)
        (cond
          [(match-params params (list register? register?))
           (let ([rd (register-offset (first params))]
                 [rd-sel (register-field (first params))]
                 [r2 (register-offset (second params))]
                 [r2-sel (register-field (second params))])
             (fill-in-bitfields
               (second enc-1)
               (hash-union
                 (hash 'op 1 'sub-op 1 'io 0 '_ 0)
                 (hash 'r2-sel r2-sel 'r2 r2 'rd-sel rd-sel 'rd rd))))]
          [(match-params params (list register? immediate?))
           (let ([rd (register-offset (first params))]
                 [rd-sel (register-field (first params))]
                 [value (immediate-value (second params))])
             (fill-in-bitfields
               (fourth enc-1)
               (hash-union
                 (hash 'op 1 'sub-op 1 'io 1)
                 (hash 'rd-sel rd-sel 'rd rd 'imm-16 value))))]
          [else (error decode-fail-msg)])))

    ; ldi
    (hash-set!
      instructions "ldi"
      (lambda (params)
        (cond
          [(match-params params (list register? immediate?))
           (let ([rd (register-offset (first params))]
                 [rd-sel (register-field (first params))]
                 [value (immediate-value (second params))])
             (fill-in-bitfields
               (fourth enc-1)
               (hash-union
                 (hash 'op 1 'sub-op 2 'io 0)
                 (hash 'rd-sel rd-sel 'rd rd 'imm-16 value))))]
          [else (error decode-fail-msg)])))

    ; lmbd and scan
    (map (lambda (name sub-op)
           (hash-set!
             instructions name
             (lambda (params)
               (cond
                 [(match-params params (list register? register?))
                  (let ([rd (register-offset (first params))]
                        [rd-sel (register-field (first params))]
                        [r2 (register-offset (second params))]
                        [r2-sel (register-field (second params))])
                    (fill-in-bitfields
                      (second enc-1)
                      (hash 'op 1 'sub-op sub-op 'io 0
                            'r2-sel r2-sel 'r2 r2
                            'rd-sel rd-sel 'rd rd)))]
                 [(match-params params (list register? immediate?))
                  (let ([rd (register-offset (first params))]
                        [rd-sel (register-field (first params))]
                        [value (immediate-value (second params))])
                    (fill-in-bitfields
                      (fifth enc-1)
                      (hash 'op 1 'sub-op sub-op 'io 1
                            'imm-8 value
                            'rd-sel rd-sel 'rd rd)))]
                 [else (error decode-fail-msg)]))))
         (list "lmbd" "scan") '(3 4))

    ; halt
    (hash-set!
      instructions "halt"
      (lambda (params)
        (cond
          [(empty? params)
           (fill-in-bitfields
             (sixth enc-1)
             (hash 'op 1 'sub-op 5 '_ 0))]
          [else (error decode-fail-msg)])))

    ; slp
    (hash-set!
      instructions "slp"
      (lambda (params)
        (cond
          [(match-params params (list immediate?))
           (let ([value (immediate-value (first params))])
             (fill-in-bitfields
               (seventh enc-1)
               (hash 'op 1 'sub-op 15 'slp value '_ 0)))]
          [else (error decode-fail-msg)])))

    ; build instructions of encoding 2
    
    (map (lambda (name op-num test)
           (hash-set!
             instructions name
             (lambda (params)
               (cond
                 [(match-params params (list immediate? register? register?))
                  (let ([broff (immediate-value (first params))]
                        [r1 (register-offset (second params))]
                        [r1-sel (register-field (second params))]
                        [r2 (register-offset (third params))]
                        [r2-sel (register-field (third params))])
                    (fill-in-bitfields
                      (first enc-2)
                      (hash 'op op-num 'test test
                            'r2-sel r2-sel 'r2 r2
                            'r1-sel r1-sel 'r1 r1
                            'broff-h (bit-slice broff 8 2)
                            'broff-l (bit-slice broff 0 8))))]
                 [(match-params params (list immediate? register? immediate?))
                  (let ([broff (immediate-value (first params))]
                        [r1 (register-offset (second params))]
                        [r1-sel (register-field (second params))]
                        [value (immediate-value (third params))])
                    (fill-in-bitfields
                      (second enc-2)
                      (hash 'op op-num 'test test
                            'imm-8 value
                            'r1-sel r1-sel 'r1 r1
                            'broff-h (bit-slice broff 8 2)
                            'broff-l (bit-slice broff 0 8))))]
                 [else (error decode-fail-msg)]))))
         (list "qblt" "qbeq" "qble" "qbgt" "qbne" "qbge" "qba")
         '(2    2    2    3    3    3    3)
         (range 1 8))
        
    ; build instructions of encoding 3

    (map (lambda (name test)
           (hash-set!
             instructions name
             (lambda (params)
               (cond
                 [(match-params params (list immediate? register? register?))
                  (let ([broff (immediate-value (first params))]
                        [r1 (register-offset (second params))]
                        [r1-sel (register-field (second params))]
                        [r2 (register-offset (third params))]
                        [r2-sel (register-field (third params))])
                    (fill-in-bitfields
                      (first enc-3)
                      (hash 'op 6 'test test 'io 0
                            'r2-sel r2-sel 'r2 r2
                            'r1-sel r1-sel 'r1 r1
                            'broff-h (bit-slice broff 8 2)
                            'broff-l (bit-slice broff 0 8))))]
                 [(match-params params (list immediate? register? immediate?))
                  (let ([broff (immediate-value (first params))]
                        [r1 (register-offset (second params))]
                        [r1-sel (register-field (second params))]
                        [value (immediate-value (third params))])
                    (fill-in-bitfields
                      (second enc-3)
                      (hash 'op 6 'test test 'io 1
                            'imm-5 value
                            'r1-sel r1-sel 'r1 r1
                            'broff-h (bit-slice broff 8 2)
                            'broff-l (bit-slice broff 0 8))))]
                 [else (error decode-fail-msg)]))))
         (list "qbbc" "qbbs") '(1 2))

    ; build instructions of encoding 4

    (map (lambda (name op-num l/s)
           (hash-set!
             instructions name
             (lambda (params)
               (cond
                 [(match-params params (list register? register? register? immediate?))
                  (let ([rb (register-offset (first params))]
                        [rb-sel (register-field (first params))]
                        [rx (register-offset (second params))]
                        [rx-sel (register-field (second params))]
                        [ro (register-offset (third params))]
                        [ro-sel (register-field (third params))]
                        [burstlen (immediate-value (fourth params))])
                    (fill-in-bitfields
                      (first enc-4)
                      (hash 'op op-num 'l/s l/s 'io 0
                            'ro-sel ro-sel 'ro ro
                            'rb rb
                            'rx-sel rx-sel 'rx rx
                            'bl-h (bit-slice burstlen 4 3)
                            'bl-m (bit-slice burstlen 1 3)
                            'bl-l (bit-slice burstlen 0 1))))]
                 [(match-params params (list register? register? immediate? immediate?))
                  (let ([rb (register-offset (first params))]
                        [rb-sel (register-field (first params))]
                        [rx (register-offset (second params))]
                        [rx-sel (register-field (second params))]
                        [value (immediate-value (third params))]
                        [burstlen (immediate-value (fourth params))])
                    (fill-in-bitfields
                      (second enc-4)
                      (hash 'op op-num 'l/s l/s 'io 1
                            'imm-8 value
                            'rb rb
                            'rx-sel rx-sel 'rx rx
                            'bl-h (bit-slice burstlen 4 3)
                            'bl-m (bit-slice burstlen 1 3)
                            'bl-l (bit-slice burstlen 0 1))))]
                 [else (error decode-fail-msg)]))))
         (list "sbbo" "lbbo" "sbco" "lbco")
         '(7    7    4    4)
         '(0    1    0    1))

    instructions
    ))

(define instructions (build-instructions))
