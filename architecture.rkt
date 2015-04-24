#lang racket

(require macro-debugger/stepper-text)

; make a number from a bit range
(define (bit-slice num start len)
  (let ([mask (- (expt 2 len) 1)])
    (bitwise-and (arithmetic-shift num (- start)) mask)))

(define-syntax let-bitfields
  (syntax-rules ()
    [(let-bitfields size value ([name len]) body)
     (let ([name (bit-slice value (- size len) len)]) body)]
    [(let-bitfields size value ([name len] name-len2 ...) body)
     (let ([name (bit-slice value (- size len) len)])
       (let-bitfields (- size len) value (name-len2 ...) body))]))

(define-syntax let-instruction-fields
  (syntax-rules ()
    [(let-instruction-fields instruction (name-len ...) body)
     (let-bitfields 32 instruction (name-len ...) body)]))

; produces a hash where keys name the values of fields of bits
; fields are specified in a list of lists like '((a 3) (b 5))
; fields are extracted from the given number left to right
(define (extract-bitfields size value fields)
  (let* ([field-names (map car fields)]
         [field-lengths (map second fields)]
         [total-length (foldl + 0 field-lengths)]
         [positions (map (lambda (i)
                           (let* ([lengths-to-left (take field-lengths i)]
                                  [sum-to-left (foldl + 0 lengths-to-left)])
                             (- size sum-to-left)))
                         (range (length fields)))]
         [field-values (map (lambda (len pos) (bit-slice value (- pos len) len))
                            field-lengths positions)])
    (if (<= total-length size)
      (make-hash (map cons field-names field-values))
      (error "fields extract more bits than value contains."))))

;define format-0 '([op 3] [alu-op 4] [io 1] [imm-8 8] [rsel1 3] [rs1 5] [rdsel 3] [rd 5]))

;instr ([op 3] [sub-op 4] [io 1] [imm-16 16] [rdsel 3] [rd 5])
;instr ([op 2] [test 3] [br-off-h 2] [io 1] [imm-8 8] [rsel1 3] [rs1 5] [br-off-l 8])
;instr ([op 3] [test 2] [br-off-h 2] [io 1] [rsel2 3] [rs2/imm-5 5] [rsel1 3] [rs1 5] [br-off-l 8])
;instr ([op 3] [load/store 1] [blen-h 3] [io 1] [imm-8 8] [blen-m 3] [rb 5] [blen-l 1] [rx-byte 2] [rx 5])

(provide bit-slice extract-bitfields let-bitfields let-instruction-fields)
