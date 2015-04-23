#lang racket

(require "architecture.rkt")

; split list into lists of n elements
(define (split-by lst n)
  (if (not (empty? lst))
    (cons (take lst n) (split-by (drop lst n) n))
    '()))

(define (read-instructions port)
  (let ([instr-bytes (bytes->list (port->bytes port))])
    (if (= (modulo (length instr-bytes) 4) 0)
      (map (lambda (bs) (integer-bytes->integer (apply bytes bs) #f))
           (split-by instr-bytes 4))
      (error "File is not purely double words (not divisible by 4)."))))

(define selector-names
  (list ".b0" ".b1" ".b2" ".b3" ".w0" ".w1" ".w2" ""))

(define (sel v)
  (list-ref selector-names v))

(define (fmt-instr types instr . values)
  (define param-fmts (hash 'naked "" 'reg "r~v~a" 'imm "0x~x"))

  (string-join
    (list
      (format "~a\t" instr)
      (string-join
        (map (lambda (t v)
               (let ([fmt (hash-ref param-fmts t)])
                 (if (list? v)
                   (apply ((curry format) fmt) v)
                   (format fmt v))))
             types values)
        ", "))
    ""))

(define (disassemble-fmt-0 instr)
  (define instructions
    (list   "add" "adc" "sub" "suc"
            "lsl" "lsr" "rsb" "rsc"
            "and" "or"  "xor" "not"
            "min" "max" "clr" "set"))

  (let-instruction-fields
    instr ([op 3] [alu-op 4] [io 1] [imm-8 8] [rsel1 3] [rs1 5] [rdsel 3] [rd 5])
    (begin
      (define rsel2   (bit-slice imm-8 5 3))
      (define rs2     (bit-slice imm-8 0 5))

      (case io
        [(0) (fmt-instr '(reg reg reg)
                        (list-ref instructions alu-op)
                        (list rd  rdsel)
                        (list rs1 rsel1)
                        (list rs2 rsel2))]
        [(1) (fmt-instr '(reg reg imm)
                        (list-ref instructions alu-op)
                        (list rd  rdsel)
                        (list rs1 rsel1)
                        (list rs2 rsel2))]))))

(define (disassemble-fmt-1 instr)
  (let-instruction-fields
    instr ([op 3] [sub-op 4] [io 1] [imm-16 16] [rdsel 3] [rd 5])
    (extract-bitfields
      16 imm-16 ([rsel2 3] [rs2 5] [rsel1 3] [rs1 5])
      (begin
        (define imm-8 (bit-slice imm-16 8 8))
        (define bit-slp (bit-slice instr 23 1))

        (case sub-op
          [(0) (case io
                 [(0) (fmt-instr '(reg) "jmp" (list rs2 rsel2))]
                 [(1) (fmt-instr '(imm) "jmp" imm-16)])]
          [(1) (case io
                 [(0) (fmt-instr '(reg reg) "jal" (list rd rdsel) (list rs2 rsel2))]
                 [(1) (fmt-instr '(reg imm) "jal" (list rd rdsel) imm-16)])]
          [(2) (fmt-instr '(reg imm) "ldi" (list rd rdsel) imm-16)]
          [(3) (case io
                 [(0) (fmt-instr '(reg reg reg) "lmbd" (list rd rdsel) (list rs1 rsel1) (list rs2 rsel2))]
                 [(1) (fmt-instr '(reg reg imm) "lmbd" (list rd rdsel) (list rs1 rsel1) imm-8)])]
          [(4) (case io
                 [(0) (fmt-instr '(reg reg) "scan" (list rd rdsel) (list rs2 rsel2))]
                 [(1) (fmt-instr '(reg reg imm) "scan" (list rd rdsel) (list rs1 rsel1) imm-8)])]
          [(5)  (fmt-instr '() "halt")]
          [(15) (fmt-instr '(imm) "slp" bit-slp)])))))

(define (disassemble-fmt-2 instr)
  (define instructions (list "xxx" "qblt" "qbeq" "qble" "qbgt" "qbne" "qbge" "qba"))

  (let-instruction-fields
    instr ([op 2] [test 3] [br-off-h 2] [io 1] [imm-8 8] [rsel1 3] [rs1 5] [br-off-l 8])
    (begin
      (define br-off (bitwise-ior (arithmetic-shift br-off-h 8) br-off-l))
      (define rsel2 (bit-slice imm-8 5 3))
      (define rs2   (bit-slice imm-8 0 5))

      ; FIXME error on xxx
      (define opname (list-ref instructions test))

      (case io
        [(0) (fmt-instr '(imm reg reg) opname br-off (list rs1 rsel1) (list rs2 rsel2))]
        [(1) (fmt-instr '(imm reg imm) opname br-off (list rs1 rsel1) imm-8)]))))

(define (disassemble-fmt-3 instr)
  (define instructions (list "xxx" "qbbc" "qbbs" "xxx"))

  (let-instruction-fields
    instr ([op 3] [test 2] [br-off-h 2] [io 1] [rsel2 3] [rs2/imm-5 5] [rsel1 3] [rs1 5] [br-off-l 8])
    (begin
      (define br-off (bitwise-ior (arithmetic-shift br-off-h 8) br-off-l))

      ; FIXME error on xxx
      (define opname (list-ref instructions test))

      (case io
        [(0) (fmt-instr '(imm reg reg) opname br-off (list rs1 rsel1) (list rs2/imm-5 rsel2))]
        [(1) (fmt-instr '(imm reg imm) opname br-off (list rs1 rsel1) rs2/imm-5)]))))

(define (disassemble-fmt-4 instr)
  (define (instruction op load-store)
    (match (list op load-store)
      ['(7 0) "sbbo"]
      ['(7 1) "lbbo"]
      ['(4 0) "sbco"]
      ['(4 1) "lbco"]
      [_ (error "unrecognized instruction.")]))

  (let-instruction-fields
    instr ([op 3] [load/store 1] [blen-h 3] [io 1] [imm-8 8] [blen-m 3] [rb 5] [blen-l 1] [rx-byte 2] [rx 5])
    (begin
      (define rosel (bit-slice imm-8 5 3))
      (define ro (bit-slice imm-8 0 5))
      (define burst-len
        (bitwise-ior
          (arithmetic-shift blen-h 4)
          (arithmetic-shift blen-m 1)
          blen-l))

      (if (< burst-len 124)
        (case io
          [(0) (fmt-instr '(reg reg reg imm) (instruction op load/store) (list rx rx-byte) (list rb 7) (list ro rosel) burst-len)]
          [(1) (fmt-instr '(reg reg imm imm) (instruction op load/store) (list rx rx-byte) (list rb 7) imm-8 burst-len)])
        (let ([rsel0 (- burst-len 124)])
          (case io
            [(0) (fmt-instr '(reg reg reg reg) (instruction op load/store) (list rx rx-byte) (list rb 7) (list ro rosel) (list 0 rsel0))]
            [(1) (fmt-instr '(reg reg imm reg) (instruction op load/store) (list rx rx-byte) (list rb 7) imm-8 (list 0 rsel0))]))))))

(define (disassemble-instruction instr)
  (let ([opcode (bit-slice instr 29 3)])
    (case opcode
      [(0)      (disassemble-fmt-0 instr)]
      [(1)      (disassemble-fmt-1 instr)]
      [(2 3)    (disassemble-fmt-2 instr)]
      [(6)      (disassemble-fmt-3 instr)]
      [(4 7)    (disassemble-fmt-4 instr)]
      [else "unknown"])))

(define (disassemble filepath)
  (if (file-exists? filepath)
    (let ([file (open-input-file filepath)])
      (map disassemble-instruction (read-instructions file)))
    (error "File does not exist!")))

(define (format-assembly-code lines)
  (string-join lines "\n"))

(provide disassemble format-assembly-code)
