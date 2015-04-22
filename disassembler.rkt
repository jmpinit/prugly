#lang racket

; split list into lists of n elements
(define (split-by lst n)
  (if (not (empty? lst))
    (cons (take lst n) (split-by (drop lst n) n))
    '()))

; make a number from a bit range
(define (bit-slice num start len)
  (let ([mask (- (expt 2 len) 1)])
    (bitwise-and (arithmetic-shift num (- start)) mask)))


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

; TODO special syntax for bit fields
(define (disassemble-fmt-0 instr)
  (define instructions
    (list   "add" "adc" "sub" "suc"
            "lsl" "lsr" "rsb" "rsc"
            "and" "or"  "xor" "not"
            "min" "max" "clr" "set"))

  (let ([alu-op     (bit-slice instr 25 4)]
        [io         (bit-slice instr 24 1)]
        [imm-8      (bit-slice instr 16 8)]
        [rsel2      (bit-slice instr 21 3)]
        [rs2        (bit-slice instr 16 5)]
        [rsel1      (bit-slice instr 13 3)]
        [rs1        (bit-slice instr 8 5)]
        [rdsel      (bit-slice instr 5 3)]
        [rd         (bit-slice instr 0 5)])

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
                      (list rs2 rsel2))])))

(define (disassemble-fmt-1 instr)
  (let ([sub-opcode (bit-slice instr 25 4)]
        [io         (bit-slice instr 24 1)]
        [imm-16     (bit-slice instr 8 16)]
        [imm-8      (bit-slice instr 16 8)]
        [rsel2      (bit-slice instr 21 3)]
        [rs2        (bit-slice instr 16 5)]
        [rsel1      (bit-slice instr 13 3)]
        [rs1        (bit-slice instr 8 5)]
        [rdsel      (bit-slice instr 5 3)]
        [rd         (bit-slice instr 0 5)]
        [bit-slp    (bit-slice instr 23 1)])
    (case sub-opcode
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
      [(15) (fmt-instr '(imm) "slp" bit-slp)])))

(define (disassemble-fmt-2 instr)
  (define instructions (list "xxx" "qblt" "qbeq" "qble" "qbgt" "qbne" "qbge" "qba"))

  (let ([test       (bit-slice instr 27 3)]
        [io         (bit-slice instr 24 1)]
        [imm-8      (bit-slice instr 16 8)]
        [rsel2      (bit-slice instr 21 3)]
        [rs2        (bit-slice instr 16 5)]
        [rsel1      (bit-slice instr 13 3)]
        [rs1        (bit-slice instr 8 5)]
        [br-off     (bitwise-ior
                      (arithmetic-shift (bit-slice instr 25 2) 8)
                      (bit-slice instr 0 8))])

    ; FIXME error on xxx
    (define opname (list-ref instructions test))

    (case io
      [(0) (fmt-instr '(imm reg reg) opname br-off (list rs1 rsel1) (list rs2 rsel2))]
      [(1) (fmt-instr '(imm reg imm) opname br-off (list rs1 rsel1) imm-8)])))

(define (disassemble-fmt-3 instr)
  (define instructions (list "xxx" "qbbc" "qbbs" "xxx"))

  (let ([test       (bit-slice instr 27 2)]
        [io         (bit-slice instr 24 1)]
        [imm-5      (bit-slice instr 16 5)]
        [rsel2      (bit-slice instr 21 3)]
        [rs2        (bit-slice instr 16 5)]
        [rsel1      (bit-slice instr 13 3)]
        [rs1        (bit-slice instr 8 5)]
        [br-off     (bitwise-ior
                      (arithmetic-shift (bit-slice instr 25 2) 8)
                      (bit-slice instr 0 8))])

    ; FIXME error on xxx
    (define opname (list-ref instructions test))

    (case io
      [(0) (fmt-instr '(imm reg reg) opname br-off (list rs1 rsel1) (list rs2 rsel2))]
      [(1) (fmt-instr '(imm reg imm) opname br-off (list rs1 rsel1) imm-5)])))

(define (disassemble-fmt-4 instr)
  (define (instruction op load-store)
    (match (list op load-store)
      ['(7 0) "sbbo"]
      ['(7 1) "lbbo"]
      ['(4 0) "sbco"]
      ['(4 1) "lbco"]
      [_ (error "unrecognized instruction.")]))

  (let ([op         (bit-slice instr 29 3)]
        [load-store (bit-slice instr 28 1)]
        [io         (bit-slice instr 24 1)]
        [imm-8      (bit-slice instr 16 8)]
        [rosel      (bit-slice instr 21 3)]
        [ro         (bit-slice instr 15 5)]
        [rb         (bit-slice instr 8 5)]
        [rx-byte    (bit-slice instr 5 2)]
        [rx         (bit-slice instr 0 5)]
        [burst-len  (bitwise-ior
                      (arithmetic-shift (bit-slice instr 25 3) 4)
                      (arithmetic-shift (bit-slice instr 13 3) 1)
                      (bit-slice instr 7 1))])

    (if (< burst-len 124)
      (case io
        [(0) (fmt-instr '(reg reg reg imm) (instruction op load-store) (list rx rx-byte) (list rb 7) (list ro rosel) burst-len)]
        [(1) (fmt-instr '(reg reg imm imm) (instruction op load-store) (list rx rx-byte) (list rb 7) imm-8 burst-len)])
      (let ([rsel0 (- burst-len 124)])
        (case io
          [(0) (fmt-instr '(reg reg reg reg) (instruction op load-store) (list rx rx-byte) (list rb 7) (list ro rosel) (list 0 rsel0))]
          [(1) (fmt-instr '(reg reg imm reg) (instruction op load-store) (list rx rx-byte) (list rb 7) imm-8 (list 0 rsel0))])))))

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
