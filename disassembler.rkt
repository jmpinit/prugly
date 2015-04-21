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

(define (fmt-instr . values)
  (match values
         [(list 'naked instr)
          (format "~a" instr)]

         [(list 'reg instr r rsel)
          (format "~a\tr~v~a" instr r (sel rsel))]

         [(list 'reg-reg instr rd rdsel r1 rsel1 r2 rsel2)
          (format "~a\tr~v~a, r~v~a" instr rd (sel rdsel) r1 (sel rdsel) r2 (sel rdsel))]

         [(list 'reg-reg-reg instr rd rdsel r1 rsel1 r2 rsel2)
          (format "~a\tr~v~a, r~v~a, r~v~a" instr rd (sel rdsel) r1 (sel rdsel) r2 (sel rdsel))]

         [(list 'imm instr imm-value)
          (format "~a\t0x~x" instr imm-value)]

         [(list 'reg-imm instr rd rdsel imm-value)
          (format "~a\tr~v~a, 0x~x" instr rd (sel rdsel) imm-value)]

         [_ (error "unrecognized format.")]))

; TODO special syntax for bit fields
(define (disassemble-fmt-0 instr) "___")

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
             [(0) (fmt-instr 'reg "jmp" rs2 rsel2)]
             [(1) (fmt-instr 'imm "jmp" imm-16)])]
      [(1) (case io
             [(0) (fmt-instr 'reg-reg "jal" rd rdsel rs2 rsel2)]
             [(1) (fmt-instr 'reg-imm "jal" rd rdsel imm-16)])]
      [(2) (fmt-instr 'reg-imm "ldi" rd rdsel imm-16)]
      [(3) (case io
             [(0) (fmt-instr 'reg-reg-reg "lmbd" rd rdsel rs1 rsel1 rs2 rsel2)]
             [(1) (fmt-instr 'reg-reg-imm "lmbd" rd rdsel rs1 rsel1 imm-8)])]
      [(4) (case io
             [(0) (fmt-instr 'reg-reg "scan" rd rdsel rs2 rsel2)]
             [(1) (fmt-instr 'reg-reg-imm "scan" rd rdsel rs1 rsel1 imm-8)])]
      [(5)  (fmt-instr 'naked "halt")]
      [(15) (fmt-instr 'imm "slp" bit-slp)])))

(define (disassemble-fmt-2 instr) "___")
(define (disassemble-fmt-3 instr) "___")
(define (disassemble-fmt-4 instr) "___")

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
