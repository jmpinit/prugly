#lang racket

(require "prussdrv.rkt")
(require "pru-debug.rkt")
(require "assembler.rkt")
(require "disassembler.rkt")

(define (print-registers)
  (let* ([reg-nums (range 31)]
         [reg-values (map (lambda (r) (register-read 0 r)) reg-nums)])
    (void
      (map (lambda (i v) (fprintf (current-output-port) "r~a = ~X\n" i v))
           reg-nums reg-values))))

(define program-reg-test
  (assemble "mov r31.b0, 35"
            "halt"))

(define (pru-reset)
  (void
    (drv_init)
    (drv_open PRU_EVTOUT_0)
    (drv_pruintc_init)
    ;(drv_exec_program 0 "./test.bin")
    ;(drv_exec_code 0 program-reg-test)
    (drv_pru_wait_event PRU_EVTOUT_0)))

(define (pru-kill)
  (void
    (drv_pru_clear_event PRU_EVTOUT_0 PRU0_ARM_INTERRUPT)
    (drv_pru_disable 0)
    (drv_exit)))

(void (display
  (format-assembly-code (disassemble "./test.bin"))))

;(displayln program-reg-test)

;(pru-reset)
;(print-registers)
;(pru-kill)

