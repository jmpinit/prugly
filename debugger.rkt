#lang racket

(require "prussdrv.rkt")

(provide pru-run)

(provide read-register
         read-registers
         read-status
         read-control)

(provide running?
         counting?
         single-stepping?
         sleeping?
         enabled?)

(define PRU_CTRL_REG        #x0000)
(define PRU_STATUS_REG      #x0001)
(define PRU_INTGPR_REG      #x0100)

(define PRU_REG_RUNSTATE        #x00008000)
(define PRU_REG_COUNT_EN        #x00000008)
(define PRU_REG_SINGLE_STEP     #x00000100)
(define PRU_REG_SLEEPING        #x00000004)
(define PRU_REG_PROC_EN         #x00000002)

(define PRU_REG_PCRESET_MASK    #x0000FFFF)
(define PRU_REG_SOFT_RESET      #x00000001)

(struct pru (number offset-instructions offset-data offset-control))

(define prus (list (pru 0 #xD000 #x0000 #x8800)
                   (pru 1 #xE000 #x0800 #x9000)))

; create function that returns
; true if status bit is set
(define (control-checker bitmask)
    (lambda (prunum)
      (> (bitwise-and bitmask (read-control prunum))
         0)))

(define running?            (control-checker PRU_REG_RUNSTATE))
(define counting?           (control-checker PRU_REG_COUNT_EN))
(define single-stepping?    (control-checker PRU_REG_SINGLE_STEP))
(define sleeping?           (control-checker PRU_REG_SLEEPING))
(define enabled?            (control-checker PRU_REG_PROC_EN))

(define (register-address prunum reg)
  (let* ([the-pru (list-ref prus prunum)]
         [reg-offset (+ PRU_INTGPR_REG (pru-offset-control the-pru))])
    (+ reg-offset reg)))

(define (read-status prunum)
  (let* ([the-pru (list-ref prus prunum)]
         [reg-offset (+ PRU_STATUS_REG (pru-offset-control the-pru))])
    (drv_read_mem reg-offset)))

(define (read-control prunum)
  (let* ([the-pru (list-ref prus prunum)]
         [reg-offset (+ PRU_CTRL_REG (pru-offset-control the-pru))])
    (drv_read_mem reg-offset)))

(define (read-register prunum reg)
  (drv_read_mem (register-address prunum reg)))

(define (pru-run pru dwords)
  (let* ([bin-path (make-temporary-file "temp-~a.bin")]
         [bin-port (open-output-file bin-path #:exists 'replace)]
         [program-values
           (flatten
             (map (lambda (dword)
                    (bytes->list (integer->integer-bytes dword 4 #f)))
                  dwords))]
         [program-bytes (apply bytes program-values)])
    ; get ready
    (drv_init)
    (drv_open PRU_EVTOUT_0)
    (drv_pruintc_init)

    ; run code
    (write-bytes program-bytes bin-port)
    (close-output-port bin-port)
    (drv_exec_program pru bin-path)

    ; clean up
    (drv_pru_wait_event PRU_EVTOUT_0)
    (drv_pru_clear_event PRU_EVTOUT_0 PRU0_ARM_INTERRUPT)
    (drv_pru_disable pru)
    (drv_exit)))

(define (pru-reset pru)
  (if (or (= pru 0) (= pru 1))
    (begin
      (drv_init)
      (drv_open PRU_EVTOUT_0)
      (drv_pruintc_init)
      ;(drv_exec_program 0 "./test.bin")
      ;(drv_exec_code 0 program-reg-test)
      (drv_pru_wait_event PRU_EVTOUT_0))
    (error (format "PRU~a does not exist."))))

(define (pru-kill pru)
  (if (or (= pru 0) (= pru 1))
    (begin
      (drv_pru_clear_event PRU_EVTOUT_0 PRU0_ARM_INTERRUPT)
      (drv_pru_disable 0)
      (drv_exit))
    (error (format "PRU~a does not exist."))))

(define (read-registers pru)
  (if (or (= pru 0) (= pru 1))
    (map ((curry read-register) pru) (range 32))
    (error (format "PRU~a does not exist."))))
