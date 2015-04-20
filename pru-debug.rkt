#lang racket/base

(require "prussdrv.rkt")

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
      (> (bitwise-and bitmask (control-register prunum))
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

(define (status-register prunum)
  (let* ([the-pru (list-ref prus prunum)]
         [reg-offset (+ PRU_STATUS_REG (pru-offset-control the-pru))])
    (drv_read_mem reg-offset)))

(define (control-register prunum)
  (let* ([the-pru (list-ref prus prunum)]
         [reg-offset (+ PRU_CTRL_REG (pru-offset-control the-pru))])
    (drv_read_mem reg-offset)))

(define (register-read prunum reg)
  (drv_read_mem (register-address prunum reg)))

(provide register-read status-register control-register)
(provide running? counting? single-stepping? sleeping? enabled?)
