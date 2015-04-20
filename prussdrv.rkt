#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-drv (ffi-lib "simpledrv/libsimpledrv"))

(define-drv drv_init (_fun -> _int))
(define-drv drv_open (_fun _int -> _int))
(define-drv drv_pruintc_init (_fun -> _int))
(define-drv drv_exec_program (_fun _int _string -> _int))
(define-drv drv_pru_wait_event (_fun _int -> _int))
(define-drv drv_pru_clear_event (_fun _int _int -> _int))
(define-drv drv_pru_disable (_fun _int -> _int))
(define-drv drv_exit (_fun -> _int))

(define PRU_EVTOUT_0 0) ; from prussdrv.h
(define PRU0_ARM_INTERRUPT 19) ; from pruss_intc_mapping.h

(displayln (drv_init))
(displayln (drv_open PRU_EVTOUT_0))
(displayln (drv_pruintc_init))
(displayln (drv_exec_program 0 "./test.bin"))
(displayln (drv_pru_wait_event PRU_EVTOUT_0))
(displayln (drv_pru_clear_event PRU_EVTOUT_0 PRU0_ARM_INTERRUPT))
(displayln (drv_pru_disable 0))
(displayln (drv_exit))

(displayln "done!")
