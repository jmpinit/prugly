#lang racket/base

(require "prussdrv.rkt")

(displayln (drv_init))
(displayln (drv_open PRU_EVTOUT_0))
(displayln (drv_pruintc_init))
(displayln (drv_exec_program 0 "./test.bin"))
(displayln (drv_pru_wait_event PRU_EVTOUT_0))
(displayln (drv_pru_clear_event PRU_EVTOUT_0 PRU0_ARM_INTERRUPT))
(displayln (drv_pru_disable 0))
(displayln (drv_exit))

(displayln "done!")
