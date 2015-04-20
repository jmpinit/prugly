#include <pru.h>

.origin 0

start:
    // we're already done
    mov     r31.b0, PRU0_ARM_INTERRUPT + 16
    halt
