#include <pru.h>

.origin 0

start:
    mov     r0, 0xdeadbeef
    mov     r1, 0x0cab005e
    mov     r29, 0x5ca1eab1
    mov     r31.b0, PRU0_ARM_INTERRUPT + 16
    halt
