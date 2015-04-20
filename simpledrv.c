//#include <stdio.h>
//#include <stdlib.h>
//#include <stdint.h>
//#include <sys/mman.h>
//#include <fcntl.h>
//#include <errno.h>
//#include <unistd.h>
//#include <string.h>

#include "prussdrv.h"
#include <pruss_intc_mapping.h>

#include "simpledrv.h"

int drv_init() {
    return prussdrv_init();
}

int drv_open(unsigned int host_interrupt) {
    return prussdrv_open(host_interrupt);
}

int drv_pruintc_init() {
    tpruss_intc_initdata pruss_intc_initdata = PRUSS_INTC_INITDATA;
    return prussdrv_pruintc_init(&pruss_intc_initdata);
}

int drv_exec_program(int prunum, const char *filename) {
    return prussdrv_exec_program(prunum, filename);
}

int drv_pru_wait_event(unsigned int host_interrupt) {
    return prussdrv_pru_wait_event(host_interrupt);
}

int drv_pru_clear_event(unsigned int host_interrupt, unsigned int sysevent) {
    return prussdrv_pru_clear_event(host_interrupt, sysevent);
}

int drv_pru_disable(unsigned int prunum) {
    return prussdrv_pru_disable(prunum);
}

int drv_exit() {
    return prussdrv_exit();
}
