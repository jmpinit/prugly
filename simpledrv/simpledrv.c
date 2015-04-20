#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

#include "prussdrv.h"
#include <pruss_intc_mapping.h>

#include "simpledrv.h"

uint32_t* pru = NULL;

int drv_init() {
    int fd = open("/dev/mem", O_RDWR | O_SYNC);

    if (fd == -1) {
        printf("ERROR: could not open /dev/mem.\n\n");
        return -1;
    }

    int pruss_len = 0x40000;
    int opt_pruss_addr = 0x4A300000;
    pru = (uint32_t*)mmap(0, pruss_len, PROT_READ | PROT_WRITE, MAP_SHARED, fd, opt_pruss_addr);

    if (pru == MAP_FAILED) {
        printf("ERROR: could not map memory.\n\n");
        return 1;
    }

    close(fd);

    printf("Using /dev/mem device.\n");

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

uint32_t drv_read_mem(int offset) {
    if (pru == NULL) {
        printf("pru is NULL!\n");
        return 0;
    } else {
        uint32_t v = pru[offset];
        return v;
    }
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
