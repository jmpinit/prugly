#ifndef _H_SIMPLEDRV_
#define _H_SIMPLEDRV_

int drv_init(void);
int drv_open(unsigned int host_interrupt);
int drv_pruintc_init(void);
int drv_exec_program(int prunum, const char *filename);
int drv_pru_wait_event(unsigned int host_interrupt);
int drv_pru_clear_event(unsigned int host_interrupt, unsigned int sysevent);
int drv_pru_disable(unsigned int prunum);
int drv_exit(void);

#endif
