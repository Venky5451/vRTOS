/*
 * vkernel.h
 *
 * Round-robin kernel for PSoC4HV (Cortex-M0+).
 * Tasks are switched every SysTick tick via PendSV.
 */

#ifndef VKERNEL_H
#define VKERNEL_H

#include <stdint.h>

/* Max number of tasks the kernel can hold */
#define vMAX_TASKS     5

/* Stack depth per task in 32-bit words */
#define vSTACK_SIZE    128

/* SysTick reload value
 * Default assumes 48 MHz => 1 ms tick. */
#define vSYSTICK_RELOAD   47999UL

/* Task function signature */
typedef void (*vTask_t)(void);

/* Thread Control Block - one per task */
typedef struct vTCB_s {
    uint32_t       *vStackPointer;  /* saved stack pointer (PSP) */
    struct vTCB_s  *vNext;          /* next TCB in the round-robin ring */
} vTCB;

/* ---- kernel API ---- */

/* Initialise SysTick and set PendSV to lowest priority */
void vKernel_Init(void);

/* Add a task to the scheduler ring */
void vKernel_AddTask(vTask_t fn);

/* Start the kernel - never returns */
void vKernel_Start(void);

#endif /* VKERNEL_H */
