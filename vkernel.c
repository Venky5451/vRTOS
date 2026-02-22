/*
 * \file vkernel.c
 *
 * Minimal round-robin kernel for PSoC4HV (Cortex-M0+).
 *
 * @brief
 *   SysTick fires every tick, pends PendSV.
 *   PendSV (lowest priority) saves the current task's R4-R11 below its PSP,
 *   advances vCurrentTCB to the next node in the ring, restores R4-R11 of
 *   the incoming task and lets exception-return pop R0-R3, R12, LR, PC, xPSR.
 *
 * Stack frame layout per task (low address -> high address):
 *   [R4][R5][R6][R7][R8][R9][R10][R11]  <- software-saved (PendSV)
 *   [R0][R1][R2][R3][R12][LR][PC][xPSR] <- hardware exception frame
 *
 * vTCB.vStackPointer always points at the R4 slot.
 */

#include "vkernel.h"

/* Stacks - one per task slot */
static uint32_t vStacks[vMAX_TASKS][vSTACK_SIZE];

/* TCB pool */
static vTCB vTcbPool[vMAX_TASKS];

/* Number of tasks added so far */
static uint8_t vTaskCount = 0;

/* Tail of the circular ring (used during task creation only) */
static vTCB *vTail = NULL;

/* Current running task - accessed from PendSV assembly */
vTCB *vCurrentTCB = NULL;



/* vKernel_Init*/

void vKernel_Init(void)
{
    /* Set PendSV to the lowest possible priority.*/
    *((volatile uint32_t *)0xE000ED20UL) |= (0xFFUL << 16);

    /* Configure SysTick.
     * LOAD (0xE000E014): reload value -> tick period
     * VAL  (0xE000E018): clear current counter
     * CTRL (0xE000E010): bit0=enable, bit1=tickint, bit2=use CPU clock */
    *((volatile uint32_t *)0xE000E014UL) = vSYSTICK_RELOAD;
    *((volatile uint32_t *)0xE000E018UL) = 0UL;
    *((volatile uint32_t *)0xE000E010UL) = 0x07UL;
}

/* ------------------------------------------------------------------ */
/* vKernel_AddTask                                                      */
/* ------------------------------------------------------------------ */
void vKernel_AddTask(vTask_t fn)
{
    if (vTaskCount >= vMAX_TASKS) return;

    /* Build the initial stack frame from the top of this task's stack.
     * Stack grows downward, so we pre-decrement. */
    uint32_t *sp = &vStacks[vTaskCount][vSTACK_SIZE];

    /* --- hardware exception frame (Cortex-M0+ exception entry order) --- */
    *(--sp) = 0x01000000UL;      /* xPSR  - Thumb bit must be set        */
    *(--sp) = (uint32_t)fn;      /* PC    - task entry                   */
    *(--sp) = 0xFFFFFFFDUL;      /* LR    - EXC_RETURN: thread, use PSP  */
    *(--sp) = 0UL;               /* R12                                  */
    *(--sp) = 0UL;               /* R3                                   */
    *(--sp) = 0UL;               /* R2                                   */
    *(--sp) = 0UL;               /* R1                                   */
    *(--sp) = 0UL;               /* R0                                   */

    /* --- software-saved context (R4-R11, saved/restored by PendSV) --- */
    *(--sp) = 0UL;               /* R11 */
    *(--sp) = 0UL;               /* R10 */
    *(--sp) = 0UL;               /* R9  */
    *(--sp) = 0UL;               /* R8  */
    *(--sp) = 0UL;               /* R7  */
    *(--sp) = 0UL;               /* R6  */
    *(--sp) = 0UL;               /* R5  */
    *(--sp) = 0UL;               /* R4  <- vStackPointer points here     */

    vTcbPool[vTaskCount].vStackPointer = sp;

    /* Link the new TCB into the circular ring */
    if (vTaskCount == 0) {
        vTcbPool[0].vNext = (struct vTCB_s *)&vTcbPool[0]; /* single-node ring */
    } else {
        vTcbPool[vTaskCount].vNext = (struct vTCB_s *)&vTcbPool[0]; /* wrap back to head */
        vTail->vNext = (struct vTCB_s *)&vTcbPool[vTaskCount];      /* old tail -> new   */
    }
    vTail = &vTcbPool[vTaskCount];

    vTaskCount++;
}


/* ------------------------------------------------------------------ */
/* SysTick_Handler - request a context switch every tick               */
/* ------------------------------------------------------------------ */
void SysTick_Handler(void)
{
    /* Pend the PendSV exception so the switch happens after this ISR exits */
    *((volatile uint32_t *)0xE000ED04UL) = (1UL << 28);
}
