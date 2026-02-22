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

    /* Build the initial stack frame from the top of this task's stack.*/
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
/* PendSV_Handler - context switch (Cortex-M0+ assembly, IAR)          */
/* ------------------------------------------------------------------ */
/*
 * When PendSV fires the hardware has already saved R0-R3, R12, LR, PC,
 * xPSR (exception frame) onto the current task's PSP.
 * PendSV saves R4-R11 manually, switches vCurrentTCB, then restores
 * R4-R11 of the next task and returns (hardware restores the rest).
 */
__asm void PendSV_Handler(void)
{
    IMPORT  vCurrentTCB

    /* ---- save current task ---- */
    MRS     R0, PSP             /* R0 = PSP of the running task          */

    SUBS    R0, R0, #32         /* make room for R4-R11 (8 * 4 bytes)    */
    STMIA   R0!, {R4-R7}        /* save R4-R7; R0 advances +16           */

    MOV     R4, R8              /* move R8-R11 into low registers so     */
    MOV     R5, R9              /* STMIA can reach them (CM0+ limit)     */
    MOV     R6, R10
    MOV     R7, R11
    STMIA   R0!, {R4-R7}        /* save R8-R11; R0 advances +16          */

    SUBS    R0, R0, #32         /* R0 back to start of software frame    */

    /* store updated SP into current TCB */
    LDR     R3, =vCurrentTCB    /* R3 = &vCurrentTCB                     */
    LDR     R2, [R3]            /* R2 = vCurrentTCB                      */
    STR     R0, [R2]            /* vCurrentTCB->vStackPointer = R0       */

    /* ---- advance to next task (round-robin) ---- */
    LDR     R1, [R2, #4]        /* R1 = vCurrentTCB->vNext               */
    STR     R1, [R3]            /* vCurrentTCB = vNext                   */

    /* ---- restore next task ---- */
    LDR     R0, [R1]            /* R0 = new task's vStackPointer         */

    /* restore R8-R11 first (via R4-R7 temp), then restore R4-R7 cleanly */
    ADDS    R1, R0, #16         /* R1 points to R8-slot in saved frame   */
    LDMIA   R1!, {R4-R7}        /* load saved R8-R11 into R4-R7 temp     */
    MOV     R8,  R4
    MOV     R9,  R5
    MOV     R10, R6
    MOV     R11, R7

    LDMIA   R0!, {R4-R7}        /* restore actual R4-R7; R0 advances +16 */
    ADDS    R0, R0, #16         /* skip the R8-R11 slots already loaded  */

    MSR     PSP, R0             /* PSP now points at the hw exception frame */
    BX      LR                  /* EXC_RETURN: hardware pops hw frame       */
}


/* ------------------------------------------------------------------ */
/* SysTick_Handler - request a context switch every tick               */
/* ------------------------------------------------------------------ */
void SysTick_Handler(void)
{
    /* Pend the PendSV exception so the switch happens after this ISR exits */
    *((volatile uint32_t *)0xE000ED04UL) = (1UL << 28);
}


/* ------------------------------------------------------------------ */
/* vKernel_Start - launch the first task, never returns                */
/* ------------------------------------------------------------------ */
/*
 * Sets PSP to the top of the first task's stack, switches CONTROL to
 * use PSP in thread mode, then jumps directly to the task entry. All
 * subsequent tasks are launched by the exception-return mechanism in
 * PendSV_Handler when they are first scheduled.
 */
__asm void vKernel_Start(void)
{
    IMPORT  vCurrentTCB

    /* get first task's saved SP (points at its R4 slot) */
    LDR     R0, =vCurrentTCB
    LDR     R0, [R0]            /* R0 = vCurrentTCB                      */
    LDR     R0, [R0]            /* R0 = vStackPointer                    */

    /* extract task PC from pre-built frame (R4-R11 = 8 words, then
     * R0,R1,R2,R3,R12,LR,PC => PC is word 14 from base = offset 56) */
    LDR     R1, [R0, #56]       /* R1 = task entry function              */

    /* set PSP to just above the full 16-word pre-built frame */
    ADDS    R0, R0, #64
    MSR     PSP, R0             /* PSP = top of first task's stack       */

    /* switch thread mode to use PSP (CONTROL bit1 = 1) */
    MOVS    R0, #2
    MSR     CONTROL, R0
    ISB                         /* synchronise after CONTROL change      */

    /* enable interrupts - SysTick will now fire */
    CPSIE   I

    /* jump to first task */
    BX      R1
}
