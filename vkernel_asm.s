;/******************************************************************************
; * vkernel_asm.s
; *
; * Context-switch and kernel-start routines for the vkernel.
; * These must live in a dedicated .s file because IAR's C compiler
; * pre-processes __asm blocks, which breaks '#' immediate operands
; * and 'IMPORT' directives.
; *
; * Cortex-M0+ (Thumb only - no Thumb-2 wide instructions).
; ******************************************************************************/

        MODULE  vkernel_asm

        EXTERN  vCurrentTCB     ; pointer to current task TCB (vkernel.c)

        PUBLIC  PendSV_Handler
        PUBLIC  vKernel_Start

        SECTION .text:CODE:REORDER:NOROOT(2)
        THUMB


;/******************************************************************************
; * PendSV_Handler
; *
; * Called at the lowest interrupt priority after SysTick sets PENDSVSET.
; * The hardware has already pushed {R0-R3, R12, LR, PC, xPSR} onto the
; * current task's PSP before entering this handler.
; *
; * Stack frame (growing downward, low addr first):
; *   [R4][R5][R6][R7][R8][R9][R10][R11]  <- software frame (we save this)
; *   [R0][R1][R2][R3][R12][LR][PC][xPSR] <- hardware frame (CPU saves/restores)
; *
; * vCurrentTCB->vStackPointer always points at the R4 slot.
; ******************************************************************************/
PendSV_Handler:

        ; --- save current task context ---

        MRS     R0, PSP             ; R0 = current task's PSP
        SUBS    R0, R0, #32         ; reserve 8 words for R4-R11
        STMIA   R0!, {R4-R7}        ; save R4-R7  (R0 advances +16)

        ; CM0+ STMIA can only reach R0-R7 directly, so move R8-R11 first
        MOV     R4, R8
        MOV     R5, R9
        MOV     R6, R10
        MOV     R7, R11
        STMIA   R0!, {R4-R7}        ; save R8-R11  (R0 advances +16)

        SUBS    R0, R0, #32         ; R0 back to top of software frame (R4 slot)

        ; store updated SP into current TCB
        LDR     R3, =vCurrentTCB    ; R3 = &vCurrentTCB
        LDR     R2, [R3]            ; R2 = vCurrentTCB
        STR     R0, [R2]            ; vCurrentTCB->vStackPointer = R0

        ; --- advance to next task (round-robin ring) ---

        LDR     R1, [R2, #4]        ; R1 = vCurrentTCB->vNext
        STR     R1, [R3]            ; vCurrentTCB = vNext

        ; --- restore next task context ---

        LDR     R0, [R1]            ; R0 = new task's vStackPointer (R4 slot)

        ; restore R8-R11 via temp registers (CM0+ limitation)
        ; ADDS Rd,Rn,#imm3 only allows 0-7 when Rd!=Rn, so copy first
        MOV     R1, R0
        ADDS    R1, R1, #16         ; R1 -> R8 slot in saved frame
        LDMIA   R1!, {R4-R7}        ; load saved R8-R11 into R4-R7
        MOV     R8,  R4
        MOV     R9,  R5
        MOV     R10, R6
        MOV     R11, R7

        ; now restore R4-R7 properly
        LDMIA   R0!, {R4-R7}        ; load saved R4-R7  (R0 advances +16)
        ADDS    R0, R0, #16         ; skip R8-R11 slots already loaded

        MSR     PSP, R0             ; PSP now at the hardware exception frame
        BX      LR                  ; EXC_RETURN: hardware pops the hw frame


;/******************************************************************************
; * vKernel_Start
; *
; * Launches the first task.  Called once from C after all tasks are added.
; * Never returns.
; *
; * 1. Reads the pre-built stack frame of the first task.
; * 2. Extracts its entry PC (word 14, offset 56 from the R4 slot).
; * 3. Advances PSP past the full 16-word frame (software + hardware = 64 bytes).
; * 4. Switches thread mode to use PSP (CONTROL.SPSEL = 1).
; * 5. Enables interrupts - SysTick will start switching tasks.
; * 6. Branches to the first task entry function.
; ******************************************************************************/
vKernel_Start:

        LDR     R0, =vCurrentTCB    ; R0 = &vCurrentTCB
        LDR     R0, [R0]            ; R0 = vCurrentTCB
        LDR     R0, [R0]            ; R0 = vStackPointer  (points to R4 slot)

        ; PC is at offset 56 in the pre-built frame (8 sw words + 6 hw words)
        LDR     R1, [R0, #56]       ; R1 = task entry address

        ; move PSP to top of frame (64 bytes past R4 slot)
        ADDS    R0, R0, #64
        MSR     PSP, R0             ; load PSP

        ; switch thread mode to PSP (CONTROL bit 1)
        MOVS    R0, #2
        MSR     CONTROL, R0
        ISB                         ; instruction sync after CONTROL change

        ; enable interrupts - SysTick can now fire
        CPSIE   I

        ; jump to the first task
        BX      R1


        END
