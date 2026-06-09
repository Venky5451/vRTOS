    .syntax unified
    .cpu cortex-m3
    .thumb

    .section .stack, "w"
    .align 3
    .space 0x400
__StackTop:

    .section .vectors, "a"
    .align 2
    .long __StackTop
    .long Reset_Handler
    .long NMI_Handler
    .long HardFault_Handler
    .long 0
    .long 0
    .long 0
    .long 0
    .long 0
    .long 0
    .long 0
    .long SVC_Handler
    .long 0
    .long 0
    .long PendSV_Handler
    .long SysTick_Handler

    .text
    .thumb_func
    .global Reset_Handler
    .type Reset_Handler, %function
Reset_Handler:
    ldr  r0, =__StackTop
    mov  sp, r0

    ldr  r0, =_sdata
    ldr  r1, =_edata
    ldr  r2, =_sidata
copy:
    cmp  r0, r1
    bge  zero
    ldr  r3, [r2], #4
    str  r3, [r0], #4
    b    copy
zero:
    ldr  r0, =_sbss
    ldr  r1, =_ebss
    mov  r2, #0
zerol:
    cmp  r0, r1
    bge  start
    str  r2, [r0], #4
    b    zerol
start:
    bl   main
    b    .

    .weak NMI_Handler
    .thumb_func
NMI_Handler:
    b .

    .weak HardFault_Handler
    .thumb_func
HardFault_Handler:
    b .

    .weak SVC_Handler
    .thumb_func
SVC_Handler:
    b .

    .weak PendSV_Handler
    .thumb_func
PendSV_Handler:
    b .

    .weak SysTick_Handler
    .thumb_func
SysTick_Handler:
    b .
