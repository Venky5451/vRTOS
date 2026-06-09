#include <stdint.h>

/* mps2-an385 UART0 base address */
#define UART0_BASE   0x40004000
#define UART0_DATA   (*((volatile uint32_t *)(UART0_BASE + 0x00)))
#define UART0_STATE  (*((volatile uint32_t *)(UART0_BASE + 0x04)))
#define UART0_CTRL   (*((volatile uint32_t *)(UART0_BASE + 0x08)))
#define UART_STATE_TXFULL  (1 << 0)
#define UART_CTRL_TXEN     (1 << 0)

void uart_init(void) {
    UART0_CTRL = UART_CTRL_TXEN;
}

void uart_putc(char c) {
    while (UART0_STATE & UART_STATE_TXFULL);
    UART0_DATA = c;
}

void uart_puts(const char *s) {
    while (*s) uart_putc(*s++);
}

void delay(volatile uint32_t n) {
    while (n--);
}

int main(void) {
    uart_init();
    uart_puts("Hello from ARM Cortex-M3!\r\n");
    uart_puts("Running on QEMU mps2-an385\r\n");

    uint32_t count = 0;
    while (1) {
        uart_puts("Tick...\r\n");
        delay(500000);
        count++;
    }
    return 0;
}
