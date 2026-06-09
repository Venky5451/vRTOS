CROSS   = arm-none-eabi-
CC      = $(CROSS)gcc
OBJCOPY = $(CROSS)objcopy
SIZE    = $(CROSS)size
GDB     = gdb-multiarch

TARGET  = build/firmware
CPU     = cortex-m3

CFLAGS  = -mcpu=$(CPU) -mthumb -mfloat-abi=soft
CFLAGS += -O0 -g3 -Wall -nostdlib
CFLAGS += -ffunction-sections -fdata-sections

LDFLAGS = -mcpu=$(CPU) -mthumb
LDFLAGS += -T linker/cortexm.ld
LDFLAGS += -Wl,--gc-sections -nostdlib -nostartfiles

SRCS  = src/main.c
ASRCS = startup/startup_cortexm.s

OBJS  = $(patsubst src/%.c,    build/%.o, $(SRCS))
OBJS += $(patsubst startup/%.s,build/%.o, $(ASRCS))

.PHONY: all clean qemu qemu-debug gdb

all: $(TARGET).bin

build/%.o: src/%.c
	@mkdir -p build
	$(CC) $(CFLAGS) -c $< -o $@

build/%.o: startup/%.s
	@mkdir -p build
	$(CC) $(CFLAGS) -c $< -o $@

$(TARGET).elf: $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $@
	$(SIZE) $@

$(TARGET).bin: $(TARGET).elf
	$(OBJCOPY) -O binary $< $@

qemu: $(TARGET).elf
	qemu-system-arm \
		-machine mps2-an385 \
		-cpu cortex-m3 \
		-nographic \
		-semihosting \
		-kernel $(TARGET).elf

qemu-debug: $(TARGET).elf
	qemu-system-arm \
		-machine mps2-an385 \
		-cpu cortex-m3 \
		-nographic \
		-semihosting \
		-kernel $(TARGET).elf \
		-S -gdb tcp::1234

gdb: $(TARGET).elf
	$(GDB) \
		-ex "target remote localhost:1234" \
		-ex "load" \
		-ex "break main" \
		-ex "continue" \
		$(TARGET).elf

clean:
	rm -rf build
