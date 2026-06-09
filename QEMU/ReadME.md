# ARM Cortex-M3 QEMU Development on GitHub Codespaces

![ARM](https://img.shields.io/badge/ARM-Cortex--M3-blue)
![QEMU](https://img.shields.io/badge/QEMU-mps2--an385-green)
![License](https://img.shields.io/badge/License-MIT-yellow)

---

## Overview

This project demonstrates how to set up a complete ARM Cortex-M3 bare-metal
development environment inside GitHub Codespaces using:

- GCC ARM Toolchain  - for cross-compilation
- QEMU               - for emulation (machine: mps2-an385)
- GDB Multiarch      - for command line debugging
- VSCode Cortex-Debug - for GUI debugging

---

## Project Structure

QEMU/
├── .devcontainer/
│   ├── devcontainer.json       # Codespace container config
│   └── setup.sh                # Auto-install tools script
├── src/
│   └── main.c                  # Main application (UART demo)
├── startup/
│   └── startup_cortexm.s       # ARM startup assembly
├── linker/
│   └── cortexm.ld              # Linker script (memory map)
├── .vscode/
│   ├── launch.json             # VSCode debug configuration
│   └── tasks.json              # VSCode build tasks
├── Makefile                    # Build system
└── README.md                   # This file

---

## Hardware / Emulation Target

| Parameter     | Value              |
|---------------|--------------------|
| CPU           | ARM Cortex-M3      |
| QEMU Machine  | mps2-an385         |
| Flash Origin  | 0x00000000         |
| Flash Size    | 256KB              |
| RAM Origin    | 0x20000000         |
| RAM Size      | 256KB              |
| UART Base     | 0x40004000         |
| Stack Size    | 0x400 (1KB)        |

---

## What We Built

### 1. Bare-Metal Firmware

Custom startup code in assembly (startup_cortexm.s)
  - Vector table setup
  - Stack pointer initialization
  - .data section copy from Flash to RAM
  - .bss section zero initialization
  - Jump to main()

Custom linker script (cortexm.ld)
  - Defines FLASH and RAM memory regions
  - Places vector table at address 0x00000000
  - Handles .text .data .bss sections

Main application (main.c)
  - UART driver for mps2-an385
  - Prints hello message and runs counter loop

### 2. Build System
  - Cross-compile with arm-none-eabi-gcc
  - Generates .elf and .bin files
  - Makefile with QEMU run and debug targets

### 3. Debug Setup
  - GDB command-line debugging via gdb-multiarch
  - VSCode GUI debugging via cortex-debug extension

---

## Prerequisites

| Tool                  | Purpose                    |
|-----------------------|----------------------------|
| arm-none-eabi-gcc     | ARM Cross Compiler         |
| arm-none-eabi-objcopy | Binary conversion          |
| qemu-system-arm       | ARM Emulator               |
| gdb-multiarch         | Debugger                   |
| make                  | Build system               |

---

## Setup

### Option A - GitHub Codespaces Auto Setup

Tools are auto-installed via .devcontainer/setup.sh
Just open the Codespace and wait for setup to complete

### Option B - Manual Install in Terminal

sudo apt-get update -y

sudo apt-get install -y \
    gcc-arm-none-eabi \
    gdb-multiarch \
    qemu-system-arm \
    make \
    cmake \
    binutils-arm-none-eabi \
    libnewlib-arm-none-eabi

### Verify Installation

arm-none-eabi-gcc --version
qemu-system-arm --version
gdb-multiarch --version

---

## Build Steps

### Step 1 - Clean Previous Build

make clean

### Step 2 - Build Firmware

make all

### Expected Output

arm-none-eabi-gcc -mcpu=cortex-m3 -mthumb -mfloat-abi=soft \
  -O0 -g3 -Wall -nostdlib -c src/main.c -o build/main.o

arm-none-eabi-gcc -mcpu=cortex-m3 -mthumb -mfloat-abi=soft \
  -O0 -g3 -Wall -nostdlib -c startup/startup_cortexm.s -o build/startup_cortexm.o

arm-none-eabi-gcc -mcpu=cortex-m3 -mthumb \
  -T linker/cortexm.ld -Wl,--gc-sections \
  -nostdlib -nostartfiles \
  build/main.o build/startup_cortexm.o -o build/firmware.elf

   text    data     bss     dec     hex filename
    400       0    1024    1424     590 build/firmware.elf

arm-none-eabi-objcopy -O binary build/firmware.elf build/firmware.bin

### Step 3 - Verify Build Output

# Check files
ls -lh build/

# Check ELF info
file build/firmware.elf

# Check symbols
arm-none-eabi-nm build/firmware.elf

# Disassemble
arm-none-eabi-objdump -d build/firmware.elf

# Size breakdown
arm-none-eabi-size -A build/firmware.elf

---

## Run in QEMU

### Run Command

make qemu

### Expected Output

Hello from ARM Cortex-M3!
Running on QEMU mps2-an385
Tick...
Tick...
Tick...

### Stop QEMU

Press:  Ctrl + A   then   X

### Manual QEMU Command

qemu-system-arm \
    -machine mps2-an385 \
    -cpu cortex-m3 \
    -nographic \
    -semihosting \
    -kernel build/firmware.elf

---

## Debug with GDB - Command Line

### Step 1 - Open Terminal 1 and Start QEMU Debug Server

make qemu-debug

QEMU starts and waits for GDB connection on port 1234
Terminal will appear frozen - this is normal

### Step 2 - Open Terminal 2 and Connect GDB

make gdb

### OR Connect GDB Manually

gdb-multiarch build/firmware.elf \
    -ex "target remote localhost:1234" \
    -ex "load" \
    -ex "break main" \
    -ex "continue"

---

### GDB Commands Reference

-- Connection --

target remote localhost:1234    # Connect to QEMU
load                            # Load firmware
monitor reset halt              # Reset CPU and halt

-- Breakpoints --

break main                      # Break at main()
break uart_puts                 # Break at function
break src/main.c:25             # Break at line number
info breakpoints                # List all breakpoints
delete 1                        # Delete breakpoint 1
clear main                      # Clear breakpoint at main

-- Execution --

continue                        # Run until breakpoint
next                            # Step over (source line)
step                            # Step into function
nexti                           # Step over (instruction)
stepi                           # Step into (instruction)
finish                          # Run until function returns

-- Registers --

info registers                  # Show all registers
print $pc                       # Show Program Counter
print $sp                       # Show Stack Pointer
print $lr                       # Show Link Register

-- Memory --

x/10x  0x20000000               # Show 10 hex words from RAM
x/10i  0x00000000               # Show 10 instructions from Flash
x/s    0x20000100               # Show string at address
x/4xb  0x40004000               # Show 4 bytes from UART

-- Variables --

print count                     # Print variable
print/x count                   # Print in hex
info locals                     # Show local variables
info globals                    # Show global variables

-- Stack --

info stack                      # Show call stack
backtrace                       # Same as info stack
frame 0                         # Select stack frame

-- Disassembly --

disassemble main                # Disassemble function
disassemble /m main             # With source lines
layout asm                      # TUI assembly view
layout src                      # TUI source view
layout regs                     # TUI register view

-- Watchpoints --

watch count                     # Break when variable changes
rwatch count                    # Break when variable is read

-- Quit --

quit                            # Exit GDB

---

## Debug with VSCode GUI

### Step 1 - Install Extensions

1. Open VSCode Extensions   Ctrl + Shift + X
2. Search  Cortex-Debug
3. Install marus25.cortex-debug
4. Install ms-vscode.cpptools

### Step 2 - Create .vscode/launch.json

{
  "version": "0.2.0",
  "configurations": [
    {
      "name": "QEMU Debug Cortex-M3",
      "type": "cortex-debug",
      "request": "launch",
      "servertype": "qemu",
      "cwd": "${workspaceRoot}",
      "executable": "${workspaceRoot}/build/firmware.elf",
      "cpu": "cortex-m3",
      "machine": "mps2-an385",
      "qemuArgs": [
        "-nographic",
        "-semihosting"
      ],
      "gdbPath": "gdb-multiarch",
      "preLaunchTask": "build",
      "runToEntryPoint": "main",
      "showDevDebugOutput": "none"
    },
    {
      "name": "GDB Attach Cortex-M3",
      "type": "cortex-debug",
      "request": "attach",
      "servertype": "external",
      "gdbTarget": "localhost:1234",
      "executable": "${workspaceRoot}/build/firmware.elf",
      "gdbPath": "gdb-multiarch",
      "cwd": "${workspaceRoot}"
    }
  ]
}

### Step 3 - Create .vscode/tasks.json

{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "build",
      "type": "shell",
      "command": "make all",
      "group": {
        "kind": "build",
        "isDefault": true
      },
      "presentation": {
        "reveal": "always",
        "panel": "shared"
      },
      "problemMatcher": "$gcc"
    },
    {
      "label": "clean",
      "type": "shell",
      "command": "make clean",
      "group": "build"
    },
    {
      "label": "run qemu",
      "type": "shell",
      "command": "make qemu",
      "group": "test",
      "presentation": {
        "reveal": "always",
        "panel": "dedicated"
      }
    }
  ]
}

### Step 4 - Start Debug Session

Method A - Auto Launch (Recommended)
--------------------------------------
1. Press F5
   OR Click Run and Debug icon in sidebar
   OR Click Run -> Start Debugging

2. Select "QEMU Debug Cortex-M3"

3. Debugger will automatically:
   - Build firmware
   - Start QEMU
   - Connect GDB
   - Break at main()

Method B - Manual Attach
--------------------------
1. Start QEMU in terminal first:
   make qemu-debug

2. Press F5 in VSCode
3. Select "GDB Attach Cortex-M3"

---

### VSCode Debug Controls

| Action       | Button | Keyboard       |
|--------------|--------|----------------|
| Continue     |   ▶    | F5             |
| Pause        |   ⏸    | F6             |
| Step Over    |   ↷    | F10            |
| Step Into    |   ↓    | F11            |
| Step Out     |   ↑    | Shift + F11    |
| Restart      |   ⟲    | Ctrl+Shift+F5  |
| Stop         |   ■    | Shift + F5     |

### VSCode Debug Panels

VARIABLES    - Watch local and global variables live
WATCH        - Add custom expressions to monitor
CALL STACK   - See function call hierarchy
BREAKPOINTS  - Manage all breakpoints
REGISTERS    - View CPU registers (Cortex-Debug panel)
MEMORY       - View raw memory (Cortex-Debug panel)
DISASSEMBLY  - View ARM assembly instructions

### Setting Breakpoints in VSCode

1. Open src/main.c
2. Click in the gutter left of line number
3. Red dot appears = breakpoint is set
4. Right click red dot = set conditional breakpoint

---

## Makefile Targets

| Target        | Command           | Description                    |
|---------------|-------------------|--------------------------------|
| Build all     | make all          | Compile and link firmware      |
| Clean         | make clean        | Remove build directory         |
| Run QEMU      | make qemu         | Run firmware in QEMU           |
| Debug server  | make qemu-debug   | Start QEMU with GDB server     |
| Connect GDB   | make gdb          | Connect GDB to QEMU            |

---

## Troubleshooting

### Build Errors

Problem: Makefile missing separator error
  Error:  Makefile:29: *** missing separator. Stop.
  Cause:  Spaces instead of TAB characters in Makefile
  Fix:    Regenerate Makefile using Python script
          python3 script ensures correct TAB characters

Problem: Undefined reference errors
  Error:  undefined reference to _exit
  Fix:    Add -nostdlib -nostartfiles to LDFLAGS

Problem: File not found
  Error:  startup/startup_cortexm.s No such file
  Fix:    mkdir -p startup src linker

### QEMU Errors

Problem: Wrong machine for Cortex-M
  Error:  qemu-system-arm: This board cannot be used with Cortex-M CPUs
  Cause:  Using versatilepb machine
  Fix:    Change machine to mps2-an385

Problem: No UART output
  Cause:  Wrong UART base address
  Fix:    mps2-an385 UART0 address is 0x40004000
          NOT 0x101F1000 (that is versatilepb)

Problem: QEMU stuck
  Fix:    Press Ctrl+A then X to exit
          Or run: pkill -f qemu-system-arm

### GDB Errors

Problem: Connection refused
  Error:  Connection refused localhost:1234
  Fix:    Start QEMU debug server first
          Run: make qemu-debug

Problem: Port already in use
  Fix:    pkill -f qemu-system-arm
          lsof -i :1234

---

## QEMU Machine Reference

| Machine      | CPU         | Notes                  |
|--------------|-------------|------------------------|
| mps2-an385   | cortex-m3   | Used in this project   |
| mps2-an386   | cortex-m4   | With FPU support       |
| mps2-an500   | cortex-m7   | High performance       |
| mps2-an505   | cortex-m33  | TrustZone support      |
| lm3s6965evb  | cortex-m3   | Stellaris LM3S board   |
| versatilepb  | cortex-a    | NOT for Cortex-M       |

---

## File Descriptions

| File                         | Description                          |
|------------------------------|--------------------------------------|
| src/main.c                   | Main app with UART driver            |
| startup/startup_cortexm.s    | Vector table and Reset handler       |
| linker/cortexm.ld            | Memory layout for mps2-an385         |
| Makefile                     | Build run and debug targets          |
| .vscode/launch.json          | VSCode debug configurations          |
| .vscode/tasks.json           | VSCode build tasks                   |
| .devcontainer/setup.sh       | Auto-install script for Codespaces   |
| build/firmware.elf           | Compiled ELF with debug symbols      |
| build/firmware.bin           | Raw binary for flashing              |

---

## Quick Start Summary

# 1. Install tools
sudo apt-get install -y gcc-arm-none-eabi gdb-multiarch qemu-system-arm make

# 2. Build
make all

# 3. Run
make qemu

# 4. Debug with GDB
make qemu-debug          (Terminal 1)
make gdb                 (Terminal 2)

# 5. Debug with VSCode
Press F5 and select "QEMU Debug Cortex-M3"

---

## Author

Venky5451
GitHub Codespaces - ARM Cortex-M3 Bare Metal QEMU Development

---

## License

MIT License - Free to use and modify