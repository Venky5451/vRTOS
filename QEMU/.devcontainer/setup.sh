#!/bin/bash
set -e
apt-get update -y
apt-get install -y \
    gcc-arm-none-eabi \
    gdb-multiarch \
    qemu-system-arm \
    cmake \
    make \
    binutils-arm-none-eabi \
    libnewlib-arm-none-eabi
echo "Done!"
