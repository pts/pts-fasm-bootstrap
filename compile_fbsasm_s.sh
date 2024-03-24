#! /bin/sh --
# by pts@fazekas.hu at Sun Mar 24 01:39:21 CET 2024
set -ex

test "${0%/*}" = "$0" || cd "${0%/*}"

# Missing: --32 -march=i386
debian-2.1-slink/as -o exit35.o exit35.s
debian-2.1-slink/ld -m elf_i386 -N -s -o exit35 exit35.o

./as --32 -march=i386 -o exit35.o exit35.s
./ld -m elf_i386 -N -s -o exit35 exit35.o

./nasm2as.pl exit35n.nasm exit35n.s
debian-2.1-slink/as -o exit35.o exit35n.s
debian-2.1-slink/ld -m elf_i386 -N -s -o exit35 exit35n.o
./as --32 -march=i386 -o exit35n.o exit35n.s
./ld -m elf_i386 -N -s -o exit35n exit35n.o

./nasm2as.pl fbsasm.nasm fbsasm.s
./as --32 -march=i386 -o fbsasm.o fbsasm.s
./ld -m elf_i386 -N -s -o fbsasm fbsasm.o  # -N to make .text read-write-execute.

: "$0" OK.
