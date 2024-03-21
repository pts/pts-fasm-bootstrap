#! /bin/sh --
# by pts@fazekas.hu at Thu Mar 21 03:12:23 CET 2024
set -ex

test "${0%/*}" = "$0" || cd "${0%/*}"
comrade=../comrade

test -f "$comrade/fasm130.zip"  # !! Do it with something earlier.
test -f "$comrade/fasm137.zip"  # fasm 1.37 is the first with a Linux source or binary. It's UPX-compressed and it doesn't work, because it tries to use sysinfo.freeram and interpret it as bytes (too few). 
test -f "$comrade/fasm-1.42.tar.gz"  # Last version before `format ELF executable' support.
test -f "$comrade/fasm-1.50.tar.gz"

rm -f fasm-orig-* fasm-pass?-* fasm-re-*
rm -rf fasm-src-* tmp

rm -rf tmp
mkdir tmp
(cd tmp && tar xzvf ../"$comrade/fasm-1.42.tar.gz" fasm/fasm fasm/source) || exit "$?"
mv tmp/fasm/fasm fasm-orig-1.42
chmod 755 fasm-orig-1.42  # Not needed.
mv tmp/fasm/source/Linux tmp/fasm/source/linux
cp -a tmp/fasm/source fasm-src-1.42
rm -rf tmp
mv fasm-src-1.42/linux/fasm.asm fasm-src-1.42/linux/fasm.asm.orig
awk >fasm-src-1.42/linux/fasm.asm <fasm-src-1.42/linux/fasm.asm.orig '{gsub(/^include '\''..\\/, "include '\''../"); print}'
mv fasm-src-1.42/linux/system.inc fasm-src-1.42/linux/system.inc.orig
awk >fasm-src-1.42/linux/system.inc <fasm-src-1.42/linux/system.inc.orig '{if(/allocate_memory:/){print"\tmov dword [buffer+14h],0x100000  ; PATCH\r"}print}'  # Try to use at least 1 MiB of memory.  !! less
#mv fasm-src-1.42/formats.inc fasm-src-1.42/formats.inc.orig
#awk <fasm-src-1.42/formats.inc.orig >fasm-src-1.42/formats.inc 'BEGIN{print"salc equ setalc"}{print}'  # fasm 1.30 doesn't know salc.
awk <fasm-src-1.42/linux/fasm.asm >fasm-src-1.42/linux/fasmb-1.30.asm 'BEGIN{print"salc equ setalc"}{print}'  # fasm 1.30 doesn't know salc.

#rm -rf tmp
#mkdir tmp
#(cd tmp && tar xzvf ../"$comrade/fasm-1.50.tar.gz" fasm/fasm) || exit "$?"
#mv tmp/fasm/fasm fasm-orig-1.50
#chmod 755 fasm-orig-1.50  # Not needed.
#rm -rf tmp

rm -rf tmp
mkdir tmp
(cd tmp && unzip ../"$comrade/fasm137.zip")
mv tmp/SOURCE/LINUX/FASM fasm-orig-1.37  # Packed with UPX.
chmod 755 fasm-orig-1.37  # Needed.
mv tmp/SOURCE/LINUX tmp/SOURCE/linux
mv tmp/SOURCE/linux/FASM.ASM tmp/SOURCE/linux/fasm.asm
mv tmp/SOURCE/linux/SYSTEM.INC tmp/SOURCE/linux/system.inc
mv tmp/SOURCE/ASSEMBLE.INC tmp/SOURCE/assemble.inc
mv tmp/SOURCE/ERRORS.INC tmp/SOURCE/errors.inc
mv tmp/SOURCE/EXPRESSI.INC tmp/SOURCE/expressi.inc
mv tmp/SOURCE/FORMATS.INC tmp/SOURCE/formats.inc
mv tmp/SOURCE/PARSER.INC tmp/SOURCE/parser.inc
mv tmp/SOURCE/PREPROCE.INC tmp/SOURCE/preproce.inc
mv tmp/SOURCE/TABLES.INC tmp/SOURCE/tables.inc
mv tmp/SOURCE/VERSION.INC tmp/SOURCE/version.inc
rm -rf tmp/SOURCE/DOS tmp/SOURCE/WIN32
mv tmp/SOURCE fasm-src-1.37
rm -rf tmp
mv fasm-src-1.37/linux/fasm.asm fasm-src-1.37/linux/fasm.asm.orig
awk >fasm-src-1.37/linux/fasm.asm <fasm-src-1.37/linux/fasm.asm.orig '{gsub(/^include '\''..\\/, "include '\''../"); print}'
mv fasm-src-1.37/linux/system.inc fasm-src-1.37/linux/system.inc.orig
awk >fasm-src-1.37/linux/system.inc <fasm-src-1.37/linux/system.inc.orig '{if(/allocate_memory:/){print"\tmov dword [buffer+14h],0x100000  ; PATCH\r"}print}'  # Try to use at least 1 MiB of memory.  !! Use less.

rm -rf tmp
mkdir tmp
(cd tmp && unzip ../"$comrade/fasm130.zip")
rm -rf tmp/SOURCE/DOS
mv tmp/SOURCE fasm-src-1.30
rm -rf tmp
mkdir fasm-src-1.30/linux
# !! Rather than this copy, discard 1.37 and generate fasm-src-1.30/linux/fasm.asm from fasm-src-1.30/fasm.asm (Win32) instead.
cp -a fasm-src-1.37/linux/fasm.asm fasm-src-1.37/linux/system.inc fasm-src-1.30/linux/

# Compile version $2 using executable binary version $1, and then $2 by itself.
# Input:  fasm-$1-re fasm-$2-src/
# Output: fasm-$2-re
compile() {
  local src=fasm.asm
  test -f fasm-src-"$2"/linux/fasmb-"$1".asm && src=fasmb-"$1".asm
  (cd fasm-src-"$2"/linux && ../../fasm-re-"$1" "$src" ../../fasm-pass1-"$2"-by-"$1") || exit "$?"
  chmod 755 fasm-pass1-"$2"-by-"$1"
  (cd fasm-src-"$2"/linux && ../../fasm-pass1-"$2"-by-"$1" fasm.asm ../../fasm-pass2-"$2") || exit "$?"
  rm -f fasm-pass1-"$2"-by-"$1"
  chmod 755 fasm-pass2-"$2"
  (cd fasm-src-"$2"/linux && ../../fasm-pass2-"$2" fasm.asm ../../fasm-re-"$2") || exit "$?"
  chmod 755 fasm-re-"$2"
  cmp fasm-pass2-"$2" fasm-re-"$2"  
  rm -f fasm-pass2-"$2"
  if test -f fasm-golden-"$2"; then
    cmp fasm-golden-"$2" fasm-re-"$2"
  fi
}

cp -a ../../fasm-1.73.30/fasm fasm-re-bootstrap  # !! Use earlier fasm or do a binary patch.

compile bootstrap 1.30
compile 1.30 1.42

: "$0" OK.
