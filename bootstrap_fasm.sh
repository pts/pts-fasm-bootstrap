#! /bin/sh --
# by pts@fazekas.hu at Thu Mar 21 03:12:23 CET 2024
set -ex

test "${0%/*}" = "$0" || cd "${0%/*}"
comrade=../comrade
regular=../regular

test -f "$comrade/fasm130.zip"  # 2002-01-18 (file timestamps are wrong, they indicate 2001-01-18). !! Do it with something earlier.
test -f "$comrade/fasm137.zip"  # 2002-06-12 fasm 1.37 is the first with a Linux source or binary. It's UPX-compressed and it doesn't work, because it tries to use sysinfo.freeram and interpret it as bytes (too few). 
test -f "$comrade/fasm-1.43.tar.gz"  # First version with `format ELF executable' support, and it's already using it.
test -f "$regular/fasm-1.73.32.tgz"

rm -f fasm-orig-* fasm-pass?-* fasm-re-*
rm -rf fasm-src-* tmp

rm -rf tmp
mkdir tmp
(cd tmp && tar xzvf ../"$comrade/fasm-1.43.tar.gz" fasm/fasm fasm/source) || exit "$?"
mv tmp/fasm/fasm fasm-orig-1.43
chmod 755 fasm-orig-1.43  # Not needed.
cp -a tmp/fasm/source fasm-src-1.43
rm -rf tmp
mv fasm-src-1.43/Linux/fasm.asm fasm-src-1.43/Linux/fasm.asm.orig
awk >fasm-src-1.43/Linux/fasm.asm <fasm-src-1.43/Linux/fasm.asm.orig '{gsub(/^include '\''..\\/, "include '\''../"); print}'
mv fasm-src-1.43/Linux/system.inc fasm-src-1.43/Linux/system.inc.orig
awk >fasm-src-1.43/Linux/system.inc <fasm-src-1.43/Linux/system.inc.orig '{if(/allocate_memory:/){print"\tmov dword [buffer+14h],0x280000  ; PATCH\r"}print}'  # Try to use at least 2.5 MiB of memory. 2 MiB is not enough.
# fasm 1.30 does not know salc.
awk <fasm-src-1.43/Linux/fasm.asm >fasm-src-1.43/Linux/fasmb-1.30.asm '
    BEGIN{print"salc equ setalc"}
    {
      if (/^[ \t]*format[ \t]/) {
        print"program_base = 0x8048000\r";print"org program_base\r";print"use32\r"
        print"file_header:\r";print"db 0x7F,'\''ELF'\'',1,1,1,3\r";print"rb file_header+0x10-$\r";print"dw 2,3\r";print"dd 1,start\r";print"dd program_header-file_header,0,0\r";print"dw program_header-file_header,0x20,1,0,0,0\r"
        print"program_header:\r";print"dd 1,0,program_base,0\r";print"dd bss-program_base,program_end-program_base,7,0x1000\r"
      } else if (/^[ \t]*entry[ \t]/) {
      } else if (/^[ \t]*align[ \t]+4[ \t]*\r?$/) {
        print"bss:\r";print
      } else {
        print
      }
    }
    END{print"program_end:\r"}'

rm -rf tmp
mkdir tmp
(cd tmp && unzip ../"$comrade/fasm137.zip")
mv tmp/SOURCE/LINUX/FASM fasm-orig-1.37  # Packed with UPX.
chmod 755 fasm-orig-1.37  # Needed.
mv tmp/SOURCE/LINUX tmp/SOURCE/Linux
mv tmp/SOURCE/Linux/FASM.ASM tmp/SOURCE/Linux/fasm.asm
mv tmp/SOURCE/Linux/SYSTEM.INC tmp/SOURCE/Linux/system.inc
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
mv fasm-src-1.37/Linux/fasm.asm fasm-src-1.37/Linux/fasm.asm.orig
awk >fasm-src-1.37/Linux/fasm.asm <fasm-src-1.37/Linux/fasm.asm.orig '{gsub(/^include '\''..\\/, "include '\''../"); print}'
mv fasm-src-1.37/Linux/system.inc fasm-src-1.37/Linux/system.inc.orig
awk >fasm-src-1.37/Linux/system.inc <fasm-src-1.37/Linux/system.inc.orig '{if(/allocate_memory:/){print"\tmov dword [buffer+14h],0x200000  ; PATCH\r"}print}'  # Try to use at least 2 MiB of memory. 1 MiB is not enough.

rm -rf tmp
mkdir tmp
(cd tmp && unzip ../"$comrade/fasm130.zip")
rm -rf tmp/SOURCE/DOS
mv tmp/SOURCE fasm-src-1.30
rm -rf tmp
mkdir fasm-src-1.30/Linux
# !! Rather than this copy, discard 1.37 and generate fasm-src-1.30/Linux/fasm.asm from fasm-src-1.30/fasm.asm (Win32) instead.
cp -a fasm-src-1.37/Linux/fasm.asm fasm-src-1.37/Linux/system.inc fasm-src-1.30/Linux/

rm -rf tmp
mkdir tmp
(cd tmp && tar xzvf ../"$regular/fasm-1.73.32.tgz" fasm/fasm fasm/source) || exit "$?"
mv tmp/fasm/fasm fasm-orig-1.73.32
chmod 755 fasm-orig-1.73.32  # Not needed.
cp -a tmp/fasm/source fasm-src-1.73.32
rm -rf tmp
awk <fasm-src-1.73.32/Linux/fasm.asm >fasm-src-1.73.32/Linux/fasmb-1.43.asm '
    BEGIN{print"macro align value { rb (value-1) - ($ + value-1) mod value }\r"}
    {
      if (/^[ \t]*format[ \t]+ELF[ \t]+executable[ \t]/) {
        print "format ELF executable\r"
      } else if (/^[ \t]*segment[ \t]/) {
      } else {
        print
      }
    }'

# Compile version $2 using executable binary version $1, and then $2 by itself.
# Input:  fasm-$1-re fasm-$2-src/
# Output: fasm-$2-re
compile() {
  local src=fasm.asm
  test -f fasm-src-"$2"/Linux/fasmb-"$1".asm && src=fasmb-"$1".asm
  (cd fasm-src-"$2"/Linux && ../../fasm-re-"$1" "$src" ../../fasm-pass1-"$2"-by-"$1") || exit "$?"
  chmod 755 fasm-pass1-"$2"-by-"$1"
  (cd fasm-src-"$2"/Linux && ../../fasm-pass1-"$2"-by-"$1" fasm.asm ../../fasm-pass2-"$2") || exit "$?"
  rm -f fasm-pass1-"$2"-by-"$1"
  chmod 755 fasm-pass2-"$2"
  (cd fasm-src-"$2"/Linux && ../../fasm-pass2-"$2" fasm.asm ../../fasm-re-"$2") || exit "$?"
  chmod 755 fasm-re-"$2"
  cmp fasm-pass2-"$2" fasm-re-"$2"  
  rm -f fasm-pass2-"$2"
  if test -f fasm-golden-"$2"; then
    cmp fasm-golden-"$2" fasm-re-"$2"
  fi
}

#cp -a ../../fasm-1.73.30/fasm fasm-re-bootstrap  # !! Use earlier fasm or do a binary patch.
cp -a fasm-golden-1.30 fasm-re-bootstrap

compile bootstrap 1.30
compile 1.30 1.43
compile 1.43 1.73.32

: "$0" OK.