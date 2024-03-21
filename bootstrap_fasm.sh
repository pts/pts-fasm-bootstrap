#! /bin/sh --
# by pts@fazekas.hu at Thu Mar 21 03:12:23 CET 2024
set -ex

test "${0%/*}" = "$0" || cd "${0%/*}"
comrade=../comrade
regular=../regular

test -f "$comrade/fasm120.zip"  # 2001-11-17  No Linux support, segfault with fasm.asm and system.inc from fasm 1.37.
#test -f "$comrade/fasm130.zip"  # 2002-01-18 (file timestamps are wrong, they indicate 2001-01-18).
#test -f "$comrade/fasm137.zip"  # 2002-06-12 fasm 1.37 is the first with a Linux source or binary. It's UPX-compressed and it doesn't work, because it tries to use sysinfo.freeram and interpret it as bytes (too few). 
#test -f "$comrade/fasm-1.43.tar.gz"  # First version with `format ELF executable' support, and it's already using it.
test -f "$regular/fasm-1.73.32.tgz"  # 2023-12-04

rm -f fasm-orig-* fasm-pass?-* fasm-re-*
rm -rf fasm-src-* tmp

rm -rf tmp
mkdir tmp
(cd tmp && unzip ../"$comrade/fasm120.zip")
rm -rf tmp/SOURCE/DOS
mv tmp/SOURCE/ASSEMBLE.INC tmp/SOURCE/assemble.inc
mv tmp/SOURCE/ERRORS.INC tmp/SOURCE/errors.inc
mv tmp/SOURCE/EXPRESSI.INC tmp/SOURCE/expressi.inc
mv tmp/SOURCE/FORMATS.INC tmp/SOURCE/formats.inc
mv tmp/SOURCE/PARSER.INC tmp/SOURCE/parser.inc
mv tmp/SOURCE/PREPROCE.INC tmp/SOURCE/preproce.inc
mv tmp/SOURCE/TABLES.INC tmp/SOURCE/tables.inc
mv tmp/SOURCE/VERSION.INC tmp/SOURCE/version.inc
rm -rf tmp/SOURCE/WIN32
mv tmp/SOURCE fasm-src-1.20
rm -rf tmp
mkdir fasm-src-1.20/Linux
cp -a fasm-patch-1.20/Linux/fasm.asm fasm-patch-1.20/Linux/system.inc fasm-src-1.20/Linux/

rm -rf tmp
mkdir tmp
(cd tmp && tar xzvf ../"$regular/fasm-1.73.32.tgz" fasm/fasm fasm/source) || exit "$?"
mv tmp/fasm/fasm fasm-orig-1.73.32
chmod 755 fasm-orig-1.73.32  # Not needed.
cp -a tmp/fasm/source fasm-src-1.73.32
rm -rf tmp
awk <fasm-src-1.73.32/Linux/fasm.asm >fasm-src-1.73.32/Linux/fasmb-1.20.asm '
    {
      if (/^[ \t]*format[ \t]/) {
        print"salc equ setalc\r"
        print"macro pushd value { push dword value }\r"
        print"macro align value { rb (value-1) - ($ + value-1) mod value }\r"
        print"program_base = 0x8048000\r";print"org program_base\r";print"use32\r"
        print"file_header:\r";print"db 0x7F,'\''ELF'\'',1,1,1,3\r";print"rb file_header+0x10-$\r";print"dw 2,3\r";print"dd 1,start\r";print"dd program_header-file_header,0,0\r";print"dw program_header-file_header,0x20,1,0,0,0\r"
        print"program_header:\r";print"dd 1,0,program_base,0\r";print"dd bss-program_base,program_end-program_base,7,0x1000\r"
      } else if (/^[ \t]*entry[ \t]/) {
      } else if (/^[ \t]*segment[ \t]/) {
      } else if (/^[ \t]*align[ \t]+4[ \t]*\r?$/) {
        print"bss:\r";print
      } else if (/^[ \t]*include[ \t]/) {
        sub(/^include '\''..\\/, "include '\''../"); print
      } else {
        print
      }
    }
    END{print"program_end:\r"}'

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

#cp -a ../../fasm-1.73.30/fasm fasm-re-bootstrap
#cp -a fasm-golden-1.30 fasm-re-bootstrap

# This is the bootstrap assembler, it can assemble fasm 1.30 (and probably earlier).
nasm-0.98.39 -O1 -w+orphan-labels -f bin -o fbsasm fbsasm.nasm
chmod 755 fbsasm
cp -a fbsasm fasm-re-bootstrap

compile bootstrap 1.20
#ls -l fbsasm fasm-re-1.30
rm -f fbsasm fasm-re-bootstrap
compile 1.20 1.73.32

: "$0" OK.
