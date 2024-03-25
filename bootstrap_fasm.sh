#! /bin/sh --
# by pts@fazekas.hu at Thu Mar 21 03:12:23 CET 2024
set -ex

test "${0%/*}" = "$0" || cd "${0%/*}"

test -f "orig/fasm120.zip"  # 2001-11-17  No Linux support, segfault with fasm.asm and system.inc from fasm 1.37.
#test -f "orig/fasm130.zip"  # 2002-01-18 (file timestamps are wrong, they indicate 2001-01-18).
#test -f "orig/fasm137.zip"  # 2002-06-12 fasm 1.37 is the first with a Linux source or binary. It's UPX-compressed and it doesn't work, because it tries to use sysinfo.freeram and interpret it as bytes (too few).
#test -f "orig/fasm-1.43.tar.gz"  # First version with `format ELF executable' support, and it's already using it.
test -f "orig/fasm-1.73.32.tgz"  # 2023-12-04

rm -f fasm-orig-* fasm-pass?-* fasm-re-* fbsasm fbsasm.bin fbsasm.o fbsasm.obj fbsasm.os1 folink1t.com folink1.obj f.u00
rm -rf fasm-src-* tmp

rm -rf tmp
mkdir tmp
(cd tmp && unzip ../"orig/fasm120.zip")
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
(cd tmp && tar xzvf ../"orig/fasm-1.73.32.tgz" fasm/fasm fasm/source) || exit "$?"
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
cp -a fasm-src-1.73.32/Linux/fasmb-1.20.asm fasm-src-1.73.32/Linux/fasmb-bootstrap.asm

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

# Build the bootstrap assembler, it can assemble patched fasm 1.20, 1.30 and 1.73.32.
case "$1" in  # Any of these below will work.
 fasm* | --fasm*)
  ./fasm-golden-1.20 fbsasm.fasm fbsasm && chmod +x fbsasm
  #./fasm-golden-1.30 fbsasm.fasm fbsasm && chmod +x fbsasm
  #./fasm-golden-1.73.32 fbsasm.fasm fbsasm && chmod +x fbsasm
  ;;
 as* | gas* | --as* | --gas*)  # GNU as(1) assembler and GNU ld(1) linker, from GNU Binutils. Example: --as=debian-2.1-slink/as
  ASPROG="${1#*=}"
  test "$ASPROG" = "$1" && ASPROG=as
  ASPROGDIR="${ASPROG%/*}/"
  ASPROGBASE="${ASPROG##*/}";
  test "$ASPROGDIR" = "$ASPROG/" && ASPROGDIR=
  LDPROG="${ASPROGDIR}ld${ASPROGBASE#*as}"
  if "$ASPROG" --32 -march=i386 --version >/dev/null; then  # Newer GNU as(1) (tested with 2.22 and 2.30).
    "$ASPROG" --32 -march=i386 -o fbsasm.o fbsasm.s
  else  # Old GNU as(1) (tested with 2.9.1 and 2.9.5) for i386.
    "$ASPROG" -o fbsasm.o fbsasm.s
  fi
  "$LDPROG" -m elf_i386 -N -s -o fbsasm fbsasm.o  # -N to make .text read-write-execute.
  rm -f fbsasm.o
  ;;
 tasm* | --tasm*)
  if true; then
    tasm/kvikdos tasm/tasm.exe /t /DSEG1 fbsasm.tas, fbsasm.os1  # Output file: fbsasm.os1
    cp -a folink1.tas f.u00  # The TASM hack below works with TASM 4.1 and only if the filename is f.u00.
    tasm/kvikdos tasm/tasm.exe /t /m999 /q f.u00 folink1t.com
    rm -f f.u00
    # TODO(pts): Get program_base automatically from fbsasm.tas.
    tasm/kvikdos folink1t.com fbsasm.os1 fbsasms1 0x8048000 <fbsasm.os1 >fbsasm
    rm -f fbsasm.os1
  else
    tasm/kvikdos tasm/tasm.exe /t fbsasm.tas  # Output file: fbsasm.obj !! Redistribute kvikdos.
    # TODO(pts): Get program_base automatically from fbsasm.tas.
    # TODO(pts): Make wlink generate fbsasm (without .bin).
    tasm/wlink format raw bin option offset=0x8048000 option quiet name fbsasm file fbsasm.obj
    rm -f fbsasm.obj
    mv fbsasm.bin fbsasm
  fi
  ;;
 nasm* | --nasm* | "")  # Default.
  nasm-0.98.39 -O0 -w+orphan-labels -f bin -o fbsasm fbsasm.nasm  # Fast.
  ;;
 *)
  set +x;
  echo "Usage: $0 [<bootstrap-lang>]" >&2
  echo "<bootstrap-lang> values: nasm (default), fasm" >&2
  exit 1 ;;
esac

#nasm-0.98.39 -O1 -Dnear_o0= -w+orphan-labels -f bin -o fbsasm fbsasm.nasm  # Slower.
#nasm-0.98.39 -O999999999 -Dnear_o0= -w+orphan-labels -f bin -o fbsasm fbsasm.nasm  # Even slower.
chmod 755 fbsasm
cp -a fbsasm fasm-re-bootstrap

compile bootstrap 1.20
#ls -l fbsasm fasm-re-1.20
compile bootstrap 1.73.32
rm -f fbsasm fasm-re-bootstrap
compile 1.20 1.73.32

: "$0" OK.
