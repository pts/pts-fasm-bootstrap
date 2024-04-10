#! /bin/sh --
# by pts@fazekas.hu at Thu Mar 21 03:12:23 CET 2024
set -ex

test "${0%/*}" = "$0" || cd "${0%/*}"

test -f "orig/fasm120.zip"  # 2001-11-17  No Linux support, segfault with fasm.asm and system.inc from fasm 1.37.
#test -f "orig/fasm130.zip"  # 2002-01-18 (file timestamps are wrong, they indicate 2001-01-18).
#test -f "orig/fasm137.zip"  # 2002-06-12 fasm 1.37 is the first with a Linux source or binary. It's UPX-compressed and it doesn't work, because it tries to use sysinfo.freeram and interpret it as bytes (too few).
#test -f "orig/fasm-1.43.tar.gz"  # First version with `format ELF executable' support, and it's already using it.
test -f "orig/fasm-1.73.32.tgz"  # 2023-12-04

rm -f fasm-orig-* fasm-pass?-* fasm-re-* fbsasm fbsasm-pass? fbsasm.bin fbsasm.o fbsasm.obj folink2t.com folink2l.com folink2.obj f.u00 f.upu f.t fbsasm.und fbsasm.err fbsasm.bin fbsasm.nas fbsasm.as8 fbsasm.sym fbsasms.as8 fbsasm.mws.o
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
 as86* | --as86*)  # Must come before `as' because of the prefix match.
  as86="${1#--}"
  if test "${as86#as86=}" = "$as86"; then
    as86=as86  # tools/as86011.exe
  else
    as86="${as86#*=}"
  fi
  rm -f fbsasm.sym  # We don't need it, but without specifying it as86 generates binary file headers.
  if test "${as86%.exe}" = "$as86"; then
    # Dropping -O0 and -w+orphan-labels, because old NASM versions don't support it.
    "$as86" -s fbsasm.sym -b fbsasm fbsasm.as86  # Fast or slow, depending on the NASM defaults.
  else
    ln -s fbsasm.as86 fbsasm.as8  # Workaround for DOS (and kvikdos), because they cannot open files with extension longer than 3 characters.
    case "${as86##*/}" in
     as86016*.exe)
      tools/kvikdos "$as86" -s fbsasm.sym -b fbsasm fbsasm.as8  # Fast or slow, depending on the NASM defaults.
      ;;
     *) # Shorten labels to avoid the `symbol table overflow' error.
      perl=perl
      test -f tools/miniperl-5.004.04 && perl=tools/miniperl-5.004.04
      "$perl" -e 'BEGIN { $^W = 1 } use integer; use strict;
          die "fatal: error opening: $ARGV[0]\n" if !open(FIN, "< $ARGV[0]");
          my %h; my $hi = 0;
          while (<FIN>) {
            if (m@^\s*([_a-zA-Z]\w*\s*):@) {
              die "fatal: duplicate label: $1\n" if exists($h{$1});
              $h{$1} = ++$hi;
            }
          }
          my $re = join("|", map { quotemeta($_) } sort(keys(%h)));
          die "fatal: error opening: $ARGV[0]\n" if !open(FIN, "< $ARGV[0]");
          die "fatal: error opening for write: $ARGV[1]\n" if !open(FOUT, "> $ARGV[1]");
          while (<FIN>) { s@(".*?")|\b($re)\b@ defined($1) ? $1 : "L$h{$2}" @goe; print FOUT $_ }
          die if !close(FOUT);' fbsasm.as8 fbsasms.as8
      tools/kvikdos "$as86" -s fbsasm.sym -b fbsasm fbsasms.as8  # Fast or slow, depending on the NASM defaults.
      rm -f fbsasms.as8
      ;;
    esac
    rm -f fbsasm.as8
  fi
  rm -f fbsasm.sym
  ;;
 as* | gas* | --as* | --gas*)  # GNU as(1) assembler and GNU ld(1) linker, from GNU Binutils. Example: --as=debian-2.1-slink/as
  ASPROG="${1#*=}"
  test "$ASPROG" = "$1" && ASPROG=as
  ASPROGDIR="${ASPROG%/*}/"
  ASPROGBASE="${ASPROG##*/}";
  test "$ASPROGDIR" = "$ASPROG/" && ASPROGDIR=
  LDPROG="${ASPROGDIR}ld${ASPROGBASE#*as}"
  if "$ASPROG" --32 -march=i386 --version >/dev/null 2>&1; then  # Newer GNU as(1) (tested with 2.22 and 2.30).
    "$ASPROG" --32 -march=i386 -o fbsasm.o fbsasm.s
  else  # Old GNU as(1) (tested with 2.7, 2.9.1 and 2.9.5) for i386.
    "$ASPROG" -o fbsasm.o fbsasm.s
  fi
  # TODO(pts): Write a custom linker which can do this (supports a single
  # ELF 32-bit .o input file with .text and .bss only).
  "$LDPROG" -m elf_i386 -N -s -o fbsasm fbsasm.o  # -N to make .text read-write-execute.
  rm -f fbsasm.o
  ;;
 tasm* | --tasm*)  # Example: --tasm=tasm/tasm41.exe . Example: --tasm=tasm/tasm32ps
  tasm="${1#--}"
  if test "${tasm#tasm=}" = "$tasm"; then
    tasm=tasm/tasm.exe
  else
    tasm="${tasm#*=}"
  fi
  tkvikdos=tools/kvikdos
  test "${tasm%.exe}" = "$tasm" && tkvikdos=command
  # Use the /m999 switch to optimize the output for size. But TASM 1.01
  # doesn't support it, so we don't use it.
  #
  # Tested and found working with TASM 1.01 (1989), TASM 2.0 (1990) and 4.1
  # (1996, last version for DOS 8086). It doesn't work with TASM 1.0 (1988),
  # it reports this for many lines: `Forward reference needs override'.
  #
  # Alternatively, this also works with TASM 5.3: tasm/tasm32ps /t fbsasm.tas fbsasm.obj
  "$tkvikdos" "$tasm" /t fbsasm.tas fbsasm.obj  # Output file: fbsasm.obj
  #"$tkvikdos" "$tasm" >/dev/null #  Just print help to make sure that kvikdos and TASM work.
  cp -a folink2.tas f.upu  # The TASM nolink-hack below works with TASM 4.1 and only if the filename is f.upu.
  # Alternatively, this also works with TASM 5.3: tasm/tasm32ps /t f.upu folink2t.com
  # This is the TASM nolink-hack: the generated OMF .obj file is a valid DOS .com program.
  if "$tkvikdos" "$tasm" /t f.upu folink2t.com; then  # Build folink2 with TASM 2.0--.
    # Needs TASM 2.0 (1990) or later, because earlier versions convert the
    # filename in the OMF THEADR record to uppercase.
    :
  else  # Build folink2 with TASM 1.01.
    cp -a folink2.tas f.t  # The TASM nolink-hack below works with TASM 1.01, TASM 4.1 and TASM 5.x and only if the filename is f.t (uppercase in TASM invocation below).
    # Needs TASM 2.0 (1990) or later, because earlier versions don't support the /q switch.
    "$tkvikdos" "$tasm" /t /dd F.T folink2t.com  # This is the TASM nolink-hack: the generated OMF .obj file is a valid DOS .com program.
    rm -f f.t
  fi
  rm -f f.upu
  tools/kvikdos folink2t.com fbsasm.obj fbsasm
  #./folink2 fbsasm.obj fbsasm
  rm -f fbsasm.obj folink2t.com
  ;;
 lzasm* | --lzasm*)  # Example: --lzasm=tasm/lzasm.exe . Example: --tasm=tasm/lzasm
  # Tested with LZASM 0.56 (2007-10-04).
  #
  # TODO(pts): Release dosbox.nox.static.
  # TODO(pts): Port the Win32 lzasm.exe to Linux i386. Also release the folink2l.exe.
  lzasm="${1#--}"
  if test "${lzasm#lzasm=}" = "$lzasm"; then
    lzasm=tasm/lzasm.exe
  else
    lzasm="${lzasm#*=}"
  fi
  ldosbox="dosbox.nox.static --cmd --mem-mb=3"
  test "${lzasm%.exe}" = "$lzasm" && ldosbox=command
  $ldosbox "$lzasm" /t fbsasm.tas  # Output file: fbsasm.obj  # This also works with LZASM 0.56.
  cp -a folink2.tas f.upu  # The TASM hack below works with TASM 4.1 and only if the filename is f.upu.
  # Needs TASM 2.0 (1990) or later, because earlier versions don't support the /q switch.
  $ldosbox "$lzasm" /t f.upu folink2l.com  # This is the TASM hack: the generated OMF .obj file is a valid DOS .com program.
  rm -f f.upu
  # Alternatively, built from folink2l.nasm: ./folink2 fbsasm.obj fbsasm
  tools/kvikdos folink2l.com fbsasm.obj fbsasm
  rm -f fbsasm.obj folink2l.com
  ;;
 a386* | --a386*)
  # A386 is a single-pass assembler, it generates suboptimal (longer)
  # encoding for instructions with forward references.
  tools/kvikdos tasm/a386.com +EDSP3 fbsasm.8 fbsasm.bin
  mv fbsasm.bin fbsasm
  ;;
 vasm* | --vasm*)  # Also includes --vasmx86. http://www.compilers.de/vasm.html  http://sun.hasenbraten.de/vasm/
  tasm/vasmx86 -quiet -mi386 -Fbin -o fbsasm fbsasm.vasm
  ;;
 mw* | --mw*)  # src/compile.sh src/as from https://github.com/pts/pts-mw386as-linux
  tools/mw386as fbsasm.mws  # Creates fbsasm.mws.o.
  tools/miniperl-5.004.04 -x tools/link3coff.pl --elf fbsasm.mws.o fbsasm 0x700000
  rm -f fbsasm.mws.o
  ;;
 nasm-0.9[0-9] | --nasm-0.9[0-9] | nasm-0.9[0-9]-linux | --nasm-0.9[0-9]-linux)
  prog="${1#--}"
  prog="${prog%-linux}"
  tasm/"$prog" -f bin -o fbsasm fbsasm.nasm  # Fast.
  ;;
 nasm-0.9[0-9]-dos | --nasm-0.9[0-9]-dos)
  prog="${1#--}"
  prog="${prog#nasm-0.9}"
  prog="${prog%-dos}"
  ln -s fbsasm.nasm fbsasm.nas
  tools/kvikdos tasm/nasm09"$prog".exe -f bin -o fbsasm fbsasm.nasm
  ;;
 nasm* | --nasm* | "")  # Default.
  nasm="${1#--}"
  if test "${nasm#nasm=}" = "$nasm"; then
    nasm=nasm-0.98.39  # tools/nasm-0.98.39
  else
    nasm="${nasm#*=}"
  fi
  if test "${nasm%.exe}" = "$nasm"; then
    # Dropping -O0 and -w+orphan-labels, because old NASM versions don't support it.
    "$nasm" -f bin -o fbsasm fbsasm.nasm  # Fast or slow, depending on the NASM defaults.
  else
    ln -s fbsasm.nasm fbsasm.nas  # Workaround for DOS (and kvikdos), because they cannot open files with extension longer than 3 characters.
    tools/kvikdos "$nasm" -f bin -o fbsasm fbsasm.nas  # Fast or slow, depending on the NASM defaults.
    rm -f fbsasm.nas
  fi
  #$nkvikdos "$nasm" -O0 -w+orphan-labels -f bin -o fbsasm fbsasm.nasm  # Fast.
  #nasm-0.98.39 -O1 -Dnear_o0= -w+orphan-labels -f bin -o fbsasm fbsasm.nasm  # Slower.
  #nasm-0.98.39 -O999999999 -Dnear_o0= -w+orphan-labels -f bin -o fbsasm fbsasm.nasm  # Even slower.
  #cp -a fbsasm fbsasm.nasm.bin
  ;;
 *)
  set +x;
  echo "Usage: $0 [<bootstrap-lang>]" >&2
  echo "<bootstrap-lang> values: nasm (default), fasm" >&2
  exit 1 ;;
esac

chmod 755 fbsasm
cp -a fbsasm fasm-re-bootstrap
if test -f fbsasm.fasm; then  # Bootstrap fbsasm from fasm source fbsasm.fasm, just for checking.
  ./fbsasm fbsasm.fasm fbsasm-pass1
  chmod 755 fbsasm-pass1
  ./fbsasm-pass1 fbsasm.fasm fbsasm-pass2
  cmp fbsasm-pass1 fbsasm-pass2
  rm -f fbsasm-pass1 fbsasm-pass2
fi

compile bootstrap 1.20
#ls -l fbsasm fasm-re-1.20
compile bootstrap 1.73.32
rm -f fbsasm fasm-re-bootstrap
compile 1.20 1.73.32

: "$0" OK.
