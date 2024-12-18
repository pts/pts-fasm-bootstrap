#! /bin/sh --
# by pts@fazekas.hu at Thu Mar 21 03:12:23 CET 2024
set -ex

test "${0%/*}" = "$0" || cd "${0%/*}"

# Build the bootstrap assembler fbsasm, it can assemble patched fasm 1.20, 1.30, 1.37 and 1.73.32.
case "$1" in  # Any of these below will work.
 fasm* | --fasm*)
  ./fasm-golden-1.20 fbsasm.fasm fbsasm && chmod +x fbsasm
  #./fasm-golden-1.30 fbsasm.fasm fbsasm && chmod +x fbsasm
  #./fasm-golden-1.73.32 fbsasm.fasm fbsasm && chmod +x fbsasm
  ;;
 as86* | --as86*)  # Must come before `as' because of the prefix match.
  as86arg="${1#--}"
  as86="$as86arg"
  if test "${as86#*=}" = "$as86"; then
    as86=as86  # tools/as86011.exe
  else
    as86="${as86#*=}"
  fi
  is_as86_0=0
  if test "${as86arg#as860}" = "$as86arg"; then  # Not --as860 works with as86 >=0.0.7.
    case "${as86##*/}" in *-0.0.[0-6]) is_as86_0=1 ;; esac
  else  # --as860 works with as86 0.0.0 .. 0.0.8.
    is_as86_0=1
  fi
  rm -f fbsasm.sym  # We don't need it, but without specifying it as86 generates leadning NUL bytes on its -b output.
  if test "$is_as86_0" = 1; then
    "$as86" -w -j -s fbsasm0.sym -b fbsasm0.bs8 fbsasm0.as86
    perl=perl
    test -f tools/miniperl-5.004.04 && perl=tools/miniperl-5.004.04
    # Remove the leading 5 bytes generated by as86 0.0.0 .. 0.0.6.
    "$perl" -e 'die if(read(STDIN,$_,10)or 0)!=10 or!s@^(?:\0..\0\0)?(?=\x7fELF\x01)@@s;print;print while<STDIN>' <fbsasm0.bs8 >fbsasm
  elif test "${as86%.exe}" = "$as86"; then
    "$as86" -s fbsasm.sym -b fbsasm fbsasm.as86  # Specifying -s prevents as86 from adding org NUL bytes to the beginning.
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
  fi
  rm -f fbsasm.sym fbsasm.bs8 fbsasm0.sym fbsasm0.bs8
  ;;
 as* | gas* | --as* | --gas*)  # GNU as(1) assembler and GNU ld(1) linker, from GNU Binutils. Example: --as=debian-2.1-slink/as
  # These don't work:
  # --as=debian-1.1-buzz/as  (2.6.0.2, it has too many code generation bugs)
  # All these work:
  # --as=debian-1.2-rex/as  (2.7.0.3, code generation bugs worked around by nasm2as.pl)
  # --as=debian-2.1-slink/as  (2.9.1.0.19, code generation bugs worked around by nasm2as.pl)
  # --as=debian-2.2-potato/as  (2.9.5.0.37, no code generation bugs encountered)
  # --as=tools/gaself32-2.30
  # --as=tools/gasgo32coff-2.24
  # --as=tools/gasgo32coff-2.26
  # --as=tools/gaswin32coff-2.20
  # --as=tools/gaswin32coff-2.23.52
  # --as=toolset/bin/gaself32-2.7
  # --as=toolset/bin/gaself32-2.9.5.0.37
  # --as=toolset/bin/gascoff32-2.9.5.0.37
  # --as=toolset/bin/gasgo32coff-2.9.5.0.37
  # --as=toolset/bin/gaswin32coff-2.9.5.0.37
  # --as=toolset/bin/gaself32-2.22
  # --as=toolset/bin/gaself32-2.24
  ASPROG="${1#*=}"
  test "$ASPROG" = "$1" && ASPROG=as
  ASPROGDIR="${ASPROG%/*}/"
  ASPROGBASE="${ASPROG##*/}";
  test "$ASPROGDIR" = "$ASPROG/" && ASPROGDIR=
  case "$ASPROGBASE" in
    gascoff32-* | gasgo32coff-* | gaswin32coff-*) LDPROG="${ASPROGDIR}ld-${ASPROGBASE#*-}"; test -f "$LDPROG" || LDPROG=toolset/bin/wlink-ow2023-03-04 ;;  # tools/ld-2.22 doesn't support COFF as input. `LDPROG=ld' would also work with >= 2.22 on Debian or Ubuntu.
    gaself32-*) LDPROG="${ASPROGDIR}ld-${ASPROGBASE#*-}"; test -f "$LDPROG" || LDPROG=tools/ld-2.22 ;;
    as* | gas*) LDPROG="${ASPROGDIR}ld${ASPROGBASE#*as*}" ;;
    *) LDPROG=ld ;;
  esac
  if "$ASPROG" --32 -march=i386 --version >/dev/null 2>&1; then  # Newer GNU as(1) (tested with 2.22 and 2.30).
    "$ASPROG" --32 -march=i386 -o fbsasm.o fbsasm.s
  else  # Old GNU as(1) (tested with 2.7, 2.9.1 and 2.9.5) for i386.
    "$ASPROG" -o fbsasm.o fbsasm.s
  fi
  # TODO(pts): Write a custom linker which can do this (supports a single
  # ELF 32-bit .o input file with .text and .bss only).
  case "${LDPROG##*/}" in
   wlink*)
    "$LDPROG" op q op start=_start form elf op noext op norelocs n fbsasm f fbsasm.o
    perl -we 'die if !open(F, "+< ", $ARGV[0]); binmode(F); die if !sysseek(F, 0x4c, 0); $_ = "\7\0\0\0"; die if (syswrite(F, $_, 4) or 0) != 4' fbsasm  # Post-processing to make .text read-write-execute.
    ;;
   *) "$LDPROG" -m elf_i386 -N -s -o fbsasm fbsasm.o ;;  # -N to make .text read-write-execute.
  esac
  rm -f fbsasm.o ;;
 svr3* | --svr3*)  # Also includes toolset/bin/sunos4as-1988-11-16
  ASPROG="${1#*=}"
  test "$ASPROG" = "$1" && ASPROG=toolset/bin/svr3as-1987-10-28
  "$ASPROG" -dt -dg -dv fbsasms.s
  tools/miniperl-5.004.04 -x tools/link3coff.pl --elf fbsasms.o fbsasm 0x700000
  rm -f fbsasms.o ;;
 svr4* | --svr4*)  # Also includes toolset/bin/sunos4as-1988-11-16
  ASPROG="${1#*=}"
  test "$ASPROG" = "$1" && ASPROG=toolset/bin/svr4as-1993-01-16
  "$ASPROG" fbsasms.s
  tools/ld-2.22 -m elf_i386 -N -s -o fbsasm fbsasms.o  # -N to make .text read-write-execute.
  rm -f fbsasms.o ;;
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
    if false; then  # Just an experiment for using folink2.mas instead of folink2.tas.
      rm -f folink2t.com
      cp -a folink2.mas f.uau
      "$tkvikdos" tools/masm500.exe /t f.uau ,foli2m.com,nul, nul
      mv foli2m.com folink2t.com
      rm -f f.uau
    fi
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
 nasm* | --nasm* | "" | --notest)  # Default.
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
 * | --help)
  set +x;
  echo "Usage: $0 [<bootstrap-lang>] [--notest]" >&2
  echo "<bootstrap-lang> values: nasm (default), fasm" >&2
  exit 1 ;;
esac
test $# = 0 || test "$1" = --notest || shift
chmod +x fbsasm

if test "$1" != --notest; then
  # Test the fbsasm just built.
  set +ex
  . ./bootstrap2.sh --fasm=./fbsasm || exit "$?"
  set -ex
fi

: "$0" OK.
