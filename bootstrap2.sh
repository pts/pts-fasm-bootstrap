#!/bin/sh --
#
# bootstrap2.sh: shell script to build a recent Linux i386 fasm executable program using fasm
# by pts@fazekas.hu at Sun Nov 24 20:35:34 CET 2024
#
# This script works with shells: bash, zsh, dash, ash (typically symlink to dash), ksh93, mksh, lksh, pdksh (typically symlink to lksh), posh, busybox sh.
# It doesn't work with the following shells: csh, tcsh, fish, sash.
#
# This script uses external commands: cat, tr, cmp, rm, mv, unzip, (--unzip=...), fasm (--fasm=...).
#

eval '(exit $?0)' || eval 'echo fatal: csh not supported; exec false'  # No way to write to stderr in csh(1).

f=food; if test "${f##*o}" != d; then echo "fatal: missing string substitution" >&2; exit 1; fi  # All pass.

# We detect a single-argument echo command $echo which appends a newline,
# doesn't interpret backslash and doesn't interpret the `-n' flag at the
# beginning.
if test "$(export PATH=/dev/null/missing; printf '%s\n' 'foo\\bar' 2>/dev/null)" = 'foo\\bar'; then echo='printf %s\n'  # Shell builtin. lksh(1) has it. Why is lksh slow then?
elif test "$(export PATH=/dev/null/missing; print -r 'foo\\bar' 2>/dev/null)" = 'foo\\bar'; then echo='print -r'  # Shell builtin.
elif test "$(printf '%s\n' 'foo\\bar' 2>/dev/null)" = 'foo\\bar'; then echo='printf %s\n'  # External command printf(1). Only needed by posh(1).
else echo "fatal: could not find a reliable echo command" >&2; exit 1
fi

test "$ZSH_VERSION" && set -y 2>/dev/null  # SH_WORD_SPLIT for zsh(1). It's an invalid option in bash(1), and it's harmful (prevents echo) in ash(1).
cr=""  # "\r".
tab="	"  # "\t".

src_zip_file=orig/fasmw17332.zip  # fasm 1.73.32, a recent fasm.
# Works with fasm 1.20 (2001-11-17) patched by bootstrap1.pl, fasm 1.37 (2002-06-12) patched by bootstrap1.pl, fasm 1.43 (2003-12-30) patched, fasm 1.73.32 (2023-12-04).
fasm_cmd=./fasm1
unzip_cmd=unzip

while test $# != 0; do
  case "$1" in
   --) shift; break ;;
   -) break ;;
   --fasm=*) fasm_cmd="${1#*=}"; shift ;;
   --unzip=*) unzip_cmd="${1#*=}"; shift ;;
   --srczip=*) src_zip_file="${1#*=}"; shift ;;
   -*) $echo "fatal: unknown command-line flag: $1" >&2; exit 1 ;;
   *) break ;; # Pattern [^-] doesn't work in lksh.
  esac
done
if test $# != 0; then
  src_zip_file="$1"; shift
  if test $# != 0; then $echo "fatal: too many command-line arguments" >&2; exit 1; fi
fi
if ! test -f "$src_zip_file"; then $echo "fatal: missing src .zip file: $src_zip_file" >&2; exit 1; fi

src_zip_base="${src_zip_file##*/}"
f="$src_zip_base"; f="${f#fasm}"; f="${f#w}"; f="${f%%.*}"
case "$src_zip_base" in
 fasm[0-9][0-9][0-9].* | fasmw[0-9][0-9][0-9].*) src_zip_version_dot="${f%??}.${f#?}" ;;  # Example: fasm137.zip --> 1.37 .
 fasm1[0-9][0-9][0-9].* | fasmw1[0-9][0-9][0-9].*) f="${f#1}"; src_zip_version_dot="1.${f%?}.${f#??}" ;;
 fasm1[0-9][0-9][0-9][0-9].* | fasmw1[0-9][0-9][0-9][0-9].*) f="${f#1}"; src_zip_version_dot="1.${f%??}.${f#??}" ;;  # Example: fasmw17332.zip --> 1.73.32 .
 *) src_zip_version_dot=-1 ;;
esac

# Get the full list of source files to $@.
set $("$unzip_cmd" -l "$src_zip_file" | while read -r f; do g="${f#* SOURCE/}"; test "$g" = "$f" || $echo "SOURCE/$g"; done)
if test "$?" != 0 || test $# = 0; then $echo "fatal: unzip -l failed" >&2; exit 1; fi
rm -f "$@"
for f in "$@"; do
  if test -f "$f" || test -d "$f"; then $echo "fatal: file must not exist: $f" >&2; exit 1; fi  # dash(1) doesn't have `test -a' or `test -f'.
done
if ! "$unzip_cmd" "$src_zip_file" "$@"; then $echo "fatal: unzip failed" >&2; exit 1; fi

if ! exec <SOURCE/LINUX/FASM.ASM; then $echo "fatal: error opening for reading: SOURCE/LINUX/FASM.ASM" >&2; exit 1; fi
(
  #$echo "salc equ setalc$cr"  # This doesn't work in fasm-1.43.
  $echo "salc equ db 0xd6$cr"
  $echo "macro pushd value { push dword value }$cr"
  $echo "macro align value { rb (value-1) - (\$ + value-1) mod value }$cr"
  $echo "ELF32_program_base = 0x8048000$cr"
  $echo "org ELF32_program_base$cr"
  $echo "use32$cr"
  $echo "ELF32_file_header:$cr"  # An approximation of ELF32 header generation by `format elf executable 3' in recent fasm.
  $echo "db 0x7F,'ELF',1,1,1,3$cr"
  $echo "rb ELF32_file_header+0x10-\$$cr"
  $echo "dw 2,3$cr"
  $echo "dd 1,start$cr"
  $echo "dd ELF32_program_header-ELF32_file_header,0,0$cr"
  $echo "dw ELF32_program_header-ELF32_file_header,0x20,1,0x28,0,0$cr"
  $echo "ELF32_program_header:$cr"
  $echo "dd 1,0,ELF32_program_base,0$cr"
  $echo "dd ELF32_bss-ELF32_program_base,ELF32_program_end-ELF32_program_base,7,0x1000$cr"
  while read -r f; do
    case "$f" in
     format* | entry[\ $tab]* | segment*) ;; # Not understood by NASM 1.20 and 1.37.
     align[\ $tab]4"$cr") $echo "ELF32_bss:$cr"; $echo "$f" ;;  # fasm-1.20 segfaults unless source line ending is CRLF (\r\n).
     include[\ $tab]*)
      # $echo "$f" >&2
      g="$($echo "${f#include}" | tr a-z\\\\ A-Z/ | tr -d \'\\r\ "$tab")"
      case "$g" in ../*) g="SOURCE/${g#../}" ;; *) g="SOURCE/LINUX/$g" ;; esac
      if ! test -f "$g"; then $echo "fatal: missing included source file: $g" >&2; exit 1; fi
      if ! cat "$g"; then $echo "fatal: cat failed" >&2; exit 1; fi
      # $echo "$g" >&2
      # s@^(\w+[ \t]*=[ \t]*0[0-7]*)([ \t]*$cr?$)@${1}o$2@mg;  # Fix octal constants in system.inc. This affects the `create:` function. It's not needed for recent fasm.
      # if ($fn eq "SOURCE/LINUX/SYSTEM.INC") {
      #   # In system.inc, try to use at least 2.5 MiB of memory. 1 MiB is not
      #   # enough. 2 MiB is enough for compiling 1.43. 2.5 MiB is enough for
      #   # compilig fasm 1.73.32.
      #   my $heap_size = 0x280000;
      #   # Typically this is missing in recent fasm, no need to patch.
      #   s@^[ \t]*mov[ \t]+ebx[ \t]*,[ \t]*buffer[ \t]*$cr?\n[ \t]*mov[ \t]+eax[ \t]*,[ \t]*116[ \t]*$cr?\n[ \t]*int[ \t]+0x80[ \t]*$cr?\n([ \t]*allocate_memory:)@\tmov dword [buffer+14h],$heap_size  ; PATCH$cr\n$1@m;
      # }
      ;;
     *) $echo "$f" ;;
   esac
 done
  $echo "ELF32_program_end:$cr"
) >fasm3.fasm
test "$?" = 0 || exit "$?"

# Just process the `include` directives.
if ! exec <SOURCE/LINUX/FASM.ASM; then $echo "fatal: error opening for reading: SOURCE/LINUX/FASM.ASM" >&2; exit 1; fi
(
  while read -r f; do
    case "$f" in
     include[\ $tab]*)
      g="$($echo "${f#include}" | tr a-z\\\\ A-Z/ | tr -d \'\\r\ "$tab")"
      case "$g" in ../*) g="SOURCE/${g#../}" ;; *) g="SOURCE/LINUX/$g" ;; esac
      if ! test -f "$g"; then $echo "fatal: missing included source file: $g" >&2; exit 1; fi
      if ! cat "$g"; then $echo "fatal: cat failed" >&2; exit 1; fi
      ;;
     *) $echo "$f" ;;
   esac
 done
  $echo "ELF32_program_end:$cr"
) >fasm4.fasm
test "$?" = 0 || exit "$?"


if ! rm -f fasm4 fasm5 fasm6; then $echo "fatal: command rm failed" >&2; exit 1; fi
$echo "info: running: $fasm_cmd fasm3.fasm fasm4" >&2
if ! "$fasm_cmd" fasm3.fasm fasm4; then $echo "fatal: command $fasm_cmd failed" >&2; exit 1; fi
chmod +x fasm4 || exit "$?"
# Compile the unpatched fasm4.fasm with the newly compiled fasm.
$echo "info: running: ./fsam4 fasm4.fasm fasm5" >&2
if ! ./fasm4 fasm4.fasm fasm5; then $echo "fatal: command ./fasm4 failed" >&2; exit 1; fi
chmod +x fasm5 || exit "$?"
$echo "info: running: ./fsam5 fasm4.fasm fasm6" >&2
if ! ./fasm5 fasm4.fasm fasm6; then $echo "fatal: command ./fasm5 failed" >&2; exit 1; fi
chmod +x fasm5 || exit "$?"
if ! cmp fasm5 fasm6; then $echo "fatal: file content mismatch: fasm5 vs fasm6" >&2; exit 1; fi
fn7="fasm-golden-$src_zip_version_dot"
if test -f "$fn7"; then
  $echo "info: comparing: fasm5 vs $fn7" >&2
  if ! cmp fasm5 "$fn7"; then $echo "fatal: file content mismatch: fasm5 vs $fn7" >&2; exit 1; fi
fi
mv fasm6 "fasm-re-$src_zip_version_dot" || exit "$?"

$echo "info: bootstrap2 OK: $0" >&2
