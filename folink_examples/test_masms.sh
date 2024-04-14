#! /bin/sh
#
# test_masms.sh: tests compiling an ELF-32 program with MASM-compatible assemblers
# by pts@fazekas.hu at Wed Apr 10 14:37:11 CEST 2024
#

export LC_ALL=C
mydir=.
test "${0%/*}" = "$0" || mydir="${0%/*}"
if test "$1" != --sh-script; then
  tools="$mydir"/../tools
  test -d "$mydir"/tools && tools="$mydir"/tools
  exec "$tools"/busybox sh "$0" --sh-script "$@"
  exit 1
fi
shift

set -ex
cd "$mydir"
tools=../tools
test -d tools && tools=tools

asmfile="$1"
test -f "$asmfile"
shift
objfile="${asmfile%.*}.obj"
binfile="${asmfile%.*}.bin"
progfile="${asmfile%.*}"
runfile="$progfile"
test "${runfile#*}" = "$runfile" && runfile=./"$runfile"  # Make it run even if . is not on $PATH.
org=8048000h
test "$asmfile" != fbsasm.mas || org=700000h

# Test with all methods. Not with lzasm-0.56, because it doesn't support MASM compatibility mode (default).
test $# != 0 || set masm500.exe masm510.exe masm510a.exe ml600b.exe tasm41.exe tasm32-5.3psg wasm jwasm-2.11a bin-jwasm-2.11a asmc-2.34.49 bin-asmc-2.34.49

methodsp=methods
test $# != 1 || methodsp=method

for method in "$@"; do
  rm -f -- "$objfile" "$progfile" "$binfile"
  do_link=1
  case "$method" in
   masm*.exe) ../tools/kvikdos ../tools/"$method" /t /Ml "$asmfile" ,,nul,nul ;;
   # /Zm: MASM 5.10 compatibility in 6.00b.
   # It looks like we can't hide the `Assembling: ...' message.
   ml.exe | ml60*.exe) ../tools/kvikdos ../tools/"$method" /nologo /c /Ta"$asmfile" ;;
   ml61.exe | ml61[01]*.exe) dosbox.nox.static --cmd --mem-mb=2 ../tools/"$method" /nologo /c /Ta"$asmfile" ;;
   #ml61.exe ml61[01]*.exe) wine ../tools/"$method" /nologo /c /Ta"$asmfile" ;;  # Wine also works instead of DOSBox.
   ml61[2345]*.exe | ml[789]*.exe) wine ../tools/"$method" /nologo /c /omf /Ta"$asmfile" ;;  # It doesn't work with mwperun.exe, because it doesn't contain relocations.
   tasm.exe | tasm[1234v]*.exe) ../tools/kvikdos ../tasm/"$method" /t "$asmfile" ;;
   tasm32 | tasm32-*) ../tools/"$method" /t "$asmfile" ;;
   wasmr.exe) ../tools/kvikdos ../tools/"$method" -zq -fo=.obj "$asmfile" ;;  # From OpenWatcom. `Error: Out of Memory' for larger programs.
   # wasm.exe is from OpenWatcom. wasm*.exe are very slow, probably because of DOSBox. It can only process fbsasm.was. wasm1[01]*.exe need w32run.exe.
   # wasm.exe in Watcom C 10.0a (WATCOM Assembler Version 1) is bugdy, it generates incorrect output file, even the ELF-32 header is broken.
   wasm1[0-9]*.exe | wasm.exe) dosbox.nox.static --cmd --mem-mb=2 ../tools/"$method" -zq -fo=.obj "$asmfile" ;;
   # Creates a 12-byte OMF `COMENT(88) bits 80h, class fdh' record for each
   # `db ?' byte in non-_BSS, even repeated ones. So we use _BSS.
   wasm*) ../tools/"$method" -zq -fo=.obj "$asmfile" ;;
   # It also supports -nologo, but -q hides more.
   jwasm*) ../tools/"$method" -q -Fo"$objfile" "$asmfile" ;;
   # This is not needed: -Fo"$objfile"
   asmc*) ../tools/"$method" -q "$asmfile" ;;
   bin-jwasm* | bin-asmc*) ../tools/"${method#*-}" -q -bin -DBIN="$org" -Fo"$binfile" "$asmfile"; mv "$binfile" "$progfile"; do_link= ;;
   *) echo "fatal: unknown method: $method" >&2; exit 2 ;;
  esac

  if test "$do_link"; then 
    if test "$asmfile" = helloli3.mas; then
      ../tools/dmpobj -q "$objfile"
    fi
    #../tools/wlink output raw offset="$org" option quiet option noextension file "$objfile" name "$progfile"  # Generates 4096-byte file, pads with \0s.
    #../tools/wlink output raw offset="$org" option offset="$org" option quiet option noextension file "$objfile" name "$progfile"  # Generates 4096-byte file, pads with \0s. `option offset' doesn't make a difference.
    #../tools/wlink format raw bin option offset="$org" option quiet option noextension file "$objfile" name "$progfile"  # SUXX: Adds the `db ?' within the _TEXT segment as NUL bytes to the end.
    ../tools/wlink form raw bin op off=0x"${org%h}" op q op noext f "$objfile" n "$progfile"
  fi

  chmod +x -- "$progfile"
  if test "$asmfile" = helloli3.mas; then
    "$runfile"
    test "$("$runfile")" = "Hello, World!"
    test "$(../tools/miniperl-5.004.04 -e 'print(-s($ARGV[0]))' "$progfile")" -lt 131  # Check executable file size.
  fi
done

: "$0" OK with $# "$methodsp".
