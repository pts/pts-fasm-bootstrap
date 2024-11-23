#! /bin/sh --
# by pts@fazekas.hu at Sat Nov 23 03:48:25 CET 2024

if test "$1" != --sh-script; then exec "${0%/*}/busybox" env -i "${0%/*}/busybox" sh "$0" --sh-script "$@" || exit 125; fi
shift
export PATH=/dev/null/missing  # Use only busybox tools.
export LC_ALL=en_US.UTF-8  # For deterministic output.
export TZ=GMT  # For deterministic output.
#mydir="${0%/*}"
busybox="$mydir/busybox"
test "${0%/*}" = "$0" || cd "${0%/*}"
set -ex

test -f ../orig/fasm137.zip
rm -rf SOURCE
unzip -qo ../orig/fasm137.zip SOURCE/ASSEMBLE.INC SOURCE/ERRORS.INC SOURCE/EXPRESSI.INC SOURCE/FORMATS.INC SOURCE/PARSER.INC SOURCE/PREPROCE.INC SOURCE/TABLES.INC SOURCE/VERSION.INC SOURCE/LINUX/FASM.ASM SOURCE/LINUX/SYSTEM.INC

./miniperl -wne '
    s@\r?\n@\r\n@;  # fasm-1.20 segfaults unless source line ending is CRLF (\r\n).
    if (m@^include\s+\x27(.*?)\x27@) {  # Process `include` directive.
      $_ = uc($1);
      s@^..[/\\]@SOURCE/@ or s@^@SOURCE/LINUX/@;
      my $fn = $_;
      die "fatal: open: $_" if !open(F, "< $_");
      $_ = join("", <F>);
      die if !close(F);
      s@[\r\n]+\Z(?!\n)@@;
      s@\r?\n@\r\n@g;  # fasm-1.20 segfaults unless source line ending is CRLF (\r\n).
      s@^(\w+[ \t]*=[ \t]*0\w*)([ \t]*\r?$)@${1}o$2@mg;  # Fix octal constants in system.inc. This affects the `create:` function.
      if ($fn eq "SOURCE/LINUX/SYSTEM.INC") {
        # In system.inc, try to use at least 2.5 MiB of memory. 1 MiB is not
        # enough. 2 MiB is enough for compiling 1.43. 2.5 MiB is enough for
        # compilig fasm 1.73.32.
        my $heap_size = 0x280000;
        die "fatal: missing alloc call\n" if !s@^[ \t]*mov[ \t]+ebx[ \t]*,[ \t]*buffer[ \t]*\r?\n[ \t]*mov[ \t]+eax[ \t]*,[ \t]*116[ \t]*\r?\n[ \t]*int[ \t]+0x80[ \t]*\r?\n([ \t]*allocate_memory:)@\tmov dword [buffer+14h],$heap_size  ; PATCH\r\n$1@m;
        # This shorter patch also works, but keeps a useless system call in the program.
        #die fatal: missing alloc call\n" if !s@^([ \t]*allocate_memory:)@\tmov dword [buffer+14h],$heap_size  ; PATCH\r\n$1@m;
      }
      print $_, "\r\n";
    } else {
      print STDERR if m@O_RDONLY@;
      print
    }' <SOURCE/LINUX/FASM.ASM >fasm.fasm

# TODO(pts): Is it feasible to it with "$busybox" awk
# TODO(pts): Use .bss (`absolute $') at `bss:'.
./miniperl -we '
    use integer;
    use strict;
    my %longsizes = qw(b byte  w word  d dword);
    my %sizes;
    while (<STDIN>) {  # Scan variable definitions for sizes, populate $sizes{$varname} = $size;
      $sizes{$1} = $2 if m@^\s*([a-zA-Z_]\w*)\s*:?\s*[dr]([bwd])\b@;
    }
    my $varname_re = "([ ,])\\[\\s*(" . join("|", sort(keys(%sizes))) . ")\\b";
    die "rewind" if !seek(STDIN, 0, 0);
    my %vars; $vars{VERSION} = ""; $vars{VERSION_STRING} = ""; $vars{program_base} = "";
    while (<STDIN>) {  # Convert from fasm to NASM syntax.
      y@"@\x27@;
      if (m@;@) {  # Remove single-line comment.
        if (m@\x27@) { s@^((?:[^\x27;]+|\x27[^\x27]*\x27)*)(.*)@ my $s = $1; $s =~ s/^;.*//;  $s @e }  # Complicated case: ; may be inside quotes.
        else { s@;.*@@ }  # Simple case.
      }
      s@^\s+@@; s@\s+\Z(?!\n)@@g;
      next if !m@\S@;
      s@^(\S+)\s+@$1 @;
      if (!m@\x27[^\x27][^\x27]@) {
        s@(\w+)\s+shl\s+(\w+)@($1<<$2)@g; s@(\w+|\([-\w]+\))\s+shr\s+(\w+)@($1>>$2)@g;
        s@([ ,])not\b@$1~@g; s@\s+[?]$@ 0@; s@\s+db\s+[?0]\s*,\s*[?0]$@ db 0, 0@;
        s@$varname_re@ exists($sizes{$2}) ? "$1$longsizes{$sizes{$2}} [$2" : "${1}[$2" @oe if m@\[@ and !m@\b(byte|word|dword)\b@;  # Replace e.g. `[input_file]` with `dword [input_file]`.
        s@\b(0[0-7]*)o\b@${1}q@g;  # For NASM 0.97.
      } else {
        s@(\x27equ\x27) shl 8\b@($1<<8)@;
      }
      s@,VERSION,@,$vars{VERSION},@g;
      s@,VERSION_STRING,@,$vars{VERSION_STRING},@g;
      if (m@^(VERSION|VERSION_STRING|program_base)\s+(?:equ|=)\s+(.*)$@) { $vars{$1} = $2; next if $1 eq "VERSION" or $1 eq "VERSION_STRING" }  # NASM does not support string after equ.
      if (s@^org program_base$@org $vars{program_base}@) {}  # For NASM 0.97.
      elsif (s@^([a-zA-Z_]\w*)\s*=\s*(.*)$@$1 equ $2@) {}
      elsif (m@^macro @) { next }
      elsif (s@^([a-zA-Z_]\w*)\s+dq\s+0$@$1 dd 0, 0@) {}
      elsif (s@^([a-zA-Z_]\w*)\s+r([bwd])\s+(.+)@$1: times $3 d$2 0@) {}
      elsif (s@^r([bwd]) (.+)@times $2 d$1 0@) {}
      elsif (s@^rq (.+)@times ($1)<<1 dd 0@) {}
      elsif (s@^dm (.+)@db $1, 0@) {}
      elsif (s@^push (?!e?(?:[abcd]x|e[sd]i|e[sb]p)|[cdefgs]s)(?=[a-zA-Z_]\w*\Z)@push dword @) {}  # For NASM 0.97.
      elsif (s@^((?:push|pop) (?:(?:byte|word|dword)\b\s*)?)(\S.*)@ my $in = $1; my $args = $2; $in =~ s/\s+\Z(?!\n)//; map { print "$in $_\n" } split(" ", $args); "" @e) { next }
      elsif (s@^(if|else)\b@%$1@) { s@<>@!=@g }
      elsif (s@^end if\b@%endif@) {}
      elsif (s@^((?:rep[a-z]*\s+)?)(lod|sto|mov|cmp|sca)s\s+(byte|word|dword)\b.*@ $1.$2."s".substr($3,0,1) @e) {}
      elsif (s@^jmp near\s+(e[abcd]x|e[sd]i|e[sb]p)$@jmp $1@) {}
      elsif (s@^(j(?!mp |e?cxz )[a-z]+) (?!near\s+|short\s+)@$1 near @) {}  # Force conditional jumps to be `neaar`, to avoid NASM error: short jump is out of range, without `nasm -O1`.
      elsif (s@^setalc$@salc@) {}
      elsif (s@^use32$@bits 32@) {}  # For NASM 0.97.
      elsif (s@^loopnzd @loopnz @) {}
      elsif (s@^xlat byte\b.*@xlatb@) {}
      print "$_\n";
    }' <fasm.fasm >fasm.nasm
rm -f fasm0 fasm1
# Works with Perl 5.004.04 (1997-10-15) or later.
# Works with NASM 0.97 (1997-12-06), 0.98.39 (2005-01-15), 2.10.09 (2013-07-23), 2.13.02 (2017-12-03).
#./nasm-0.98.39 -o fasm0 fasm.nasm  # Works. 0.98.39.
#./nasm-2.13.02 -o fasm0 fasm.nasm  # Works.
#./nasm-2.10.09 -o fasm0 fasm.nasm  # Works.
./nasm-0.97.uclibc -o fasm0 fasm.nasm  # Works.
chmod +x fasm0
./fasm0 fasm.fasm fasm1
chmod +x fasm1
./fasm1 ||:
cmp ../fasm-golden-1.37 fasm1

: "$0" OK.
