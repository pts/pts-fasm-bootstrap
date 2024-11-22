#! /bin/sh --
# by pts@fazekas.hu at Thu Nov 21 15:53:06 CET 2024
#

set -ex
test "$0" = "${0%/*}" || cd "${0%/*}"

# The output remains valid for GNU as(1) well.
# !! TODO(pts): Undo some of the .byte workarounds emitted by nasm2as.pl. But we'd have to redo them, the same workarounds are also needed.
# !! GNU as(1) generates jcxz; SVR3 as generates jecxz
# !! TODO(pts): At least detect: We don't fix here (because there aren't any) the precedence in multi-operator expressions: 0x29|10<<8 ==> 0x9|[10<<8]
<fbsasm.s >fbsasms.s perl -we '
    use integer;
    use strict;
    my $lc = 0;  # Label counter;
    my $hlc = 0;  # Helper label counter.
    my %renames = qw(repne repnz  repe repz  xlatb xlat  jecxz jcxz  cwd  cwtd  cdq cltd  cwde cwtl  cbw cbtw  shld shldl  shrd shrdl  jmpl jmp   .asciz .string  .word .value  .short .value);
    my %eqvars;
    print "/* This source file works with the SVR3, SunOS 4.0.1 and SVR4 asseblers. */\n";
    print ".file \"fbsasms.s\"\n";  # SunOS 4.0.1 assembler needs it.
    while (<STDIN>) { AGAIN:
      if (s@^\s*#+(.*)@/*$1 */@) { print; next }
      my $comment = s@#+(.*)@@ ? "  /*$1 */" : "";  # SVR3 supports only / and /* comments, no #
      s@\s+\Z(?!\n)@@;
      s@^(\s*)@@; my $ws = $1;
      if (s@^([a-zA-Z_][^-+:\s]*)\s*:\s*@@) { print $ws, $1, ":\n"; next if !length($_) and !length($comment); }  # Put label to separate line (just to help parsing).
      s@\x27([^\\\x27])\x27?@ sprintf("0x%02x", ord($1)) @ge;  # Replace character literal with hex byte.
      if (s@^((?:cmp|lod|mov|sca|sto)s[bwl])\s.*@$1@) {}  # Drop the argument of string instruction.
      elsif (s@^(xor|or|and|not|test|cmp|neg|inc|dec|mov|xchg|add|sub|adc|sbb|push|pop)\b\s+(?=(?:[^,][^,]*(?:\([^()]+\))?,\s*)?%[abcd][lh](?:\s*,|\Z))@${1}b @) {}  # Add byte size to the mnemonic.
      elsif (s@^(xor|or|and|not|test|cmp|neg|inc|dec|mov|xchg|add|sub|adc|sbb|push|pop)\b\s+(?=(?:[^,][^,]*(?:\([^()]+\))?,\s*)?%(?:[abcd]x|[sd]i|[sb]p)(?:\s*,|\Z))@${1}w @) {}  # Add word size to the mnemonic.
      elsif (s@^(s[ah][rl]|r[oc][lr])\b((?:\s+[^,]+,)?)(?=\s+(?:[^%,][^,]*,\s*)?%[abcd][lh]\Z)@${1}b$2@) {}  # Add byte size to the mnemonic.
      elsif (s@^(s[ah][rl]|r[oc][lr])\b((?:\s+[^,]+,)?)(?=\s+(?:[^%,][^,]*,\s*)?%(?:[abcd]x|[sd]i|[sb]p)\Z)@${1}w$2@) {}  # Add word size to the mnemonic.
      elsif (s@^(imul\s+)([^,]+),\s*(%[^,]+)$@$1$2, $3, $3@) {}  # Repeat register name in `imul`.
      elsif (s@^(sh[lr])dl?\s+%cl\s*,\s*@${1}dl @) {}  # Drop the `%cl` argument of `shld` and `shrd`.
      elsif (s@^(repne|repe|xlatb|jecxz|cwd|cdq|cwde|cbw|shld|shrd|jmpl|[.]asciz|[.]word|[.]short)\b@$renames{$1}@) {}  # Rename instruction or directive. Only `jmpl *` has to be renamed for `jmpl`.
      elsif (m@^[.]ascii\s+"((?:[^"\\]+|\\[\\"])*)"$@s) { my $s = $1; $_ = <STDIN>; if (defined($_) && m@^\s*[.]byte\s+0\s+@) { $_ = ".string \"$s\"" } else { $s =~ s@\\(.)@$1@gs; $s =~ s@(.)@ sprintf(", 0x%02x", ord($1)) @gse; $s =~ s@^, @.byte @; print $ws, $s, $comment, "\n"; goto AGAIN } }  # Convert .ascii to bytes.
      if (!m@"@) {
        s`\b([a-zA-Z_][.\w]*-(?:assembler|instructions))\b` my $s = $1; $eqvars{$s} = 1; $s =~ s@-@___@g; "--$s" `ge;  # Move subtraction to the end of the .s file. SVR3 needs it, SVR4 does not.
        # This workaround is needed only by the SVR4 assembler because it has code generation bugs, but it harmless with others.
        if (s@^(\s*)cmpw\s+\$--([a-zA-Z_]\w*?___[a-zA-Z_]\w*)\s*,\s*\(%ebx\)@$1.value 0x6690, 0x3b81, --$2  /* cmpw \$--..., (%ebx) */@) {}
        elsif (s@^(\s*)cmpw\s+\$--([a-zA-Z_]\w*?___[a-zA-Z_]\w*)\s*,\s*1\(%esi\)@$1.value 0x8166, 0x017e, --$2  /* cmpw \$--..., 1(%esi) */@) {}
        elsif (s@^(\s*)movw\s+\$--([a-zA-Z_]\w*?___[a-zA-Z_]\w*)\s*,\s*\(%ebx\)@$1.value 0x6690, 0x03c7, --$2  /* movw \$--..., (%ebx) */@) {}
        elsif (m@^(\s*)\w+w\s+\$(?!([-+<>|&()\[\]]+|\d\w*)+(?:[\s,]|\Z))@) { die "fatal: possibly broken instruction in SVR3: $_\n" }  # We could add a similar workaround above.
      }
      if (m@^[.]@) {
        if (m@^[.](?:globl|byte|string|long|value|text|bss)\b@) {  # Keep known directives.
          #if (m@^[.]globl\b\b@s) {}
          #elsif (m@^[.]string\b@s) {}
          #elsif (m@^[.](?:byte|long|value)\s+-?\d\w*(\s*,\s*-?\d\w*)*$@) { $_ = "nop" }
          #elsif (m@^[.](byte|long|value|string)\s+[^,]+$@) { print STDERR "nopxed: $_\n"; $_ = ".$1 0x90909090" }  # !! For ab.diff debugging.
          #elsif (m@^[.](?:byte|long|value|string)\b@) { print STDERR "nopped: $_\n"; $_ = "nop" }  # !! For ab.diff debugging.
          #else { die "fatal: unknown nop: $_\n" }
          #else { $_ = "nop" }
        } elsif (m@^[.]align\b@) {  # Pass it on.
        } elsif (m@^[.]if\s+!CASE_SENSITIVE$@) {  # Skip lines below, assumint that the .if condition is false.
          print $ws, "/*", $_, "*/", $comment, "\n";
          my $is_pending = 1;
          while (<STDIN>) {
            if (m@^\s*([.](?:else|endif))\s*$@) { $is_pending = 0; $_ = "/*$1*/"; $comment = ""; last }
            die "fatal: nested .if\n" if m@^\s*[.]if@;
            s@^(\s*)@$1/*@; s@\s*\Z(?!\n)@*/@;
            print $_, "\n";
          }
          die "fatal: missing .endif\n" if $is_pending;
        } elsif (m@[.]endif@) { $_ = "/*$_*/" }
        elsif (m@^[.]fill\s+([^,]+),\s*1,\s*0$@) { $_ = ". = . + [$1]" }  # SVR4 has `.zero $1`, SVR3 does not.
        else { die "fatal: unsupported directive: ($_)\n" if m@^[.]@ }
      } else {
        s@^([a-z]+\s+\$)([^(,]*)\(([^,]+)\)([^,]*)\s*,@ my $x = "$1${2}[$3]$4,"; $x =~ y~()~[]~; $x  @e ;  # Replace $(...) with $[...] .
        s@^([a-z]+\s+)\((\w+)\)([-+*])\((\w+)\)(?=\()@${1}$2$3$4@;  # Specialized replace (...)...(...) with ...+...( , keeping it working for GNU as (1).
        s@^([a-z]+\s+)\(([^(),]+)\)([^(),]*)\(([^(),]+)\)(?=\()@${1}[$2]${3}[$4]@;  # Unused specialized replace (...)...(...) with [...]...[...]( .
        s@\$\s*~\s*(\w+)\s*(?=,|\Z)@\$[-$1-1]@g; # Replace ~NUM with [-NUM-1].  # Replacing with [-1!NUM] would be correct for SVR3 assembler, but incorrect for GNU as(1).
      }
      if (0 and !m@"@ and !m@^[.]\s*=\s*[.]\s*[+]@) {  # Detect and print expressions with multiple operators, for debugging.
        my $s= $_;
        $s =~ s@^jmp\s+[*]@@ or $s =~ s@^\S+(?:\s*=)?\s*@@;
        $s =~ s@[a-zA-Z_][.\w]*@ @g;
        $s =~ s@[()\[\],\w\$%]+@ @g;
        #print STDERR "expr: $s: $_\n" if $s =~ m@\S@;
        print STDERR "multiop_expr: $s: $_\n" if $s =~ m@\S\s+\S@;  # Multiple operators.
      }
      print $ws, $_, $comment, "\n";
    }
    for my $k (sort(keys(%eqvars))) { my $s = $k; $s =~ s@-@___@g; print "$s = $k\n" }
    '

: --- GNU "as(1)."

rm -f fbsasms.o fbsasm
perl -pe 's@^(\s*)jcxz\b@${1}jecxz@' <fbsasms.s >fbsasmsg.s  # !! Is it equivalent without it? It generates code with a size prefix.
#toolset/bin/gaself32-2.22 --32 -o fbsasms.o fbsasmsg.s  # This also works.
toolset/bin/gaself32-2.7 -o fbsasms.o fbsasmsg.s
#!!toolset/bin/ld-2.22 -m elf_i386 -N -static -nostdlib -s -o fbsasm fbsasms.o
toolset/bin/ld-2.22 -m elf_i386 -N -static -nostdlib -o fbsasm fbsasms.o
cp -a fbsasm fbsasmg
#<fbsasm ndisasm -b 32 -e 0x54 -o 0x8048054 - >a0.ndisasm
#<fbsasm ndisasm -b 32 -e 0x54 - | perl -ne 'next if !m@^\w@; die if !m@^(\w+\s+)\w+\s+(.*)@; my($addr,$y) = ($1,$2); next if $y eq "nop"; if ($y =~ m@^xchg (\w+),(\w+)$@) { my @l=sort{$a cmp$b}$1,$2; $y="xchg $l[0],$l[1]" } $y =~ s@(?:byte [-+]?|)0x[0-9a-f]+@N@g; $y =~ s@ (?:near|short) @ @; $y =~ s@\[dword @\[@; print "$y\n"' >a.ndisasm
exit_code=0
./fbsasm || exit_code="$?"
test "$exit_code" = 1
./fbsasm fbsasm.fasm fbsasm-pass1
cmp -l fbsasm-pass1.good fbsasm-pass1

: --- SVR4.

if true; then  # !! Make it work. All instructions are correct (except possibly the numbers, which are unchecked). !! Everything works, cmp succeeds if `end if' is commented out in fbsasm.fasm.
  rm -f fbsasms.o fbsasm
  #perl -pe 's@___@-@g if !m@"@ and !m@ = @' <fbsasms.s >fbsasms4.s  # Work around this: .value prefix_instruction___assembler: illegal 16-bit relocation
  toolset/bin/svr4as-1993-01-16 fbsasms.s  # Generates fbsasms.s as ELF-32 relocatable (object) file.
  #!!tools/ld-2.22 -m elf_i386 -N -static -nostdlib -s -o fbsasm fbsasms.o
  tools/ld-2.22 -m elf_i386 -N -static -nostdlib -o fbsasm fbsasms.o
  #<fbsasm ndisasm -b 32 -e 0x54 -o 0x00700054 - >b0.ndisasm
  #<fbsasm ndisasm -b 32 -e 0x54 - | perl -ne 'next if !m@^\w@; die if !m@^(\w+\s+)\w+\s+(.*)@; my($addr,$y) = ($1,$2); next if $y eq "nop"; if ($y =~ m@^xchg (\w+),(\w+)$@) { my @l=sort{$a cmp$b}$1,$2; $y="xchg $l[0],$l[1]" } $y =~ s@(?:byte [-+]?|)0x[0-9a-f]+@N@g; $y =~ s@ (?:near|short) @ @; $y =~ s@\[dword @\[@; print "$y\n"' >b.ndisasm
  #diff -u a.ndisasm b.ndisasm >ab.diff ||:  # !! Don't generate it by default.
  exit_code=0
  # !! Without ___: fbsasm.fasm [10174]: end if: unexpected instruction. !! something wrong with structures_buffer
  ./fbsasm || exit_code="$?"
  test "$exit_code" = 1
  ./fbsasm fbsasm.fasm fbsasm-pass1
  cmp -l fbsasm-pass1.good fbsasm-pass1
fi

: --- SVR3.

for as in toolset/bin/svr3as-1987-10-28 toolset/bin/svr3as-1988-05-27 toolset/bin/svr3as-1989-10-03 toolset/bin/sunos4as-1988-11-16; do
  rm -f fbsasms.o fbsasm
  "$as" -dt -dg -dv fbsasms.s
  #<fbsasms.o ndisasm -b 32 -e 0x8c - >bc.ndisasm
  if true; then
    tools/miniperl-5.004.04 -x tools/link3coff.pl --elf fbsasms.o fbsasm 0x700000
  else  # !! Buggy, see bug1.txt.
    tools/miniperl-5.004.04 -x tools/fixcoff.pl fbsasms.o
    ld -m elf_i386 -N -static -nostdlib -s -o fbsasm fbsasms.o
  fi
  #<fbsasm ndisasm -b 32 -e 0x54 -o 0x00700054 - >b0.ndisasm
  #<fbsasm ndisasm -b 32 -e 0x54 - | perl -ne 'next if !m@^\w@; die if !m@^(\w+\s+)\w+\s+(.*)@; my($addr,$y) = ($1,$2); next if $y eq "nop"; if ($y =~ m@^xchg (\w+),(\w+)$@) { my @l=sort{$a cmp$b}$1,$2; $y="xchg $l[0],$l[1]" } $y =~ s@(?:byte [-+]?|)0x[0-9a-f]+@N@g; $y =~ s@ near @ @; print "$y\n"' >b.ndisasm
  exit_code=0
  ./fbsasm || exit_code="$?"
  test "$exit_code" = 1
  # !! SunOS: aline 11491     : Cannot Open Temporary (rel) File
  ./fbsasm fbsasm.fasm fbsasm-pass1
  cmp -l fbsasm-pass1.good fbsasm-pass1
done

: "$0" OK.
