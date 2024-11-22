#!/bin/sh --
eval 'PERL_BADLANG=x;export PERL_BADLANG;exec perl -x "$0" "$@";exit 1'
#!perl  # Start marker used by perl -x.
+0 if 0;eval("\n\n\n\n".<<'__END__');die$@if$@;__END__

#
# nasm2as.pl: convert (restricted) NASM syntax to i386 GNU as (AT&T) syntax
# by pts@fazekas.hu at Fri Mar 22 01:05:44 CET 2024
#
# Usage: ./nasm2as.pl <fbsasm.nasm >fbsasm.s
#
# This program is incomplete and hacky. It's customized for fbsasm.nasm.
#

BEGIN { $^W = 1 }
use integer;
use strict;

my %gpregs = map { $_ => 1 } qw(eax ebx ecx edx esi edi esp ebp ax bx cx dx si di sp bp al ah bl bh cl ch dl dh);
my $insts1_re = q@mov|xchg|push|pop|cmp|test|int|inc|dec|neg|not|mul|imul|div|idiv|add|sub|or|xor|and|sbb|adc|shr|shl|sar|ror|rol|rcl|rcr|rcl|lea|jmp|call|setn?[ze]|bts@;

my %str_insts = (
    'cmpsb' => 'cmpsb %es:(%edi), %ds:(%esi)',
    'cmpsw' => 'cmpsw %es:(%edi), %ds:(%esi)',
    'cmpsd' => 'cmpsl %es:(%edi), %ds:(%esi)',  # GNU as(1) 2.9.1 needs thus. GNU as(1) 2.22 can do just `cmpsd'.
    'lodsb' => 'lodsb %ds:(%esi), %al',
    'lodsw' => 'lodsw %ds:(%esi), %ax',
    'lodsd' => 'lodsl %ds:(%esi), %eax',
    'movsb' => 'movsb %ds:(%esi), %es:(%edi)',
    'movsw' => 'movsw %ds:(%esi), %es:(%edi)',
    'movsd' => 'movsl %ds:(%esi), %es:(%edi)',
    'scasb' => 'scasb %es:(%edi), %al',
    'scasw' => 'scasw %es:(%edi), %ax',
    'scasd' => 'scasl %es:(%edi), %eax',
    'stosb' => 'stosb %al, %es:(%edi)',
    'stosw' => 'stosw %ax, %es:(%edi)',
    'stosd' => 'stosl %eax, %es:(%edi)',
);

sub fix_chars($) {
  my $chars = $_[0];
  my $size = length($chars);
  die "fatal: unsupported chars size ($.): '$chars'\n" if $size > 4;
  return 0 if !$size;
  my $sa = -8;
  # Fix multibyte character constant. We need the parens for (...<<...)
  # because some assemblers such as the SVR3 assembler have incorrect
  # operator precedence.
  $chars =~ s@([^!-\[\]-~])|(.)@ $sa += 8; ($sa ? "|('" : "'") . (defined($1) ? sprintf("'\\x%02x", ord($1)) : $2) . ($sa ? "<<$sa)" : "") @sge;
  $chars = "($chars)" if $size > 1;
  $chars
}

sub escape_as_str($) {
  my $str = $_[0];
  $str =~ s@([\x00-\x1f\\"\x7f-\xff])@ ($1 eq "\\" or $1 eq "\"") ? "\\$1" : sprintf("\\x%02x", ord($1)) @ge;
  $str
}

sub parse_bin($) {
  my $str = $_[0];
  my $result = 0;
  for (my $i = 0; $i < length($str); ++$i) {
    $result = $result << 1 | (substr($str, $i, 1) ne "0");
  }
  $result;
}

my $last_label = "";

sub fix_expr($) {
  my $expr = $_[0];
  $expr =~ s@([0-9][0-9a-fA-F]*)[hH]|([01]+)[bB]|0[xX]([0-9a-fA-F]+)|([0-7]+)[oOqQ]|([1-9][0-9]*|0(?!0))|'([^']{1,4})'|(byte|[dqtoyz]?word)|([_a-zA-Z][.\w]*)|([-+\[\]\s()~*]|<<|>>)|(\W+|\w+)@
    defined($1) ? sprintf("0x%x", hex($1)) :
    defined($2) ? parse_bin($2) :
    defined($3) ? sprintf("0x%x", hex($3)) :
    defined($4) ? sprintf("0%o", oct($4)) :
    defined($5) ? 0 + $5 :
    defined($6) ? fix_chars($6) :
    defined($7) ? die("fatal: unexpected size keyword ($.): $expr: $7\n") :
    defined($8) ? (exists $gpregs{$8} ? "\%$8" : $8) :  # Register or label.
    defined($9) ? $9 :  # Operator.
    die("fatal: unexpected operator ($.): $expr: $10\n")
  @ges;
  die $@ if $@;
  $expr
}

sub fix_inst_arg($) {
  my $arg = fix_expr($_[0]);
  return $arg if substr($arg, 0, 1) eq "%" and exists($gpregs{substr($arg, 1)});
  # Input-specific fixes:
  # [esi+5+eax] -->
  # [esi+4+eax+1] -->
  # TODO(pts): Support whitespace.
  if ($arg =~ m@^\[\%(e(?:[abcd]x|[sd]i|[sb]p))[+]\%(e(?:[abcd]x|[sd]i|[sb]p))(?:[*]([1248]))?\]$@s) {
    my $factor = defined($3) ? $3 : 1;
    return "(\%$1,\%$2,$factor)";
  } elsif ($arg =~ m@^\[\%(e(?:[abcd]x|[sd]i|[sb]p))[+]([^%*]+)[+]\%(e(?:[abcd]x|[sd]i|[sb]p))(?:[*]([1248]))?\]$@s) {
    my $factor = defined($4) ? $4 : 1;
    return "$2(\%$1,\%$3,$factor)";
  } elsif ($arg =~ m@^\[\%(e(?:[abcd]x|[sd]i|[sb]p))[+]([^%*]+)[+]\%(e(?:[abcd]x|[sd]i|[sb]p))(?:[*]([1248]))?[+]([^%*]+)\]$@s) {
    my $factor = defined($4) ? $4 : 1;
    return "($2)+($5)(\%$1,\%$3,$factor)";
  } elsif ($arg =~ m@^\[\%(e(?:[abcd]x|[sd]i|[sb]p))([-+])(.+)\]$@s) {  # [esi-1] --> -1(%esi)
    my $sep = $2 eq "+" ? "" : "-";
    return "$sep$3(\%$1)";
  } elsif ($arg =~ m@^\[\%(.*)\]$@s and exists($gpregs{$1})) {
    return "(\%$1)";
  } elsif ($arg =~ m@^\[(.*?)[+]\%(e(?:[abcd]x|[sd]i|[sb]p))[*]([1248])\]$@s) {
    return "$1(,\%$2,$3)";
  } elsif ($arg =~ m@^\[(.*)\]$@s) {
    my $result = $1;
    die "fatal: unexpected register in expr ($.): $arg\n" if $arg =~ m@\%@;
    return $result;
  } elsif ($arg !~ m@^\[@) {
    return "\$$arg";
  }
  die "fatal: unsupported arg ($.): $arg\n";
}

# --- main().

if (!@ARGV or $ARGV[0] eq "--help") {
  print STDERR "Usage: $0 <prog_in.nasm> <prog_out.s>\n";
  exit(@ARGV ? 0 : 1);
}
if (@ARGV and $ARGV[0] eq "--") {
} else {
  if (@ARGV) {
    die "fatal: opening for input: $ARGV[0]\n" if !open(STDIN, "<", $ARGV[0]);
    shift(@ARGV);
  }
  if (@ARGV) {
    die "fatal: opening for output: $ARGV[0]\n" if !open(STDOUT, ">", $ARGV[0]);
    shift(@ARGV);
  }
}

my $logo = "# This file is autognerated by nasm2as.pl";
my $do_skip = 1;
my %defs;
while (<STDIN>) {
  s@\s+\Z(?!\n)@@;
  die "fatal: non-ASCII character found ($.): $_\n" if m@[^\t -~]@;
  my $comment = "\n";
  if (m@;@) {
    s@('[^']*')|\s*;(.*)@
      if (defined($1)) { $1 }
      else { $comment = "  # $2"; $comment =~ s/\s+\Z(?!\n)//; $comment .= "\n"; "" }
    @ge;
  }
  my $prews = "";
  $prews = $1 if s@^(\s+)@@;
  if ($do_skip) {
    if ($_ eq "start:") {
      print qq(.globl _start\n_start:  # Program entry point.\n);
      $do_skip = 0;
    } elsif (m@^%endm@) {
      $_ = "##$_";
      $do_skip = 0;
    } elsif (length($logo) and !length($_) and length($comment) <= 1) {
      $_ = $logo; $logo = "";
    } else {
      $_ = "##$_";
    }
  } elsif (m@^(?:([_a-zA-Z][.\w]*)[:\s]\s*)?d([bwd]|bw)\s+(.*)$@) {
    my $label = $1;
    $last_label = $1 if defined($1) and substr($1, 0, 1) ne ".";
    my $directive = $2 eq "b" ? ".byte" : $2 eq "bw" ? ".byte" : $2 eq "w" ? ".word" : ".long";
    my $dbw_expr;
    my $args = $3;
    if ($2 eq "bw") {
      die "fatal: unsupported dbw last expr ($.): $_\n" if $args !~ s@\s*,\s*([^,']+)\Z(?!\n)@@;
      $dbw_expr = fix_expr($1);
    }
    #die "$label--$directive--$args.\n";
    my $out = "";
    $out .= "$prews$label:\n" if length($label);
    pos($args) = 0;
    while ($args =~ m@'([^']*)'|(')|([^']+)@g) {
      if (defined($1)) {
        die "fatal: string literal must be db ($.): $_\n" if $directive ne ".byte";
        $out .= "$prews.ascii \"" . escape_as_str($1) . "\"\n";
      } elsif (defined($2)) {
        die "fatal: unsupported string literal ($.): $_\n";
      } else {
        my @args = grep { length($_) } map { fix_expr($_) } split(m@\s*,\s*@, $3);
        if (grep { exists($defs{$_}) } @args) {
          for my $arg (@args) {
            if (exists($defs{$arg})) {  # Example: "VERSION_STRING".
              $out .= "$prews.ascii \"" . escape_as_str($defs{$arg}) . "\"\n";
            } else {
              $out .= "$prews$directive $arg\n";
            }
          }
        } else {
          $out .= "$prews$directive " . join(", ", @args) . "\n";
        }
      }
    }
    $out .= "$prews.word $dbw_expr\n" if defined($dbw_expr);
    ($_, $prews) = ($out, "");
    chomp($_);
  } elsif (m@^%macro\s+dbw\s+3\s*$@) {
    # %macro dbw 3
    #   db %1, %2
    #   dw %3
    # %endm
    $_ = "##$_";
    $do_skip = 1;
  } elsif (m@^%macro\s+dm\s+1\s*$@) {
    # %macro dm 1
    #   db %1, 0
    # %endm
    $_ = "##$_";
    $do_skip = 1;
  } elsif (m@^%define\s+(VERSION_STRING)\s+'([^']*)'\s*$@) {
    $defs{$1} = $2;
    $_ = "##$_";
  } elsif (m@^dm\s+"([^"\\]*)"\s*$@) {
    $_ = ".asciz \"$1\"";
  } elsif (length($_)) {
    if (m@'@) {
      pos($_) = 0;
      my $is_ok = 1;
      s@(',')|('[^']*')|'@
        if (defined($1)) { ord(',') }  # Fix arg parsing. TODO(pts): Add ',' to $comment.
        elsif (defined($2)) { $is_ok = 0 if index($2, ",") >= 0; $2 }
        else { $is_ok = 0; "" }
      @ge;
      die "fatal: quotes unsupported ($.): $_\n" if !$is_ok;
    }
    s@^(jmp|call)\s+near\s+dword\s*\[@$1 dword [@;
    if (m@^([_a-zA-Z][.\w]*)\s*:$@) {  # Label.
      $last_label = $1 if substr($1, 0, 1) ne ".";
      $_ = "$1:";
    } elsif (m@^([.][_a-zA-Z][.\w]*)\s*:$@) {  # Label.
      $_ = "$last_label$1:";
    } elsif (m@^(?:ret|clc|stc|cdq|cbw|cwde|xlatb)$@) {
    } elsif (m@(\w+)\s+equ\s+(\S.*)$@) {
      if ($_ eq q(bss_align equ ($$-$)&3)) {
        # ld(1) will add proper alignment, but not if the ELF-32 object file was generated by the SVR4 assembler.
        # .align 4 is a workaround here, it may add 1--3 bytes to the program (.text section).
        $_ = ".align 4\n# bss_align = 0";
      } else {
        $_ = "$1 = " . fix_expr($2);
      }
    } elsif (m@^((?:rep(?:n?[ez]|)\s+)?(?:lod|sto|cmp|mov|sca)s[bwd])$@) {
      # die "fatal: assert: missing whitespace after rep: $_" if ...;
      s@\s+@\n$prews@;  # Multiple lines needed by GNU as(1) 2.9.1.
      die "fatal: assert: unknown string instruction: $1\n" if !s@\b(\w+)$@@ or !defined($str_insts{$1});
      $_ .= $str_insts{$1};
    } elsif (m@^(mov[sz])x\s+(e[^,]*?)\s*,\s*(byte\s*|word\s*)([^,]+)\s*$@) {
      $_ = $1 . substr($3, 0, 1) . "l " . fix_inst_arg($4) . ", " . fix_inst_arg($2);
    } elsif (m@^(mov[sz])x\s+(e[^,]*?)\s*,\s*([abcd][lh])@) {
      $_ = "${1}bl \%$3, \%$2";
    } elsif (m@^(mov[sz])x\s+(e[^,]*?)\s*,\s*([abcd]x|[sd]i|[sb]p)@) {
      $_ = "${1}wl \%$3, \%$2";
    } elsif (m@^(call|loop(?:n?[ez]|)|j[a-z]+)\s+(?:near_o0\s+|near\s+|short\s+|)(.?[_a-zA-Z]\w*)$@ and !exists($gpregs{$2})) {  # Earlier than the general branch.
      my $ll = substr($2, 0, 1) eq "." ? $last_label: "";
      $_ = "$1 $ll$2";
    } elsif (m@^($insts1_re)\s+(e(?:[abcd]x|[sd]i|[sb]p))\s*,\s*dword\b\s*([^,]+?)\s*$@o or  # Earlier than the general branch.
             m@^($insts1_re)\s+([abcd]x|[sd]i|[sb]p)\s*,\s*word\b\s*([^,]+?)\s*$@o or  # Earlier than the general branch.
             m@^($insts1_re)\s+([abcd][lh])\s*,\s*byte\b\s*([^,]+?)\s*$@o) {  # Earlier than the general branch.
      $_ = "$1 " . fix_inst_arg($3) . ", \%$2";
    } elsif (m@^($insts1_re)\s+(byte\b\s*|word\b\s*|dword\b\s*|)(.*)@o) {  # This is the general branch.
      my $inst = $1; my $size = $2; my @args = reverse map { fix_inst_arg($_) } split(m@\s*,\s*@, $3);
      die "fatal: too many arguments for $inst ($.): $_\n" if @args > 2;
      $size = "l" if substr($size, 0, 1) eq "d";
      $inst .= substr($size, 0, 1);
      if (($inst eq "jmp" or $inst eq "jmpl" or $inst eq "call" or $inst eq "calll") and @args == 1) {
        $_ = "$inst *$args[0]";
      } else {
        $_ = "$inst " . join(", ", @args);
        s@^((?:shr|shl|sar|ror|rol|rcl|rcr|rcl)[bwl]?) \$1, @$1 @;  # Optimization for GNU as(1) 2.9.1. GNU as(1) 2.22 does it.
        if (m@^movb? %al, ([_a-zA-Z][.\w]*)$@) {  # This is just an optimization to match instruction byte sizes generated by different versions of GNU as(1).
          ($_, $prews) = ("$prews.byte 0xa2  #WORKAROUNDL movb %al, $1\n$prews.long $1", "");
        } elsif (m@^cmpw? \$(.*?), %bx$@) {  # Bugfix for GNU as(1) 2.9.1. It's already fixed in GNU as(1) 2.9.5.
          ($_, $prews) = ("$prews.byte 0x66, 0x81, 0xfb  #WORKAROUNDW cmpw \$$1, %bx\n$prews.word $1", "");
        } elsif (m@^cmpw? \$(.*?), %ax$@) {  # Bugfix for GNU as(1) 2.9.1. It's already fixed in GNU as(1) 2.9.5.
          ($_, $prews) = ("$prews.byte 0x66, 0x3d  #WORKAROUNDW cmpw \$$1, %ax\n$prews.word $1", "");
        } elsif (m@^movw? \$(.*?), %ax$@) {  # Bugfix for GNU as(1) 2.9.1. It's already fixed in GNU as(1) 2.9.5.
          ($_, $prews) = ("$prews.byte 0x66, 0xb8  #WORKAROUNDW movw \$$1, %ax\n$prews.word $1", "");
        #} elsif ($_ eq "xor %al, %al") {  # Bugfix for GNU as(1) 2.6, it generates `xor %eax, %eax'. It's alrady fixed in GNU as(1) 2.9.1. There are way too many (hundreds) to fix here.
        #  $_ = ".byte 0x30, 0xc0  # $_";
        }
      }
    } elsif (m@^(sh[lr]d)\s+([^,]+?)\s*,\s*(e(?:[abcd]x|[sd]i|[sb]p))\s*,\s*([^,]+)$@) {
      $_ = "$1 " . fix_inst_arg($4) . ", \%$3, " . fix_inst_arg($2);
    } elsif (m@^\%if\s@ or $_ eq "%else" or $_ eq "%endif") {
      substr($_, 0, 1) = ".";
    } elsif ($_ eq "absolute \$" or $_ eq "section .bss" or $_ eq "section .bss align=1") {
      $_ = ".bss";  # Some versions of GNU as(1) emit the wrong COFF section flags for `.section .bss'.
    } elsif (m@^alignb?\s+([1248])$@) {
      $_ = ".align $1";
    } elsif (m@^(?:([_a-zA-Z][.\w]*)[:\s]\s*)?resb\s+(.*)$@) {
      my $label = $1;
      $last_label = $1 if defined($1) and substr($1, 0, 1) ne ".";
      $_ = ".fill " . fix_expr($2) . ", 1, 0";
      substr($_, 0, 0) = "# " if $2 eq "bss_align";
      if (defined($label)) {
        substr($_, 0, 0) = "$prews$label:\n$prews";
        $prews = "";
      }
    } elsif ($_ eq "%define CASE_SENSITIVE") {
      $_ = "CASE_SENSITIVE = 1";
    } elsif ($_ eq "%ifndef CASE_SENSITIVE") {
      $_ = ".if !CASE_SENSITIVE\n";
    } else {
      #print qq(.string "foo"\n); last;
      die "fatal: unsupported instruction ($.): $_\n";
    }
  }
  $comment =~ s@^ +@@ if !length($_);
  print $prews, $_, $comment;
}

__END__
