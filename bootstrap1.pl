#!/bin/sh --
eval 'PERL_BADLANG=x;export PERL_BADLANG;exec perl -x "$0" "$@";exit 1'
#!perl  # Start marker used by perl -x.
+0 if 0;eval("\n\n\n\n".<<'__END__');die$@if$@;__END__

#
# bootstrap1.pl: Perl script to build an early Linux i386 fasm executable program using nasm(1) and unzip(1)
# by pts@fazekas.hu at Sat Nov 23 03:48:25 CET 2024
#
# This script works with Perl 5.004.04 (1997-10-15) or later.
#

BEGIN { $ENV{LC_ALL} = "C" }  # For deterministic output. Typically not needed. Is it too late for Perl?
BEGIN { $ENV{TZ} = "GMT" }  # For deterministic output. Typically not needed. Perl respects it immediately.
BEGIN { $^W = 1 }  # Enable warnings.
use integer;
use strict;

my $src_zip_file = "orig/fasm137.zip";
# Works with NASM 0.95 (1997-07-27), 0.97 (1997-12-06), 0.98.39 (2005-01-15), 2.10.09 (2013-07-23), 2.13.02 (2017-12-03).
my $nasm_cmd = "nasm";
my $unzip_cmd = "unzip";
{ my $i = 0;
  while ($i < @ARGV) {
    my $arg = $ARGV[$i++];
    if ($arg eq "--") { last }
    elsif ($arg eq "-" or substr($arg, 0, 1) ne "-") { --$i; last }
    elsif ($arg =~ m@--nasm=(.*)@s) { $nasm_cmd = $1 }
    elsif ($arg =~ m@--unzip=(.*)@s) { $unzip_cmd = $1 }
    elsif ($arg =~ m@--srczip=(.*)@s) { $src_zip_file = $1 }
    else { die "fatal: unknown command-line flag: $arg\n" }
  }
  if ($i != @ARGV) {
    $src_zip_file = $ARGV[$i++];
    die "fatal: too many command-line arguments\n" if $i != @ARGV;
  }
}

die "fatal: missing src .zip file: $src_zip_file\n" if !-f($src_zip_file);
my $src_zip_base = $src_zip_file;
$src_zip_base =~ s@^.*[/\\]@@;
my $src_zip_version = $src_zip_base =~ m@^fasm-?1[.]?(\d+)\b@ ? $1 + 0 : -1;
my $have_linux_src = $src_zip_version >= 37;
my @src_files = qw(SOURCE/ASSEMBLE.INC SOURCE/ERRORS.INC SOURCE/EXPRESSI.INC SOURCE/FORMATS.INC SOURCE/PARSER.INC SOURCE/PREPROCE.INC SOURCE/TABLES.INC SOURCE/VERSION.INC);
push @src_files, qw(SOURCE/LINUX/FASM.ASM SOURCE/LINUX/SYSTEM.INC) if $src_zip_version >= 37;  # Linux target was introduced in fasm 1.37.
unlink(@src_files, qw(SOURCE/LINUX/FASM.ASM SOURCE/LINUX/SYSTEM.INC));
sub system_checked(@) {
  print STDERR "info: running: @_\n";
  die "fatal: command $_[0] failed\n" if system(@_);
}
system_checked($unzip_cmd, $src_zip_file, @src_files);

die "fatal: open: SOURCE/LINUX/FASM.ASM: $!\n" if !open(FA, "< SOURCE/LINUX/FASM.ASM");
binmode(FA);
die if !open(FF, "> fasm.fasm");
binmode(FF);
sub fnopenq($) { $_[0] =~ m@[-+.\w]@ ? $_[0] : "./" . $_[0] }
sub read_file($) {
  my $fn = $_[0];
  die "fatal: open: $fn: $!\n" if !open(FR, "< " . fnopenq($fn));
  binmode(FR);
  my $s = join("", <FR>);
  die if !close(FR);
  $s
}
while (<FA>) {
  s@\r?\n@\r\n@;  # fasm-1.20 segfaults unless source line ending is CRLF (\r\n).
  if (m@^include\s+\x27(.*?)\x27@) {  # Process `include` directive.
    $_ = uc($1);
    s@^..[/\\]@SOURCE/@ or s@^@SOURCE/LINUX/@;
    my $fn = $_;
    $_ = read_file($fn);
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
    print FF $_, "\r\n";
  } else {
    print FF;
  }
}
# We may want to do additional patches so that the NUL bytes of .bss are not
# added to the file. But the few KiB of savings in a temporary program file
# are not compelling enough.
die if !close(FF);

die if !open(FF, "< fasm.fasm");
binmode(FF);
die if !open(FN, "> fasm.nasm");
binmode(FN);
my %longsizes = qw(b byte  w word  d dword);
my %sizes;
while (<FF>) {  # Scan variable definitions for sizes, populate $sizes{$varname} = $size;
  $sizes{$1} = $2 if m@^\s*([a-zA-Z_]\w*)\s*:?\s*[dr]([bwd])\b@;
}
my $varname_re = "([ ,])\\[\\s*(" . join("|", sort(keys(%sizes))) . ")\\b";
die "fatal: rewind: fasm.fasm: $!\n" if !seek(FF, 0, 0);
my %vars; $vars{VERSION} = ""; $vars{VERSION_STRING} = ""; $vars{program_base} = "";
while (<FF>) {  # Convert from fasm to NASM syntax. Only works for some versions of fasm sources.
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
  elsif (s@^((?:push|pop) (?:(?:byte|word|dword)\b\s*)?)(\S.*)@ my $in = $1; my $args = $2; $in =~ s/\s+\Z(?!\n)//; map { print FN "$in $_\n" } split(" ", $args); "" @e) { next }
  elsif (m@^(if\b|el|end\s*if)\b@) { print STDERR "fatal: conditional assembly not supported by NASM 0.95: $_\n" }
  #elsif (s@^(if|else)\b@%$1@) { s@<>@!=@g }
  #elsif (s@^end if\b@%endif@) {}
  elsif (s@^((?:rep[a-z]*\s+)?)(lod|sto|mov|cmp|sca)s\s+(byte|word|dword)\b.*@ $1.$2."s".substr($3,0,1) @e) {}
  elsif (s@^jmp near\s+(e[abcd]x|e[sd]i|e[sb]p)$@jmp $1@) {}
  elsif (s@^(j(?!mp |e?cxz )[a-z]+) (?!near\s+|short\s+)@$1 near @) {}  # Force conditional jumps to be `neaar`, to avoid NASM error: short jump is out of range, without `nasm -O1`.
  elsif (s@^setalc$@salc@) {}
  elsif (s@^use32$@bits 32@) {}  # For NASM 0.97.
  elsif (s@^loopnzd @loopnz @) {}
  elsif (s@^xlat byte\b.*@xlatb@) {}
  elsif (s@^align (.+)$@times (\$\$-\$\)&(($1)-1) db 0@) {}  # For NASM 0.95.
  print FN "$_\n";
}
die if !close(FN);
die if !close(FF);

unlink(qw(fasm0 fasm1 fasm2));
system_checked($nasm_cmd, qw(-o fasm0 fasm.nasm));
sub chmod_x($) {  # Make file $_[0] executable.
  my $fn = $_[0];
  my @stat = stat($fn);  # TODO(pts): Test it on Win32.
  die "fatal: stat: $fn: $!\n" if !@stat;
  if ($^O !~ m@^win@i) {
    my $mode = ($stat[2] & 0777) | (($stat[2] & 0444) >> 2);  # Add executable bit where there is a readable bit.
    die "fatal: chmod: $fn: $!\n" if !eval { chmod($mode, $fn) };
  }
}
chmod_x("fasm0");
system_checked(qw(./fasm0 fasm.fasm fasm1));
chmod_x("fasm1");
my $s1 = read_file("fasm1");
system_checked(qw(./fasm1 fasm.fasm fasm2));
chmod_x("fasm2");
my $s2 = read_file("fasm2");
die "fatal: file content mismatch: fasm1 vs fasm2\n" if $s1 ne $s2;
my $fn3;
if ($src_zip_base =~ m@^fasm-?1[.]?37\b@ and -f($fn3 = "fasm-golden-1.37")) {
  print STDERR "info: comparing: fasm1 vs $fn3\n";
  my $s3 = read_file($fn3);
  die "fatal: file content mismatch: fasm1 vs $fn3\n" if $s1 ne $s3;
}

print STDERR "info: bootstrap1 OK: $0\n";

__END__
