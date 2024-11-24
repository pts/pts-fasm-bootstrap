#!/bin/sh --
eval 'PERL_BADLANG=x;export PERL_BADLANG;exec perl -x "$0" "$@";exit 1'
#!perl  # Start marker used by perl -x.
+0 if 0;eval("\n\n\n\n".<<'__END__');die$@if$@;__END__

#
# bootstrap2.pl: Perl script to build a recent Linux i386 fasm executable program using fasm
# by pts@fazekas.hu at Sat Nov 23 14:12:29 CET 2024
#
# This script works with Perl 5.004.04 (1997-10-15) or later.
#

BEGIN { $ENV{LC_ALL} = "C" }  # For deterministic output. Typically not needed. Is it too late for Perl?
BEGIN { $ENV{TZ} = "GMT" }  # For deterministic output. Typically not needed. Perl respects it immediately.
BEGIN { $^W = 1 }  # Enable warnings.
use integer;
use strict;

my $src_zip_file = "orig/fasmw17332.zip";
# Works with fasm 1.20 (2001-11-17) patched by bootstrap1.pl, fasm 1.37 (2002-06-12) patched by bootstrap1.pl, fasm 1.43 (2003-12-30) patched, fasm 1.73.32 (2023-12-04).
my $fasm_cmd = "./fasm1";
my $unzip_cmd = "unzip";
{ my $i = 0;
  while ($i < @ARGV) {
    my $arg = $ARGV[$i++];
    if ($arg eq "--") { last }
    elsif ($arg eq "-" or substr($arg, 0, 1) ne "-") { --$i; last }
    elsif ($arg =~ m@--fasm=(.*)@s) { $fasm_cmd = $1 }
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
my $src_zip_version = $src_zip_base =~ m@^fasmw?-?1[.]?(\d+)\b@ ? $1 + 0 : -1;  # Examples: -1, 20, 37, 7332.
my $src_zip_version_dot = "1.$src_zip_version"; $src_zip_version_dot =~ s@^(1[.]\d\d)(?=\d)@$1.@;  # Example: "1.73.32".
sub system_checked(@) {
  print STDERR "info: running: @_\n";
  die "fatal: command $_[0] failed\n" if system(@_);
}
die "fatal: dup STDOUT: $!\n" if !open(OLDOUT, ">&STDOUT");
die "fatal: reopen STDOUT: unzip.lst: $!\n" if !open(STDOUT, "> unzip.lst");
binmode(STDOUT);
system_checked($unzip_cmd, "-l", $src_zip_file);
die "fatal: undup STDOUT: $!\n" if !open(STDOUT, ">&OLDOUT");
die if !close(OLDOUT);
die "fatal: open: unzip.lst: $!\n" if !open(FL, "< unzip.lst");
binmode(FL);
my @src_files;
while (<FL>) {
  push @src_files, $1 if m@\s(SOURCE/\S+)\r?$@;
}
die if !close(FL);

unlink(@src_files);
system_checked($unzip_cmd, $src_zip_file, @src_files);

die "fatal: open: SOURCE/LINUX/FASM.ASM: $!\n" if !open(FA, "< SOURCE/LINUX/FASM.ASM");
binmode(FA);
die if !open(FF, "> fasm3.fasm");
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
#print FF "salc equ setalc\r\n";  # This doesn't work in fasm-1.43.
print FF "salc equ db 0xd6\r\n";
print FF "macro pushd value { push dword value }\r\n";
print FF "macro align value { rb (value-1) - (\$ + value-1) mod value }\r\n";
print FF "ELF32_program_base = 0x8048000\r\n";
print FF "org ELF32_program_base\r\n";
print FF "use32\r\n";
print FF "ELF32_file_header:\r\n";  # An approximation of ELF32 header generation by `format elf executable 3' in recent fasm.
print FF "db 0x7F,\x27ELF\x27,1,1,1,3\r\n";
print FF "rb ELF32_file_header+0x10-\$\r\n";
print FF "dw 2,3\r\n";
print FF "dd 1,start\r\n";
print FF "dd ELF32_program_header-ELF32_file_header,0,0\r\n";
print FF "dw ELF32_program_header-ELF32_file_header,0x20,1,0x28,0,0\r\n";
print FF "ELF32_program_header:\r\n";
print FF "dd 1,0,ELF32_program_base,0\r\n";
print FF "dd ELF32_bss-ELF32_program_base,ELF32_program_end-ELF32_program_base,7,0x1000\r\n";
while (<FA>) {
  next if m@^[ \t]*(?:format|entry|segment)\b@;  # Not understood by NASM 1.20 and 1.37.
  s@\r?\n@\r\n@;  # fasm-1.20 segfaults unless source line ending is CRLF (\r\n).
  if (m@^[ \t]*include\s+\x27(.*?)\x27@) {  # Process `include` directive.
    $_ = uc($1); y@\\@/@;
    s@^..[/\\]@SOURCE/@ or s@^@SOURCE/LINUX/@;
    my $fn = $_;
    $_ = read_file($fn);
    s@[\r\n]+\Z(?!\n)@@;
    s@\r?\n@\r\n@g;  # fasm-1.20 segfaults unless source line ending is CRLF (\r\n).
    s@^(\w+[ \t]*=[ \t]*0[0-7]*)([ \t]*\r?$)@${1}o$2@mg;  # Fix octal constants in system.inc. This affects the `create:` function. It's not needed for recent fasm.
    if ($fn eq "SOURCE/LINUX/SYSTEM.INC") {
      # In system.inc, try to use at least 2.5 MiB of memory. 1 MiB is not
      # enough. 2 MiB is enough for compiling 1.43. 2.5 MiB is enough for
      # compilig fasm 1.73.32.
      my $heap_size = 0x280000;
      # Typically this is missing in recent fasm, no need to patch.
      s@^[ \t]*mov[ \t]+ebx[ \t]*,[ \t]*buffer[ \t]*\r?\n[ \t]*mov[ \t]+eax[ \t]*,[ \t]*116[ \t]*\r?\n[ \t]*int[ \t]+0x80[ \t]*\r?\n([ \t]*allocate_memory:)@\tmov dword [buffer+14h],$heap_size  ; PATCH\r\n$1@m;
    }
    print FF $_, "\r\n";
  } elsif (m@^[ \t]*align[ \t]+4[ \t]*\r?$@) {
    print FF "ELF32_bss:\r\n", $_;
  } else {
    print FF;
  }
}
print FF "ELF32_program_end:\r\n";
# We may want to do additional patches so that the NUL bytes of .bss are not
# added to the file. But the few KiB of savings in a temporary program file
# are not compelling enough.
die if !close(FF);
die if !close(FA);

# Just process the `include` directives.
die "fatal: open: SOURCE/LINUX/FASM.ASM: $!\n" if !open(FA, "< SOURCE/LINUX/FASM.ASM");
binmode(FA);
die if !open(FF, "> fasm4.fasm");
binmode(FF);
while (<FA>) {
  s@\r?\n@\r\n@;  # fasm-1.20 segfaults unless source line ending is CRLF (\r\n).
  if (m@^[ \t]*include\s+\x27(.*?)\x27@) {  # Process `include` directive.
    $_ = uc($1); y@\\@/@;
    s@^..[/\\]@SOURCE/@ or s@^@SOURCE/LINUX/@;
    my $fn = $_;
    $_ = read_file($fn);
    s@[\r\n]+\Z(?!\n)@@;
    s@\r?\n@\r\n@g;  # fasm-1.20 segfaults unless source line ending is CRLF (\r\n).
    print FF $_, "\r\n";
  } else {
    print FF;
  }
}
die if !close(FF);
die if !close(FA);

unlink(qw(fasm4 fasm5 fasm6));
system_checked($fasm_cmd, qw(fasm3.fasm fasm4));
sub chmod_x($) {  # Make file $_[0] executable.
  my $fn = $_[0];
  my @stat = stat($fn);  # TODO(pts): Test it on Win32.
  die "fatal: stat: $fn: $!\n" if !@stat;
  if ($^O !~ m@^win@i) {
    my $mode = ($stat[2] & 0777) | (($stat[2] & 0444) >> 2);  # Add executable bit where there is a readable bit.
    die "fatal: chmod: $fn: $!\n" if !eval { chmod($mode, $fn) };
  }
}
chmod_x("fasm4");
system_checked(qw(./fasm4 fasm4.fasm fasm5));  # Compile the unpatched fasm4.fasm with the newly compiled fasm.
chmod_x("fasm5");
my $s5 = read_file("fasm5");
system_checked(qw(./fasm5 fasm4.fasm fasm6));
chmod_x("fasm6");
my $s6 = read_file("fasm6");
die "fatal: file content mismatch: fasm5 vs fasm6\n" if $s5 ne $s6;
my $fn7 = "fasm-golden-$src_zip_version_dot";
if (-f($fn7)) {
  print STDERR "info: comparing: fasm5 vs $fn7\n";
  my $s7 = read_file($fn7);
  die "fatal: file content mismatch: fasm5 vs $fn7\n" if $s5 ne $s7;
}
die "fatal: rename\n" if !rename("fasm6", "fasm-re-$src_zip_version_dot");

print STDERR "info: bootstrap2 OK: $0\n";

__END__
