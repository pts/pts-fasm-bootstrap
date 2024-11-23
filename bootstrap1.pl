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

if (!$have_linux_src and $src_zip_version == 20) {  # This can be removed if bootstrapping support for fasm 1.20 is not needed.
  die if !open(FW, "> SOURCE/LINUX/FASM.ASM");
  binmode(FW);
  die if !print(FW <<'__ENDF__');
; flat assembler source
; Copyright (c) 1999-2001, Tomasz Grysztar
; All rights reserved.

	program_base = 0x700000

	org	program_base
	use32

	macro	align value { rb (value-1) - ($ + value-1) mod value }

file_header:
	db	0x7F,'ELF',1,1,1,3
	rb	file_header+0x10-$
	dw	2,3
	dd	1,start
	dd	program_header-file_header,0,0
	dw	program_header-file_header,0x20,1,0x28,0,0

program_header:
	dd	1,0,program_base,0
	dd	bss-program_base,program_end-program_base,7,0x1000

start:

	mov	edx,_logo
	call	display_string

	pop	eax
	cmp	eax,3
	jne	information
	pop	eax
	pop	[input_file]
	pop	[output_file]

	call	init_memory

	mov	edi,characters
	mov	ecx,100h
	xor	al,al
	rep	stosb
	mov	edi,characters
	mov	esi,special_characters+1
	movzx	ecx,byte [esi-1]
	xor	ebx,ebx
      convert_table:
	lodsb
	mov	bl,al
	mov	byte [edi+ebx],0FFh
	loop	convert_table

	;call	[GetTickCount]
	mov	eax,78
	mov	ebx,buffer
	xor	ecx,ecx
	int	0x80
	mov	eax,dword [buffer]
	mov	ecx,1000
	mul	ecx
	mov	ebx,eax
	mov	eax,dword [buffer+4]
	div	ecx
	add	eax,ebx
	mov	[start_time],eax


	call	preprocessor
	call	parser
	call	assembler
	call	formatter

	movzx	eax,[current_pass]
	inc	al
	call	display_number
	mov	edx,_passes_suffix
	call	display_string

	;call	[GetTickCount]
	mov	eax,78
	mov	ebx,buffer
	xor	ecx,ecx
	int	0x80
	mov	eax,dword [buffer]
	mov	ecx,1000
	mul	ecx
	mov	ebx,eax
	mov	eax,dword [buffer+4]
	div	ecx
	add	eax,ebx
	sub	eax,[start_time]

	xor	edx,edx
	mov	ebx,100
	div	ebx
	or	eax,eax
	jz	display_bytes_count
	xor	edx,edx
	mov	ebx,10
	div	ebx
	push	edx
	call	display_number
	mov	dl,'.'
	call	display_character
	pop	eax
	call	display_number
	mov	edx,_seconds_suffix
	call	display_string
      display_bytes_count:
	mov	eax,[written_size]
	call	display_number
	mov	edx,_bytes_suffix
	call	display_string
	xor	al,al
	jmp	exit_program

information:
	mov	edx,_usage
	call	display_string
	mov	al,1
	jmp	exit_program

include 'system.inc'

include '../version.inc'
include '../errors.inc'
include '../expressi.inc'
include '../preproce.inc'
include '../parser.inc'
include '../assemble.inc'
include '../formats.inc'
include '../tables.inc'

_copyright db 'Copyright (c) 1999-2001, Tomasz Grysztar',0

_logo db 'flat assembler  version ',VERSION,0Dh,0Ah,0
_usage db 'usage: fasm source output',0Dh,0Ah,0

_passes_suffix db ' passes, ',0
_seconds_suffix db ' seconds, ',0
_bytes_suffix db ' bytes.',0Dh,0Ah,0

bss:

align 4

additional_memory dd ?
additional_memory_end dd ?
memory_start dd ?
memory_end dd ?

input_file dd ?
output_file dd ?

source_start dd ?
code_start dd ?
code_size dd ?
real_code_size dd ?

start_time dd ?
written_size dd ?

params rb 100h
characters rb 100h
buffer rb 100h

program_end:

__ENDF__
  die if !close(FW);
  die if !open(FW, "> SOURCE/LINUX/SYSTEM.INC");
  binmode(FW);
  die if !print(FW <<'__ENDF__');
; flat assembler source
; Copyright (c) 1999-2001, Tomasz Grysztar
; All rights reserved.

O_ACCMODE  = 00003
O_RDONLY   = 00000
O_WRONLY   = 00001
O_RDWR	   = 00002
O_CREAT    = 00100
O_EXCL	   = 00200
O_NOCTTY   = 00400
O_TRUNC    = 01000
O_APPEND   = 02000
O_NONBLOCK = 04000

S_ISUID    = 04000
S_ISGID    = 02000
S_ISVTX    = 01000
S_IRUSR    = 00400
S_IWUSR    = 00200
S_IXUSR    = 00100
S_IRGRP    = 00040
S_IWGRP    = 00020
S_IXGRP    = 00010
S_IROTH    = 00004
S_IWOTH    = 00002
S_IXOTH    = 00001

init_memory:
	xor	ebx,ebx
	mov	eax,45
	int	0x80
	mov	[additional_memory],eax
	mov	ebx,buffer
	mov	eax,116
	int	0x80
    allocate_memory:
	mov	ebx,[additional_memory]
	add	ebx,dword [buffer+14h]
	mov	eax,45
	int	0x80
	mov	[memory_end],eax
	sub	eax,[additional_memory]
	jz	not_enough_memory
	shr	eax,3
	add	eax,[additional_memory]
	mov	[additional_memory_end],eax
	mov	[memory_start],eax
	ret
    not_enough_memory:
	shr	dword [buffer+14h],1
	cmp	dword [buffer+14h],4000h
	jb	out_of_memory
	jmp	allocate_memory

exit_program:  ; Input: AL: exit code.
	movzx	ebx,al
	mov	eax,1
	int	0x80

open:  ; Input: EDX: filename.
	push	edx esi edi ebp
	mov	ebx,edx
	mov	eax,5
	mov	ecx,O_RDONLY
	xor	edx,edx
	int	0x80
	pop	ebp edi esi edx
	test	eax,eax
	js	file_error
	mov	ebx,eax
	clc
	ret
    file_error:
	stc
	ret

create:  ; Input: EDX: filename.
	push	edx esi edi ebp
	mov	ebx,edx
	mov	eax,5
	mov	ecx,O_CREAT+O_TRUNC+O_WRONLY
	mov	edx,S_IRUSR+S_IWUSR+S_IRGRP
	int	0x80
	pop	ebp edi esi edx
	test	eax,eax
	js	file_error
	mov	ebx,eax
	clc
	ret

write:  ; Input: EBX: fd; EDX: data pointer; ECX: byte count.
	push	edx esi edi ebp
	mov	eax,4
	xchg	ecx,edx
	int	0x80
	pop	ebp edi esi edx
	test	eax,eax
	js	file_error
	clc
	ret

read:  ; Input: EBX: fd; EDX: data pointer; ECX: byte count.
	push	ecx edx esi edi ebp
	mov	eax,3
	xchg	ecx,edx
	int	0x80
	pop	ebp edi esi edx ecx
	test	eax,eax
	js	file_error
	cmp	eax,ecx
	jne	file_error
	clc
	ret

close:  ; Input: EBX: fd.
	mov	eax,6
	int	0x80
	ret

lseek:  ; Imput: EBX: fd; EDX: offset; AL: whence.
	mov	ecx,edx
	xor	edx,edx
	mov	dl,al
	mov	eax,19
	int	0x80
	clc
	ret

display_string:  ; Input: EDX: string data.
	push	ebx
	mov	edi,edx
	or	ecx,-1
	xor	al,al
	repne	scasb
	neg	ecx
	sub	ecx,2
	mov	eax,4
	mov	ebx,1
	xchg	ecx,edx
	int	0x80
	pop	ebx
	ret

; This is the ABI in fasm 1.30.
;display_string_esi_not:  ; Input: ESI: string data.
;	push	ebx
;	mov	edi,esi
;	mov	edx,esi
;	or	ecx,-1
;	xor	al,al
;	repne	scasb
;	neg	ecx
;	sub	ecx,2
;	mov	eax,4
;	mov	ebx,1
;	xchg	ecx,edx
;	int	0x80
;	pop	ebx
;	ret

display_block:  ; Input: ESI: data; ECX: byte size.
	push	ebx
	mov	eax,4
	mov	ebx,1
	mov	edx,ecx
	mov	ecx,esi
	int	0x80
	pop	ebx
	ret

display_character:  ; Input: DL.
	push	ebx
	push	edx
	mov	eax,4
	mov	ebx,1
	mov	ecx,esp  ; The DL value is at [esp] now.
	mov	edx,ebx
	int	0x80
	pop	edx
	pop	ebx
	ret

display_number:  ; Input: EAX.
	push	ebx
	mov	ecx,1000000000
	xor	edx,edx
	xor	bl,bl
      display_loop:
	div	ecx
	push	edx
	cmp	ecx,1
	je	display_digit
	or	bl,bl
	jnz	display_digit
	or	al,al
	jz	digit_ok
	not	bl
      display_digit:
	mov	dl,al
	add	dl,30h
	push	ebx ecx
	call	display_character
	pop	ecx ebx
      digit_ok:
	mov	eax,ecx
	xor	edx,edx
	mov	ecx,10
	div	ecx
	mov	ecx,eax
	pop	eax
	or	ecx,ecx
	jnz	display_loop
	pop	ebx
	ret

fatal_error:  ; Input: return address: error message.
	mov	edx,error_prefix
	call	display_string
	pop	edx
	call	display_string
	mov	edx,error_suffix
	call	display_string
	mov	al,0FFh
	jmp	exit_program

assembler_error:  ; Input: return address: error message.
	mov	edx,[home_line]
	mov	ebp,[edx]
	call	display_line_number
	mov	edx,[current_line]
	cmp	edx,[home_line]
	je	line_number_ok
	mov	ebp,[edx]
	mov	dl,20h
	call	display_character
	call	display_line_number
      line_number_ok:
	mov	edx,line_number_end
	call	display_string  ; !!
	mov	edx,[home_line]
	add	edx,5
	call	display_string
	mov	edx,cr_lf
	call	display_string
	mov	edx,error_prefix
	call	display_string
	pop	edx
	call	display_string
	mov	edx,error_suffix
	call	display_string
	mov	al,2
	jmp	exit_program
      display_line_number:
	mov	ecx,ebp
	shr	ecx,20
	dec	ecx
	mov	esi,[files_list]
	inc	esi
      get_error_file:
	jecxz	error_file_found
      skip_file_name:
	lods	byte [esi]
	or	al,al
	jnz	skip_file_name
	add	esi,5
	loop	get_error_file
      error_file_found:
	mov	edx,esi
	call	display_string
	mov	edx,line_number_start
	call	display_string
	mov	eax,ebp
	and	eax,0FFFFFh
	call	display_number
	mov	dl,']'
	call	display_character
	ret

error_prefix db 'error: ',0
error_suffix db '.'
cr_lf db 0Dh,0Ah,0

line_number_start db ' [',0
line_number_end db ':',0Dh,0Ah,0

macro dm string { db string,0 }
__ENDF__
  die if !close(FW);
}

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
if (($src_zip_version == 37 and -f($fn3 = "fasm-golden-1.37")) or
    ($src_zip_version == 20 and -f($fn3 = "fasm-golden-1.20"))) {
  print STDERR "info: comparing: fasm1 vs $fn3\n";
  my $s3 = read_file($fn3);
  die "fatal: file content mismatch: fasm1 vs $fn3\n" if $s1 ne $s3;
}

print STDERR "info: bootstrap1 OK: $0\n";

__END__
