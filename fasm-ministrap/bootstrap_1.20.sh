#! /bin/sh --
# by pts@fazekas.hu at Sat Nov 23 00:17:29 CET 2024

if test "$1" != --sh-script; then exec "${0%/*}/busybox" env -i "${0%/*}/busybox" sh "$0" --sh-script "$@" || exit 125; fi
shift
export PATH=/dev/null/missing  # Use only busybox tools.
export LC_ALL=en_US.UTF-8  # For deterministic output.
export TZ=GMT  # For deterministic output.
#mydir="${0%/*}"
busybox="$mydir/busybox"
test "${0%/*}" = "$0" || cd "${0%/*}"
set -ex

test -f ../orig/fasm120.zip
rm -rf SOURCE
unzip -qo ../orig/fasm120.zip SOURCE/ASSEMBLE.INC SOURCE/ERRORS.INC SOURCE/EXPRESSI.INC SOURCE/FORMATS.INC SOURCE/PARSER.INC SOURCE/PREPROCE.INC SOURCE/TABLES.INC SOURCE/VERSION.INC
(
set +x
echo "

; --- fasm.fasm

; flat assembler source
; Copyright (c) 1999-2001, Tomasz Grysztar
; All rights reserved.

	program_base = 0x700000

	org	program_base
	use32

	macro	align value { rb (value-1) - (\$ + value-1) mod value }

file_header:
	db	0x7F,'ELF',1,1,1,3
	rb	file_header+0x10-\$
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

; --- system.inc

; flat assembler source
; Copyright (c) 1999-2001, Tomasz Grysztar
; All rights reserved.

O_ACCMODE  = 00003o
O_RDONLY   = 00000o
O_WRONLY   = 00001o
O_RDWR	   = 00002o
O_CREAT    = 00100o
O_EXCL	   = 00200o
O_NOCTTY   = 00400o
O_TRUNC    = 01000o
O_APPEND   = 02000o
O_NONBLOCK = 04000o

S_ISUID    = 04000o
S_ISGID    = 02000o
S_ISVTX    = 01000o
S_IRUSR    = 00400o
S_IWUSR    = 00200o
S_IXUSR    = 00100o
S_IRGRP    = 00040o
S_IWGRP    = 00020o
S_IXGRP    = 00010o
S_IROTH    = 00004o
S_IWOTH    = 00002o
S_IXOTH    = 00001o

; !! remove when not needed
CREATE_NEW	       =   1
CREATE_ALWAYS	       =   2
OPEN_EXISTING	       =   3
OPEN_ALWAYS	       =   4
TRUNCATE_EXISTING      =   5
GENERIC_READ	       =   80000000h
GENERIC_WRITE	       =   40000000h
STD_INPUT_HANDLE       =   0FFFFFFF6h
STD_OUTPUT_HANDLE      =   0FFFFFFF5h
STD_ERROR_HANDLE       =   0FFFFFFF4h

init_memory:
	xor	ebx,ebx
	mov	eax,45
	int	0x80
	mov	[additional_memory],eax
	;mov	ebx,buffer
	;mov	eax,116
	;int	0x80
	mov dword [buffer+14h],0x280000  ; PATCH Use up to 2.5 MiB of memory.
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

CASE_INSENSITIVEX = 0
nop  ; !!
 if CASE_INSENSITIVEX<>0
 ;if FOO>0
 db 6,'binary',18h,1
 db 4,'code',19h,5
 db 7,'console',1Bh,3
 db 4,'data',19h,6
 db 11,'discardable',19h,25
 db 3,'dll',1Bh,80h
 db 10,'executable',19h,29
 db 6,'export',1Ah,0
 db 6,'fixups',1Ah,5
 db 3,'gui',1Bh,2
 db 4,'i386',1Bh,43h
 db 4,'i486',1Bh,44h
 db 4,'i586',1Bh,45h
 db 6,'import',1Ah,1
 db 2,'mz',18h,2
 db 6,'native',1Bh,1
 db 2,'pe',18h,3
 db 8,'readable',19h,30
 db 8,'resource',1Ah,2
 db 9,'shareable',19h,28
 db 5,'udata',19h,7
 db 9,'writeable',19h,31
 else
 db 3,'DLL',1Bh,80h
 db 3,'GUI',1Bh,2
 db 2,'MZ',18h,2
 db 2,'PE',18h,3
 db 6,'binary',18h,1
 db 4,'code',19h,5
 db 7,'console',1Bh,3
 db 4,'data',19h,6
 db 11,'discardable',19h,25
 db 10,'executable',19h,29
 db 6,'export',1Ah,0
 db 6,'fixups',1Ah,5
 db 4,'i386',1Bh,43h
 db 4,'i486',1Bh,44h
 db 4,'i586',1Bh,45h
 db 6,'import',1Ah,1
 db 6,'native',1Bh,1
 db 8,'readable',19h,30
 db 8,'resource',1Ah,2
 db 9,'shareable',19h,28
 db 5,'udata',19h,7
 db 9,'writeable',19h,31
 end if

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

; --- fasm *.inc source files"
for f in SOURCE/VERSION.INC SOURCE/ERRORS.INC SOURCE/EXPRESSI.INC SOURCE/PREPROCE.INC SOURCE/PARSER.INC SOURCE/ASSEMBLE.INC SOURCE/FORMATS.INC SOURCE/TABLES.INC; do
  echo; echo; echo "; --- fasm source file: $f"; echo
  set -x
  cat "$f"
  set +x
done
echo "

; --- rest of fasm.fasm

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
") >fasm.fasm
./miniperl -pi -e 's@\r?\n@\r\n@' fasm.fasm  # fasm-1.20 segfaults unless source line ending is CRLF (\r\n).

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
    my %vars; $vars{VERSION} = ""; $vars{program_base} = "";
    while (<STDIN>) {  # Convert from fasm to NASM syntax.
      y@"@\x27@;
      if (m@;@) {  # Remove single-line comment.
        if (m@\x27@) { s@^((?:[^\x27;]+|\x27[^\x27]*\x27)*);.*@$1@ }  # Complicated case.
        else { s@\s*;.*@@ }  # Simple case.
      }
      s@^\s+@@; s@\s+\Z(?!\n)@@g;
      next if !m@\S@;
      s@^(\S+)\s+@$1 @;
      if (!m@\x27[^\x27][^\x27]@) {
        s@(\w+)\s+shl\s+(\w+)@($1<<$2)@g; s@([ ,])not\b@$1~@g; s@\s+[?]$@ 0@; s@\s+db\s+[?]\s*,\s*[?]$@ db 0, 0@;
        s@$varname_re@ exists($sizes{$2}) ? "$1$longsizes{$sizes{$2}} [$2" : "${1}[$2" @oe if m@\[@ and !m@\b(byte|word|dword)\b@;  # Replace e.g. `[input_file]` with `dword [input_file]`.
        s@\b(0[0-7]*)o\b@${1}q@g;  # For NASM 0.97.
      }
      s@,VERSION,@,$vars{VERSION},@g;
      if (m@^(VERSION|program_base)\s+(?:equ|=)\s+(.*)$@) { $vars{$1} = $2; next if $1 eq "VERSION" }  # NASM does not support string after equ.
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
      elsif (s@^((?:rep[a-z]*\s+)?)(lod|sto|mov|cmp)s\s+(byte|word|dword)\b.*@ $1.$2."s".substr($3,0,1) @e) {}
      elsif (s@^jmp near\s+(e[abcd]x|e[sd]i|e[sb]p)$@jmp $1@) {}
      elsif (s@^(j(?!mp |e?cxz )[a-z]+) (?!near\s+|short\s+)@$1 near @) {}  # Force conditional jumps to be `neaar`, to avoid NASM error: short jump is out of range, without `nasm -O1`.
      elsif (s@^setalc$@salc@) {}
      elsif (s@^use32$@bits 32@) {}  # For NASM 0.97.
      print "$_\n";
    }' <fasm.fasm >fasm.nasm
rm -f fasm0 fasm1
# Works with Perl 5.004.04 (1997-10-15) or later.
# Works with NASM 0.97 (1997-12-06), 0.98.39 (2005-01-15), 2.10.09 (2013-07-23), 2.13.02 (2017-12-03).
./nasm-0.98.39 -o fasm0 fasm.nasm  # Works. 0.98.39.
#./nasm-2.13.02 -o fasm0 fasm.nasm  # Works.
#./nasm-2.10.09 -o fasm0 fasm.nasm  # Works.
#./nasm-0.97.uclibc -o fasm0 fasm.nasm  # Works.
chmod +x fasm0
./fasm0 fasm.fasm fasm1
chmod +x fasm1
./fasm1 ||:
cmp ../fasm-golden-1.20 fasm1

: "$0" OK.
