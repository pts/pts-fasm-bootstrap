/* This source file works with the SVR3, SunOS 4.0.1 and SVR4 asseblers. */
.file "fbsasms.s"
/*  #  by pts@fazekas.hu at Thu Mar 21 07:44:40 CET 2024 */
/*  # */
/*  #  Minimum NASM version required to compile: NASM 0.95 (1997-07-27). The */
/*  #  nasm.exe bundled with NASM 0.95 (nasm095s.zip) precompiled for DOS 8086 */
/*  #  also works. */
/*  # */
/*  #  This is a subset of the source code of fasm 1.30 (with some CPU instructions, */
/*  #  `format MZ' and `format PE' removed), ported to NASM syntax, for Linux i386 */
/*  #  only. It's useful for bootstrapping fasm. */
/*  # */
/*  #  Compile with: nasm-0.98.39 -O0 -w+orphan-labels -f bin -o fbsasm fbsasm.asm && chmod +x fbsasm  # Fast. */
/*  #  Compile with: nasm-0.98.39 -O1 -Dnear_o0= -w+orphan-labels -f bin -o fbsasm fbsasm.asm && chmod +x fbsasm */
/*  #  Compile with: nasm-0.98.39 -O999999999 -Dnear_o0= -w+orphan-labels -f bin -o fbsasm fbsasm.asm && chmod +x fbsasm */
/*  # */
/*  #  Compile the GNU as(1) version (fbsasm.s) with: as --32 -march=i386 -o fbsasm.o fbsasm && ld -m elf_i386 -N -s -o fbsasm fbsasm.o */
/*  #  Compile the GNU as(1) version (fbsasm.s) with earlier versions of GNU as(1) with: as -o fbsasm.o fbsasm && ld -m elf_i386 -N -s -o fbsasm fbsasm.o */
/*  # */
/*  #  Lines in fbsasm.nasm and fbsasm.fasm correspond to each other. */
/*  # */
/* This file is autognerated by nasm2as.pl */
/* */
/* */
/* */
/* */
/* */
/* */
/*  #  flat assembler 0.37 source, fasm.asm */
/*  #  Copyright (c) 1999-2002, Tomasz Grysztar */
/*  #  All rights reserved. */
/* */
/*%define program_base 0x700000  #  NASM 0.95 doesn't support `program_base equ 0x700000' with `org program_base'. */
/* */
/*%ifndef near_o0 */
/*%define near_o0 near  #  For `nasm -O0'. */
/*%endif */
/* */
/*org	program_base */
/*bits 32  #  NASM 0.97 supports this but ignores `use32'. */
/*  # cpu 386  ; True but NASM 0.97 doesn't support it. */
/* */
/*  # 	macro	align value { rb (value-1) - ($ + value-1) mod value } */
/* */
/*file_header: */
/*db	0x7F,'ELF',1,1,1 */
/*times	file_header+0x10-$ db 0 */
/*dw	2,3 */
/*dd	1,start */
/*dd	program_header-file_header,0,0 */
/*dw	program_header-file_header,0x20,1,0x28,0,0 */
/* */
/*program_header: */
/*dd	1,0,program_base,0 */
/*dd	prebss-file_header,program_end-bss+prebss-file_header,7,0x1000 */
/* */
.globl _start
_start:
  /* Program entry point. */
start:

	mov $_logo, %esi
	call display_string

	pop %eax
	cmp $3, %eax
	jne information
	pop %eax
	popl input_file
	popl output_file

	call init_memory

	mov $characters, %edi
	mov $0x100, %ecx
	xorb %al, %al
      make_characters_table:
	stosb
	incb %al
	loop make_characters_table
	mov $characters+0x61, %esi
	mov $characters+0x41, %edi
	mov $26, %ecx
	rep
	movsb
	mov $characters, %edi
	mov $symbol_characters+1, %esi
	movzbl -1(%esi), %ecx
	xor %ebx, %ebx
      convert_table:
	lodsb
	movb %al, %bl
	movb $0, (%edi,%ebx,1)
	loop convert_table

	push %eax
	push %eax  /*  alloca(8) for the gettimeofday buffer. */
	mov $78, %eax  /*  SYS_gettimeofday. */
	mov %esp, %ebx
	xor %ecx, %ecx
	int $0x80
	mov (%esp), %eax
	mov $1000, %ecx
	mul %ecx
	mov %eax, %ebx
	mov 4(%esp), %eax
	div %ecx
	pop %ecx
	pop %ecx  /*  Free the gettimeofday buffer. */
	add %ebx, %eax
	movl %eax, start_time

	call preprocessor
	call parser
	call assembler

	movzbl current_pass, %eax
	incb %al
	call display_number
	mov $_passes_suffix, %esi
	call display_string
	push %eax
	push %eax  /*  alloca(8) for the gettimeofday buffer. */
	mov $78, %eax  /*  SYS_gettimeofday. */
	mov %esp, %ebx
	xor %ecx, %ecx
	int $0x80
	mov (%esp), %eax
	mov $1000, %ecx
	mul %ecx
	mov %eax, %ebx
	mov 4(%esp), %eax
	div %ecx
	pop %ecx
	pop %ecx  /*  Free the gettimeofday buffer. */
	add %ebx, %eax
	sub start_time, %eax
	jnc time_ok
	add $3600000, %eax
      time_ok:
	xor %edx, %edx
	mov $100, %ebx
	div %ebx
	or %eax, %eax
	jz display_bytes_count
	xor %edx, %edx
	mov $10, %ebx
	div %ebx
	push %edx
	call display_number
	movb $0x2e, %dl
	call display_character
	pop %eax
	call display_number
	mov $_seconds_suffix, %esi
	call display_string
      display_bytes_count:
	mov written_size, %eax
	call display_number
	mov $_bytes_suffix, %esi
	call display_string
	xorb %al, %al
	jmp exit_program

information:
	mov $_usage, %esi
	call display_string
	movb $1, %al
	jmp exit_program

/* %include 'system.inc' */

/*  flat assembler 0.37 source, system.inc */
/*  Copyright (c) 1999-2002, Tomasz Grysztar */
/*  All rights reserved. */

O_ACCMODE = 03
O_RDONLY = 00
O_WRONLY = 01
O_RDWR = 02
O_CREAT = 0100
O_EXCL = 0200
O_NOCTTY = 0400
O_TRUNC = 01000
O_APPEND = 02000
O_NONBLOCK = 04000

S_ISUID = 04000
S_ISGID = 02000
S_ISVTX = 01000
S_IRUSR = 0400
S_IWUSR = 0200
S_IXUSR = 0100
S_IRGRP = 040
S_IWGRP = 020
S_IXGRP = 010
S_IROTH = 04
S_IWOTH = 02
S_IXOTH = 01

init_memory:
	xor %ebx, %ebx
	mov $45, %eax  /*  SYS_brk. */
	int $0x80
	movl %eax, additional_memory
/* mov	ebx,syscall_buffer */
/* mov	eax,116  ; SYS_sysinfo. We are interested only the sysinfo.freeram field ([syscall_buffer+14h]), but on modern Linux it's not bytes anymore (see mem_unit in sysinfo(2)), so it's meaningless below. */
/* int	0x80 */
/* mov dword [available_memory],0x100000  ; Hardcode allocating maximum 1 MiB. 1 MiB enough, but 0.75 MiB is not enough to compile fasm 1.30. */
	movl $0x280000, available_memory  /*  Hardcode allocating maximum 2.5 MiB. 1 MiB enough, but 0.75 MiB is not enough to compile fasm 1.30. 2.5 MiB is enough to compile fasm 1.73.32. */
    allocate_memory:
	mov additional_memory, %ebx
	add available_memory, %ebx
	mov $45, %eax  /*  SYS_brk. */
	int $0x80
	movl %eax, memory_end
	sub additional_memory, %eax
	jz not_enough_memory
	shr $3, %eax
	add additional_memory, %eax
	movl %eax, additional_memory_end
	movl %eax, memory_start
	ret
    not_enough_memory:
	shrl available_memory
	cmpl $0x4000, available_memory
	jb out_of_memory
	jmp allocate_memory

exit_program:
	movzbl %al, %ebx
	mov $1, %eax  /*  SYS_exit. */
	int $0x80

open:
	push %edx
	push %esi
	push %edi
	push %ebp
	mov %edx, %ebx
	mov $5, %eax  /*  SYS_open. */
	mov $O_RDONLY, %ecx
	xor %edx, %edx
	int $0x80
	pop %ebp
	pop %edi
	pop %esi
	pop %edx
	test %eax, %eax
	js file_error
	mov %eax, %ebx
	clc
	ret
    file_error:
	stc
	ret
create:
	push %edx
	push %esi
	push %edi
	push %ebp
	mov %edx, %ebx
	mov $5, %eax  /*  SYS_open. */
	mov $O_CREAT+O_TRUNC+O_WRONLY, %ecx
	mov $S_IRUSR+S_IWUSR+S_IRGRP, %edx
	int $0x80
	pop %ebp
	pop %edi
	pop %esi
	pop %edx
	test %eax, %eax
	js file_error
	mov %eax, %ebx
	clc
	ret
close:
	mov $6, %eax  /*  SYS_close. */
	int $0x80
	ret
read:
	push %ecx
	push %edx
	push %esi
	push %edi
	push %ebp
	mov $3, %eax  /*  SYS_read. */
	xchg %edx, %ecx
	int $0x80
	pop %ebp
	pop %edi
	pop %esi
	pop %edx
	pop %ecx
	test %eax, %eax
	js file_error
	cmp %ecx, %eax
	jne file_error
	clc
	ret
write:
	push %edx
	push %esi
	push %edi
	push %ebp
	mov $4, %eax  /*  SYS_write. */
	xchg %edx, %ecx
	int $0x80
	pop %ebp
	pop %edi
	pop %esi
	pop %edx
	test %eax, %eax
	js file_error
	clc
	ret
lseek:
	mov %edx, %ecx
	xor %edx, %edx
	movb %al, %dl
	mov $19, %eax  /*  SYS_lseek. */
	int $0x80
	clc
	ret

display_string:
	push %ebx
	mov %esi, %edi
	mov %esi, %edx
	or $-1, %ecx
	xorb %al, %al
	repnz
	scasb
	neg %ecx
	sub $2, %ecx
	mov $4, %eax  /*  SYS_write. */
	mov $1, %ebx
	xchg %edx, %ecx
	int $0x80
	pop %ebx
	ret
display_block:
	push %ebx
	mov $4, %eax  /*  SYS_write. */
	mov $1, %ebx
	mov %ecx, %edx
	mov %esi, %ecx
	int $0x80
	pop %ebx
	ret
display_character:
	push %ebx
	movb %dl, character
	mov $4, %eax  /*  SYS_write. */
	mov $1, %ebx
	mov $character, %ecx
	mov %ebx, %edx
	int $0x80
	pop %ebx
	ret
display_number:
	push %ebx
	mov $1000000000, %ecx
	xor %edx, %edx
	xorb %bl, %bl
      display_loop:
	div %ecx
	push %edx
	cmp $1, %ecx
	je display_digit
	orb %bl, %bl
	jnz display_digit
	orb %al, %al
	jz digit_ok
	notb %bl
      display_digit:
	movb %al, %dl
	addb $0x30, %dl
	push %ebx
	push %ecx
	call display_character
	pop %ecx
	pop %ebx
      digit_ok:
	mov %ecx, %eax
	xor %edx, %edx
	mov $10, %ecx
	div %ecx
	mov %eax, %ecx
	pop %eax
	or %ecx, %ecx
	jnz display_loop
	pop %ebx
	ret

fatal_error:
	mov $error_prefix, %esi
	call display_string
	pop %esi
	call display_string
	mov $error_suffix, %esi
	call display_string
	movb $0xff, %al
	jmp exit_program
assembler_error:
	call flush_display_buffer
	mov current_line, %ebx
      find_error_home:
	testb $0x80, 7(%ebx)
	jz error_home_ok
	mov 8(%ebx), %ebx
	jmp find_error_home
      error_home_ok:
	mov (%ebx), %esi
	call display_string
	mov $line_number_start, %esi
	call display_string
	mov 4(%ebx), %eax
	call display_number
	movb $0x5d, %dl
	call display_character
	cmp current_line, %ebx
	je line_number_ok
	movb $0x20, %dl
	call display_character
	mov current_line, %esi
	mov (%esi), %esi
	movzbl (%esi), %ecx
	inc %esi
	call display_block
	mov $line_number_start, %esi
	call display_string
	mov current_line, %esi
	mov 4(%esi), %eax
	and $0x7fffffff, %eax
	call display_number
	movb $0x5d, %dl
	call display_character
      line_number_ok:
	mov $line_data_start, %esi
	call display_string
	mov %ebx, %esi
	mov (%esi), %edx
	call open
	movb $2, %al
	xor %edx, %edx
	call lseek
	mov 8(%esi), %edx
	sub %edx, %eax
	push %eax
	xorb %al, %al
	call lseek
	mov (%esp), %ecx
	mov memory_start, %edx
	call read
	call close
	pop %ecx
	mov memory_start, %esi
      get_line_data:
	movb (%esi), %al
	cmpb $0xa, %al
	je display_line_data
	cmpb $0xd, %al
	je display_line_data
	cmpb $0x1a, %al
	je display_line_data
	orb %al, %al
	jz display_line_data
	inc %esi
	loop get_line_data
      display_line_data:
	mov %esi, %ecx
	mov memory_start, %esi
	sub %esi, %ecx
	call display_block
	mov $lf, %esi
	call display_string
	mov $error_prefix, %esi
	call display_string
	pop %esi
	call display_string
	mov $error_suffix, %esi
	call display_string
	jmp exit_program

character:
.byte 0, 0

error_prefix:
.string "error: "
error_suffix:
.byte 0x2e
lf:
.byte 0xa, 0
line_number_start:
.string " ["
line_data_start:
.byte 0x3a
.byte 0xa, 0

/*%macro dm 1 */
/*db %1, 0 */
/*%endm */

/* %include '../version.inc' */

/*  flat assembler  version 1.30 */
/*  Copyright (c) 1999-2002, Tomasz Grysztar */
/*  All rights reserved. */
/* */
/*  This programs is free for commercial and non-commercial use as long as */
/*  the following conditions are aheared to. */
/* */
/*  Redistribution and use in source and binary forms, with or without */
/*  modification, are permitted provided that the following conditions are */
/*  met: */
/* */
/*  1. Redistributions of source code must retain the above copyright notice, */
/*     this list of conditions and the following disclaimer. */
/*  2. Redistributions in binary form must reproduce the above copyright */
/*     notice, this list of conditions and the following disclaimer in the */
/*     documentation and/or other materials provided with the distribution. */
/* */
/*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS */
/*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED */
/*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A */
/*  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR */
/*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, */
/*  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, */
/*  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR */
/*  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF */
/*  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING */
/*  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS */
/*  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. */
/* */
/*  The licence and distribution terms for any publically available */
/*  version or derivative of this code cannot be changed. i.e. this code */
/*  cannot simply be copied and put under another distribution licence */
/*  (including the GNU Public Licence). */

/*%define VERSION_STRING '1.30-bootstrap' */



VERSION_MAJOR = 1
VERSION_MINOR = 30

/* %include '../errors.inc' */

/*  flat assembler source */
/*  Copyright (c) 1999-2001, Tomasz Grysztar */
/*  All rights reserved. */

out_of_memory:
	call fatal_error
	.string "out of memory"
main_file_not_found:
	call fatal_error
	.string "source file not found"
write_failed:
	call fatal_error
	.string "write failed"
code_cannot_be_generated:
	call fatal_error
	.string "code cannot be generated"
unexpected_end_of_file:
	call fatal_error
	.string "unexpected end of file"
file_not_found:
	call assembler_error
	.string "file not found"
error_reading_file:
	call assembler_error
	.string "error reading file"
invalid_macro_arguments:
	call assembler_error
	.string "invalid macro arguments"
unexpected_characters:
	call assembler_error
	.string "unexpected characters"
invalid_argument:
	call assembler_error
	.string "invalid argument"
illegal_instruction:
	call assembler_error
	.string "illegal instruction"
unexpected_instruction:
	call assembler_error
	.string "unexpected instruction"
invalid_operand:
	call assembler_error
	.string "invalid operand"
invalid_operand_size:
	call assembler_error
	.string "invalid size of operand"
operand_size_not_specified:
	call assembler_error
	.string "operand size not specified"
operand_sizes_do_not_match:
	call assembler_error
	.string "operand sizes do not match"
invalid_address_size:
	call assembler_error
	.string "invalid size of address value"
address_sizes_do_not_agree:
	call assembler_error
	.string "address sizes do not agree"
invalid_expression:
	call assembler_error
	.string "invalid expression"
invalid_address:
	call assembler_error
	.string "invalid address"
invalid_value:
	call assembler_error
	.string "invalid value"
value_out_of_range:
	call assembler_error
	.string "value out of range"
invalid_use_of_symbol:
	call assembler_error
	.string "invalid use of symbol"
relative_jump_out_of_range:
	call assembler_error
	.string "relative jump out of range"
extra_characters_on_line:
	call assembler_error
	.string "extra characters on line"
name_too_long:
	call assembler_error
	.string "name too long"
invalid_name:
	call assembler_error
	.string "invalid name"
reserved_word_used_as_symbol:
	call assembler_error
	.string "reserved word used as symbol"
symbol_already_defined:
	call assembler_error
	.string "symbol already defined"
missing_end_quote:
	call assembler_error
	.string "missing end quote"

/* %include '../expressi.inc' */

/*  flat assembler source */
/*  Copyright (c) 1999-2001, Tomasz Grysztar */
/*  All rights reserved. */

convert_expression:
	push %ebp
	mov %esp, %ebp
	push %edi
	mov $operators, %edi
	call get_operator
	pop %edi
	orb %al, %al
	jz expression_loop
	push %ebp
	cmpb $0x80, %al
	je init_positive
	cmpb $0x81, %al
	je init_negative
	jmp invalid_expression
      init_positive:
	xorb %al, %al
	jmp expression_number
      init_negative:
	movb $0xd1, %al
	jmp expression_number
      expression_loop:
	push %ebp
	push %edi
	mov $single_operand_operators, %edi
	call get_operator
	pop %edi
      expression_number:
	push %eax
	cmpb $0, (%esi)
	je invalid_expression
	call convert_number
	pop %eax
	orb %al, %al
	jz expression_operator
	stosb
      expression_operator:
	push %edi
	mov $operators, %edi
	call get_operator
	pop %edi
	pop %ebp
	orb %al, %al
	jz expression_end
      operators_loop:
	cmp %ebp, %esp
	je push_operator
	movb %al, %bl
	andb $0xf0, %bl
	movb (%esp), %bh
	andb $0xf0, %bh
	cmpb %bh, %bl
	ja push_operator
	popw %bx
	movb %bl, (%edi)
	inc %edi
	jmp operators_loop
      push_operator:
	pushw %ax
	jmp expression_loop
      expression_end:
	cmp %ebp, %esp
	je expression_converted
	popw %ax
	stosb
	jmp expression_end
      expression_converted:
	pop %ebp
	ret

convert_number:
	cmpb $0x28, (%esi)
	je expression_value
	inc %edi
	call get_number
	jc symbol_value
	or %ebp, %ebp
	jz valid_number
	movb $0xf, -1(%edi)
	ret
      valid_number:
	cmpl $0, 4(%edi)
	jne qword_number
	cmpw $0, 2(%edi)
	jne dword_number
	cmpb $0, 1(%edi)
	jne word_number
      byte_number:
	movb $1, -1(%edi)
	inc %edi
	ret
      qword_number:
	movb $8, -1(%edi)
	scasl
	scasl
	ret
      dword_number:
	movb $4, -1(%edi)
	scasl
	ret
      word_number:
	movb $2, -1(%edi)
	scasw
	ret
      expression_value:
	inc %esi
	call convert_expression
	lodsb
	cmpb $0x29, %al
	jne invalid_expression
	ret
      symbol_value:
	lodsb
	cmpb $0x1a, %al
	jne invalid_value
	lodsb
	movzbl %al, %ecx
	push %ecx
	push %esi
	push %edi
	mov $address_registers, %edi
	call get_symbol
	jnc register_value
	mov $symbols, %edi
	call get_symbol
	jnc invalid_value
	pop %edi
	pop %esi
	pop %ecx
	call get_label_id
	movb $0x11, -1(%edi)
	stosl
	ret
      register_value:
	pop %edi
	add $8, %esp
	movb $0x10, -1(%edi)
	movb %ah, %al
	stosb
	ret

get_number:
	xor %ebp, %ebp
	lodsb
	cmpb $0x22, %al
	je get_text_number
	cmpb $0x1a, %al
	jne not_number
	lodsb
	movzbl %al, %ecx
	movl %esi, number_start
	movb (%esi), %al
	subb $0x30, %al
	jb invalid_number
	cmpb $9, %al
	ja invalid_number
	mov %esi, %eax
	add %ecx, %esi
	push %esi
	sub $2, %esi
	movl $0, (%edi)
	movl $0, 4(%edi)
	inc %esi
	cmpw $[0x30|[0x78<<8]], (%eax)  /*  Same multibyte character constant order in fasm and NASM. */
	je get_hex_number
	dec %esi
	cmpb $0x68, 1(%esi)
	je get_hex_number
	cmpb $0x6f, 1(%esi)
	je get_oct_number
	cmpb $0x62, 1(%esi)
	je get_bin_number
	cmpb $0x64, 1(%esi)
	je get_dec_number
	inc %esi
	cmpb $0x30, (%eax)
	je get_oct_number
      get_dec_number:
	xor %edx, %edx
	mov $1, %ebx
      get_dec_digit:
	cmp number_start, %esi
	jb number_ok
	movzbl (%esi), %eax
	subb $0x30, %al
	jc bad_number
	cmpb $9, %al
	ja bad_number
	mov %eax, %ecx
	jcxz next_dec_digit
      convert_dec_digit:
	addl %ebx, (%edi)
	adcl %edx, 4(%edi)
	loop convert_dec_digit
      next_dec_digit:
	dec %esi
	mov %edx, %ecx
	mov $10, %eax
	mul %ebx
	mov %eax, %ebx
	imul $10, %ecx, %ecx
	jo dec_out_of_range
	add %ecx, %edx
	jnc get_dec_digit
      dec_out_of_range:
	or $1, %ebp
	jmp get_dec_digit
      bad_number:
	pop %eax
      invalid_number:
	mov number_start, %esi
	dec %esi
      not_number:
	dec %esi
	stc
	ret
      get_bin_number:
	xorb %bl, %bl
      get_bin_digit:
	cmp number_start, %esi
	jb number_ok
	movzbl (%esi), %eax
	subb $0x30, %al
	jc bad_number
	cmpb $1, %al
	ja bad_number
	xor %edx, %edx
	movb %bl, %cl
	dec %esi
	cmpb $64, %bl
	je bin_out_of_range
	incb %bl
	cmpb $32, %cl
	jae bin_digit_high
	shl %cl, %eax
	orl %eax, (%edi)
	jmp get_bin_digit
      bin_digit_high:
	subb $32, %cl
	shl %cl, %eax
	orl %eax, 4(%edi)
	jmp get_bin_digit
      bin_out_of_range:
	or $1, %ebp
	jmp get_bin_digit
      get_hex_number:
	xorb %bl, %bl
      get_hex_digit:
	cmp number_start, %esi
	jb number_ok
	movzbl (%esi), %eax
	cmpb $0x78, %al
	je hex_number_ok
	subb $0x30, %al
	jc bad_number
	cmpb $9, %al
	jbe hex_digit_ok
	subb $7, %al
	cmpb $15, %al
	jbe hex_digit_ok
	subb $0x20, %al
	jc bad_number
	cmpb $15, %al
	ja bad_number
      hex_digit_ok:
	xor %edx, %edx
	movb %bl, %cl
	dec %esi
	cmpb $64, %bl
	je hex_out_of_range
	addb $4, %bl
	cmpb $32, %cl
	jae hex_digit_high
	shl %cl, %eax
	orl %eax, (%edi)
	jmp get_hex_digit
      hex_digit_high:
	subb $32, %cl
	shl %cl, %eax
	orl %eax, 4(%edi)
	jmp get_hex_digit
      hex_out_of_range:
	or $1, %ebp
	jmp get_hex_digit
      get_oct_number:
	xorb %bl, %bl
      get_oct_digit:
	cmp number_start, %esi
	jb number_ok
	movzbl (%esi), %eax
	subb $0x30, %al
	jc bad_number
	cmpb $7, %al
	ja bad_number
      oct_digit_ok:
	xor %edx, %edx
	movb %bl, %cl
	dec %esi
	cmpb $64, %bl
	jae oct_out_of_range
	addb $3, %bl
	cmpb $32, %cl
	jae oct_digit_high
	shl %cl, %eax
	orl %eax, (%edi)
	jmp get_oct_digit
      oct_digit_high:
	subb $32, %cl
	shl %cl, %eax
	orl %eax, 4(%edi)
	jmp get_oct_digit
      oct_out_of_range:
	or $1, %ebp
	jmp get_oct_digit
      hex_number_ok:
	dec %esi
	cmp number_start, %esi
	jne bad_number
      number_ok:
	pop %esi
      number_done:
	clc
	ret
      get_text_number:
	lodsl
	mov %eax, %edx
	xorb %bl, %bl
	movl $0, (%edi)
	movl $0, 4(%edi)
      get_text_character:
	sub $1, %edx
	jc number_done
	movzbl (%esi), %eax
	inc %esi
	movb %bl, %cl
	cmpb $64, %bl
	je text_out_of_range
	addb $8, %bl
	cmpb $32, %cl
	jae text_character_high
	shl %cl, %eax
	orl %eax, (%edi)
	jmp get_text_character
      text_character_high:
	subb $32, %cl
	shl %cl, %eax
	orl %eax, 4(%edi)
	jmp get_text_character
      text_out_of_range:
	or $1, %ebp
	jmp get_text_character

calculate_expression:
	lodsb
	orb %al, %al
	jz get_string_value
	cmpb $0x2e, %al
	je convert_fp
	cmpb $1, %al
	je get_byte_number
	cmpb $2, %al
	je get_word_number
	cmpb $4, %al
	je get_dword_number
	cmpb $8, %al
	je get_qword_number
	cmpb $0xf, %al
	je value_out_of_range
	cmpb $0x10, %al
	je get_register
	cmpb $0x11, %al
	je get_label
	cmpb $0x29, %al
	je expression_calculated
	cmpb $0x5d, %al
	je expression_calculated
	sub $0x10, %edi
	mov %edi, %ebx
	sub $0x10, %ebx
	movw 8(%ebx), %dx
	orw 8(%edi), %dx
	cmpb $0xe0, %al
	je calculate_rva
	cmpb $0xd0, %al
	je calculate_not
	cmpb $0xd1, %al
	je calculate_neg
	cmpb $0x80, %al
	je calculate_add
	cmpb $0x81, %al
	je calculate_sub
	movb 12(%ebx), %ah
	orb 12(%edi), %ah
	jnz invalid_use_of_symbol
	cmpb $0x90, %al
	je calculate_mul
	cmpb $0x91, %al
	je calculate_div
	orw %dx, %dx
	jnz invalid_expression
	cmpb $0xa0, %al
	je calculate_mod
	cmpb $0xb0, %al
	je calculate_and
	cmpb $0xb1, %al
	je calculate_or
	cmpb $0xb2, %al
	je calculate_xor
	cmpb $0xc0, %al
	je calculate_shl
	cmpb $0xc1, %al
	je calculate_shr
	jmp invalid_expression
      expression_calculated:
	sub $0x10, %edi
	ret
      get_byte_number:
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	xor %eax, %eax
	lodsb
	stosl
	xorb %al, %al
	stosl
	scasl
	scasl
	jmp calculate_expression
      get_word_number:
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	xor %eax, %eax
	lodsw
	stosl
	xorw %ax, %ax
	stosl
	scasl
	scasl
	jmp calculate_expression
      get_dword_number:
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	movsl
	xor %eax, %eax
	stosl
	scasl
	scasl
	jmp calculate_expression
      get_qword_number:
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	movsl
	movsl
	scasl
	scasl
	jmp calculate_expression
      get_register:
	movb $0, 9(%edi)
	movb $0, 12(%edi)
	lodsb
	movb %al, 8(%edi)
	movb $1, 10(%edi)
	xor %eax, %eax
	stosl
	stosl
	scasl
	scasl
	jmp calculate_expression
      get_label:
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	lodsl
	or %eax, %eax
	jz current_offset_label
	cmp $1, %eax
	je counter_label
	mov %eax, %ebx
	testb $1, 8(%ebx)
	jz label_undefined
	testb $4, 8(%ebx)
	jz label_defined
	movb current_pass, %al
	cmpb 9(%ebx), %al
	jne label_undefined
      label_defined:
	movb 11(%ebx), %al
	cmpb $0, next_pass_needed
	je label_type_ok
	cmpb $0, current_pass
	jne label_type_ok
	xorb %al, %al
      label_type_ok:
	movb %al, 12(%edi)
	mov 12(%ebx), %eax
	mov %eax, 8(%edi)
	mov (%ebx), %eax
	stosl
	mov 4(%ebx), %eax
	stosl
	scasl
	scasl
	movb 10(%ebx), %al
	orb %al, %al
	jz calculate_expression
	cmpb $2, forced_size
	je calculate_expression
	cmpb $1, forced_size
	jne check_size
	cmpb $0, operand_size
	jne calculate_expression
	.byte 0xa2  /*WORKAROUNDL movb %al, operand_size */
	.long operand_size
	jmp calculate_expression
      check_size:
	xchgb %al, operand_size
	orb %al, %al
	jz calculate_expression
	cmpb operand_size, %al
	jne operand_sizes_do_not_match
	jmp calculate_expression
      current_offset_label:
	cmpb $0, reloc_labels
	je get_current_offset
	movb $2, 12(%edi)
      get_current_offset:
	mov current_offset, %eax
	sub org_start, %eax
	cltd
	stosl
	mov %edx, %eax
	stosl
	mov org_sib, %eax
	stosl
	scasl
	jmp calculate_expression
      counter_label:
	mov counter, %eax
	stosl
	xor %eax, %eax
	stosl
	scasl
	scasl
	jmp calculate_expression
      label_undefined:
	cmpb $0, current_pass
	jne invalid_value
	orb $-1, next_pass_needed
	movb $0, 12(%edi)
	xor %eax, %eax
	stosl
	stosl
	scasl
	scasl
	jmp calculate_expression
      calculate_add:
	cmpb $0, next_pass_needed
	jne add_values
	cmpb $0, 12(%edi)
	je add_values
	cmpb $0, 12(%ebx)
	jne invalid_use_of_symbol
      add_values:
	movb 12(%edi), %al
	orb %al, 12(%ebx)
	mov (%edi), %eax
	add %eax, (%ebx)
	mov 4(%edi), %eax
	adc %eax, 4(%ebx)
	orw %dx, %dx
	jz calculate_expression
	push %esi
	mov %ebx, %esi
	lea 10(%edi), %ebx
	movb 8(%edi), %cl
	call add_register
	lea 11(%edi), %ebx
	movb 9(%edi), %cl
	call add_register
	pop %esi
	jmp calculate_expression
      add_register:
	orb %cl, %cl
	jz add_register_done
      add_register_start:
	cmpb %cl, 8(%esi)
	jne add_in_second_slot
	movb (%ebx), %al
	addb %al, 10(%esi)
	jnz add_register_done
	movb $0, 8(%esi)
	ret
      add_in_second_slot:
	cmpb %cl, 9(%esi)
	jne create_in_first_slot
	movb (%ebx), %al
	addb %al, 11(%esi)
	jnz add_register_done
	movb $0, 9(%esi)
	ret
      create_in_first_slot:
	cmpb $0, 8(%esi)
	jne create_in_second_slot
	movb %cl, 8(%esi)
	movb (%ebx), %al
	movb %al, 10(%esi)
	ret
      create_in_second_slot:
	cmpb $0, 9(%esi)
	jne invalid_expression
	movb %cl, 9(%esi)
	movb (%ebx), %al
	movb %al, 11(%esi)
      add_register_done:
	ret
      calculate_sub:
	xorb %ah, %ah
	cmpb $0, next_pass_needed
	jne sub_values
	movb 12(%ebx), %ah
	movb 12(%edi), %al
	orb %al, %al
	jz sub_values
	cmpb %ah, %al
	jne invalid_use_of_symbol
	xorb %ah, %ah
      sub_values:
	movb %ah, 12(%ebx)
	mov (%edi), %eax
	sub %eax, (%ebx)
	mov 4(%edi), %eax
	sbb %eax, 4(%ebx)
	orw %dx, %dx
	jz calculate_expression
	push %esi
	mov %ebx, %esi
	lea 10(%edi), %ebx
	movb 8(%edi), %cl
	call sub_register
	lea 11(%edi), %ebx
	movb 9(%edi), %cl
	call sub_register
	pop %esi
	jmp calculate_expression
      sub_register:
	orb %cl, %cl
	jz add_register_done
	negb (%ebx)
	jmp add_register_start
      calculate_mul:
	orw %dx, %dx
	jz mul_start
	cmpw $0, 8(%ebx)
	jne mul_start
	mov (%ebx), %eax
	xchg (%edi), %eax
	mov %eax, (%ebx)
	mov 4(%ebx), %eax
	xchg 4(%edi), %eax
	mov %eax, 4(%ebx)
	mov 8(%ebx), %eax
	xchg 8(%edi), %eax
	mov %eax, 8(%ebx)
	mov 12(%ebx), %eax
	xchg 12(%edi), %eax
	mov %eax, 12(%ebx)
      mul_start:
	push %esi
	pushw %dx
	mov %ebx, %esi
	xorb %bl, %bl
	testl $1 << 31, 4(%esi)
	jz mul_first_sign_ok
	notl (%esi)
	notl 4(%esi)
	addl $1, (%esi)
	adcl $0, 4(%esi)
	notb %bl
      mul_first_sign_ok:
	testl $1 << 31, 4(%edi)
	jz mul_second_sign_ok
	notl (%edi)
	notl 4(%edi)
	addl $1, (%edi)
	adcl $0, 4(%edi)
	notb %bl
      mul_second_sign_ok:
	cmpl $0, 4(%esi)
	jz mul_numbers
	cmpl $0, 4(%edi)
	jnz value_out_of_range
      mul_numbers:
	mov 4(%esi), %eax
	mull (%edi)
	or %edx, %edx
	jnz value_out_of_range
	mov %eax, %ecx
	mov (%esi), %eax
	mull 4(%edi)
	or %edx, %edx
	jnz value_out_of_range
	add %eax, %ecx
	jc value_out_of_range
	mov (%esi), %eax
	mull (%edi)
	add %ecx, %edx
	jc value_out_of_range
	mov %eax, (%esi)
	mov %edx, 4(%esi)
	orb %bl, %bl
	jz mul_ok
	notl (%esi)
	notl 4(%esi)
	addl $1, (%esi)
	adcl $0, 4(%esi)
      mul_ok:
	popw %dx
	orw %dx, %dx
	jz mul_calculated
	cmpw $0, 8(%edi)
	jne invalid_value
	cmpb $0, 8(%esi)
	je mul_first_register_ok
	movb (%edi), %al
	cbtw
	cwtl
	cltd
	cmp 4(%edi), %edx
	jne value_out_of_range
	cmp (%edi), %eax
	jne value_out_of_range
	imulb 10(%esi)
	movb %ah, %dl
	cbtw
	cmpb %dl, %ah
	jne value_out_of_range
	movb %al, 10(%esi)
      mul_first_register_ok:
	cmpb $0, 9(%esi)
	je mul_calculated
	movb (%edi), %al
	cbtw
	cwtl
	cltd
	cmp 4(%edi), %edx
	jne value_out_of_range
	cmp (%edi), %eax
	jne value_out_of_range
	imulb 11(%esi)
	movb %ah, %dl
	cbtw
	cmpb %dl, %ah
	jne value_out_of_range
	movb %al, 11(%esi)
      mul_calculated:
	pop %esi
	jmp calculate_expression
      calculate_div:
	push %esi
	pushw %dx
	mov %ebx, %esi
	call div_64
	popw %dx
	orw %dx, %dx
	jz div_calculated
	cmpb $0, 8(%esi)
	je div_first_register_ok
	movb (%edi), %al
	cbtw
	cwtl
	cltd
	cmp 4(%edi), %edx
	jne value_out_of_range
	cmp (%edi), %eax
	jne value_out_of_range
	orb %al, %al
	jz value_out_of_range
	movb 10(%esi), %al
	cbtw
	idivb (%edi)
	movb %al, 10(%esi)
      div_first_register_ok:
	cmpb $0, 9(%esi)
	je div_calculated
	movb (%edi), %al
	cbtw
	cwtl
	cltd
	cmp 4(%edi), %edx
	jne value_out_of_range
	cmp (%edi), %eax
	jne value_out_of_range
	orb %al, %al
	jz value_out_of_range
	movb 11(%esi), %al
	cbtw
	idivb (%edi)
	movb %al, 11(%esi)
      div_calculated:
	pop %esi
	jmp calculate_expression
      calculate_mod:
	push %esi
	mov %ebx, %esi
	call div_64
	mov %eax, (%esi)
	mov %edx, 4(%esi)
	pop %esi
	jmp calculate_expression
      calculate_and:
	mov (%edi), %eax
	and %eax, (%ebx)
	mov 4(%edi), %eax
	and %eax, 4(%ebx)
	jmp calculate_expression
      calculate_or:
	mov (%edi), %eax
	or %eax, (%ebx)
	mov 4(%edi), %eax
	or %eax, 4(%ebx)
	jmp calculate_expression
      calculate_xor:
	cmpb $1, value_size
	je xor_byte
	cmpb $2, value_size
	je xor_word
	cmpb $4, value_size
	je xor_dword
	cmpb $6, value_size
	je xor_pword
      xor_qword:
	mov (%edi), %eax
	xor %eax, (%ebx)
	mov 4(%edi), %eax
	xor %eax, 4(%ebx)
	jmp calculate_expression
      xor_byte:
	cmpl $0, 4(%edi)
	jne xor_qword
	cmpw $0, 2(%edi)
	jne xor_qword
	cmpb $0, 1(%edi)
	jne xor_qword
	movb (%edi), %al
	xorb %al, (%ebx)
	jmp calculate_expression
      xor_word:
	cmpl $0, 4(%edi)
	jne xor_qword
	cmpw $0, 2(%edi)
	jne xor_qword
	movw (%edi), %ax
	xorw %ax, (%ebx)
	jmp calculate_expression
      xor_dword:
	cmpl $0, 4(%edi)
	jne xor_qword
	mov (%edi), %eax
	xor %eax, (%ebx)
	jmp calculate_expression
      xor_pword:
	cmpw $0, 6(%edi)
	jne xor_qword
	mov (%edi), %eax
	xor %eax, (%ebx)
	movw 4(%edi), %ax
	xorw %ax, 4(%ebx)
	jmp calculate_expression
      calculate_shl:
	mov 4(%edi), %eax
	test $1 << 31, %eax
	jnz shl_negative
	or %eax, %eax
	jnz zero_value
	mov (%edi), %ecx
	cmp $64, %ecx
	jae zero_value
	cmp $32, %ecx
	jae shl_high
	mov 4(%ebx), %edx
	mov (%ebx), %eax
	shldl %eax, %edx
	shl %cl, %eax
	mov %eax, (%ebx)
	mov %edx, 4(%ebx)
	jmp calculate_expression
      shl_high:
	subb $32, %cl
	mov (%ebx), %eax
	shl %cl, %eax
	mov %eax, 4(%ebx)
	movl $0, (%ebx)
	jmp calculate_expression
      shl_negative:
	notl (%edi)
	notl 4(%edi)
	addl $1, (%edi)
	adcl $0, 4(%edi)
      calculate_shr:
	mov 4(%edi), %eax
	test $1 << 31, %eax
	jnz shr_negative
	or %eax, %eax
	jnz zero_value
	mov (%edi), %ecx
	cmp $64, %ecx
	jae zero_value
	cmp $32, %ecx
	jae shr_high
	mov 4(%ebx), %edx
	mov (%ebx), %eax
	shrdl %edx, %eax
	shr %cl, %edx
	mov %eax, (%ebx)
	mov %edx, 4(%ebx)
	jmp calculate_expression
      shr_high:
	subb $32, %cl
	mov 4(%ebx), %eax
	shr %cl, %eax
	mov %eax, (%ebx)
	movl $0, 4(%ebx)
	jmp calculate_expression
      shr_negative:
	notl (%edi)
	notl 4(%edi)
	addl $1, (%edi)
	adcl $0, 4(%edi)
	jmp calculate_shl
      zero_value:
	movl $0, (%ebx)
	movl $0, 4(%ebx)
	jmp calculate_expression
      calculate_not:
	cmpw $0, 8(%edi)
	jne invalid_expression
	cmpb $0, 12(%edi)
	jne invalid_use_of_symbol
	cmpb $1, value_size
	je not_byte
	cmpb $2, value_size
	je not_word
	cmpb $4, value_size
	je not_dword
	cmpb $6, value_size
	je not_pword
      not_qword:
	notl (%edi)
	notl 4(%edi)
	add $0x10, %edi
	jmp calculate_expression
      not_byte:
	cmpl $0, 4(%edi)
	jne not_qword
	cmpw $0, 2(%edi)
	jne not_qword
	cmpb $0, 1(%edi)
	jne not_qword
	notb (%edi)
	add $0x10, %edi
	jmp calculate_expression
      not_word:
	cmpl $0, 4(%edi)
	jne not_qword
	cmpw $0, 2(%edi)
	jne not_qword
	notw (%edi)
	add $0x10, %edi
	jmp calculate_expression
      not_dword:
	cmpl $0, 4(%edi)
	jne not_qword
	notl (%edi)
	add $0x10, %edi
	jmp calculate_expression
      not_pword:
	cmpw $0, 6(%edi)
	jne not_qword
	notl (%edi)
	notw 4(%edi)
	add $0x10, %edi
	jmp calculate_expression
      calculate_neg:
	cmpw $0, 8(%edi)
	jne invalid_expression
	cmpb $0, 12(%edi)
	jne invalid_use_of_symbol
	mov (%edi), %eax
	mov 4(%edi), %edx
	movl $0, (%edi)
	movl $0, 4(%edi)
	sub %eax, (%edi)
	sbb %edx, 4(%edi)
	add $0x10, %edi
	jmp calculate_expression
      calculate_rva:
	cmpw $0, 8(%edi)
	jne invalid_expression
	movb 12(%edi), %al
	cmpb $2, %al
	je rva_ok
	orb %al, %al
	jnz invalid_use_of_symbol
	cmpb $0, next_pass_needed
	je invalid_use_of_symbol
      rva_ok:
	movb $0, 12(%edi)
	mov header_data, %eax
	mov 0x34(%eax), %eax
	sub %eax, (%edi)
	sbbl $0, 4(%edi)
	add $0x10, %edi
	jmp calculate_expression
      div_64:
	xorb %bl, %bl
	cmpl $0, (%edi)
	jne divider_ok
	cmpl $0, 4(%edi)
	jne divider_ok
	cmpb $0, next_pass_needed
	je value_out_of_range
	jmp div_done
      divider_ok:
	testl $1 << 31, 4(%esi)
	jz div_first_sign_ok
	notl (%esi)
	notl 4(%esi)
	addl $1, (%esi)
	adcl $0, 4(%esi)
	notb %bl
      div_first_sign_ok:
	testl $1 << 31, 4(%edi)
	jz div_second_sign_ok
	notl (%edi)
	notl 4(%edi)
	addl $1, (%edi)
	adcl $0, 4(%edi)
	notb %bl
      div_second_sign_ok:
	cmpl $0, 4(%edi)
	jne div_high
	mov (%edi), %ecx
	mov 4(%esi), %eax
	xor %edx, %edx
	div %ecx
	mov %eax, 4(%esi)
	mov (%esi), %eax
	div %ecx
	mov %eax, (%esi)
	mov %edx, %eax
	xor %edx, %edx
	jmp div_done
      div_high:
	mov 4(%esi), %eax
	xor %edx, %edx
	divl 4(%edi)
	mov (%esi), %ebx
	mov %eax, (%esi)
	movl $0, 4(%esi)
	mov %edx, %ecx
	mull (%edi)
      div_high_loop:
	cmp %edx, %ecx
	ja div_high_done
	jb div_high_change
	cmp %eax, %ebx
	jae div_high_done
      div_high_change:
	decl (%esi)
	sub (%edi), %eax
	sbb 4(%edi), %edx
	jnc div_high_loop
      div_high_done:
	sub %eax, %ebx
	sbb %edx, %ecx
	mov %ecx, %edx
	mov %ebx, %eax
	ret
      div_done:
	orb %bl, %bl
	jz div_ok
	notl (%esi)
	notl 4(%esi)
	addl $1, (%esi)
	adcl $0, 4(%esi)
      div_ok:
	ret
      convert_fp:
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	movb value_size, %al
	cmpb $4, %al
	je convert_fp_dword
	cmpb $8, %al
	je convert_fp_qword
	jmp invalid_value
      convert_fp_dword:
	xor %eax, %eax
	cmpw $0x8000, 8(%esi)
	je fp_dword_store
	movw 8(%esi), %bx
	mov 4(%esi), %eax
	shl %eax
	shr $9, %eax
	jnc fp_dword_ok
	inc %eax
	test $1 << 23, %eax
	jz fp_dword_ok
	and $[1 << 23] - 1, %eax
	incw %bx
	shr %eax
      fp_dword_ok:
	addw $0x7f, %bx
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x100, %bx */
	.value 0x100
	jae value_out_of_range
	shl $23, %ebx
	or %ebx, %eax
	movb 11(%esi), %bl
	shl $31, %ebx
	or %ebx, %eax
      fp_dword_store:
	mov %eax, (%edi)
	xor %eax, %eax
	mov %eax, 4(%edi)
	add $12, %esi
	ret
      convert_fp_qword:
	xor %eax, %eax
	xor %edx, %edx
	cmpw $0x8000, 8(%esi)
	je fp_qword_store
	movw 8(%esi), %bx
	mov (%esi), %eax
	mov 4(%esi), %edx
	shl %eax
	rcl %edx
	mov %edx, %ecx
	shr $12, %edx
	shrdl $12, %ecx, %eax
	jnc fp_qword_ok
	add $1, %eax
	adc $0, %edx
	test $1 << 20, %edx
	jz fp_qword_ok
	and $[1 << 20] - 1, %edx
	incw %bx
	shr %edx
	rcr %eax
      fp_qword_ok:
	addw $0x3ff, %bx
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x800, %bx */
	.value 0x800
	jae value_out_of_range
	shl $20, %ebx
	or %ebx, %edx
	movb 11(%esi), %bl
	shl $31, %ebx
	or %ebx, %edx
      fp_qword_store:
	mov %eax, (%edi)
	mov %edx, 4(%edi)
	add $12, %esi
	ret
      get_string_value:
	lodsl
	mov %eax, %ecx
	cmp $8, %ecx
	ja value_out_of_range
	mov %edi, %edx
	xor %eax, %eax
	stosl
	stosl
	mov %edx, %edi
	rep
	movsb
	mov %edx, %edi
	inc %esi
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	ret

get_byte_value:
	movb $1, value_size
	movb $2, forced_size
	movl %edi, current_offset
	call calculate_expression
	cmpw $0, 8(%edi)
	jne invalid_value
	cmpb $0, 12(%edi)
	jne invalid_use_of_symbol
	mov (%edi), %eax
	cmpl $0, 4(%edi)
	je byte_positive
	cmpl $-1, 4(%edi)
	jne range_exceeded
	cmp $-0x80, %eax
	jb range_exceeded
	ret
      byte_positive:
	cmp $0x100, %eax
	jae range_exceeded
      return_value:
	ret
      range_exceeded:
	cmpl $0, error_line
	jne return_value
	mov current_line, %eax
	movl %eax, error_line
	movl $value_out_of_range, error
	ret
get_word_value:
	movb $2, value_size
	movb $2, forced_size
	movl %edi, current_offset
	call calculate_expression
	cmpw $0, 8(%edi)
	jne invalid_value
	movb 12(%edi), %al
	cmpb $2, %al
	je invalid_use_of_symbol
	.byte 0xa2  /*WORKAROUNDL movb %al, value_type */
	.long value_type
      check_word_value:
	mov (%edi), %eax
	cmpl $0, 4(%edi)
	je word_positive
	cmpl $-1, 4(%edi)
	jne range_exceeded
	cmp $-0x8000, %eax
	jb range_exceeded
	ret
      word_positive:
	cmp $0x10000, %eax
	jae range_exceeded
	ret
get_dword_value:
	movb $4, value_size
	movb $2, forced_size
	movl %edi, current_offset
	call calculate_expression
	cmpw $0, 8(%edi)
	jne invalid_value
	movb 12(%edi), %al
	.byte 0xa2  /*WORKAROUNDL movb %al, value_type */
	.long value_type
      check_dword_value:
	mov (%edi), %eax
	cmpl $0, 4(%edi)
	je dword_positive
	cmpl $-1, 4(%edi)
	jne range_exceeded
	test $1 << 31, %eax
	jz range_exceeded
      dword_positive:
	ret
get_pword_value:
	movb $6, value_size
	movb $2, forced_size
	movl %edi, current_offset
	call calculate_expression
	cmpw $0, 8(%edi)
	jne invalid_value
	movb 12(%edi), %al
	.byte 0xa2  /*WORKAROUNDL movb %al, value_type */
	.long value_type
	mov (%edi), %eax
	mov 4(%edi), %edx
	cmp $0x10000, %edx
	jge range_exceeded
	cmp $-0x8000, %edx
	jl range_exceeded
	ret
get_qword_value:
	movb $8, value_size
	movb $2, forced_size
	movl %edi, current_offset
	call calculate_expression
	cmpw $0, 8(%edi)
	jne invalid_value
	movb 12(%edi), %al
	.byte 0xa2  /*WORKAROUNDL movb %al, value_type */
	.long value_type
	mov (%edi), %eax
	mov 4(%edi), %edx
	ret
get_value:
	movb $0, operand_size
	movb $0, forced_size
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	jne invalid_value
	movb operand_size, %al
	cmpb $1, %al
	je value_byte
	cmpb $2, %al
	je value_word
	cmpb $4, %al
	je value_dword
	cmpb $6, %al
	je value_pword
	cmpb $8, %al
	je value_qword
	orb %al, %al
	jnz invalid_value
      value_qword:
	call get_qword_value
	ret
      value_pword:
	call get_pword_value
	movzwl %dx, %edx
	ret
      value_dword:
	call get_dword_value
	xor %edx, %edx
	ret
      value_word:
	call get_word_value
	xor %edx, %edx
	movzwl %ax, %eax
	ret
      value_byte:
	call get_byte_value
	xor %edx, %edx
	movzbl %al, %eax
	ret

get_address:
	movb $0, segment_register
	movb $0, address_size
	movb $4, value_size
	pushl $address_ok
	movb (%esi), %al
	andb $240, %al
	cmpb $0x60, %al
	jne get_size_prefix
	lodsb
	subb $0x60, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, segment_register */
	.long segment_register
	movb (%esi), %al
	andb $240, %al
      get_size_prefix:
	cmpb $0x70, %al
	jne calculate_address
	lodsb
	subb $0x70, %al
	cmpb $4, %al
	ja invalid_address_size
	.byte 0xa2  /*WORKAROUNDL movb %al, address_size */
	.long address_size
	.byte 0xa2  /*WORKAROUNDL movb %al, value_size */
	.long value_size
	jmp calculate_address
get_address_value:
	movb $0, address_size
	movb $4, value_size
	pushl $address_ok
      calculate_address:
	movl %edi, current_offset
	call calculate_expression
	movb 12(%edi), %al
	.byte 0xa2  /*WORKAROUNDL movb %al, value_type */
	.long value_type
	cmpb $1, %al
	je invalid_use_of_symbol
	orb %al, %al
	jz address_symbol_ok
	movb $0x84, %al
	xchgb %al, address_size
	orb %al, %al
	jz address_symbol_ok
	cmpb $4, %al
	jne address_sizes_do_not_agree
      address_symbol_ok:
	xorw %bx, %bx
	xorb %cl, %cl
	movb address_size, %ch
	cmpw $0, 8(%edi)
	je check_dword_value
	movb 8(%edi), %al
	movb 10(%edi), %dl
	call get_address_register
	movb 9(%edi), %al
	movb 11(%edi), %dl
	call get_address_register
	movw %bx, %ax
	shrb $4, %ah
	shrb $4, %al
	orb %bh, %bh
	jz check_address_registers
	orb %bl, %bl
	jz check_address_registers
	cmpb %ah, %al
	jne invalid_address
      check_address_registers:
	orb %ah, %al
	cmpb $2, %al
	je address_16bit
	cmpb $4, %al
	jne invalid_address
	orb %bh, %bh
	jnz check_index_scale
	cmpb $2, %cl
	je special_index_scale
	cmpb $3, %cl
	je special_index_scale
	cmpb $5, %cl
	je special_index_scale
	cmpb $9, %cl
	je special_index_scale
      check_index_scale:
	orb %cl, %cl
	jz address_registers_ok
	cmpb $1, %cl
	je address_registers_ok
	cmpb $2, %cl
	je address_registers_ok
	cmpb $4, %cl
	je address_registers_ok
	cmpb $8, %cl
	je address_registers_ok
	jmp invalid_address
      special_index_scale:
	movb %bl, %bh
	decb %cl
      address_registers_ok:
	jmp check_dword_value
      address_16bit:
	orb %cl, %cl
	jz check_word_value
	cmpb $1, %cl
	je check_word_value
	jmp invalid_address
      get_address_register:
	orb %al, %al
	jz address_register_ok
	cmpb $1, %dl
	jne scaled_register
	orb %bh, %bh
	jnz scaled_register
	movb %al, %bh
      address_register_ok:
	ret
      scaled_register:
	orb %bl, %bl
	jnz invalid_address
	movb %al, %bl
	movb %dl, %cl
	jmp address_register_ok
      address_ok:
	mov %eax, %edx
	ret

calculate_logical_expression:
	call get_logical_value
      logical_loop:
	pushw %ax
	lodsb
	cmpb $0x7c, %al
	je logical_or
	cmpb $0x26, %al
	je logical_and
	dec %esi
	popw %ax
	ret
      logical_or:
	call get_logical_value
	popw %bx
	orb %bl, %al
	jmp logical_loop
      logical_and:
	call get_logical_value
	popw %bx
	andb %bl, %al
	jmp logical_loop

get_logical_value:
	xorb %al, %al
	cmpb $0x7e, (%esi)
	jne negation_ok
	inc %esi
	orb $-1, %al
      negation_ok:
	pushw %ax
	cmpb $0x7b, (%esi)
	je logical_expression
	push %esi
	cmpb $0x11, (%esi)
	jne check_for_values
	add $2, %esi
      check_for_values:
	xorb %bl, %bl
	cmpb $0x28, (%esi)
	jne find_eq_symbol
	call skip_symbol
	lodsb
	cmpb $0x3d, %al
	je compare_values
	cmpb $0x3e, %al
	je compare_values
	cmpb $0x3c, %al
	je compare_values
	cmpb $0xf2, %al
	je compare_values
	cmpb $0xf3, %al
	je compare_values
	cmpb $0xf6, %al
	je compare_values
	dec %esi
      find_eq_symbol:
	cmpb $0x81, (%esi)
	je compare_symbols
	cmpb $0x83, (%esi)
	je scan_symbols_list
	call check_character
	jc logical_number
	cmpb $44, %al
	jne next_eq_symbol
	movb $1, %bl
      next_eq_symbol:
	call skip_symbol
	jmp find_eq_symbol
      compare_symbols:
	inc %esi
	pop %ebx
	mov %esi, %edx
	push %edi
	mov %ebx, %edi
	mov %esi, %ecx
	dec %ecx
	sub %edi, %ecx
	repz
	cmpsb
	pop %edi
	je symbols_equal
	mov %edx, %esi
      symbols_different:
	call check_character
	jc return_false
	call skip_symbol
	jmp symbols_different
      symbols_equal:
	call check_character
	jc return_true
	jmp symbols_different
      scan_symbols_list:
	orb %bl, %bl
	jnz invalid_expression
	xorw %bp, %bp
	inc %esi
	lodsb
	cmpb $0x3c, %al
	jne invalid_expression
	pop %ebx
	mov %esi, %ecx
	sub $2, %ecx
	sub %ebx, %ecx
      compare_in_list:
	mov %esi, %edx
	push %ecx
	push %edi
	mov %ebx, %edi
	repz
	cmpsb
	pop %edi
	pop %ecx
	jne not_equal_in_list
	cmpb $44, (%esi)
	je skip_rest_of_list
	cmpb $0x3e, (%esi)
	jne not_equal_in_list
      skip_rest_of_list:
	call check_character
	jc invalid_expression
	cmpb $0x3e, %al
	je list_return_true
	call skip_symbol
	jmp skip_rest_of_list
      list_return_true:
	inc %esi
	jmp return_true
      not_equal_in_list:
	mov %edx, %esi
      skip_list_item:
	call check_character
	jc invalid_expression
	cmpb $0x3e, %al
	je list_return_false
	cmpb $44, %al
	je next_in_list
	call skip_symbol
	jmp skip_list_item
      next_in_list:
	inc %esi
	jmp compare_in_list
      list_return_false:
	inc %esi
	jmp return_false
      check_character:
	movb (%esi), %al
	orb %al, %al
	jz stop
	cmpb $0xf, %al
	je stop
	cmpb $0x7d, %al
	je stop
	cmpb $0x7c, %al
	je stop
	cmpb $0x26, %al
	je stop
	clc
	ret
      stop:
	stc
	ret
      compare_values:
	pop %esi
	call get_value
	movb value_type, %bl
	push %eax
	push %edx
	pushw %bx
	lodsb
	.byte 0xa2  /*WORKAROUNDL movb %al, compare_type */
	.long compare_type
	call get_value
	popw %bx
	cmpb $0, next_pass_needed
	jne values_ok
	cmpb value_type, %bl
	jne invalid_use_of_symbol
      values_ok:
	pop %ecx
	pop %ebx
	cmpb $0x3d, compare_type
	je check_equal
	cmpb $0x3e, compare_type
	je check_greater
	cmpb $0x3c, compare_type
	je check_less
	cmpb $0xf2, compare_type
	je check_not_less
	cmpb $0xf3, compare_type
	je check_not_greater
	cmpb $0xf6, compare_type
	je check_not_equal
	jmp invalid_expression
      check_equal:
	cmp %ebx, %eax
	jne return_false
	cmp %ecx, %edx
	jne return_false
	jmp return_true
      check_greater:
	cmp %ecx, %edx
	jl return_true
	jg return_false
	cmp %ebx, %eax
	jb return_true
	jae return_false
      check_less:
	cmp %ecx, %edx
	jl return_false
	jg return_true
	cmp %ebx, %eax
	jbe return_false
	ja return_true
      check_not_less:
	cmp %ecx, %edx
	jl return_true
	jg return_false
	cmp %ebx, %eax
	jbe return_true
	ja return_false
      check_not_greater:
	cmp %ecx, %edx
	jl return_false
	jg return_true
	cmp %ebx, %eax
	jb return_false
	jae return_true
      check_not_equal:
	cmp %ebx, %eax
	jne return_true
	cmp %ecx, %edx
	jne return_true
	jmp return_false
      logical_number:
	pop %esi
	call get_value
	cmpb $0, value_type
	jne invalid_expression
	or %edx, %eax
	jnz return_true
      return_false:
	xorb %al, %al
	jmp logical_value_ok
      return_true:
	orb $-1, %al
	jmp logical_value_ok
      logical_expression:
	inc %esi
	call calculate_logical_expression
	pushw %ax
	lodsb
	cmpb $0x7d, %al
	jne invalid_expression
	popw %ax
      logical_value_ok:
	popw %bx
	xorb %bl, %al
	ret

/* %include '../preproce.inc' */

/*  flat assembler source */
/*  Copyright (c) 1999-2001, Tomasz Grysztar */
/*  All rights reserved. */

preprocessor:
	mov memory_start, %eax
	movl %eax, source_start
	pushl additional_memory
	mov additional_memory, %eax
	movl %eax, macros_list
	mov additional_memory_end, %eax
	movl %eax, labels_list
	movl $0, display_buffer
	movb $0, macro_status
	mov input_file, %edx
	mov memory_start, %edi
	call preprocess_file
	jc main_file_not_found
	cmpb $0, macro_status
	jne unexpected_end_of_file
	popl additional_memory
	movl %edi, code_start
	ret

preprocess_file:
	pushl memory_end
	push %edx
	call open
	jc no_source_file
	movb $2, %al
	xor %edx, %edx
	call lseek
	push %eax
	xorb %al, %al
	xor %edx, %edx
	call lseek
	pop %ecx
	mov memory_end, %edx
	dec %edx
	movb $0x1a, (%edx)
	sub %ecx, %edx
	jc out_of_memory
	mov %edx, %esi
	cmp %edi, %edx
	jbe out_of_memory
	movl %edx, memory_end
	call read
	call close
	pop %edx
	xor %ecx, %ecx
	mov %esi, %ebx
      preprocess_source:
	inc %ecx
	movl %edi, current_line
	mov %edx, %eax
	stosl
	mov %ecx, %eax
	stosl
	mov %esi, %eax
	sub %ebx, %eax
	stosl
	push %ebx
	push %edx
	call convert_line
	call preprocess_line
	pop %edx
	pop %ebx
      next_line:
	cmpb $0x1a, -1(%esi)
	jne preprocess_source
      file_end:
	popl memory_end
	clc
	ret
      no_source_file:
	pop %eax
	pop %eax
	stc
	ret

convert_line:
	push %ecx
	cmpb $0, macro_status
	jle convert_line_data
	.byte 0x66, 0xb8  /*WORKAROUNDW mov $0x3b, %ax */
	.value 0x3b
	stosw
      convert_line_data:
	cmp memory_end, %edi
	jae out_of_memory
	lodsb
	cmpb $0x20, %al
	je convert_line_data
	cmpb $9, %al
	je convert_line_data
	dec %esi
	lodsb
	movb %al, %ah
	mov $characters, %ebx
	xlat
	orb %al, %al
	jz convert_separator
	cmpb $0x27, %ah
	je convert_string
	cmpb $0x22, %ah
	je convert_string
	movb $0x1a, (%edi)
	scasw
	stosb
	mov $characters, %ebx
	xor %ecx, %ecx
      convert_symbol:
	lodsb
	xlat
	stosb
	orb %al, %al
	loopnz convert_symbol
	neg %ecx
	cmp $255, %ecx
	ja name_too_long
	dec %edi
	mov %edi, %ebx
	sub %ecx, %ebx
	movb %cl, -1(%ebx)
	movb -1(%esi), %ah
      convert_separator:
	xchgb %ah, %al
	cmpb $0x20, %al
	jb control_character
	je convert_line_data
      symbol_character:
	cmpb $0x3b, %al
	je ignore_comment
	cmpb $0x5c, %al
	je concate_lines
	stosb
	jmp convert_line_data
      control_character:
	cmpb $0x1a, %al
	je line_end
	cmpb $0xd, %al
	je cr_character
	cmpb $0xa, %al
	je lf_character
	cmpb $9, %al
	je convert_line_data
	orb %al, %al
	jnz symbol_character
	jmp line_end
      lf_character:
	lodsb
	cmpb $0xd, %al
	je line_end
	dec %esi
	jmp line_end
      cr_character:
	lodsb
	cmpb $0xa, %al
	je line_end
	dec %esi
	jmp line_end
      convert_string:
	movb $0x22, %al
	stosb
	scasl
	mov %edi, %ebx
      copy_string:
	lodsb
	stosb
	cmpb $0xa, %al
	je missing_end_quote
	cmpb $0xd, %al
	je missing_end_quote
	orb %al, %al
	jz missing_end_quote
	cmpb $0x1a, %al
	je missing_end_quote
	cmpb %ah, %al
	jne copy_string
	lodsb
	cmpb %ah, %al
	je copy_string
	dec %esi
	dec %edi
	mov %edi, %eax
	sub %ebx, %eax
	mov %eax, -4(%ebx)
	jmp convert_line_data
      concate_lines:
	lodsb
	cmpb $0x20, %al
	je concate_lines
	cmpb $9, %al
	je concate_lines
	cmpb $0x1a, %al
	je unexpected_end_of_file
	cmpb $0xa, %al
	je concate_lf
	cmpb $0xd, %al
	je concate_cr
	cmpb $0x3b, %al
	jne extra_characters_on_line
      find_concated_line:
	lodsb
	cmpb $0xa, %al
	je concate_lf
	cmpb $0xd, %al
	je concate_cr
	orb %al, %al
	jz concate_ok
	cmpb $0x1a, %al
	jne find_concated_line
	jmp unexpected_end_of_file
      concate_lf:
	lodsb
	cmpb $0xd, %al
	je concate_ok
	dec %esi
	jmp concate_ok
      concate_cr:
	lodsb
	cmpb $0xa, %al
	je concate_ok
	dec %esi
      concate_ok:
	incl (%esp)
	jmp convert_line_data
      ignore_comment:
	lodsb
	cmpb $0xa, %al
	je lf_character
	cmpb $0xd, %al
	je cr_character
	orb %al, %al
	jz line_end
	cmpb $0x1a, %al
	jne ignore_comment
      line_end:
	xorb %al, %al
	stosb
	pop %ecx
	ret

preprocess_line:
	pushl struc_name
	push %ecx
	push %esi
	mov current_line, %esi
	add $12, %esi
	movb macro_status, %al
	decb %al
	jz find_macro_block
	decb %al
	jz skip_macro_block
      preprocess_instruction:
	lodsb
	cmpb $0x3a, %al
	je preprocess_instruction
	movzbl (%esi), %ecx
	inc %esi
	cmpb $0x1a, %al
	jne not_preprocessor_symbol
	push %edi
	mov $preprocessor_directives, %edi
	call get_symbol
	pop %edi
	jc not_preprocessor_directive
	movb $0x3b, -2(%edx)
	movzwl %ax, %ebx
	add $preprocessor, %ebx
	xor %eax, %eax
	jmp *%ebx
      not_preprocessor_directive:
	movb %cl, %al
	xorb %ah, %ah
	call get_macro
	jc not_macro
	movb $0x3b, -2(%edx)
	movl $0, struc_name
	jmp use_macro
      not_macro:
	movl %esi, struc_name
	add %ecx, %esi
	lodsb
	cmpb $0x3a, %al
	je preprocess_instruction
	cmpb $0x1a, %al
	jne not_preprocessor_symbol
	cmpl $3+[[0x65|[0x71<<8]|[0x75<<16]] << 8], (%esi)  /*  Same multibyte character constant order in fasm and NASM. */
	je define_symbolic_constant
	lodsb
	movb $1, %ah
	call get_macro
	jc not_preprocessor_symbol
	movb $0x3a, -2(%edx)
	movb $0x3b, %al
	xchgb -1(%edx), %al
	decb %al
	movb %al, (%edx)
	jmp use_macro
      not_preprocessor_symbol:
	mov current_line, %esi
	add $12, %esi
	call process_symbolic_constants
      line_preprocessed:
	pop %esi
	pop %ecx
	popl struc_name
	ret
get_macro:
	mov %esi, %edx
	mov %edi, %ebp
	mov additional_memory, %ebx
      check_macro:
	movb %al, %cl
	cmp macros_list, %ebx
	je no_macro_found
	sub $8, %ebx
	cmpw (%ebx), %ax
	jne check_macro
	mov 4(%ebx), %edi
	repz
	cmpsb
	je macro_ok
	mov %edx, %esi
	jmp check_macro
      no_macro_found:
	mov %ebp, %edi
	stc
	ret
      macro_ok:
	mov %ebp, %edi
	clc
	ret
process_symbolic_constants:
	mov %esi, %ebp
	lodsb
	cmpb $0x1a, %al
	je check_symbol
	cmpb $0x22, %al
	je ignore_string
	orb %al, %al
	jnz process_symbolic_constants
	dec %esi
	ret
      ignore_string:
	lodsl
	add %eax, %esi
	jmp process_symbolic_constants
      check_symbol:
	movzbl (%esi), %ecx
	inc %esi
	call replace_symbolic_constant
	jnc process_after_replaced
	add %ecx, %esi
	jmp process_symbolic_constants
      replace_symbolic_constant:
	push %edi
	mov %esi, %ebx
	mov %ecx, %eax
	mov labels_list, %edx
      scan_symbolic_constants:
	mov %eax, %ecx
	mov %ebx, %esi
	cmp additional_memory_end, %edx
	je not_symbolic_constant
	cmpb (%edx), %al
	jne next_symbolic_constant
	mov 4(%edx), %edi
	repz
	cmpsb
	je symbolic_constant_found
      next_symbolic_constant:
	add $16, %edx
	jmp scan_symbolic_constants
      not_symbolic_constant:
	pop %edi
	stc
	ret
      symbolic_constant_found:
	pop %edi
	mov 8(%edx), %ecx
	mov 12(%edx), %edx
	xchg %edx, %esi
	xor %eax, %eax
	shr %ecx
	rclb %al
	shr %ecx
	rclb %ah
	rep
	movsl
	movb %ah, %cl
	rep
	movsw
	movb %al, %cl
	rep
	movsb
	mov %edx, %esi
	clc
	ret
      process_after_replaced:
	lodsb
	cmpb $0x1a, %al
	je symbol_after_replaced
	stosb
	cmpb $0x22, %al
	je string_after_replaced
	orb %al, %al
	jnz process_after_replaced
	mov %edi, %ecx
	sub %esi, %ecx
	mov %ebp, %edi
	xor %eax, %eax
	shr %ecx
	rclb %al
	shr %ecx
	rclb %ah
	rep
	movsl
	movb %ah, %cl
	rep
	movsw
	movb %al, %cl
	rep
	movsb
	ret
      string_after_replaced:
	lodsl
	stosl
	mov %eax, %ecx
	rep
	movsb
	jmp process_after_replaced
      symbol_after_replaced:
	movzbl (%esi), %ecx
	inc %esi
	call replace_symbolic_constant
	jnc process_after_replaced
	movb $0x1a, %al
	movb %cl, %ah
	stosw
	rep
	movsb
	jmp process_after_replaced
include_file:
	lodsb
	cmpb $0x22, %al
	jne invalid_argument
	lodsl
	mov %esi, %edx
	add %eax, %esi
	cmpb $0, (%esi)
	jne extra_characters_on_line
	call preprocess_file
	jc file_not_found
	jmp line_preprocessed
define_symbolic_constant:
	add $4, %esi
	push %esi
	call process_symbolic_constants
	pop %ebx
	mov labels_list, %edx
	sub $16, %edx
	cmp additional_memory, %edx
	jb out_of_memory
	movl %edx, labels_list
	mov %edi, %ecx
	dec %ecx
	sub %ebx, %ecx
	mov %ecx, 8(%edx)
	mov %ebx, 12(%edx)
	mov struc_name, %ebx
	movb $0x3b, -2(%ebx)
	movb -1(%ebx), %al
	movb %al, (%edx)
	mov %ebx, 4(%edx)
	jmp line_preprocessed
define_struc:
	orb $1, %ah
define_macro:
	cmpb $0, macro_status
	jne unexpected_instruction
	lodsb
	cmpb $0x1a, %al
	jne invalid_name
	lodsb
	mov additional_memory, %ebx
	movw %ax, (%ebx)
	mov %esi, 4(%ebx)
	add $8, %ebx
	cmp labels_list, %ebx
	jae out_of_memory
	movl %ebx, additional_memory
	movzbl %al, %eax
	add %eax, %esi
	movb $1, macro_status
	xorb %bl, %bl
	lodsb
	orb %al, %al
	jz line_preprocessed
	cmpb $0x7b, %al
	je found_macro_block
	dec %esi
      skip_macro_arguments:
	lodsb
	cmpb $0x1a, %al
	je skip_macro_argument
	cmpb $0x5b, %al
	jne invalid_macro_arguments
	xorb $-1, %bl
	jz invalid_macro_arguments
	lodsb
	cmpb $0x1a, %al
	jne invalid_macro_arguments
      skip_macro_argument:
	movzbl (%esi), %eax
	inc %esi
	add %eax, %esi
	lodsb
	cmpb $44, %al
	je skip_macro_arguments
	cmpb $0x5d, %al
	jne end_macro_arguments
	lodsb
	notb %bl
      end_macro_arguments:
	orb %bl, %bl
	jnz invalid_macro_arguments
	orb %al, %al
	jz line_preprocessed
	cmpb $0x7b, %al
	je found_macro_block
	jmp invalid_macro_arguments
      find_macro_block:
	add $2, %esi
	lodsb
	orb %al, %al
	jz line_preprocessed
	cmpb $0x7b, %al
	jne unexpected_characters
      found_macro_block:
	movb $2, macro_status
      skip_macro_block:
	lodsb
	cmpb $0x1a, %al
	je skip_macro_symbol
	cmpb $0x3b, %al
	je skip_macro_symbol
	cmpb $0x22, %al
	je skip_macro_string
	orb %al, %al
	jz line_preprocessed
	cmpb $0x7d, %al
	jne skip_macro_block
	lodsb
	orb %al, %al
	jnz extra_characters_on_line
	movb $0, macro_status
	jmp line_preprocessed
      skip_macro_symbol:
	movzbl (%esi), %eax
	inc %esi
	add %eax, %esi
	jmp skip_macro_block
      skip_macro_string:
	lodsl
	add %eax, %esi
	jmp skip_macro_block
purge_macro:
	lodsb
	cmpb $0x1a, %al
	jne invalid_name
	lodsb
	xorb %ah, %ah
	call get_macro
	jc macro_purged
	orb $0x80, 1(%ebx)
      macro_purged:
	lodsb
	cmpb $44, %al
	je purge_macro
	orb %al, %al
	jnz extra_characters_on_line
	jmp line_preprocessed
use_macro:
	pushl macro_constants
	pushl macro_block
	pushl macro_block_line_number
	pushl counter
	pushl counter_limit
	orb $0x80, macro_status
	orb $0x80, 1(%ebx)
	mov %esi, %edx
	movzbl (%ebx), %esi
	add 4(%ebx), %esi
	push %edi
	mov additional_memory, %edi
	movl %edi, macro_constants
	movl $0, counter
      process_macro_arguments:
	lodsb
	orb %al, %al
	jz find_macro_instructions
	cmpb $0x7b, %al
	je macro_instructions_start
	cmpb $0x5b, %al
	jne get_macro_argument
	mov %esi, %ebp
	inc %esi
	incl counter
      get_macro_argument:
	movzbl (%esi), %eax
	inc %esi
	mov %esi, 4(%edi)
	add %eax, %esi
	ror $8, %eax
	or counter, %eax
	rol $8, %eax
	mov %eax, (%edi)
	xchg %edx, %esi
	mov %esi, 12(%edi)
      get_argument_value:
	lodsb
	orb %al, %al
	jz argument_value_end
	cmpb $44, %al
	je argument_value_end
	cmpb $0x22, %al
	je argument_string
	cmpb $0x1a, %al
	jne get_argument_value
	movzbl (%esi), %eax
	inc %esi
	add %eax, %esi
	jmp get_argument_value
      argument_string:
	lodsl
	add %eax, %esi
	jmp get_argument_value
      argument_value_end:
	dec %esi
	mov %esi, %eax
	sub 12(%edi), %eax
	mov %eax, 8(%edi)
	xchg %edx, %esi
	add $16, %edi
	cmp labels_list, %edi
	jae out_of_memory
	lodsb
	cmpb $44, %al
	je next_argument
	cmpb $0x5d, %al
	je next_arguments_group
	dec %esi
	jmp arguments_end
      next_argument:
	cmpb $44, (%edx)
	jne process_macro_arguments
	inc %edx
	jmp process_macro_arguments
      next_arguments_group:
	cmpb $44, (%edx)
	jne arguments_end
	inc %edx
	incl counter
	mov %ebp, %esi
	jmp process_macro_arguments
      arguments_end:
	lodsb
	cmpb $0x7b, %al
	je macro_instructions_start
      find_macro_instructions:
	add $14, %esi
	lodsb
	orb %al, %al
	jz find_macro_instructions
	cmpb $0x7b, %al
	jne unexpected_characters
      macro_instructions_start:
	cmpb $0, (%edx)
	jne invalid_macro_arguments
	movl %edi, additional_memory
	pop %edi
	mov $0x80000000, %ecx
	pushl current_line
	movl %esi, macro_block
	movl %ecx, macro_block_line_number
	mov $1, %eax
	xchg counter, %eax
	movl %eax, counter_limit
	or %eax, %eax
	jnz process_macro_line
	movl $1, counter_limit
      process_macro_line:
	movl %edi, current_line
	mov 4(%ebx), %eax
	dec %eax
	stosl
	mov %ecx, %eax
	stosl
	mov (%esp), %eax
	stosl
	orb $0x40, macro_status
	push %ebx
	push %ecx
      process_macro:
	lodsb
	cmpb $0x7d, %al
	je macro_line_processed
	orb %al, %al
	jz macro_line_processed
	cmpb $0x1a, %al
	je process_macro_symbol
	andb $[-0x40-1], macro_status
	stosb
	cmpb $0x22, %al
	jne process_macro
      copy_macro_string:
	mov (%esi), %ecx
	add $4, %ecx
	rep
	movsb
	jmp process_macro
      process_macro_symbol:
	push %esi
	push %edi
	testb $0x40, macro_status
	jz not_macro_directive
	movzbl (%esi), %ecx
	inc %esi
	mov $macro_directives, %edi
	call get_symbol
	jnc process_macro_directive
	dec %esi
	jmp not_macro_directive
      process_macro_directive:
	movzwl %ax, %edx
	add $preprocessor, %edx
	pop %edi
	pop %eax
	movb $0, (%edi)
	inc %edi
	pop %ecx
	pop %ebx
	jmp *%edx
      not_macro_directive:
	andb $[-0x40-1], macro_status
	mov counter, %eax
	or %eax, %eax
	jnz check_for_macro_constant
	inc %eax
      check_for_macro_constant:
	shl $8, %eax
	movb (%esi), %al
	inc %esi
	movzbl %al, %ebp
	mov macro_constants, %edx
	mov %esi, %ebx
      scan_macro_constants:
	cmp additional_memory, %edx
	je not_macro_constant
	cmp (%edx), %eax
	je try_macro_constant
	cmp (%edx), %ebp
	jne next_macro_constant
      try_macro_constant:
	mov %ebp, %ecx
	mov 4(%edx), %edi
	repz
	cmpsb
	je macro_constant_found
	mov %ebx, %esi
      next_macro_constant:
	add $16, %edx
	jmp scan_macro_constants
      macro_constant_found:
	cmpl $0, counter
	jne replace_macro_constant
	mov (%edx), %eax
	shr $8, %eax
	or %eax, %eax
	jz replace_macro_constant
	cmp counter_limit, %eax
	je replace_macro_constant
	pop %edi
	mov 8(%edx), %ecx
	mov 12(%edx), %esi
	rep
	movsb
	movb $44, (%edi)
	inc %edi
	mov %ebx, %esi
	inc %eax
	shl $8, %eax
	movb -1(%esi), %al
	push %edi
	jmp scan_macro_constants
      replace_macro_constant:
	pop %edi
	pop %eax
	mov 8(%edx), %ecx
	mov 12(%edx), %edx
	xchg %edx, %esi
	rep
	movsb
	mov %edx, %esi
	jmp process_macro
      not_macro_constant:
	pop %edi
	pop %esi
	movb $0x1a, %al
	stosb
	movb (%esi), %al
	inc %esi
	stosb
	cmpb $0x2e, (%esi)
	jne copy_macro_symbol
	mov struc_name, %ebx
	or %ebx, %ebx
	jz copy_macro_symbol
	xchg %ebx, %esi
	movzbl -1(%esi), %ecx
	addb %cl, -1(%edi)
	jc name_too_long
	rep
	movsb
	xchg %ebx, %esi
      copy_macro_symbol:
	movzbl %al, %ecx
	rep
	movsb
	jmp process_macro
      macro_line_processed:
	movb $0, (%edi)
	inc %edi
	push %eax
	call preprocess_line
	pop %eax
	pop %ecx
	pop %ebx
	cmpb $0x7d, %al
	je macro_block_processed
      process_next_line:
	inc %ecx
	add $14, %esi
	jmp process_macro_line
      local_symbols:
	lodsb
	cmpb $0x1a, %al
	jne invalid_argument
	push %edi
	push %ecx
	movzbl (%esi), %ecx
	inc %esi
	mov additional_memory, %edx
	mov counter, %eax
	shl $8, %eax
	movb %cl, %al
	mov %eax, (%edx)
	mov %esi, 4(%edx)
	movzbl _counter, %eax
	mov memory_end, %edi
	sub %eax, %edi
	sub %ecx, %edi
	sub $3, %edi
	movl %edi, memory_end
	mov %edi, 12(%edx)
	addb %cl, %al
	jc name_too_long
	incb %al
	jz name_too_long
	movb $0x1a, (%edi)
	inc %edi
	movb %al, (%edi)
	inc %edi
	add $2, %eax
	mov %eax, 8(%edx)
	add $16, %edx
	cmp labels_list, %edx
	jae out_of_memory
	movl %edx, additional_memory
	rep
	movsb
	movb $0x3f, %al
	stosb
	movzbl _counter, %ecx
	push %esi
	mov $_counter+1, %esi
	rep
	movsb
	pop %esi
	pop %ecx
	pop %edi
	cmp memory_end, %edi
	jae out_of_memory
	lodsb
	cmpb $44, %al
	je local_symbols
	cmpb $0x7d, %al
	je macro_block_processed
	orb %al, %al
	jnz extra_characters_on_line
	jmp process_next_line
      common_block:
	call close_macro_block
	jc process_macro_line
	movl $0, counter
	jmp new_macro_block
      forward_block:
	call close_macro_block
	jc process_macro_line
	movl $1, counter
	jmp new_macro_block
      reverse_block:
	call close_macro_block
	jc process_macro_line
	mov counter_limit, %eax
	or $0x80000000, %eax
	movl %eax, counter
      new_macro_block:
	movl %esi, macro_block
	movl %ecx, macro_block_line_number
	jmp process_macro_line
      close_macro_block:
	push %ecx
	mov $_counter, %eax
	call increase_counter
	pop %ecx
	cmpl $0, counter
	je block_closed
	jl reverse_counter
	mov counter, %eax
	cmp counter_limit, %eax
	je block_closed
	incl counter
	jmp continue_block
      reverse_counter:
	mov counter, %eax
	dec %eax
	cmp $0x80000000, %eax
	je block_closed
	movl %eax, counter
      continue_block:
	mov macro_block, %esi
	mov macro_block_line_number, %ecx
	stc
	ret
      block_closed:
	clc
	ret
      macro_block_processed:
	call close_macro_block
	jc process_macro_line
	andb $[-0x80-1], 1(%ebx)
	popl current_line
	mov macro_constants, %eax
	movl %eax, additional_memory
	movb $0, macro_status
	popl counter_limit
	popl counter
	popl macro_block_line_number
	popl macro_block
	popl macro_constants
	jmp line_preprocessed

increase_counter:
	movzbl (%eax), %ecx
      counter_loop:
	call increase_digit
	jnc counter_ok
	movb $0x30, (%eax,%ecx,1)
	loop counter_loop
      counter_ok:
	ret
      increase_digit:
	incb (%eax,%ecx,1)
	cmpb $0x3a, (%eax,%ecx,1)
	jb digit_increased
	je letter_digit
	cmpb $0x66, (%eax,%ecx,1)
	jbe digit_increased
	stc
	ret
      letter_digit:
	movb $0x61, (%eax,%ecx,1)
      digit_increased:
	clc
	ret

/* %include '../parser.inc' */

/*  flat assembler source */
/*  Copyright (c) 1999-2001, Tomasz Grysztar */
/*  All rights reserved. */

parser:
	mov memory_end, %eax
	movl %eax, labels_list
	movl $0, current_locals_prefix
	mov source_start, %esi
	mov code_start, %edi
	pushl additional_memory
     parser_loop:
	movl %esi, current_line
	cmp labels_list, %edi
	jae out_of_memory
	movb $0xf, %al
	stosb
	mov %esi, %eax
	stosl
	add $12, %esi
	call parse_line
	cmp code_start, %esi
	jb parser_loop
	xorb %al, %al
	stosb
	popl additional_memory
	mov code_start, %eax
	movl %eax, source_start
	movl %edi, code_start
	ret

parse_line:
	movb $0, parenthesis_stack
      instruction_start:
	cmpb $0x1a, (%esi)
	jne empty_instruction
	push %edi
	inc %esi
	movzbl (%esi), %ecx
	inc %esi
	cmpb $0x3a, (%esi,%ecx,1)
	je simple_label
	push %esi
	push %ecx
	add %ecx, %esi
	cmpb $0x1a, (%esi)
	je check_for_data_label
	cmpb $0x3d, (%esi)
	je constant_label
	pop %ecx
	pop %esi
	jmp get_main_instruction
      check_for_data_label:
	inc %esi
	movzbl (%esi), %ecx
	inc %esi
	push %edi
	mov $data_directives, %edi
	call get_symbol
	pop %edi
	jnc data_label
	pop %ecx
	pop %esi
      get_main_instruction:
	call get_instruction
	jnc parse_instruction
	mov $data_directives, %edi
	call get_symbol
	jnc data_instruction
	mov $symbols, %edi
	call get_symbol
	pop %edi
	jc unknown_instruction
	stosw
	jmp parse_arguments
      data_instruction:
	movzbl %ah, %ebx
	movw data_handlers(,%ebx,2), %bx
	jmp parse_instruction
      unknown_instruction:
	sub $2, %esi
	jmp parse_arguments
      constant_label:
	pop %ecx
	pop %esi
	pop %edi
	call identify_label
	movb $3, (%edi)
	inc %edi
	stosl
	xorb %al, %al
	stosb
	inc %esi
	jmp parse_arguments
      data_label:
	pop %ecx
	pop %ebx
	pop %edi
	pushw %ax
	push %esi
	mov %ebx, %esi
	call identify_label
	movb $2, (%edi)
	inc %edi
	stosl
	pop %esi
	popw %ax
	stosb
	push %edi
	jmp data_instruction
      simple_label:
	pop %edi
	call identify_label
	movb $2, (%edi)
	inc %edi
	stosl
	inc %esi
	xorb %al, %al
	stosb
	jmp instruction_start
      identify_label:
	cmpb $0x2e, (%esi)
	je local_label_name
	call get_label_id
	mov 4(%eax), %ebx
	dec %ebx
	movl %ebx, current_locals_prefix
	ret
      local_label_name:
	call get_label_id
	ret
      parse_prefix_instruction:
	cmpb $0x1a, (%esi)
	jne parse_arguments
	push %edi
	inc %esi
	movzbl (%esi), %ecx
	inc %esi
	jmp get_main_instruction
      parse_label_directive:
	push %edi
	lodsb
	cmpb $0x1a, %al
	jne invalid_argument
	movzbl (%esi), %ecx
	lodsb
	pop %edi
	movb $2, %al
	stosb
	call identify_label
	stosl
	xorb %al, %al
	stosb
	jmp parse_arguments
      parse_instruction:
	pop %edi
	movb %al, %dl
	movb $1, %al
	stosb
	movw %bx, %ax
	stosw
	movb %dl, %al
	stosb
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $prefix_instruction-assembler, %bx */
	.value --prefix_instruction___assembler
	je parse_prefix_instruction
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $end_directive-assembler, %bx */
	.value --end_directive___assembler
	je parse_prefix_instruction
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $label_directive-assembler, %bx */
	.value --label_directive___assembler
	je parse_label_directive
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $load_directive-assembler, %bx */
	.value --load_directive___assembler
	je parse_label_directive
      parse_arguments:
	lodsb
	cmpb $0x3a, %al
	je instruction_separator
	cmpb $44, %al
	je separator
	cmpb $0x3d, %al
	je separator
	cmpb $0x7c, %al
	je separator
	cmpb $0x26, %al
	je separator
	cmpb $0x7e, %al
	je separator
	cmpb $0x3e, %al
	je greater
	cmpb $0x3c, %al
	je less
	cmpb $0x29, %al
	je close_expression
	orb %al, %al
	jz line_parsed
	cmpb $0x5b, %al
	je address_argument
	cmpb $0x5d, %al
	je separator
	dec %esi
	cmpb $0x1a, %al
	jne expression_argument
	push %edi
	mov $directive_operators, %edi
	call get_operator
	orb %al, %al
	jnz operator_argument
	inc %esi
	movzbl (%esi), %ecx
	inc %esi
	mov $symbols, %edi
	call get_symbol
	jnc symbol_argument
	mov $formatter_symbols, %edi
	call get_symbol
	jnc symbol_argument
	cmp $1, %ecx
	jne check_argument
	cmpb $0x3f, (%esi)
	jne check_argument
	pop %edi
	movsb
	jmp argument_parsed
      symbol_argument:
	pop %edi
	stosw
	jmp argument_parsed
      operator_argument:
	pop %edi
	stosb
	cmpb $0x80, %al
	je forced_expression
	jmp argument_parsed
      check_argument:
	push %esi
	push %ecx
	sub $2, %esi
	mov $single_operand_operators, %edi
	call get_operator
	pop %ecx
	pop %esi
	orb %al, %al
	jnz not_instruction
	call get_instruction
	jnc parse_instruction
	mov $data_directives, %edi
	call get_symbol
	jnc data_instruction
      not_instruction:
	pop %edi
	sub $2, %esi
      expression_argument:
	cmpb $0x22, (%esi)
	jne not_string
	mov 1(%esi), %eax
	cmp $8, %eax
	ja string_argument
	lea 5(%esi,%eax,1), %ebx
	push %ebx
	push %ecx
	push %esi
	push %edi
	movb $0x28, %al
	stosb
	call convert_expression
	movb $0x29, %al
	stosb
	pop %eax
	pop %edx
	pop %ecx
	pop %ebx
	cmp %ebx, %esi
	jne expression_parsed
	mov %eax, %edi
	mov %edx, %esi
      string_argument:
	inc %esi
	.byte 0x66, 0xb8  /*WORKAROUNDW mov $'(, %ax */
	.value 0x28
	stosw
	lodsl
	mov %eax, %ecx
	stosl
	shr %ecx
	jnc string_movsb_ok
	movsb
      string_movsb_ok:
	shr %ecx
	jnc string_movsw_ok
	movsw
      string_movsw_ok:
	rep
	movsl
	xorb %al, %al
	stosb
	jmp argument_parsed
      not_string:
	cmpb $0x28, (%esi)
	jne parse_expression
	push %esi
	push %edi
	inc %esi
	movb $0x7b, %al
	stosb
	incb parenthesis_stack
	jmp parse_arguments
      parse_expression:
      forced_expression:
	movb $0x28, %al
	stosb
      expression:
	call convert_expression
	movb $0x29, %al
	stosb
	jmp expression_parsed
      address_argument:
	movb $0x5b, %al
	stosb
	cmpw $0x21a, (%esi)
	jne convert_address
	push %esi
	add $4, %esi
	lea 1(%esi), %ebx
	cmpb $0x3a, (%esi)
	pop %esi
	jne convert_address
	add $2, %esi
	mov $2, %ecx
	push %ebx
	push %edi
	mov $symbols, %edi
	call get_symbol
	pop %edi
	pop %esi
	jc invalid_address
	cmpb $0x10, %al
	jne invalid_address
	movb %ah, %al
	andb $240, %ah
	cmpb $0x60, %ah
	jne invalid_address
	stosb
      convert_address:
	cmpb $0x1a, (%esi)
	jne convert_address_value
	push %esi
	lodsw
	movzbl %ah, %ecx
	push %edi
	mov $address_sizes, %edi
	call get_symbol
	pop %edi
	jc no_size_prefix
	movb %ah, %al
	addb $0x70, %al
	stosb
	add $4, %esp
	jmp convert_address_value
      no_size_prefix:
	pop %esi
      convert_address_value:
	call convert_expression
	lodsb
	cmpb $0x5d, %al
	jne invalid_address
	stosb
	jmp argument_parsed
      close_expression:
	movb $0x7d, %al
      separator:
	stosb
	jmp argument_parsed
      instruction_separator:
	stosb
	jmp instruction_start
      greater:
	cmpb $0x3d, (%esi)
	jne separator
	inc %esi
	movb $0xf2, %al
	jmp separator
      less:
	cmpb $0x83, -1(%edi)
	je separator
	cmpb $0x3e, (%esi)
	je not_equal
	cmpb $0x3d, (%esi)
	jne separator
	inc %esi
	movb $0xf3, %al
	jmp separator
      not_equal:
	inc %esi
	movb $0xf6, %al
	jmp separator
      argument_parsed:
	cmpb $0, parenthesis_stack
	je parse_arguments
	decb parenthesis_stack
	add $8, %esp
	jmp argument_parsed
      expression_parsed:
	cmpb $0, parenthesis_stack
	je parse_arguments
	cmpb $0x29, (%esi)
	jne argument_parsed
	decb parenthesis_stack
	pop %edi
	pop %esi
	jmp parse_expression
      empty_instruction:
	lodsb
	orb %al, %al
	jz line_parsed
	cmpb $0x3a, %al
	je empty_label
	cmpb $0x3b, %al
	je skip_preprocessed_symbol
	dec %esi
	jmp parse_arguments
      empty_label:
	mov $_counter, %eax
	call increase_counter
	movl %eax, current_locals_prefix
	jmp instruction_start
      skip_preprocessed_symbol:
	lodsb
	movzbl %al, %eax
	add %eax, %esi
      skip_next:
	lodsb
	orb %al, %al
	jz line_parsed
	cmpb $0x1a, %al
	je skip_preprocessed_symbol
	cmpb $0x22, %al
	je skip_preprocessed_string
	jmp skip_next
      skip_preprocessed_string:
	lodsl
	add %eax, %esi
	jmp skip_next
      line_parsed:
	cmpb $0, parenthesis_stack
	jne invalid_expression
	ret

/* %include '../assemble.inc' */

/*  flat assembler source */
/*  Copyright (c) 1999-2001, Tomasz Grysztar */
/*  All rights reserved. */

assembler:
	mov labels_list, %edi
	mov memory_end, %ecx
	sub %edi, %ecx
	shr $2, %ecx
	xor %eax, %eax
	rep
	stosl
	movb $0, current_pass
	movl $0, number_of_sections
	movb $0, times_working
      assembler_loop:
	mov labels_list, %eax
	movl %eax, display_buffer
	mov additional_memory_end, %eax
	movl %eax, structures_buffer
	movb $0, next_pass_needed
	movb $0, output_format
	movl $0, format_flags
	movb $16, code_type
	movb $0, reloc_labels
	movb $0, virtual_data
	mov source_start, %esi
	mov code_start, %edi
	movl %edi, org_start
	movl $0, org_sib
	movl $0, error_line
	movl $0, counter
	movl $0, number_of_relocations
      pass_loop:
	call assemble_line
	jnc pass_loop
	mov structures_buffer, %eax
	cmp additional_memory_end, %eax
	jne unexpected_end_of_file
	jmp pass_done
      pass_done:
	cmpb $0, next_pass_needed
	je assemble_done
      next_pass:
	incb current_pass
	cmpb $100, current_pass
	jae code_cannot_be_generated
	jmp assembler_loop
      pass_error:
	movl %eax, current_line
	jmp *error
      assemble_done:
	mov error_line, %eax
	or %eax, %eax
	jnz pass_error
	call flush_display_buffer
      assemble_ok:
	mov %edi, %eax
	sub code_start, %eax
	movl %eax, real_code_size
	cmp undefined_data_end, %edi
	jne calculate_code_size
	mov undefined_data_start, %edi
      calculate_code_size:
	sub code_start, %edi
	movl %edi, code_size
	movl $0, written_size
	mov output_file, %edx
	call create
	jc write_failed
      write_code:
	mov code_start, %edx
	mov code_size, %ecx
	addl %ecx, written_size
	call write
	jc write_failed
	call close
	ret

assemble_line:
	mov display_buffer, %eax
	sub $0x100, %eax
	cmp %eax, %edi
	jae out_of_memory
	lodsb
	orb %al, %al
	jz source_end
	cmpb $1, %al
	je assemble_instruction
	cmpb $2, %al
	je define_label
	cmpb $3, %al
	je define_constant
	cmpb $0xf, %al
	je new_line
	cmpb $0x13, %al
	je code_type_setting
	cmpb $0x10, %al
	jne illegal_instruction
	lodsb
	movb %al, %ah
	shrb $4, %ah
	cmpb $6, %ah
	jne illegal_instruction
	andb $15, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, segment_register */
	.long segment_register
	call store_segment_prefix
	jmp assemble_line
      code_type_setting:
	lodsb
	.byte 0xa2  /*WORKAROUNDL movb %al, code_type */
	.long code_type
	jmp line_assembled
      new_line:
	lodsl
	movl %eax, current_line
	jmp assemble_line
      define_label:
	lodsl
	mov %eax, %ebx
	lodsb
	movb %al, %dl
	xorb %ch, %ch
	cmpb $0, reloc_labels
	je label_reloc_ok
	movb $2, %ch
      label_reloc_ok:
	xchgb 11(%ebx), %ch
	movb current_pass, %al
	testb $1, 8(%ebx)
	jz new_label
	cmpb 9(%ebx), %al
	je symbol_already_defined
	movb %al, 9(%ebx)
	mov %edi, %eax
	sub org_start, %eax
	xchg %eax, (%ebx)
	cltd
	xchg %edx, 4(%ebx)
	mov org_sib, %ebp
	xchg %ebp, 12(%ebx)
	cmpb $0, current_pass
	je assemble_line
	cmp (%ebx), %eax
	jne changed_label
	cmp 4(%ebx), %edx
	jne changed_label
	cmp 12(%ebx), %ebp
	jne changed_label
	cmpb 11(%ebx), %ch
	jne changed_label
	jmp assemble_line
      changed_label:
	orb $-1, next_pass_needed
	jmp assemble_line
      new_label:
	orb $1, 8(%ebx)
	movb %al, 9(%ebx)
	movb %dl, 10(%ebx)
	mov %edi, %eax
	sub org_start, %eax
	mov %eax, (%ebx)
	cltd
	movl %edx, 4(%ebx)
	mov org_sib, %eax
	mov %eax, 12(%ebx)
	jmp assemble_line
      define_constant:
	lodsl
	push %eax
	lodsb
	pushw %ax
	call get_value
	popw %bx
	movb %bl, %ch
	pop %ebx
      make_constant:
	movb current_pass, %cl
	testb $1, 8(%ebx)
	jz new_constant
	cmpb 9(%ebx), %cl
	jne redefine_constant
	testb $2, 8(%ebx)
	jz symbol_already_defined
	orb $4, 8(%ebx)
      redefine_constant:
	movb %cl, 9(%ebx)
	xchg %eax, (%ebx)
	xchg %edx, 4(%ebx)
	movb value_type, %cl
	xchgb %cl, 11(%ebx)
	cmpb $0, current_pass
	je assemble_line
	cmp (%ebx), %eax
	jne changed_constant
	cmp 4(%ebx), %edx
	jne changed_constant
	cmpb 11(%ebx), %cl
	jne changed_constant
	jmp assemble_line
      changed_constant:
	testb $4, 8(%ebx)
	jnz assemble_line
	orb $-1, next_pass_needed
	jmp assemble_line
      new_constant:
	orb $1+2, 8(%ebx)
	movw %cx, 9(%ebx)
	mov %eax, (%ebx)
	mov %edx, 4(%ebx)
	movb value_type, %cl
	movb %cl, 11(%ebx)
	jmp assemble_line
      assemble_instruction:
	movb $0, operand_size
	movb $0, forced_size
	lodsw
	movzwl %ax, %ebx
	add $assembler, %ebx
	lodsb
	jmp *%ebx
      instruction_assembled:
	movb (%esi), %al
	cmpb $0xf, %al
	je line_assembled
	orb %al, %al
	jnz extra_characters_on_line
      line_assembled:
	clc
	ret
      source_end:
	stc
	ret
skip_line:
	call skip_symbol
	jnc skip_line
	ret
skip_symbol:
	lodsb
	orb %al, %al
	jz nothing_to_skip
	cmpb $0xf, %al
	je nothing_to_skip
	cmpb $1, %al
	je skip_instruction
	cmpb $2, %al
	je skip_label
	cmpb $3, %al
	je skip_label
	cmpb $0x20, %al
	jb skip_assembler_symbol
	cmpb $0x28, %al
	je skip_expression
	cmpb $0x5b, %al
	je skip_address
      skip_done:
	clc
	ret
      skip_label:
	add $2, %esi
      skip_instruction:
	add $2, %esi
      skip_assembler_symbol:
	inc %esi
	jmp skip_done
      skip_address:
	movb (%esi), %al
	andb $240, %al
	cmpb $0x60, %al
	jb skip_expression
	cmpb $0x70, %al
	ja skip_expression
	inc %esi
	jmp skip_address
      skip_expression:
	lodsb
	orb %al, %al
	jz skip_string
	cmpb $0x2e, %al
	je skip_fp_value
	cmpb $0x29, %al
	je skip_done
	cmpb $0x5d, %al
	je skip_done
	cmpb $0xf, %al
	je skip_expression
	cmpb $0x10, %al
	je skip_register
	cmpb $0x11, %al
	je skip_label_value
	cmpb $0x80, %al
	jae skip_expression
	movzbl %al, %eax
	add %eax, %esi
	jmp skip_expression
      skip_label_value:
	add $3, %esi
      skip_register:
	inc %esi
	jmp skip_expression
      skip_fp_value:
	add $12, %esi
	jmp skip_done
      skip_string:
	lodsl
	add %eax, %esi
	inc %esi
	jmp skip_done
      nothing_to_skip:
	dec %esi
	stc
	ret

org_directive:
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	movb $0, reloc_labels
	movb value_type, %dl
	orb %dl, %dl
	jz org_ok
	cmpb $2, %dl
	jne invalid_use_of_symbol
	orb $-1, reloc_labels
      org_ok:
	mov %edi, %ecx
	sub %eax, %ecx
	movl %ecx, org_start
	movl $0, org_sib
	jmp instruction_assembled
label_directive:
	lodsb
	cmpb $2, %al
	jne invalid_argument
	lodsl
	inc %esi
	mov %eax, %ebx
	xorb %ch, %ch
	cmpb $0x11, (%esi)
	jne label_size_ok
	lodsw
	movb %ah, %ch
      label_size_ok:
	mov %edi, %eax
	sub org_start, %eax
	mov org_sib, %ebp
	cmpb $0x80, (%esi)
	jne define_free_label
	inc %esi
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	movb $0, 11(%ebx)
	push %ebx
	pushw %cx
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_address_value
	orb %bh, %bh
	setnz %ch
	xchgb %cl, %ch
	movw %cx, %bp
	shl $16, %ebp
	movb %bh, %bl
	movw %bx, %bp
	popw %cx
	pop %ebx
	movb %al, %dl
	movb value_type, %dh
	cmpb $1, %dh
	je invalid_use_of_symbol
	jb free_label_reloc_ok
      define_free_label:
	xorb %dh, %dh
	cmpb $0, reloc_labels
	je free_label_reloc_ok
	movb $2, %dh
      free_label_reloc_ok:
	xchgb 11(%ebx), %dh
	movb current_pass, %cl
	testb $1, 8(%ebx)
	jz new_free_label
	cmpb 9(%ebx), %cl
	je symbol_already_defined
	movb %dh, %ch
	movb %cl, 9(%ebx)
	xchg %eax, (%ebx)
	cltd
	xchg %edx, 4(%ebx)
	xchg %ebp, 12(%ebx)
	cmpb $0, current_pass
	je instruction_assembled
	cmp (%ebx), %eax
	jne changed_free_label
	cmp 4(%ebx), %edx
	jne changed_free_label
	cmp 12(%ebx), %ebp
	jne changed_free_label
	cmpb 11(%ebx), %ch
	jne changed_free_label
	jmp instruction_assembled
      changed_free_label:
	orb $-1, next_pass_needed
	jmp instruction_assembled
      new_free_label:
	orb $1, 8(%ebx)
	movb %cl, 9(%ebx)
	movb %ch, 10(%ebx)
	mov %eax, (%ebx)
	cltd
	movl %edx, 4(%ebx)
	mov %ebp, 12(%ebx)
	jmp instruction_assembled
load_directive:
	lodsb
	cmpb $2, %al
	jne invalid_argument
	lodsl
	inc %esi
	push %eax
	movb $1, %al
	cmpb $0x11, (%esi)
	jne load_size_ok
	lodsb
	lodsb
      load_size_ok:
	cmpb $8, %al
	ja invalid_value
	.byte 0xa2  /*WORKAROUNDL movb %al, operand_size */
	.long operand_size
	lodsb
	cmpb $0x82, %al
	jne invalid_argument
	lodsw
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $'(, %ax */
	.value 0x28
	jne invalid_argument
	lea 4(%esi), %edx
	mov (%esi), %eax
	lea 4+1(%esi,%eax,1), %esi
	call open
	jc file_not_found
	movb $2, %al
	xor %edx, %edx
	call lseek
	xor %edx, %edx
	cmpb $0x3a, (%esi)
	jne load_position_ok
	inc %esi
	cmpb $0x28, (%esi)
	jne invalid_argument
	inc %esi
	cmpb $0x2e, (%esi)
	je invalid_value
	push %ebx
	call get_dword_value
	pop %ebx
	mov %eax, %edx
      load_position_ok:
	xorb %al, %al
	call lseek
	movl $0, value
	movl $0, value+4
	movzbl operand_size, %ecx
	mov $value, %edx
	call read
	jc error_reading_file
	call close
	mov value, %eax
	mov value+4, %edx
	pop %ebx
	xorb %ch, %ch
	movb $0, value_type
	jmp make_constant
display_directive:
	push %esi
	push %edi
      prepare_display:
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	cmpb $0, (%esi)
	jne display_byte
	inc %esi
	lodsl
	mov %eax, %ecx
	rep
	movsb
	inc %esi
	jmp display_next
      display_byte:
	call get_byte_value
	stosb
      display_next:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb
	orb %al, %al
	jz do_display
	cmpb $0xf, %al
	je do_display
	cmpb $44, %al
	jne extra_characters_on_line
	jmp prepare_display
      do_display:
	dec %esi
	mov %edi, %ebp
	pop %edi
	pop %ebx
	push %esi
	push %edi
	mov %edi, %esi
	mov %ebp, %ecx
	sub %esi, %ecx
	mov display_buffer, %edi
	sub %ecx, %edi
	sub $4, %edi
	cmp %esi, %edi
	jbe out_of_memory
	movl %edi, display_buffer
	mov %ecx, %eax
	rep
	movsb
	stosl
	pop %edi
	pop %esi
	jmp instruction_assembled
flush_display_buffer:
	mov display_buffer, %eax
	or %eax, %eax
	jz display_done
	mov labels_list, %esi
	cmp %eax, %esi
	je display_done
	movw $0, value
      display_messages:
	sub $4, %esi
	mov (%esi), %ecx
	movw value, %ax
	jcxz last_bytes_ok
	movb %ah, %al
	movb -1(%esi), %ah
	cmp $1, %ecx
	je last_bytes_ok
	movb -2(%esi), %al
      last_bytes_ok:
	movw %ax, value
	sub %ecx, %esi
	push %esi
	call display_block
	pop %esi
	cmp display_buffer, %esi
	jne display_messages
	.byte 0x66, 0xb8  /*WORKAROUNDW mov $0xa0d, %ax */
	.value 0xa0d
	cmpw %ax, value
	je display_ok
	mov $value, %esi
	movw %ax, (%esi)
	mov $2, %ecx
	call display_block
      display_ok:
	mov labels_list, %eax
	movl %eax, display_buffer
      display_done:
	ret
times_directive:
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	or %eax, %eax
	jz zero_times
	cmpb $0x3a, (%esi)
	jne times_argument_ok
	inc %esi
      times_argument_ok:
	pushl counter
	pushl counter_limit
	movl %eax, counter_limit
	movl $1, counter
      times_loop:
	push %esi
	orb $-1, times_working
	call assemble_line
	mov counter_limit, %eax
	cmpl %eax, counter
	je times_done
	incl counter
	pop %esi
	jmp times_loop
      times_done:
	movb $0, times_working
	pop %eax
	popl counter_limit
	popl counter
	jmp instruction_assembled
      zero_times:
	call skip_line
	jmp instruction_assembled

virtual_directive:
	lodsb
	cmpb $0x80, %al
	jne virtual_at_current
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_address_value
	xorb %ch, %ch
	orb %bh, %bh
	jz set_virtual
	movb $1, %ch
	jmp set_virtual
      virtual_at_current:
	dec %esi
	mov %edi, %eax
	sub org_start, %eax
	xorw %bx, %bx
	xorw %cx, %cx
	movb $0, value_type
	cmpb $0, reloc_labels
	je set_virtual
	movb $2, value_type
      set_virtual:
	mov org_sib, %edx
	movb %bh, org_sib
	movb %bl, org_sib+1
	movb %ch, org_sib+2
	movb %cl, org_sib+3
	call allocate_structure_data
	.value 0x6690, 0x03c7, --virtual_directive___assembler  /* movw $--..., (%ebx) */
	neg %eax
	add %edi, %eax
	xchgl %eax, org_start
	mov %eax, 4(%ebx)
	mov %edx, 8(%ebx)
	movb virtual_data, %al
	movb %al, 2(%ebx)
	movb reloc_labels, %al
	movb %al, 3(%ebx)
	mov %edi, 0xc(%ebx)
	orb $-1, virtual_data
	movb $0, reloc_labels
	cmpb $1, value_type
	je invalid_use_of_symbol
	cmpb $2, value_type
	jne instruction_assembled
	orb $-1, reloc_labels
	jmp instruction_assembled
      allocate_structure_data:
	mov structures_buffer, %ebx
	sub $0x10, %ebx
	cmp additional_memory, %ebx
	jb out_of_memory
	movl %ebx, structures_buffer
	ret
      find_structure_data:
	mov structures_buffer, %ebx
      scan_structures:
	cmp additional_memory_end, %ebx
	je no_such_structure
	cmpw (%ebx), %ax
	jne next_structure
	clc
	ret
      next_structure:
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $repeat_directive-assembler, %ax */
	.value --repeat_directive___assembler
	jne if_structure_ok
	.value 0x6690, 0x3b81, --if_directive___assembler  /* cmpw $--..., (%ebx) */
	je no_such_structure
      if_structure_ok:
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $if_directive-assembler, %ax */
	.value --if_directive___assembler
	jne repeat_structure_ok
	.value 0x6690, 0x3b81, --repeat_directive___assembler  /* cmpw $--..., (%ebx) */
	je no_such_structure
      repeat_structure_ok:
	add $0x10, %ebx
	jmp scan_structures
      no_such_structure:
	stc
	ret
      end_virtual:
	call find_structure_data
	jc unexpected_instruction
	movb 2(%ebx), %al
	.byte 0xa2  /*WORKAROUNDL movb %al, virtual_data */
	.long virtual_data
	movb 3(%ebx), %al
	.byte 0xa2  /*WORKAROUNDL movb %al, reloc_labels */
	.long reloc_labels
	mov 4(%ebx), %eax
	movl %eax, org_start
	mov 8(%ebx), %eax
	movl %eax, org_sib
	mov 0xc(%ebx), %edi
      remove_structure_data:
	push %esi
	push %edi
	mov structures_buffer, %esi
	mov %ebx, %ecx
	sub %esi, %ecx
	lea 0x10(%esi), %edi
	movl %edi, structures_buffer
	shr $2, %ecx
	rep
	movsl
	pop %edi
	pop %esi
	jmp instruction_assembled
repeat_directive:
	cmpb $0, times_working
	jne unexpected_instruction
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	or %eax, %eax
	jz zero_repeat
	call allocate_structure_data
	.value 0x6690, 0x03c7, --repeat_directive___assembler  /* movw $--..., (%ebx) */
	xchg counter_limit, %eax
	mov %eax, 4(%ebx)
	mov $1, %eax
	xchg counter, %eax
	mov %eax, 8(%ebx)
	mov %esi, 0xc(%ebx)
	jmp instruction_assembled
      end_repeat:
	cmpb $0, times_working
	jne unexpected_instruction
	call find_structure_data
	jc unexpected_instruction
	mov counter_limit, %eax
	incl counter
	cmpl %eax, counter
	jbe continue_repeating
	mov 4(%ebx), %eax
	movl %eax, counter_limit
	mov 8(%ebx), %eax
	movl %eax, counter
	jmp remove_structure_data
      continue_repeating:
	mov 0xc(%ebx), %esi
	jmp instruction_assembled
      zero_repeat:
	movb (%esi), %al
	orb %al, %al
	jz unexpected_end_of_file
	cmpb $0xf, %al
	jne extra_characters_on_line
	call find_end_repeat
	jmp instruction_assembled
      find_end_repeat:
	call find_structure_end
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $repeat_directive-assembler, %ax */
	.value --repeat_directive___assembler
	jne unexpected_instruction
	ret
      find_structure_end:
	call skip_line
	lodsb
	cmpb $0xf, %al
	jne unexpected_end_of_file
	lodsl
	movl %eax, current_line
      skip_labels:
	cmpb $2, (%esi)
	jne labels_ok
	add $6, %esi
	jmp skip_labels
      labels_ok:
	cmpb $1, (%esi)
	jne find_structure_end
	movw 1(%esi), %ax
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $prefix_instruction-assembler, %ax */
	.value --prefix_instruction___assembler
	je find_structure_end
	add $4, %esi
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $repeat_directive-assembler, %ax */
	.value --repeat_directive___assembler
	je skip_repeat
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $if_directive-assembler, %ax */
	.value --if_directive___assembler
	je skip_if
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $else_directive-assembler, %ax */
	.value --else_directive___assembler
	je structure_end
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $end_directive-assembler, %ax */
	.value --end_directive___assembler
	jne find_structure_end
	cmpb $1, (%esi)
	jne find_structure_end
	movw 1(%esi), %ax
	add $4, %esi
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $repeat_directive-assembler, %ax */
	.value --repeat_directive___assembler
	je structure_end
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $if_directive-assembler, %ax */
	.value --if_directive___assembler
	jne find_structure_end
      structure_end:
	ret
      skip_repeat:
	call find_end_repeat
	jmp find_structure_end
if_directive:
	cmpb $0, times_working
	jne unexpected_instruction
	call calculate_logical_expression
	movb %al, %dl
	movb (%esi), %al
	orb %al, %al
	jz unexpected_end_of_file
	cmpb $0xf, %al
	jne extra_characters_on_line
	orb %dl, %dl
	jnz if_true
	call find_else
	jc instruction_assembled
	movb (%esi), %al
	cmpb $1, %al
	jne else_true
	.value 0x8166, 0x017e, --if_directive___assembler  /* cmpw $--..., 1(%esi) */
	jne else_true
	add $4, %esi
	jmp if_directive
      if_true:
	call allocate_structure_data
	.value 0x6690, 0x03c7, --if_directive___assembler  /* movw $--..., (%ebx) */
	movb $0, 2(%ebx)
	jmp instruction_assembled
      else_true:
	orb %al, %al
	jz unexpected_end_of_file
	cmpb $0xf, %al
	jne extra_characters_on_line
	call allocate_structure_data
	.value 0x6690, 0x03c7, --if_directive___assembler  /* movw $--..., (%ebx) */
	orb $-1, 2(%ebx)
	jmp instruction_assembled
      else_directive:
	cmpb $0, times_working
	jne unexpected_instruction
	.byte 0x66, 0xb8  /*WORKAROUNDW mov $if_directive-assembler, %ax */
	.value --if_directive___assembler
	call find_structure_data
	jc unexpected_instruction
	cmpb $0, 2(%ebx)
	jne unexpected_instruction
      found_else:
	movb (%esi), %al
	cmpb $1, %al
	jne skip_else
	.value 0x8166, 0x017e, --if_directive___assembler  /* cmpw $--..., 1(%esi) */
	jne skip_else
	add $4, %esi
	call find_else
	jnc found_else
	jmp remove_structure_data
      skip_else:
	orb %al, %al
	jz unexpected_end_of_file
	cmpb $0xf, %al
	jne extra_characters_on_line
	call find_end_if
	jmp remove_structure_data
      end_if:
	cmpb $0, times_working
	jne unexpected_instruction
	call find_structure_data
	jc unexpected_instruction
	jmp remove_structure_data
      skip_if:
	call find_else
	jc find_structure_end
	cmpb $1, (%esi)
	jne skip_after_else
	.value 0x8166, 0x017e, --if_directive___assembler  /* cmpw $--..., 1(%esi) */
	jne skip_after_else
	add $4, %esi
	jmp skip_if
      skip_after_else:
	call find_end_if
	jmp find_structure_end
      find_else:
	call find_structure_end
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $else_directive-assembler, %ax */
	.value --else_directive___assembler
	je else_found
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $if_directive-assembler, %ax */
	.value --if_directive___assembler
	jne unexpected_instruction
	stc
	ret
      else_found:
	clc
	ret
      find_end_if:
	call find_structure_end
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $if_directive-assembler, %ax */
	.value --if_directive___assembler
	jne unexpected_instruction
	ret
end_directive:
	lodsb
	cmpb $1, %al
	jne invalid_argument
	lodsw
	inc %esi
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $virtual_directive-assembler, %ax */
	.value --virtual_directive___assembler
	je end_virtual
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $repeat_directive-assembler, %ax */
	.value --repeat_directive___assembler
	je end_repeat
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $if_directive-assembler, %ax */
	.value --if_directive___assembler
	je end_if
	jmp invalid_argument

data_bytes:
	lodsb
	cmpb $0x28, %al
	je get_byte
	cmpb $0x3f, %al
	jne invalid_argument
	mov %edi, %eax
	movb $0, (%edi)
	inc %edi
	call undefined_data
	jmp byte_ok
      get_byte:
	cmpb $0, (%esi)
	je get_string
	call get_byte_value
	stosb
      byte_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb
	orb %al, %al
	jz data_end
	cmpb $0xf, %al
	je data_end
	cmpb $44, %al
	jne extra_characters_on_line
	jmp data_bytes
      data_end:
	dec %esi
	jmp instruction_assembled
      get_string:
	inc %esi
	lodsl
	mov %eax, %ecx
	rep
	movsb
	inc %esi
	jmp byte_ok
      undefined_data:
	cmpb $0, virtual_data
	je mark_undefined_data
	ret
      mark_undefined_data:
	cmp undefined_data_end, %eax
	je undefined_data_ok
	movl %eax, undefined_data_start
      undefined_data_ok:
	movl %edi, undefined_data_end
	ret
data_unicode:
	orb $-1, base_code
	jmp get_words_data
data_words:
	movb $0, base_code
      get_words_data:
	lodsb
	cmpb $0x28, %al
	je get_word
	cmpb $0x3f, %al
	jne invalid_argument
	mov %edi, %eax
	movw $0, (%edi)
	scasw
	call undefined_data
	jmp word_ok
      get_word:
	cmpb $0, base_code
	je word_data_value
	cmpb $0, (%esi)
	je word_string
      word_data_value:
	call get_word_value
	call mark_relocation
	stosw
      word_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb
	orb %al, %al
	jz data_end
	cmpb $0xf, %al
	je data_end
	cmpb $44, %al
	jne extra_characters_on_line
	jmp get_words_data
      word_string:
	inc %esi
	lodsl
	mov %eax, %ecx
	jcxz word_string_ok
	xorb %ah, %ah
      copy_word_string:
	lodsb
	stosw
	loop copy_word_string
      word_string_ok:
	inc %esi
	jmp word_ok
data_dwords:
	lodsb
	cmpb $0x28, %al
	je get_dword
	cmpb $0x3f, %al
	jne invalid_argument
	mov %edi, %eax
	movl $0, (%edi)
	scasl
	call undefined_data
	jmp dword_ok
      get_dword:
	push %esi
	call get_dword_value
	pop %ebx
	cmpb $0x3a, (%esi)
	je complex_dword
	call mark_relocation
	stosl
	jmp dword_ok
      complex_dword:
	mov %ebx, %esi
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_word_value
	movw %ax, %dx
	inc %esi
	lodsb
	cmpb $0x28, %al
	jne invalid_operand
	movb value_type, %al
	pushw %ax
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_word_value
	call mark_relocation
	stosw
	popw %ax
	.byte 0xa2  /*WORKAROUNDL movb %al, value_type */
	.long value_type
	movw %dx, %ax
	call mark_relocation
	stosw
      dword_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb
	orb %al, %al
	jz data_end
	cmpb $0xf, %al
	je data_end
	cmpb $44, %al
	jne extra_characters_on_line
	jmp data_dwords
data_pwords:
	lodsb
	cmpb $0x28, %al
	je get_pword
	cmpb $0x3f, %al
	jne invalid_argument
	mov %edi, %eax
	movl $0, (%edi)
	scasl
	movw $0, (%edi)
	scasw
	call undefined_data
	jmp pword_ok
      get_pword:
	push %esi
	call get_pword_value
	pop %ebx
	cmpb $0x3a, (%esi)
	je complex_pword
	call mark_relocation
	stosl
	movw %dx, %ax
	stosw
	jmp pword_ok
      complex_pword:
	mov %ebx, %esi
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_word_value
	movw %ax, %dx
	inc %esi
	lodsb
	cmpb $0x28, %al
	jne invalid_operand
	movb value_type, %al
	pushw %ax
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	call mark_relocation
	stosl
	popw %ax
	.byte 0xa2  /*WORKAROUNDL movb %al, value_type */
	.long value_type
	movw %dx, %ax
	call mark_relocation
	stosw
      pword_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb
	orb %al, %al
	jz data_end
	cmpb $0xf, %al
	je data_end
	cmpb $44, %al
	jne extra_characters_on_line
	jmp data_pwords
data_qwords:
	lodsb
	cmpb $0x28, %al
	je get_qword
	cmpb $0x3f, %al
	jne invalid_argument
	mov %edi, %eax
	movl $0, (%edi)
	scasl
	movl $0, (%edi)
	scasl
	call undefined_data
	jmp qword_ok
      get_qword:
	call get_qword_value
	call mark_relocation
	stosl
	mov %edx, %eax
	stosl
      qword_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb
	orb %al, %al
	jz data_end
	cmpb $0xf, %al
	je data_end
	cmpb $44, %al
	jne extra_characters_on_line
	jmp data_qwords
data_twords:
	lodsb
	cmpb $0x28, %al
	je get_tbyte
	cmpb $0x3f, %al
	jne invalid_argument
	mov %edi, %eax
	movl $0, (%edi)
	scasl
	movl $0, (%edi)
	scasl
	movw $0, (%edi)
	scasw
	call undefined_data
	jmp tbyte_ok
      get_tbyte:
	lodsb
	cmpb $0x2e, %al
	jne invalid_value
	cmpw $0x8000, 8(%esi)
	je fp_zero_tbyte
	mov (%esi), %eax
	stosl
	mov 4(%esi), %eax
	stosl
	movw 8(%esi), %ax
	addw $0x3fff, %ax
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $0x8000, %ax */
	.value 0x8000
	jae value_out_of_range
	movb 11(%esi), %bl
	shlw $15, %bx
	orw %bx, %ax
	stosw
	add $12, %esi
	jmp tbyte_ok
      fp_zero_tbyte:
	xor %eax, %eax
	stosl
	stosl
	stosw
	add $12, %esi
      tbyte_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb
	orb %al, %al
	jz data_end
	cmpb $0xf, %al
	je data_end
	cmpb $44, %al
	jne extra_characters_on_line
	jmp data_twords
data_file:
	lodsw
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $'(, %ax */
	.value 0x28
	jne invalid_argument
	lea 4(%esi), %edx
	mov (%esi), %eax
	lea 4+1(%esi,%eax,1), %esi
	call open
	jc file_not_found
	movb $2, %al
	xor %edx, %edx
	call lseek
	push %eax
	xor %edx, %edx
	cmpb $0x3a, (%esi)
	jne position_ok
	inc %esi
	cmpb $0x28, (%esi)
	jne invalid_argument
	inc %esi
	cmpb $0x2e, (%esi)
	je invalid_value
	push %ebx
	call get_dword_value
	pop %ebx
	mov %eax, %edx
	sub %edx, (%esp)
      position_ok:
	cmpb $44, (%esi)
	jne size_ok
	inc %esi
	cmpb $0x28, (%esi)
	jne invalid_argument
	inc %esi
	cmpb $0x2e, (%esi)
	je invalid_value
	push %ebx
	push %edx
	call get_dword_value
	pop %edx
	pop %ebx
	mov %eax, (%esp)
      size_ok:
	cmpb $0, next_pass_needed
	jne file_reserve
	xorb %al, %al
	call lseek
	pop %ecx
	mov %edi, %edx
	add %ecx, %edi
	jc out_of_memory
	cmp display_buffer, %edi
	jae out_of_memory
	call read
	jc error_reading_file
	call close
      check_for_next_name:
	lodsb
	cmpb $44, %al
	je data_file
	dec %esi
	jmp instruction_assembled
      file_reserve:
	call close
	pop %ecx
	add %ecx, %edi
	jc out_of_memory
	cmp display_buffer, %edi
	jae out_of_memory
	jmp check_for_next_name
reserve_bytes:
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	cmp $0, %eax
	jl reserve_negative
	mov %eax, %ecx
	mov %ecx, %edx
	add %edi, %edx
	jc out_of_memory
	cmp display_buffer, %edx
	jae out_of_memory
	push %edi
	cmpb $0, next_pass_needed
	je zero_bytes
	add %ecx, %edi
	jmp reserved_data
      zero_bytes:
	xor %eax, %eax
	shr %ecx
	jnc bytes_stosb_ok
	stosb
      bytes_stosb_ok:
	shr %ecx
	jnc bytes_stosw_ok
	stosw
      bytes_stosw_ok:
	rep
	stosl
      reserved_data:
	pop %eax
	call undefined_data
	jmp instruction_assembled
      reserve_negative:
	cmpl $0, error_line
	jne instruction_assembled
	mov current_line, %eax
	movl %eax, error_line
	movl $invalid_value, error
	jmp instruction_assembled
reserve_words:
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	cmp $0, %eax
	jl reserve_negative
	mov %eax, %ecx
	mov %ecx, %edx
	shl %edx
	jc out_of_memory
	add %edi, %edx
	jc out_of_memory
	cmp display_buffer, %edx
	jae out_of_memory
	push %edi
	cmpb $0, next_pass_needed
	je zero_words
	lea (%edi,%ecx,2), %edi
	jmp reserved_data
      zero_words:
	xor %eax, %eax
	shr %ecx
	jnc words_stosw_ok
	stosw
      words_stosw_ok:
	rep
	stosl
	jmp reserved_data
reserve_dwords:
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	cmp $0, %eax
	jl reserve_negative
	mov %eax, %ecx
	mov %ecx, %edx
	shl %edx
	jc out_of_memory
	shl %edx
	jc out_of_memory
	add %edi, %edx
	jc out_of_memory
	cmp display_buffer, %edx
	jae out_of_memory
	push %edi
	cmpb $0, next_pass_needed
	je zero_dwords
	lea (%edi,%ecx,4), %edi
	jmp reserved_data
      zero_dwords:
	xor %eax, %eax
	rep
	stosl
	jmp reserved_data
reserve_pwords:
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	cmp $0, %eax
	jl reserve_negative
	mov %eax, %ecx
	shl %ecx
	jc out_of_memory
	add %eax, %ecx
	mov %ecx, %edx
	shl %edx
	jc out_of_memory
	add %edi, %edx
	jc out_of_memory
	cmp display_buffer, %edx
	jae out_of_memory
	push %edi
	cmpb $0, next_pass_needed
	je zero_words
	lea (%edi,%ecx,2), %edi
	jmp reserved_data
reserve_qwords:
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	cmp $0, %eax
	jl reserve_negative
	mov %eax, %ecx
	shl %ecx
	jc out_of_memory
	mov %ecx, %edx
	shl %edx
	jc out_of_memory
	shl %edx
	jc out_of_memory
	add %edi, %edx
	jc out_of_memory
	cmp display_buffer, %edx
	jae out_of_memory
	push %edi
	cmpb $0, next_pass_needed
	je zero_dwords
	lea (%edi,%ecx,4), %edi
	jmp reserved_data
reserve_twords:
	lodsb
	cmpb $0x28, %al
	jne invalid_argument
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	cmp $0, %eax
	jl reserve_negative
	mov %eax, %ecx
	shl $2, %ecx
	jc out_of_memory
	add %eax, %ecx
	mov %ecx, %edx
	shl %edx
	jc out_of_memory
	add %edi, %edx
	jc out_of_memory
	cmp display_buffer, %edx
	jae out_of_memory
	push %edi
	cmpb $0, next_pass_needed
	je zero_words
	lea (%edi,%ecx,2), %edi
	jmp reserved_data

simple_instruction:
	stosb
	jmp instruction_assembled
simple_instruction_16bit:
	cmpb $32, code_type
	je size_prefix
	stosb
	jmp instruction_assembled
      size_prefix:
	movb %al, %ah
	movb $0x66, %al
	stosw
	jmp instruction_assembled
simple_instruction_32bit:
	cmpb $16, code_type
	je size_prefix
	stosb
	jmp instruction_assembled
simple_extended_instruction:
	movb %al, %ah
	movb $0xf, %al
	stosw
	jmp instruction_assembled
prefix_instruction:
	stosb
	jmp assemble_line
int_instruction:
	lodsb
	call get_size_operator
	cmpb $1, %ah
	ja invalid_operand_size
	cmpb $0x28, %al
	jne invalid_operand
	call get_byte_value
	movb %al, %ah
	movb $0xcd, %al
	stosw
	jmp instruction_assembled
aa_instruction:
	pushw %ax
	movb $10, %bl
	cmpb $0x28, (%esi)
	jne aa_instruction.store
	inc %esi
	xorb %al, %al
	xchgb operand_size, %al
	cmpb $1, %al
	ja invalid_operand_size
	call get_byte_value
	movb %al, %bl
      aa_instruction.store:
	cmpb $0, operand_size
	jne invalid_operand
	popw %ax
	movb %bl, %ah
	stosw
	jmp instruction_assembled

basic_instruction:
	.byte 0xa2  /*WORKAROUNDL movb %al, base_code */
	.long base_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je basic_reg
	cmpb $0x5b, %al
	jne invalid_operand
      basic_mem:
	call get_address
	push %edx
	pushw %bx
	pushw %cx
	lodsb
	cmpb $44, %al
	jne invalid_operand
	cmpb $0x11, (%esi)
	sete %al
	.byte 0xa2  /*WORKAROUNDL movb %al, imm_sized */
	.long imm_sized
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	je basic_mem_imm
	cmpb $0x10, %al
	jne invalid_operand
      basic_mem_reg:
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	popw %cx
	popw %bx
	pop %edx
	movb %ah, %al
	cmpb $1, %al
	je basic_mem_reg_8bit
	cmpb $2, %al
	je basic_mem_reg_16bit
	cmpb $4, %al
	je basic_mem_reg_32bit
	jmp invalid_operand_size
      basic_mem_reg_8bit:
	call store_instruction
	jmp instruction_assembled
      basic_mem_reg_16bit:
	call operand_16bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
      basic_mem_reg_32bit:
	call operand_32bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
      basic_mem_imm:
	movb operand_size, %al
	cmpb $1, %al
	je basic_mem_imm_8bit
	cmpb $2, %al
	je basic_mem_imm_16bit
	cmpb $4, %al
	je basic_mem_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp basic_mem_imm_32bit
      basic_mem_imm_8bit:
	call get_byte_value
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	movb base_code, %al
	shrb $3, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	popw %cx
	popw %bx
	pop %edx
	movb $0x80, base_code
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      basic_mem_imm_16bit:
	call get_word_value
	movw %ax, value
	movb base_code, %al
	shrb $3, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	call operand_16bit_prefix
	popw %cx
	popw %bx
	pop %edx
	cmpb $0, value_type
	jne basic_mem_imm_16bit.store
	cmpb $0, imm_sized
	jne basic_mem_imm_16bit.store
	cmpw $0x80, value
	jb basic_mem_simm_8bit
	cmpw $-0x80, value
	jae basic_mem_simm_8bit
      basic_mem_imm_16bit.store:
	movb $0x81, base_code
	call store_instruction
	movw value, %ax
	call mark_relocation
	stosw
	jmp instruction_assembled
      basic_mem_simm_8bit:
	movb $0x83, base_code
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      basic_mem_imm_32bit:
	call get_dword_value
	movl %eax, value
	movb base_code, %al
	shrb $3, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	call operand_32bit_prefix
	popw %cx
	popw %bx
	pop %edx
	cmpb $0, value_type
	jne basic_mem_imm_32bit.store
	cmpb $0, imm_sized
	jne basic_mem_imm_32bit.store
	cmpl $0x80, value
	jb basic_mem_simm_8bit
	cmpl $-0x80, value
	jae basic_mem_simm_8bit
      basic_mem_imm_32bit.store:
	movb $0x81, base_code
	call store_instruction
	mov value, %eax
	call mark_relocation
	stosl
	jmp instruction_assembled
      basic_reg:
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	cmpb $0x11, (%esi)
	sete %al
	.byte 0xa2  /*WORKAROUNDL movb %al, imm_sized */
	.long imm_sized
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je basic_reg_reg
	cmpb $0x28, %al
	je basic_reg_imm
	cmpb $0x5b, %al
	jne invalid_operand
      basic_reg_mem:
	call get_address
	movb operand_size, %al
	cmpb $1, %al
	je basic_reg_mem_8bit
	cmpb $2, %al
	je basic_reg_mem_16bit
	cmpb $4, %al
	je basic_reg_mem_32bit
	jmp invalid_operand_size
      basic_reg_mem_8bit:
	addb $2, base_code
	call store_instruction
	jmp instruction_assembled
      basic_reg_mem_16bit:
	call operand_16bit_prefix
	addb $3, base_code
	call store_instruction
	jmp instruction_assembled
      basic_reg_mem_32bit:
	call operand_32bit_prefix
	addb $3, base_code
	call store_instruction
	jmp instruction_assembled
      basic_reg_reg:
	lodsb
	call convert_register
	shlb $3, %al
	movb postbyte_register, %bl
	orb %al, %bl
	orb $192, %bl
	movb %ah, %al
	cmpb $1, %al
	je basic_reg_reg_8bit
	cmpb $2, %al
	je basic_reg_reg_16bit
	cmpb $4, %al
	je basic_reg_reg_32bit
	jmp invalid_operand_size
      basic_reg_reg_32bit:
	call operand_32bit_prefix
	incb base_code
	jmp basic_reg_reg_8bit
      basic_reg_reg_16bit:
	call operand_16bit_prefix
	incb base_code
      basic_reg_reg_8bit:
	movb base_code, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      basic_reg_imm:
	movb operand_size, %al
	cmpb $1, %al
	je basic_reg_imm_8bit
	cmpb $2, %al
	je basic_reg_imm_16bit
	cmpb $4, %al
	je basic_reg_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp basic_reg_imm_32bit
      basic_reg_imm_8bit:
	call get_byte_value
	movb %al, %dl
	movb base_code, %ah
	orb $192, %ah
	movb postbyte_register, %bl
	andb $7, %bl
	orb %bl, %bl
	jz basic_al_imm
	orb %bl, %ah
	movb $0x80, %al
	stosw
	movb %dl, %al
	stosb
	jmp instruction_assembled
      basic_al_imm:
	movb base_code, %al
	addb $4, %al
	stosb
	movb %dl, %al
	stosb
	jmp instruction_assembled
      basic_reg_imm_16bit:
	call get_word_value
	movw %ax, %dx
	call operand_16bit_prefix
	movb base_code, %ah
	orb $192, %ah
	movb postbyte_register, %bl
	andb $7, %bl
	orb %bl, %ah
	cmpb $0, value_type
	jne basic_reg_imm_16bit.store
	cmpb $0, imm_sized
	jne basic_reg_imm_16bit.store
	cmpw $0x80, %dx
	jb basic_reg_simm_8bit
	cmpw $-0x80, %dx
	jae basic_reg_simm_8bit
      basic_reg_imm_16bit.store:
	orb %bl, %bl
	jz basic_ax_imm
	movb $0x81, %al
	stosw
	movw %dx, %ax
	call mark_relocation
	stosw
	jmp instruction_assembled
      basic_reg_simm_8bit:
	movb $0x83, %al
	stosw
	movw %dx, %ax
	stosb
	jmp instruction_assembled
      basic_ax_imm:
	movb base_code, %al
	addb $5, %al
	stosb
	movw %dx, %ax
	call mark_relocation
	stosw
	jmp instruction_assembled
      basic_reg_imm_32bit:
	call get_dword_value
	mov %eax, %edx
	call operand_32bit_prefix
	movb base_code, %ah
	orb $192, %ah
	movb postbyte_register, %bl
	andb $7, %bl
	orb %bl, %ah
	cmpb $0, value_type
	jne basic_reg_imm_32bit.store
	cmpb $0, imm_sized
	jne basic_reg_imm_32bit.store
	cmp $0x80, %edx
	jb basic_reg_simm_8bit
	cmp $-0x80, %edx
	jae basic_reg_simm_8bit
      basic_reg_imm_32bit.store:
	orb %bl, %bl
	jz basic_eax_imm
	movb $0x81, %al
	stosw
	mov %edx, %eax
	call mark_relocation
	stosl
	jmp instruction_assembled
      basic_eax_imm:
	movb base_code, %al
	addb $5, %al
	stosb
	mov %edx, %eax
	call mark_relocation
	stosl
	jmp instruction_assembled
single_operand_instruction:
	movb $0xf6, base_code
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je single_reg
	cmpb $0x5b, %al
	jne invalid_operand
      single_mem:
	call get_address
	movb operand_size, %al
	cmpb $1, %al
	je single_mem_8bit
	cmpb $2, %al
	je single_mem_16bit
	cmpb $4, %al
	je single_mem_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
      single_mem_8bit:
	call store_instruction
	jmp instruction_assembled
      single_mem_16bit:
	call operand_16bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
      single_mem_32bit:
	call operand_32bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
      single_reg:
	lodsb
	call convert_register
	movb postbyte_register, %bl
	shlb $3, %bl
	orb %al, %bl
	orb $192, %bl
	movb %ah, %al
	cmpb $1, %al
	je single_reg_8bit
	cmpb $2, %al
	je single_reg_16bit
	cmpb $4, %al
	je single_reg_32bit
	jmp invalid_operand_size
      single_reg_8bit:
	movb %bl, %ah
	movb $0xf6, %al
	stosw
	jmp instruction_assembled
      single_reg_16bit:
	call operand_16bit_prefix
	movb %bl, %ah
	movb $0xf7, %al
	stosw
	jmp instruction_assembled
      single_reg_32bit:
	call operand_32bit_prefix
	movb %bl, %ah
	movb $0xf7, %al
	stosw
	jmp instruction_assembled
mov_instruction:
	movb $0x88, base_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je mov_reg
	cmpb $0x5b, %al
	jne invalid_operand
      mov_mem:
	call get_address
	push %edx
	pushw %bx
	pushw %cx
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	je mov_mem_imm
	cmpb $0x10, %al
	jne invalid_operand
      mov_mem_reg:
	lodsb
	cmpb $0x60, %al
	jae mov_mem_sreg
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	popw %cx
	popw %bx
	pop %edx
	cmpb $1, %ah
	je mov_mem_reg_8bit
	cmpb $2, %ah
	je mov_mem_reg_16bit
	cmpb $4, %ah
	je mov_mem_reg_32bit
	jmp invalid_operand_size
      mov_mem_reg_8bit:
	orb %bl, %al
	orb %bh, %al
	jz mov_mem_al
	call store_instruction
	jmp instruction_assembled
      mov_mem_al:
	cmpb $2, %ch
	je mov_mem_address16_al
	testb $4, %ch
	jnz mov_mem_address32_al
	orb %ch, %ch
	jnz invalid_address_size
	cmpb $32, code_type
	je mov_mem_address32_al
	cmp $0x10000, %edx
	jb mov_mem_address16_al
      mov_mem_address32_al:
	call address_32bit_prefix
	call store_segment_prefix_if_necessary
	movb $0xa2, %al
      store_mov_address32:
	stosb
	pushl $instruction_assembled
	jmp store_address_32bit_value
      mov_mem_address16_al:
	call address_16bit_prefix
	call store_segment_prefix_if_necessary
	movb $0xa2, %al
      store_mov_address16:
	stosb
	mov %edx, %eax
	stosw
	cmp $0x10000, %edx
	jge value_out_of_range
	jmp instruction_assembled
      mov_mem_reg_16bit:
	call operand_16bit_prefix
	movb postbyte_register, %al
	orb %bl, %al
	orb %bh, %al
	jz mov_mem_ax
	incb base_code
	call store_instruction
	jmp instruction_assembled
      mov_mem_ax:
	cmpb $2, %ch
	je mov_mem_address16_ax
	testb $4, %ch
	jnz mov_mem_address32_ax
	orb %ch, %ch
	jnz invalid_address_size
	cmpb $32, code_type
	je mov_mem_address32_ax
	cmp $0x10000, %edx
	jb mov_mem_address16_ax
      mov_mem_address32_ax:
	call address_32bit_prefix
	call store_segment_prefix_if_necessary
	movb $0xa3, %al
	jmp store_mov_address32
      mov_mem_address16_ax:
	call address_16bit_prefix
	call store_segment_prefix_if_necessary
	movb $0xa3, %al
	jmp store_mov_address16
      mov_mem_reg_32bit:
	call operand_32bit_prefix
	movb postbyte_register, %al
	orb %bl, %al
	orb %bh, %al
	jz mov_mem_ax
	incb base_code
	call store_instruction
	jmp instruction_assembled
      mov_mem_sreg:
	cmpb $0x70, %al
	jae invalid_operand
	subb $0x61, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	popw %cx
	popw %bx
	pop %edx
	movb operand_size, %ah
	orb %ah, %ah
	jz mov_mem_sreg_size_ok
	cmpb $2, %ah
	je mov_mem16_sreg
	cmpb $4, %ah
	je mov_mem32_sreg
	jmp invalid_operand_size
      mov_mem32_sreg:
	call operand_32bit_prefix
	jmp mov_mem_sreg_size_ok
      mov_mem16_sreg:
	call operand_16bit_prefix
      mov_mem_sreg_size_ok:
	movb $0x8c, base_code
	call store_instruction
	jmp instruction_assembled
      mov_mem_imm:
	movb operand_size, %al
	cmpb $1, %al
	je mov_mem_imm_8bit
	cmpb $2, %al
	je mov_mem_imm_16bit
	cmpb $4, %al
	je mov_mem_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp mov_mem_imm_32bit
      mov_mem_imm_8bit:
	call get_byte_value
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	movb $0, postbyte_register
	movb $0xc6, base_code
	popw %cx
	popw %bx
	pop %edx
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      mov_mem_imm_16bit:
	call get_word_value
	movw %ax, value
	movb $0, postbyte_register
	movb $0xc7, base_code
	call operand_16bit_prefix
	popw %cx
	popw %bx
	pop %edx
	call store_instruction
	movw value, %ax
	call mark_relocation
	stosw
	jmp instruction_assembled
      mov_mem_imm_32bit:
	call get_dword_value
	movl %eax, value
	movb $0, postbyte_register
	movb $0xc7, base_code
	call operand_32bit_prefix
	popw %cx
	popw %bx
	pop %edx
	call store_instruction
	mov value, %eax
	call mark_relocation
	stosl
	jmp instruction_assembled
      mov_reg:
	lodsb
	cmpb $0x50, %al
	jae mov_sreg
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	je mov_reg_mem
	cmpb $0x28, %al
	je mov_reg_imm
	cmpb $0x10, %al
	jne invalid_operand
      mov_reg_reg:
	lodsb
	cmpb $0x50, %al
	jae mov_reg_sreg
	call convert_register
	shlb $3, %al
	movb postbyte_register, %bl
	orb %al, %bl
	orb $192, %bl
	movb %ah, %al
	cmpb $1, %al
	je mov_reg_reg_8bit
	cmpb $2, %al
	je mov_reg_reg_16bit
	cmpb $4, %al
	je mov_reg_reg_32bit
	jmp invalid_operand_size
      mov_reg_reg_32bit:
	call operand_32bit_prefix
	incb base_code
	jmp mov_reg_reg_8bit
      mov_reg_reg_16bit:
	call operand_16bit_prefix
	incb base_code
      mov_reg_reg_8bit:
	movb base_code, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      mov_reg_sreg:
	movb %al, %ah
	shrb $4, %ah
	cmpb $5, %ah
	je mov_reg_creg
	cmpb $7, %ah
	je mov_reg_dreg
	ja invalid_operand
	subb $0x61, %al
	movb postbyte_register, %bl
	shlb $3, %al
	orb %al, %bl
	orb $192, %bl
	cmpb $4, operand_size
	je mov_reg_sreg32
	cmpb $2, operand_size
	jne invalid_operand_size
	call operand_16bit_prefix
	jmp mov_reg_sreg_store
     mov_reg_sreg32:
	call operand_32bit_prefix
     mov_reg_sreg_store:
	movb $0x8c, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      mov_reg_creg:
	movb $0x20, %bh
	jmp mov_reg_xrx
      mov_reg_dreg:
	movb $0x21, %bh
      mov_reg_xrx:
	andb $7, %al
	movb postbyte_register, %bl
	shlb $3, %al
	orb %al, %bl
	orb $192, %bl
	cmpb $4, operand_size
	jne invalid_operand_size
	movb %bh, %ah
	movb $0xf, %al
	stosw
	movb %bl, %al
	stosb
	jmp instruction_assembled
      mov_reg_mem:
	call get_address
	movb operand_size, %al
	cmpb $1, %al
	je mov_reg_mem_8bit
	cmpb $2, %al
	je mov_reg_mem_16bit
	cmpb $4, %al
	je mov_reg_mem_32bit
	jmp invalid_operand_size
      mov_reg_mem_8bit:
	movb postbyte_register, %al
	orb %bl, %al
	orb %bh, %al
	jz mov_al_mem
	addb $2, base_code
	call store_instruction
	jmp instruction_assembled
      mov_al_mem:
	cmpb $2, %ch
	je mov_al_mem_address16
	testb $4, %ch
	jnz mov_al_mem_address32
	orb %ch, %ch
	jnz invalid_address_size
	cmpb $32, code_type
	je mov_al_mem_address32
	cmp $0x10000, %edx
	jb mov_al_mem_address16
      mov_al_mem_address32:
	call address_32bit_prefix
	call store_segment_prefix_if_necessary
	movb $0xa0, %al
	jmp store_mov_address32
      mov_al_mem_address16:
	call address_16bit_prefix
	call store_segment_prefix_if_necessary
	movb $0xa0, %al
	jmp store_mov_address16
      mov_reg_mem_16bit:
	call operand_16bit_prefix
	movb postbyte_register, %al
	orb %bl, %al
	orb %bh, %al
	jz mov_ax_mem
	addb $3, base_code
	call store_instruction
	jmp instruction_assembled
      mov_ax_mem:
	cmpb $2, %ch
	je mov_ax_mem_address16
	testb $4, %ch
	jnz mov_ax_mem_address32
	orb %ch, %ch
	jnz invalid_address_size
	cmpb $32, code_type
	je mov_ax_mem_address32
	cmp $0x10000, %edx
	jb mov_ax_mem_address16
      mov_ax_mem_address32:
	call address_32bit_prefix
	call store_segment_prefix_if_necessary
	movb $0xa1, %al
	jmp store_mov_address32
      mov_ax_mem_address16:
	call address_16bit_prefix
	call store_segment_prefix_if_necessary
	movb $0xa1, %al
	jmp store_mov_address16
      mov_reg_mem_32bit:
	call operand_32bit_prefix
	movb postbyte_register, %al
	orb %bl, %al
	orb %bh, %al
	jz mov_ax_mem
	addb $3, base_code
	call store_instruction
	jmp instruction_assembled
      mov_reg_imm:
	movb operand_size, %al
	cmpb $1, %al
	je mov_reg_imm_8bit
	cmpb $2, %al
	je mov_reg_imm_16bit
	cmpb $4, %al
	je mov_reg_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp mov_reg_imm_32bit
      mov_reg_imm_8bit:
	call get_byte_value
	movb %al, %ah
	movb postbyte_register, %al
	andb $7, %al
	addb $0xb0, %al
	stosw
	jmp instruction_assembled
      mov_reg_imm_16bit:
	call get_word_value
	movw %ax, %dx
	call operand_16bit_prefix
	movb postbyte_register, %al
	andb $7, %al
	addb $0xb8, %al
	stosb
	movw %dx, %ax
	call mark_relocation
	stosw
	jmp instruction_assembled
      mov_reg_imm_32bit:
	call get_dword_value
	mov %eax, %edx
	call operand_32bit_prefix
	movb postbyte_register, %al
	andb $7, %al
	addb $0xb8, %al
	stosb
	mov %edx, %eax
	call mark_relocation
	stosl
	jmp instruction_assembled
      mov_sreg:
	movb %al, %ah
	shrb $4, %ah
	cmpb $5, %ah
	je mov_creg
	cmpb $7, %ah
	je mov_dreg
	ja invalid_operand
	subb $0x61, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	je mov_sreg_mem
	cmpb $0x10, %al
	jne invalid_operand
      mov_sreg_reg:
	lodsb
	call convert_register
	orb %ah, %ah
	jz mov_sreg_reg_size_ok
	cmpb $4, %ah
	je mov_sreg_reg32
	cmpb $2, %ah
	je mov_sreg_reg16
	jmp invalid_operand_size
      mov_sreg_reg32:
	movb %al, %ah
	call operand_32bit_prefix
	movb %ah, %al
	jmp mov_sreg_reg_size_ok
      mov_sreg_reg16:
	movb %al, %ah
	call operand_16bit_prefix
	movb %ah, %al
      mov_sreg_reg_size_ok:
	movb $192, %bl
	orb %al, %bl
	movb postbyte_register, %al
	shlb $3, %al
	orb %al, %bl
	movb $0x8e, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      mov_sreg_mem:
	call get_address
	movb operand_size, %al
	orb %al, %al
	jz mov_sreg_mem_size_ok
	cmpb $2, %al
	je mov_sreg_mem16
	cmpb $4, %al
	je mov_sreg_mem32
	jmp invalid_operand_size
      mov_sreg_mem32:
	call operand_32bit_prefix
	jmp mov_sreg_mem_size_ok
      mov_sreg_mem16:
	call operand_16bit_prefix
      mov_sreg_mem_size_ok:
	movb $0x8e, base_code
	call store_instruction
	jmp instruction_assembled
      mov_creg:
	movb $0x22, %dl
	jmp mov_xrx
      mov_dreg:
	movb $0x23, %dl
      mov_xrx:
	andb $7, %al
	movb %al, %bh
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	cmpb $4, %ah
	jne invalid_operand_size
	movb $192, %bl
	orb %al, %bl
	movb %bh, %al
	shlb $3, %al
	orb %al, %bl
	movb $0xf, %al
	movb %dl, %ah
	stosw
	movb %bl, %al
	stosb
	jmp instruction_assembled
test_instruction:
	movb $0x84, base_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je test_reg
	cmpb $0x5b, %al
	jne invalid_operand
      test_mem:
	call get_address
	push %edx
	pushw %bx
	pushw %cx
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	je test_mem_imm
	cmpb $0x10, %al
	jne invalid_operand
      test_mem_reg:
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	popw %cx
	popw %bx
	pop %edx
	movb %ah, %al
	cmpb $1, %al
	je test_mem_reg_8bit
	cmpb $2, %al
	je test_mem_reg_16bit
	cmpb $4, %al
	je test_mem_reg_32bit
	jmp invalid_operand_size
      test_mem_reg_8bit:
	call store_instruction
	jmp instruction_assembled
      test_mem_reg_16bit:
	call operand_16bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
      test_mem_reg_32bit:
	call operand_32bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
      test_mem_imm:
	movb operand_size, %al
	cmpb $1, %al
	je test_mem_imm_8bit
	cmpb $2, %al
	je test_mem_imm_16bit
	cmpb $4, %al
	je test_mem_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp test_mem_imm_32bit
      test_mem_imm_8bit:
	call get_byte_value
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	movb $0, postbyte_register
	movb $0xf6, base_code
	popw %cx
	popw %bx
	pop %edx
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      test_mem_imm_16bit:
	call get_word_value
	movw %ax, value
	movb $0, postbyte_register
	movb $0xf7, base_code
	call operand_16bit_prefix
	popw %cx
	popw %bx
	pop %edx
	call store_instruction
	movw value, %ax
	call mark_relocation
	stosw
	jmp instruction_assembled
      test_mem_imm_32bit:
	call get_dword_value
	movl %eax, value
	movb $0, postbyte_register
	movb $0xf7, base_code
	call operand_32bit_prefix
	popw %cx
	popw %bx
	pop %edx
	call store_instruction
	mov value, %eax
	call mark_relocation
	stosl
	jmp instruction_assembled
      test_reg:
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	je test_reg_imm
	cmpb $0x10, %al
	jne invalid_operand
      test_reg_reg:
	lodsb
	call convert_register
	shlb $3, %al
	movb postbyte_register, %bl
	orb %al, %bl
	orb $192, %bl
	movb %ah, %al
	cmpb $1, %al
	je test_reg_reg_8bit
	cmpb $2, %al
	je test_reg_reg_16bit
	cmpb $4, %al
	je test_reg_reg_32bit
	jmp invalid_operand_size
      test_reg_reg_32bit:
	call operand_32bit_prefix
	incb base_code
	jmp basic_reg_reg_8bit
      test_reg_reg_16bit:
	call operand_16bit_prefix
	incb base_code
      test_reg_reg_8bit:
	movb base_code, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      test_reg_imm:
	movb operand_size, %al
	cmpb $1, %al
	je test_reg_imm_8bit
	cmpb $2, %al
	je test_reg_imm_16bit
	cmpb $4, %al
	je test_reg_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp test_reg_imm_32bit
      test_reg_imm_8bit:
	call get_byte_value
	movb %al, %dl
	movb $192, %ah
	movb postbyte_register, %bl
	andb $7, %bl
	orb %bl, %bl
	jz test_al_imm
	orb %bl, %ah
	movb $0xf6, %al
	stosw
	movb %dl, %al
	stosb
	jmp instruction_assembled
      test_al_imm:
	movb $0xa8, %al
	stosb
	movb %dl, %al
	stosb
	jmp instruction_assembled
      test_reg_imm_16bit:
	call get_word_value
	movw %ax, %dx
	call operand_16bit_prefix
	movb $192, %ah
	movb postbyte_register, %bl
	andb $7, %bl
	orb %bl, %bl
	jz test_ax_imm
	orb %bl, %ah
	movb $0xf7, %al
	stosw
	movw %dx, %ax
	call mark_relocation
	stosw
	jmp instruction_assembled
      test_ax_imm:
	movb $0xa9, %al
	stosb
	movw %dx, %ax
	stosw
	jmp instruction_assembled
      test_reg_imm_32bit:
	call get_dword_value
	mov %eax, %edx
	call operand_32bit_prefix
	movb $192, %ah
	movb postbyte_register, %bl
	andb $7, %bl
	orb %bl, %bl
	jz test_eax_imm
	orb %bl, %ah
	movb $0xf7, %al
	stosw
	mov %edx, %eax
	call mark_relocation
	stosl
	jmp instruction_assembled
      test_eax_imm:
	movb $0xa9, %al
	stosb
	mov %edx, %eax
	stosl
	jmp instruction_assembled
xchg_instruction:
	movb $0x86, base_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je xchg_reg
	cmpb $0x5b, %al
	jne invalid_operand
      xchg_mem:
	call get_address
	push %edx
	pushw %bx
	pushw %cx
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
      xchg_mem_reg:
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	popw %cx
	popw %bx
	pop %edx
	movb %ah, %al
	cmpb $1, %al
	je xchg_mem_reg_8bit
	cmpb $2, %al
	je xchg_mem_reg_16bit
	cmpb $4, %al
	je xchg_mem_reg_32bit
	jmp invalid_operand_size
      xchg_mem_reg_8bit:
	call store_instruction
	jmp instruction_assembled
      xchg_mem_reg_16bit:
	call operand_16bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
      xchg_mem_reg_32bit:
	call operand_32bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
      xchg_reg:
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	je xchg_reg_mem
	cmpb $0x10, %al
	jne invalid_operand
      xchg_reg_reg:
	lodsb
	call convert_register
	movb %al, %bh
	movb postbyte_register, %bl
	shlb $3, postbyte_register
	orb $192, %al
	orb %al, postbyte_register
	movb %ah, %al
	cmpb $1, %al
	je xchg_reg_reg_8bit
	cmpb $2, %al
	je xchg_reg_reg_16bit
	cmpb $4, %al
	je xchg_reg_reg_32bit
	jmp invalid_operand_size
      xchg_reg_reg_32bit:
	call operand_32bit_prefix
	orb %bh, %bh
	jz xchg_ax_reg
	xchgb %bl, %bh
	orb %bh, %bh
	jz xchg_ax_reg
	incb base_code
	jmp xchg_reg_reg_8bit
      xchg_reg_reg_16bit:
	call operand_16bit_prefix
	orb %bh, %bh
	jz xchg_ax_reg
	xchgb %bl, %bh
	orb %bh, %bh
	jz xchg_ax_reg
	incb base_code
      xchg_reg_reg_8bit:
	movb base_code, %al
	movb postbyte_register, %ah
	stosw
	jmp instruction_assembled
      xchg_ax_reg:
	movb $0x90, %al
	addb %bl, %al
	stosb
	jmp instruction_assembled
      xchg_reg_mem:
	call get_address
	movb operand_size, %al
	cmpb $1, %al
	je xchg_reg_mem_8bit
	cmpb $2, %al
	je xchg_reg_mem_16bit
	cmpb $4, %al
	je xchg_reg_mem_32bit
	jmp invalid_operand_size
      xchg_reg_mem_8bit:
	call store_instruction
	jmp instruction_assembled
      xchg_reg_mem_32bit:
	call operand_32bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
      xchg_reg_mem_16bit:
	call operand_16bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
push_instruction:
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je push_reg
	cmpb $0x28, %al
	je push_imm
	cmpb $0x5b, %al
	jne invalid_operand
      push_mem:
	call get_address
	movb operand_size, %al
	cmpb $2, %al
	je push_mem_16bit
	cmpb $4, %al
	je push_mem_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
      push_mem_16bit:
	call operand_16bit_prefix
	movb $0xff, base_code
	movb $6, postbyte_register
	call store_instruction
	jmp push_done
      push_mem_32bit:
	call operand_32bit_prefix
	movb $0xff, base_code
	movb $6, postbyte_register
	call store_instruction
	jmp push_done
      push_reg:
	lodsb
	cmpb $0x60, %al
	jae push_sreg
	call convert_register
	movb %al, %dl
	addb $0x50, %dl
	movb %ah, %al
	cmpb $2, %al
	je push_reg_16bit
	cmpb $4, %al
	je push_reg_32bit
	jmp invalid_operand_size
      push_reg_16bit:
	call operand_16bit_prefix
	movb %dl, %al
	stosb
	jmp push_done
      push_reg_32bit:
	call operand_32bit_prefix
	movb %dl, %al
	stosb
	jmp push_done
      push_sreg:
	movb operand_size, %bl
	cmpb $4, %bl
	je push_sreg32
	cmpb $2, %bl
	je push_sreg16
	orb %bl, %bl
	jz push_sreg_store
	jmp invalid_operand_size
      push_sreg16:
	movb %al, %bl
	call operand_16bit_prefix
	movb %bl, %al
	jmp push_sreg_store
      push_sreg32:
	movb %al, %bl
	call operand_32bit_prefix
	movb %bl, %al
      push_sreg_store:
	cmpb $0x70, %al
	jae invalid_operand
	subb $0x61, %al
	cmpb $4, %al
	jae push_sreg_386
	shlb $3, %al
	addb $6, %al
	stosb
	jmp push_done
      push_sreg_386:
	subb $4, %al
	shlb $3, %al
	movb $0xa0, %ah
	addb %al, %ah
	movb $0xf, %al
	stosw
	jmp push_done
      push_imm:
	movb operand_size, %al
	cmpb $2, %al
	je push_imm_16bit
	cmpb $4, %al
	je push_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $16, code_type
	je push_imm_optimized_16bit
      push_imm_optimized_32bit:
	call get_dword_value
	mov %eax, %edx
	cmpb $0, value_type
	jne push_imm_32bit_forced
	cmp $-0x80, %eax
	jl push_imm_32bit_forced
	cmp $0x80, %eax
	jge push_imm_32bit_forced
      push_imm_8bit:
	movb %al, %ah
	movb $0x6a, %al
	stosw
	jmp push_done
      push_imm_optimized_16bit:
	call get_word_value
	movw %ax, %dx
	cmpb $0, value_type
	jne push_imm_16bit_forced
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $-0x80, %ax */
	.value -0x80
	jl push_imm_16bit_forced
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $0x80, %ax */
	.value 0x80
	jge push_imm_16bit_forced
	jmp push_imm_8bit
      push_imm_16bit:
	call get_word_value
	movw %ax, %dx
	call operand_16bit_prefix
      push_imm_16bit_forced:
	movb $0x68, %al
	stosb
	movw %dx, %ax
	call mark_relocation
	stosw
	jmp push_done
      push_imm_32bit:
	call get_dword_value
	mov %eax, %edx
	call operand_32bit_prefix
      push_imm_32bit_forced:
	movb $0x68, %al
	stosb
	mov %edx, %eax
	call mark_relocation
	stosl
      push_done:
	lodsb
	dec %esi
	cmpb $0xf, %al
	je instruction_assembled
	orb %al, %al
	jz instruction_assembled
	movb $0, operand_size
	movb $0, forced_size
	jmp push_instruction
pop_instruction:
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je pop_reg
	cmpb $0x5b, %al
	jne invalid_operand
      pop_mem:
	call get_address
	movb operand_size, %al
	cmpb $2, %al
	je pop_mem_16bit
	cmpb $4, %al
	je pop_mem_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
      pop_mem_16bit:
	call operand_16bit_prefix
	movb $0x8f, base_code
	movb $0, postbyte_register
	call store_instruction
	jmp pop_done
      pop_mem_32bit:
	call operand_32bit_prefix
	movb $0x8f, base_code
	movb $0, postbyte_register
	call store_instruction
	jmp pop_done
      pop_reg:
	lodsb
	cmpb $0x60, %al
	jae pop_sreg
	call convert_register
	movb %al, %dl
	addb $0x58, %dl
	movb %ah, %al
	cmpb $2, %al
	je pop_reg_16bit
	cmpb $4, %al
	je pop_reg_32bit
	jmp invalid_operand_size
      pop_reg_16bit:
	call operand_16bit_prefix
	movb %dl, %al
	stosb
	jmp pop_done
      pop_reg_32bit:
	call operand_32bit_prefix
	movb %dl, %al
	stosb
	jmp pop_done
      pop_sreg:
	movb operand_size, %bl
	cmpb $4, %bl
	je pop_sreg32
	cmpb $2, %bl
	je pop_sreg16
	orb %bl, %bl
	jz pop_sreg_store
	jmp invalid_operand_size
      pop_sreg16:
	movb %al, %bl
	call operand_16bit_prefix
	movb %bl, %al
	jmp pop_sreg_store
      pop_sreg32:
	movb %al, %bl
	call operand_32bit_prefix
	movb %bl, %al
      pop_sreg_store:
	cmpb $0x70, %al
	jae invalid_operand
	subb $0x61, %al
	cmpb $1, %al
	je illegal_instruction
	cmpb $4, %al
	jae pop_sreg_386
	shlb $3, %al
	addb $7, %al
	stosb
	jmp pop_done
      pop_sreg_386:
	subb $4, %al
	shlb $3, %al
	movb $0xa1, %ah
	addb %al, %ah
	movb $0xf, %al
	stosw
      pop_done:
	lodsb
	dec %esi
	cmpb $0xf, %al
	je instruction_assembled
	orb %al, %al
	jz instruction_assembled
	movb $0, operand_size
	movb $0, forced_size
	jmp pop_instruction
inc_instruction:
	.byte 0xa2  /*WORKAROUNDL movb %al, base_code */
	.long base_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je inc_reg
	cmpb $0x5b, %al
	je inc_mem
	jne invalid_operand
      inc_mem:
	call get_address
	movb operand_size, %al
	cmpb $1, %al
	je inc_mem_8bit
	cmpb $2, %al
	je inc_mem_16bit
	cmpb $4, %al
	je inc_mem_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
      inc_mem_8bit:
	movb $0xfe, %al
	xchgb base_code, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	call store_instruction
	jmp instruction_assembled
      inc_mem_16bit:
	call operand_16bit_prefix
	movb $0xff, %al
	xchgb base_code, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	call store_instruction
	jmp instruction_assembled
      inc_mem_32bit:
	call operand_32bit_prefix
	movb $0xff, %al
	xchgb base_code, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	call store_instruction
	jmp instruction_assembled
      inc_reg:
	lodsb
	call convert_register
	movb %al, %dl
	shrb $4, %al
	movb %ah, %al
	cmpb $1, %al
	je inc_reg_8bit
	movb base_code, %dh
	shlb $3, %dh
	addb %dh, %dl
	addb $0x40, %dl
	cmpb $2, %al
	je inc_reg_16bit
	cmpb $4, %al
	je inc_reg_32bit
	jmp invalid_operand_size
      inc_reg_8bit:
	movb $0xfe, %al
	movb base_code, %ah
	shlb $3, %ah
	orb %dl, %ah
	orb $192, %ah
	stosw
	jmp instruction_assembled
      inc_reg_16bit:
	call operand_16bit_prefix
	movb %dl, %al
	stosb
	jmp instruction_assembled
      inc_reg_32bit:
	call operand_32bit_prefix
	movb %dl, %al
	stosb
	jmp instruction_assembled
arpl_instruction:
	.byte 0xa2  /*WORKAROUNDL movb %al, base_code */
	.long base_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je arpl_reg
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	cmpb $2, %ah
	jne invalid_operand_size
	movb $0x63, base_code
	call store_instruction
	jmp instruction_assembled
      arpl_reg:
	lodsb
	call convert_register
	cmpb $2, %ah
	jne invalid_operand_size
	movb %al, %dl
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	cmpb $2, %ah
	jne invalid_operand_size
	movb %al, %ah
	shlb $3, %ah
	orb %dl, %ah
	orb $192, %ah
	movb $0x63, %al
	stosw
	jmp instruction_assembled
bound_instruction:
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	movb operand_size, %al
	cmpb $2, %al
	je bound_16bit
	cmpb $4, %al
	je bound_32bit
	jmp invalid_operand_size
      bound_32bit:
	call operand_32bit_prefix
	movb $0x62, base_code
	call store_instruction
	jmp instruction_assembled
      bound_16bit:
	call operand_16bit_prefix
	movb $0x62, base_code
	call store_instruction
	jmp instruction_assembled
set_instruction:
	movb $0xf, base_code
	.byte 0xa2  /*WORKAROUNDL movb %al, extended_code */
	.long extended_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je set_reg
	cmpb $0x5b, %al
	jne invalid_operand
      set_mem:
	call get_address
	cmpb $1, operand_size
	ja invalid_operand_size
	movb $0, postbyte_register
	call store_instruction
	jmp instruction_assembled
      set_reg:
	lodsb
	call convert_register
	movb %al, %bl
	cmpb $1, %ah
	jne invalid_operand_size
	movb extended_code, %ah
	movb $0xf, %al
	stosw
	movb $192, %al
	orb %bl, %al
	stosb
	jmp instruction_assembled
ret_instruction_16bit:
	movb %al, %ah
	call operand_16bit_prefix
	movb %ah, %al
	jmp ret_instruction
ret_instruction_32bit:
	movb %al, %ah
	call operand_32bit_prefix
	movb %ah, %al
ret_instruction:
	.byte 0xa2  /*WORKAROUNDL movb %al, base_code */
	.long base_code
	lodsb
	dec %esi
	orb %al, %al
	jz simple_ret
	cmpb $0xf, %al
	je simple_ret
	lodsb
	call get_size_operator
	orb %ah, %ah
	jz ret_imm
	cmpb $2, %ah
	je ret_imm
	jmp invalid_operand_size
      ret_imm:
	cmpb $0x28, %al
	jne invalid_operand
	call get_word_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	movw %ax, %dx
	movb base_code, %al
	stosb
	movw %dx, %ax
	stosw
	jmp instruction_assembled
      simple_ret:
	movb base_code, %al
	incb %al
	stosb
	jmp instruction_assembled
lea_instruction:
	movb $0x8d, base_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	movb operand_size, %al
	pushw %ax
	movb $0, operand_size
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	popw %ax
	cmpb $2, %al
	je lea_16bit
	cmpb $4, %al
	je lea_32bit
	jmp invalid_operand_size
      lea_16bit:
	call operand_16bit_prefix
	call store_instruction
	jmp instruction_assembled
      lea_32bit:
	call operand_32bit_prefix
	call store_instruction
	jmp instruction_assembled
ls_instruction:
	orb %al, %al
	jz les_instruction
	cmpb $3, %al
	jz lds_instruction
	addb $0xb0, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, extended_code */
	.long extended_code
	movb $0xf, base_code
	jmp ls_code_ok
      les_instruction:
	movb $0xc4, base_code
	jmp ls_code_ok
      lds_instruction:
	movb $0xc5, base_code
      ls_code_ok:
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	addb $2, operand_size
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	movb operand_size, %al
	cmpb $4, %al
	je ls_16bit
	cmpb $6, %al
	je ls_32bit
	jmp invalid_operand_size
      ls_16bit:
	call operand_16bit_prefix
	call store_instruction
	cmpb $0, operand_size
	je instruction_assembled
	cmpb $4, operand_size
	jne invalid_operand_size
	jmp instruction_assembled
      ls_32bit:
	call operand_32bit_prefix
	call store_instruction
	cmpb $0, operand_size
	je instruction_assembled
	cmpb $6, operand_size
	jne invalid_operand_size
	jmp instruction_assembled
enter_instruction:
	lodsb
	call get_size_operator
	cmpb $2, %ah
	je enter_imm16_size_ok
	orb %ah, %ah
	jnz invalid_operand_size
      enter_imm16_size_ok:
	cmpb $0x28, %al
	jne invalid_operand
	call get_word_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	pushw %ax
	movb $0, operand_size
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $1, %ah
	je enter_imm8_size_ok
	orb %ah, %ah
	jnz invalid_operand_size
      enter_imm8_size_ok:
	cmpb $0x28, %al
	jne invalid_operand
	call get_byte_value
	movb %al, %dl
	popw %bx
	movb $0xc8, %al
	stosb
	movw %bx, %ax
	stosw
	movb %dl, %al
	stosb
	jmp instruction_assembled
sh_instruction:
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je sh_reg
	cmpb $0x5b, %al
	jne invalid_operand
      sh_mem:
	call get_address
	push %edx
	pushw %bx
	pushw %cx
	movb operand_size, %al
	pushw %ax
	movb $0, operand_size
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	je sh_mem_imm
	cmpb $0x10, %al
	jne invalid_operand
      sh_mem_reg:
	lodsb
	cmpb $0x11, %al
	jne invalid_operand
	popw %ax
	popw %cx
	popw %bx
	pop %edx
	cmpb $1, %al
	je sh_mem_cl_8bit
	cmpb $2, %al
	je sh_mem_cl_16bit
	cmpb $4, %al
	je sh_mem_cl_32bit
	orb %ah, %ah
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
      sh_mem_cl_8bit:
	movb $0xd2, base_code
	call store_instruction
	jmp instruction_assembled
      sh_mem_cl_16bit:
	movb $0xd3, base_code
	call operand_16bit_prefix
	call store_instruction
	jmp instruction_assembled
      sh_mem_cl_32bit:
	movb $0xd3, base_code
	call operand_32bit_prefix
	call store_instruction
	jmp instruction_assembled
      sh_mem_imm:
	movb operand_size, %al
	orb %al, %al
	jz sh_mem_imm_size_ok
	cmpb $1, %al
	jne invalid_operand_size
      sh_mem_imm_size_ok:
	call get_byte_value
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	popw %ax
	popw %cx
	popw %bx
	pop %edx
	cmpb $1, %al
	je sh_mem_imm_8bit
	cmpb $2, %al
	je sh_mem_imm_16bit
	cmpb $4, %al
	je sh_mem_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
      sh_mem_imm_8bit:
	cmpb $1, value
	je sh_mem_1_8bit
	movb $0xc0, base_code
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      sh_mem_1_8bit:
	movb $0xd0, base_code
	call store_instruction
	jmp instruction_assembled
      sh_mem_imm_16bit:
	cmpb $1, value
	je sh_mem_1_16bit
	movb $0xc1, base_code
	call operand_16bit_prefix
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      sh_mem_1_16bit:
	movb $0xd1, base_code
	call operand_16bit_prefix
	call store_instruction
	jmp instruction_assembled
      sh_mem_imm_32bit:
	cmpb $1, value
	je sh_mem_1_32bit
	movb $0xc1, base_code
	call operand_32bit_prefix
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      sh_mem_1_32bit:
	movb $0xd1, base_code
	call operand_32bit_prefix
	call store_instruction
	jmp instruction_assembled
      sh_reg:
	lodsb
	call convert_register
	shlb $3, postbyte_register
	orb $192, %al
	orb %al, postbyte_register
	movb %ah, %al
	pushw %ax
	movb $0, operand_size
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	je sh_reg_imm
	cmpb $0x10, %al
	jne invalid_operand
      sh_reg_reg:
	lodsb
	cmpb $0x11, %al
	jne invalid_operand
	popw %ax
	movb postbyte_register, %bl
	cmpb $1, %al
	je sh_reg_cl_8bit
	cmpb $2, %al
	je sh_reg_cl_16bit
	cmpb $4, %al
	je sh_reg_cl_32bit
	jmp invalid_operand_size
      sh_reg_cl_8bit:
	movb $0xd2, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      sh_reg_cl_16bit:
	call operand_16bit_prefix
	movb $0xd3, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      sh_reg_cl_32bit:
	call operand_32bit_prefix
	movb $0xd3, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      sh_reg_imm:
	movb operand_size, %al
	orb %al, %al
	jz sh_reg_imm_size_ok
	cmpb $1, %al
	jne invalid_operand_size
      sh_reg_imm_size_ok:
	call get_byte_value
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	popw %ax
	movb postbyte_register, %bl
	cmpb $1, %al
	je sh_reg_imm_8bit
	cmpb $2, %al
	je sh_reg_imm_16bit
	cmpb $4, %al
	je sh_reg_imm_32bit
	jmp invalid_operand_size
      sh_reg_imm_8bit:
	cmpb $1, value
	je sh_reg_1_8bit
	movb $0xc0, %al
	stosb
	movb %bl, %al
	movb value, %ah
	stosw
	jmp instruction_assembled
      sh_reg_1_8bit:
	movb $0xd0, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      sh_reg_imm_16bit:
	cmpb $1, value
	je sh_reg_1_16bit
	call operand_16bit_prefix
	movb $0xc1, %al
	stosb
	movb %bl, %al
	movb value, %ah
	stosw
	jmp instruction_assembled
      sh_reg_1_16bit:
	call operand_16bit_prefix
	movb $0xd1, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      sh_reg_imm_32bit:
	cmpb $1, value
	je sh_reg_1_32bit
	call operand_32bit_prefix
	movb $0xc1, %al
	stosb
	movb %bl, %al
	movb value, %ah
	stosw
	jmp instruction_assembled
      sh_reg_1_32bit:
	call operand_32bit_prefix
	movb $0xd1, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
shd_instruction:
	movb $0xf, base_code
	.byte 0xa2  /*WORKAROUNDL movb %al, extended_code */
	.long extended_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je shd_reg
	cmpb $0x5b, %al
	jne invalid_operand
      shd_mem:
	call get_address
	push %edx
	pushw %bx
	pushw %cx
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	xorb %al, %al
	xchgb operand_size, %al
	pushw %ax
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	je shd_mem_reg_imm
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	cmpb $0x11, %al
	jne invalid_operand
	popw %ax
	popw %cx
	popw %bx
	pop %edx
	cmpb $2, %al
	je shd_mem_reg_cl_16bit
	cmpb $4, %al
	je shd_mem_reg_cl_32bit
	jmp invalid_operand_size
      shd_mem_reg_cl_16bit:
	call operand_16bit_prefix
	incb extended_code
	call store_instruction
	jmp instruction_assembled
      shd_mem_reg_cl_32bit:
	call operand_32bit_prefix
	incb extended_code
	call store_instruction
	jmp instruction_assembled
      shd_mem_reg_imm:
	movb operand_size, %al
	orb %al, %al
	jz shd_mem_reg_imm_size_ok
	cmpb $1, %al
	jne invalid_operand_size
      shd_mem_reg_imm_size_ok:
	call get_byte_value
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	popw %ax
	popw %cx
	popw %bx
	pop %edx
	cmpb $2, %al
	je shd_mem_reg_imm_16bit
	cmpb $4, %al
	je shd_mem_reg_imm_32bit
	jmp invalid_operand_size
      shd_mem_reg_imm_16bit:
	call operand_16bit_prefix
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      shd_mem_reg_imm_32bit:
	call operand_32bit_prefix
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      shd_reg:
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	movb postbyte_register, %bl
	shlb $3, %al
	orb %al, %bl
	orb $192, %bl
	movb %ah, %al
	pushw %ax
	pushw %bx
	lodsb
	cmpb $44, %al
	jne invalid_operand
	movb $0, operand_size
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	je shd_reg_reg_imm
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	cmpb $0x11, %al
	jne invalid_operand
	popw %bx
	popw %ax
	cmpb $2, %al
	je shd_reg_reg_cl_16bit
	cmpb $4, %al
	je shd_reg_reg_cl_32bit
	jmp invalid_operand_size
      shd_reg_reg_cl_16bit:
	call operand_16bit_prefix
	jmp shd_reg_reg_cl_store
      shd_reg_reg_cl_32bit:
	call operand_32bit_prefix
      shd_reg_reg_cl_store:
	movb extended_code, %ah
	incb %ah
	movb $0xf, %al
	stosw
	movb %bl, %al
	stosb
	jmp instruction_assembled
      shd_reg_reg_imm:
	movb operand_size, %al
	orb %al, %al
	jz shd_reg_reg_imm_size_ok
	cmpb $1, %al
	jne invalid_operand_size
      shd_reg_reg_imm_size_ok:
	call get_byte_value
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	popw %bx
	popw %ax
	cmpb $2, %al
	je shd_reg_reg_imm_16bit
	cmpb $4, %al
	je shd_reg_reg_imm_32bit
	jmp invalid_operand_size
      shd_reg_reg_imm_16bit:
	call operand_16bit_prefix
	jmp shd_reg_reg_imm_store
      shd_reg_reg_imm_32bit:
	call operand_32bit_prefix
      shd_reg_reg_imm_store:
	movb extended_code, %ah
	movb $0xf, %al
	stosw
	movb %bl, %al
	stosb
	movb value, %al
	stosb
	jmp instruction_assembled
movx_instruction:
	movb $0xf, base_code
	.byte 0xa2  /*WORKAROUNDL movb %al, extended_code */
	.long extended_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	movb %ah, %al
	cmpb $2, %al
	je movx_16bit
	cmpb $4, %al
	je movx_32bit
	jmp invalid_operand_size
      movx_16bit:
	lodsb
	cmpb $44, %al
	jne invalid_operand
	movb $0, operand_size
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je movx_16bit_reg
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	movb operand_size, %al
	cmpb $1, %al
	je movx_16bit_mem_8bit
	orb %al, %al
	jnz invalid_operand_size
      movx_16bit_mem_8bit:
	call operand_16bit_prefix
	call store_instruction
	jmp instruction_assembled
      movx_16bit_reg:
	lodsb
	call convert_register
	movb postbyte_register, %bl
	shlb $3, %bl
	orb %al, %bl
	orb $192, %bl
	cmpb $1, %ah
	jne invalid_operand_size
	call operand_16bit_prefix
	movb $0xf, %al
	stosb
	movb extended_code, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      movx_32bit:
	lodsb
	cmpb $44, %al
	jne invalid_operand
	movb $0, operand_size
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je movx_32bit_reg
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	movb operand_size, %al
	cmpb $1, %al
	je movx_32bit_mem_8bit
	cmpb $2, %al
	je movx_32bit_mem_16bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
      movx_32bit_mem_8bit:
	call operand_32bit_prefix
	call store_instruction
	jmp instruction_assembled
      movx_32bit_mem_16bit:
	incb extended_code
	call operand_32bit_prefix
	call store_instruction
	jmp instruction_assembled
      movx_32bit_reg:
	lodsb
	call convert_register
	movb postbyte_register, %bl
	shlb $3, %bl
	orb %al, %bl
	orb $192, %bl
	movb %ah, %al
	cmpb $1, %al
	je movx_32bit_reg_8bit
	cmpb $2, %al
	je movx_32bit_reg_16bit
	jmp invalid_operand_size
      movx_32bit_reg_8bit:
	call operand_32bit_prefix
	movb $0xf, %al
	stosb
	movb extended_code, %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
      movx_32bit_reg_16bit:
	call operand_32bit_prefix
	movb $0xf, %al
	stosb
	movb extended_code, %al
	incb %al
	stosb
	movb %bl, %al
	stosb
	jmp instruction_assembled
bt_instruction:
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	shlb $3, %al
	addb $0x83, %al
	.byte 0xa2  /*WORKAROUNDL movb %al, extended_code */
	.long extended_code
	movb $0xf, base_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je bt_reg
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	push %eax
	pushw %bx
	pushw %cx
	lodsb
	cmpb $44, %al
	jne invalid_operand
	cmpb $0x28, (%esi)
	je bt_mem_imm
	cmpb $0x11, (%esi)
	jne bt_mem_reg
	cmpb $0x28, 2(%esi)
	je bt_mem_imm
      bt_mem_reg:
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	popw %cx
	popw %bx
	pop %edx
	movb %ah, %al
	cmpb $2, %al
	je bt_mem_reg_16bit
	cmpb $4, %al
	je bt_mem_reg_32bit
	jmp invalid_operand_size
      bt_mem_reg_16bit:
	call operand_16bit_prefix
	call store_instruction
	jmp instruction_assembled
      bt_mem_reg_32bit:
	call operand_32bit_prefix
	call store_instruction
	jmp instruction_assembled
      bt_mem_imm:
	xorb %al, %al
	xchgb operand_size, %al
	pushw %ax
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	jne invalid_operand
	movb operand_size, %al
	orb %al, %al
	jz bt_mem_imm_size_ok
	cmpb $1, %al
	jne invalid_operand_size
      bt_mem_imm_size_ok:
	movb $0xba, extended_code
	call get_byte_value
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	popw %ax
	cmpb $2, %al
	je bt_mem_imm_16bit
	cmpb $4, %al
	je bt_mem_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp bt_mem_imm_32bit
      bt_mem_imm_16bit:
	call operand_16bit_prefix
	popw %cx
	popw %bx
	pop %edx
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      bt_mem_imm_32bit:
	call operand_32bit_prefix
	popw %cx
	popw %bx
	pop %edx
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      bt_reg:
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	cmpb $0x28, (%esi)
	je bt_reg_imm
	cmpb $0x11, (%esi)
	jne bt_reg_reg
	cmpb $0x28, 2(%esi)
	je bt_reg_imm
      bt_reg_reg:
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	movb postbyte_register, %bl
	shlb $3, %al
	orb %al, %bl
	orb $192, %bl
	movb %ah, %al
	cmpb $2, %al
	je bt_reg_reg_16bit
	cmpb $4, %al
	je bt_reg_reg_32bit
	jmp invalid_operand_size
      bt_reg_reg_16bit:
	call operand_16bit_prefix
	movb extended_code, %ah
	movb $0xf, %al
	stosw
	movb %bl, %al
	stosb
	jmp instruction_assembled
      bt_reg_reg_32bit:
	call operand_32bit_prefix
	movb extended_code, %ah
	movb $0xf, %al
	stosw
	movb %bl, %al
	stosb
	jmp instruction_assembled
      bt_reg_imm:
	xorb %al, %al
	xchgb operand_size, %al
	pushw %ax
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	jne invalid_operand
	movb operand_size, %al
	orb %al, %al
	jz bt_reg_imm_size_ok
	cmpb $1, %al
	jne invalid_operand_size
      bt_reg_imm_size_ok:
	call get_byte_value
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	popw %ax
	cmpb $2, %al
	je bt_reg_imm_16bit
	cmpb $4, %al
	je bt_reg_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp bt_reg_imm_32bit
      bt_reg_imm_16bit:
	call operand_16bit_prefix
	jmp bt_reg_imm_store
      bt_reg_imm_32bit:
	call operand_32bit_prefix
      bt_reg_imm_store:
	.byte 0x66, 0xb8  /*WORKAROUNDW mov $0xba0f, %ax */
	.value 0xba0f
	stosw
	movb $192, %al
	orb postbyte_register, %al
	movb extended_code, %ah
	subb $0x83, %ah
	orb %ah, %al
	stosb
	movb value, %al
	stosb
	jmp instruction_assembled
bs_instruction:
	.byte 0xa2  /*WORKAROUNDL movb %al, extended_code */
	.long extended_code
	movb $0xf, base_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	cmpb $0x10, %al
	je bs_reg_reg
	cmpb $0x5b, %al
	jne invalid_argument
	call get_address
	movb operand_size, %al
	cmpb $2, %al
	je bs_reg_mem_16bit
	cmpb $4, %al
	je bs_reg_mem_32bit
	jmp invalid_operand_size
      bs_reg_mem_16bit:
	call operand_16bit_prefix
	call store_instruction
	jmp instruction_assembled
      bs_reg_mem_32bit:
	call operand_32bit_prefix
	call store_instruction
	jmp instruction_assembled
      bs_reg_reg:
	lodsb
	call convert_register
	movb postbyte_register, %bl
	shlb $3, %bl
	orb %al, %bl
	orb $192, %bl
	movb %ah, %al
	cmpb $2, %al
	je bs_reg_reg_16bit
	cmpb $4, %al
	je bs_reg_reg_32bit
	jmp invalid_operand_size
      bs_reg_reg_16bit:
	call operand_16bit_prefix
	jmp bs_reg_reg_store
      bs_reg_reg_32bit:
	call operand_32bit_prefix
      bs_reg_reg_store:
	movb extended_code, %ah
	movb $0xf, %al
	stosw
	movb %bl, %al
	stosb
	jmp instruction_assembled
imul_instruction:
	movb $0xf6, base_code
	movb $5, postbyte_register
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je imul_reg
	cmpb $0x5b, %al
	jne invalid_operand
      imul_mem:
	call get_address
	movb operand_size, %al
	cmpb $1, %al
	je imul_mem_8bit
	cmpb $2, %al
	je imul_mem_16bit
	cmpb $4, %al
	je imul_mem_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
      imul_mem_8bit:
	call store_instruction
	jmp instruction_assembled
      imul_mem_16bit:
	call operand_16bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
      imul_mem_32bit:
	call operand_32bit_prefix
	incb base_code
	call store_instruction
	jmp instruction_assembled
      imul_reg:
	lodsb
	call convert_register
	cmpb $44, (%esi)
	je imul_reg_
	movb postbyte_register, %bl
	shlb $3, %bl
	orb %al, %bl
	orb $192, %bl
	cmpb $1, %ah
	je imul_reg_8bit
	cmpb $2, %ah
	je imul_reg_16bit
	cmpb $4, %ah
	je imul_reg_32bit
	jmp invalid_operand_size
      imul_reg_8bit:
	movb %bl, %ah
	movb $0xf6, %al
	stosw
	jmp instruction_assembled
      imul_reg_16bit:
	call operand_16bit_prefix
	movb %bl, %ah
	movb $0xf7, %al
	stosw
	jmp instruction_assembled
      imul_reg_32bit:
	call operand_32bit_prefix
	movb %bl, %ah
	movb $0xf7, %al
	stosw
	jmp instruction_assembled
      imul_reg_:
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	inc %esi
	cmpb $0x28, (%esi)
	je imul_reg_imm
	cmpb $0x11, (%esi)
	jne imul_reg__
	cmpb $0x28, 2(%esi)
	je imul_reg_imm
      imul_reg__:
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je imul_reg_reg
	cmpb $0x5b, %al
	je imul_reg_mem
	jne invalid_operand
      imul_reg_mem:
	call get_address
	push %edx
	pushw %bx
	pushw %cx
	cmpb $44, (%esi)
	je imul_reg_mem_imm
	movb operand_size, %al
	cmpb $2, %al
	je imul_reg_mem_16bit
	cmpb $4, %al
	je imul_reg_mem_32bit
	jmp invalid_operand_size
      imul_reg_mem_16bit:
	call operand_16bit_prefix
	jmp imul_reg_mem_store
      imul_reg_mem_32bit:
	call operand_32bit_prefix
      imul_reg_mem_store:
	popw %cx
	popw %bx
	pop %edx
	movb $0xf, base_code
	movb $0xaf, extended_code
	call store_instruction
	jmp instruction_assembled
      imul_reg_mem_imm:
	inc %esi
	xorb %cl, %cl
	xchgb operand_size, %cl
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	jne invalid_operand
	movb operand_size, %al
	movb %cl, operand_size
	cmpb $1, %al
	je imul_reg_mem_imm_8bit
	cmpb $2, %al
	je imul_reg_mem_imm_16bit
	cmpb $4, %al
	je imul_reg_mem_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $2, %cl
	je imul_reg_mem_imm_16bit
	cmpb $4, %cl
	je imul_reg_mem_imm_32bit
	jmp invalid_operand_size
      imul_reg_mem_imm_8bit:
	call get_byte_value
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	popw %cx
	popw %bx
	pop %edx
	movb $0x6b, base_code
	cmpb $2, operand_size
	je imul_reg_mem_16bit_imm_8bit
	cmpb $4, operand_size
	je imul_reg_mem_32bit_imm_8bit
	jmp invalid_operand_size
      imul_reg_mem_16bit_imm_8bit:
	call operand_16bit_prefix
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      imul_reg_mem_32bit_imm_8bit:
	call operand_32bit_prefix
	call store_instruction
	movb value, %al
	stosb
	jmp instruction_assembled
      imul_reg_mem_imm_16bit:
	call get_word_value
	movw %ax, value
	popw %cx
	popw %bx
	pop %edx
	movb $0x69, base_code
	cmpb $2, operand_size
	jne invalid_operand_size
	call operand_16bit_prefix
	call store_instruction
	movw value, %ax
	call mark_relocation
	stosw
	jmp instruction_assembled
      imul_reg_mem_imm_32bit:
	call get_dword_value
	movl %eax, value
	popw %cx
	popw %bx
	pop %edx
	movb $0x69, base_code
	cmpb $4, operand_size
	jne invalid_operand_size
	call operand_32bit_prefix
	call store_instruction
	mov value, %eax
	call mark_relocation
	stosl
	jmp instruction_assembled
      imul_reg_imm:
	movb postbyte_register, %dl
	movb %dl, %bl
	dec %esi
	jmp imul_reg_reg_imm
      imul_reg_reg:
	lodsb
	call convert_register
	movb postbyte_register, %bl
	movb %al, %dl
	cmpb $44, (%esi)
	je imul_reg_reg_imm
	movb %ah, %al
	cmpb $2, %al
	je imul_reg_reg_16bit
	cmpb $4, %al
	je imul_reg_reg_32bit
	jmp invalid_operand_size
      imul_reg_reg_16bit:
	call operand_16bit_prefix
	jmp imul_reg_reg_store
      imul_reg_reg_32bit:
	call operand_32bit_prefix
      imul_reg_reg_store:
	.byte 0x66, 0xb8  /*WORKAROUNDW mov $0xaf0f, %ax */
	.value 0xaf0f
	stosw
	movb %dl, %al
	shlb $3, %bl
	orb %bl, %al
	orb $192, %al
	stosb
	jmp instruction_assembled
      imul_reg_reg_imm:
	inc %esi
	xorb %cl, %cl
	xchgb operand_size, %cl
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	jne invalid_operand
	movb operand_size, %al
	movb %cl, operand_size
	cmpb $1, %al
	je imul_reg_reg_imm_8bit
	cmpb $2, %al
	je imul_reg_reg_imm_16bit
	cmpb $4, %al
	je imul_reg_reg_imm_32bit
	orb %al, %al
	jnz invalid_operand_size
	cmpb $2, %cl
	je imul_reg_reg_imm_16bit
	cmpb $4, %cl
	je imul_reg_reg_imm_32bit
	jmp invalid_operand_size
      imul_reg_reg_imm_8bit:
	pushw %bx
	pushw %dx
	call get_byte_value
	popw %dx
	popw %bx
      imul_reg_reg_imm_8bit_store:
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	cmpb $2, operand_size
	je imul_reg_reg_16bit_imm_8bit
	cmpb $4, operand_size
	je imul_reg_reg_32bit_imm_8bit
	jmp invalid_operand_size
      imul_reg_reg_16bit_imm_8bit:
	call operand_16bit_prefix
	movb $0x6b, %al
	stosb
	movb %dl, %al
	shlb $3, %bl
	orb %bl, %al
	orb $192, %al
	stosb
	movb value, %al
	stosb
	jmp instruction_assembled
      imul_reg_reg_32bit_imm_8bit:
	call operand_32bit_prefix
	movb $0x6b, %al
	stosb
	movb %dl, %al
	shlb $3, %bl
	orb %bl, %al
	orb $192, %al
	stosb
	movb value, %al
	stosb
	jmp instruction_assembled
      imul_reg_reg_imm_16bit:
	pushw %bx
	pushw %dx
	call get_word_value
	popw %dx
	popw %bx
	cmpb $0, value_type
	jne imul_reg_reg_imm_16bit_forced
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $-0x80, %ax */
	.value -0x80
	jl imul_reg_reg_imm_16bit_forced
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $0x80, %ax */
	.value 0x80
	jl imul_reg_reg_imm_8bit_store
      imul_reg_reg_imm_16bit_forced:
	movw %ax, value
	call operand_16bit_prefix
	movb $0x69, %al
	stosb
	movb %dl, %al
	shlb $3, %bl
	orb %bl, %al
	orb $192, %al
	stosb
	movw value, %ax
	call mark_relocation
	stosw
	jmp instruction_assembled
      imul_reg_reg_imm_32bit:
	pushw %bx
	pushw %dx
	call get_dword_value
	popw %dx
	popw %bx
	cmpb $0, value_type
	jne imul_reg_reg_imm_32bit_forced
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $-0x80, %ax */
	.value -0x80
	jl imul_reg_reg_imm_32bit_forced
	.byte 0x66, 0x3d  /*WORKAROUNDW cmp $0x80, %ax */
	.value 0x80
	jl imul_reg_reg_imm_8bit_store
      imul_reg_reg_imm_32bit_forced:
	movl %eax, value
	call operand_32bit_prefix
	movb $0x69, %al
	stosb
	movb %dl, %al
	shlb $3, %bl
	orb %bl, %al
	orb $192, %al
	stosb
	mov value, %eax
	call mark_relocation
	stosl
	jmp instruction_assembled
in_instruction:
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	orb %al, %al
	jnz invalid_operand
	lodsb
	cmpb $44, %al
	jne invalid_operand
	movb %ah, %al
	pushw %ax
	movb $0, operand_size
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	je in_imm
	cmpb $0x10, %al
	je in_reg
	jmp invalid_operand
      in_reg:
	lodsb
	cmpb $0x22, %al
	jne invalid_operand
	popw %ax
	cmpb $1, %al
	je in_al_dx
	cmpb $2, %al
	je in_ax_dx
	cmpb $4, %al
	je in_eax_dx
	jmp invalid_operand_size
      in_al_dx:
	movb $0xec, %al
	stosb
	jmp instruction_assembled
      in_ax_dx:
	call operand_16bit_prefix
	movb $0xed, %al
	stosb
	jmp instruction_assembled
      in_eax_dx:
	call operand_32bit_prefix
	movb $0xed, %al
	stosb
	jmp instruction_assembled
      in_imm:
	movb operand_size, %al
	orb %al, %al
	jz in_imm_size_ok
	cmpb $1, %al
	jne invalid_operand_size
      in_imm_size_ok:
	call get_byte_value
	movb %al, %dl
	popw %ax
	cmpb $1, %al
	je in_al_imm
	cmpb $2, %al
	je in_ax_imm
	cmpb $4, %al
	je in_eax_imm
	jmp invalid_operand_size
      in_al_imm:
	movb $0xe4, %al
	stosb
	movb %dl, %al
	stosb
	jmp instruction_assembled
      in_ax_imm:
	call operand_16bit_prefix
	movb $0xe5, %al
	stosb
	movb %dl, %al
	stosb
	jmp instruction_assembled
      in_eax_imm:
	call operand_32bit_prefix
	movb $0xe5, %al
	stosb
	movb %dl, %al
	stosb
	jmp instruction_assembled
out_instruction:
	lodsb
	call get_size_operator
	cmpb $0x28, %al
	je out_imm
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	cmpb $0x22, %al
	jne invalid_operand
	lodsb
	cmpb $44, %al
	jne invalid_operand
	movb $0, operand_size
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	orb %al, %al
	jnz invalid_operand
	movb %ah, %al
	cmpb $1, %al
	je out_dx_al
	cmpb $2, %al
	je out_dx_ax
	cmpb $4, %al
	je out_dx_eax
	jmp invalid_operand_size
      out_dx_al:
	movb $0xee, %al
	stosb
	jmp instruction_assembled
      out_dx_ax:
	call operand_16bit_prefix
	movb $0xef, %al
	stosb
	jmp instruction_assembled
      out_dx_eax:
	call operand_32bit_prefix
	movb $0xef, %al
	stosb
	jmp instruction_assembled
      out_imm:
	movb operand_size, %al
	orb %al, %al
	jz out_imm_size_ok
	cmpb $1, %al
	jne invalid_operand_size
      out_imm_size_ok:
	call get_byte_value
	.byte 0xa2  /*WORKAROUNDL movb %al, value */
	.long value
	lodsb
	cmpb $44, %al
	jne invalid_operand
	movb $0, operand_size
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	orb %al, %al
	jnz invalid_operand
	movb %ah, %al
	cmpb $1, %al
	je out_imm_al
	cmpb $2, %al
	je out_imm_ax
	cmpb $4, %al
	je out_imm_eax
	jmp invalid_operand_size
      out_imm_al:
	movb $0xe6, %al
	stosb
	movb value, %al
	stosb
	jmp instruction_assembled
      out_imm_ax:
	call operand_16bit_prefix
	movb $0xe7, %al
	stosb
	movb value, %al
	stosb
	jmp instruction_assembled
      out_imm_eax:
	call operand_32bit_prefix
	movb $0xe7, %al
	stosb
	movb value, %al
	stosb
	jmp instruction_assembled
lar_instruction:
	.byte 0xa2  /*WORKAROUNDL movb %al, extended_code */
	.long extended_code
	movb $0xf, base_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je lar_reg_reg
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	movb operand_size, %al
	cmpb $2, %al
	je lar_16bit
	cmpb $4, %al
	je lar_32bit
	jmp invalid_operand_size
      lar_16bit:
	call operand_16bit_prefix
	call store_instruction
	jmp instruction_assembled
      lar_32bit:
	call operand_32bit_prefix
	call store_instruction
	jmp instruction_assembled
      lar_reg_reg:
	lodsb
	call convert_register
	movb postbyte_register, %bl
	shlb $3, %bl
	orb %al, %bl
	orb $192, %bl
	movb %ah, %al
	cmpb $2, %al
	je lar_reg_reg_16bit
	cmpb $4, %al
	je lar_reg_reg_32bit
	jmp invalid_operand_size
      lar_reg_reg_32bit:
	call operand_32bit_prefix
	jmp lar_reg_reg_store
      lar_reg_reg_16bit:
	call operand_16bit_prefix
      lar_reg_reg_store:
	movb $0xf, %al
	movb extended_code, %ah
	stosw
	movb %bl, %al
	stosb
	jmp instruction_assembled
invlpg_instruction:
	movb $0xf, base_code
	movb $1, extended_code
	movb $7, postbyte_register
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	call store_instruction
	jmp instruction_assembled
basic_486_instruction:
	movb $0xf, base_code
	.byte 0xa2  /*WORKAROUNDL movb %al, extended_code */
	.long extended_code
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	je basic_486_reg
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	push %edx
	pushw %bx
	pushw %cx
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	popw %cx
	popw %bx
	pop %edx
	movb %ah, %al
	cmpb $1, %al
	je basic_486_mem_reg_8bit
	cmpb $2, %al
	je basic_486_mem_reg_16bit
	cmpb $4, %al
	je basic_486_mem_reg_32bit
	jmp invalid_operand_size
      basic_486_mem_reg_8bit:
	call store_instruction
	jmp instruction_assembled
      basic_486_mem_reg_16bit:
	call operand_16bit_prefix
	incb extended_code
	call store_instruction
	jmp instruction_assembled
      basic_486_mem_reg_32bit:
	call operand_32bit_prefix
	incb extended_code
	call store_instruction
	jmp instruction_assembled
      basic_486_reg:
	lodsb
	call convert_register
	.byte 0xa2  /*WORKAROUNDL movb %al, postbyte_register */
	.long postbyte_register
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	movb postbyte_register, %bl
	shlb $3, %al
	orb %al, %bl
	orb $192, %bl
	movb %ah, %al
	cmpb $1, %al
	je basic_486_reg_reg_8bit
	cmpb $2, %al
	je basic_486_reg_reg_16bit
	cmpb $4, %al
	je basic_486_reg_reg_32bit
	jmp invalid_operand_size
      basic_486_reg_reg_32bit:
	call operand_32bit_prefix
	incb extended_code
	jmp basic_486_reg_reg_8bit
      basic_486_reg_reg_16bit:
	call operand_16bit_prefix
	incb extended_code
      basic_486_reg_reg_8bit:
	movb $0xf, %al
	movb extended_code, %ah
	stosw
	movb %bl, %al
	stosb
	jmp instruction_assembled
bswap_instruction:
	lodsb
	call get_size_operator
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	call convert_register
	movb %al, %ah
	addb $0xc8, %ah
	cmpb $4, %ah
	jne invalid_operand_size
	call operand_32bit_prefix
	movb $0xf, %al
	stosw
	jmp instruction_assembled
conditional_jump:
	.byte 0xa2  /*WORKAROUNDL movb %al, base_code */
	.long base_code
	lodsb
	call get_jump_operator
	cmpb $2, jump_type
	je invalid_operand
	call get_size_operator
	cmpb $0x28, %al
	jne invalid_operand
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	cmpb $1, value_type
	je invalid_use_of_symbol
	sub %edi, %eax
	add org_start, %eax
	sub $2, %eax
	cmpl $0, org_sib
	jne invalid_use_of_symbol
	movb operand_size, %bl
	cmpb $1, %bl
	je conditional_jump_8bit
	cmpb $2, %bl
	je conditional_jump_16bit
	cmpb $4, %bl
	je conditional_jump_32bit
	orb %bl, %bl
	jnz invalid_operand_size
	cmp $0x80, %eax
	jb conditional_jump_8bit
	cmp $-0x80, %eax
	jae conditional_jump_8bit
	cmpb $16, code_type
	je conditional_jump_16bit
      conditional_jump_32bit:
	sub $4, %eax
	mov %eax, %edx
	mov %edi, %ecx
	call operand_32bit_prefix
	sub %edi, %edx
	add %ecx, %edx
	movb base_code, %ah
	addb $0x10, %ah
	movb $0xf, %al
	stosw
	mov %edx, %eax
	stosl
	jmp instruction_assembled
      conditional_jump_16bit:
	sub $2, %eax
	mov %eax, %edx
	mov %edi, %ecx
	call operand_16bit_prefix
	sub %edi, %edx
	add %ecx, %edx
	movb base_code, %ah
	addb $0x10, %ah
	movb $0xf, %al
	stosw
	mov %edx, %eax
	stosw
	cmp $0x10000, %eax
	jge jump_out_of_range
	cmp $-0x10000, %eax
	jl jump_out_of_range
	jmp instruction_assembled
      conditional_jump_8bit:
	mov %eax, %edx
	movb %al, %ah
	movb base_code, %al
	stosw
	cmp $0x80, %edx
	jge jump_out_of_range
	cmp $-0x80, %edx
	jl jump_out_of_range
	jmp instruction_assembled
      jump_out_of_range:
	cmpl $0, error_line
	jne instruction_assembled
	mov current_line, %eax
	movl %eax, error_line
	movl $relative_jump_out_of_range, error
	jmp instruction_assembled
loop_instruction_16bit:
	movb %al, %cl
	call address_16bit_prefix
	movb %cl, %al
	jmp loop_instruction
loop_instruction_32bit:
	movb %al, %cl
	call address_32bit_prefix
	movb %cl, %al
loop_instruction:
	.byte 0xa2  /*WORKAROUNDL movb %al, base_code */
	.long base_code
	lodsb
	call get_jump_operator
	cmpb $2, jump_type
	je invalid_operand
	call get_size_operator
	cmpb $0x28, %al
	jne invalid_operand
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	cmpb $1, value_type
	je invalid_use_of_symbol
	sub %edi, %eax
	add org_start, %eax
	cmpl $0, org_sib
	jne invalid_use_of_symbol
	movb operand_size, %bl
	cmpb $1, %bl
	je loop_8bit
	orb %bl, %bl
	jnz invalid_operand_size
      loop_8bit:
	sub $2, %eax
	mov %eax, %edx
	movb base_code, %al
	stosb
	mov %edx, %eax
	stosb
	cmp $0x80, %eax
	jge jump_out_of_range
	cmp $-0x80, %eax
	jl jump_out_of_range
	jmp instruction_assembled
call_instruction:
	movb $2, postbyte_register
	movb $0xe8, base_code
	movb $0x9a, extended_code
	jmp process_jmp
jmp_instruction:
	movb $4, postbyte_register
	movb $0xe9, base_code
	movb $0xea, extended_code
      process_jmp:
	lodsb
	call get_jump_operator
	call get_size_operator
	cmpb $0x10, %al
	je jmp_reg
	cmpb $0x28, %al
	je jmp_imm
	cmpb $0x5b, %al
	jne invalid_operand
      jmp_mem:
	call get_address
	movb $0xff, base_code
	mov %eax, %edx
	movb operand_size, %al
	orb %al, %al
	jz jmp_mem_size_not_specified
	cmpb $2, %al
	je jmp_mem_16bit
	cmpb $4, %al
	je jmp_mem_32bit
	cmpb $6, %al
	je jmp_mem_48bit
	jmp invalid_operand_size
      jmp_mem_size_not_specified:
	cmpb $2, jump_type
	je jmp_mem_far
	cmpb $1, jump_type
	je jmp_mem_near
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
      jmp_mem_near:
	cmpb $16, code_type
	je jmp_mem_16bit
	jmp jmp_mem_near_32bit
      jmp_mem_far:
	cmpb $16, code_type
	je jmp_mem_far_32bit
      jmp_mem_48bit:
	cmpb $1, jump_type
	je invalid_operand_size
	call operand_32bit_prefix
	incb postbyte_register
	call store_instruction
	jmp instruction_assembled
      jmp_mem_32bit:
	cmpb $2, jump_type
	je jmp_mem_far_32bit
	cmpb $1, jump_type
	je jmp_mem_near_32bit
	cmpb $16, code_type
	je jmp_mem_far_32bit
      jmp_mem_near_32bit:
	call operand_32bit_prefix
	call store_instruction
	jmp instruction_assembled
      jmp_mem_far_32bit:
	call operand_16bit_prefix
	incb postbyte_register
	call store_instruction
	jmp instruction_assembled
      jmp_mem_16bit:
	cmpb $2, jump_type
	je invalid_operand_size
	call operand_16bit_prefix
	call store_instruction
	jmp instruction_assembled
      jmp_reg:
	lodsb
	call convert_register
	movb %al, %bl
	orb $192, %bl
	movb %ah, %al
	cmpb $2, %al
	je jmp_reg_16bit
	cmpb $4, %al
	je jmp_reg_32bit
	jmp invalid_operand_size
      jmp_reg_32bit:
	cmpb $2, jump_type
	je jmp_reg_far32bit
	cmpb $1, jump_type
	je jmp_reg_near32bit
	cmpb $16, code_type
	je jmp_reg_far32bit
      jmp_reg_near32bit:
	call operand_32bit_prefix
	movb postbyte_register, %al
	shlb $3, %al
	orb %al, %bl
	movb %bl, %ah
	movb $0xff, %al
	stosw
	jmp instruction_assembled
      jmp_reg_far32bit:
	call operand_32bit_prefix
	movb postbyte_register, %al
	incb %al
	shlb $3, %al
	orb %al, %bl
	movb %bl, %ah
	movb $0xff, %al
	stosw
	jmp instruction_assembled
      jmp_reg_16bit:
	cmpb $2, jump_type
	je invalid_operand_size
	call operand_16bit_prefix
	movb postbyte_register, %al
	shlb $3, %al
	orb %al, %bl
	movb %bl, %ah
	movb $0xff, %al
	stosw
	jmp instruction_assembled
      jmp_imm:
	push %esi
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	pop %ebx
	cmpb $0x3a, (%esi)
	je jmp_far
	cmpb $1, value_type
	je invalid_use_of_symbol
	cmpb $2, jump_type
	je invalid_operand
	sub %edi, %eax
	add org_start, %eax
	sub $2, %eax
	cmpl $0, org_sib
	jne invalid_use_of_symbol
	movb operand_size, %bl
	cmpb $1, %bl
	je jmp_8bit
	cmpb $2, %bl
	je jmp_16bit
	cmpb $4, %bl
	je jmp_32bit
	orb %bl, %bl
	jnz invalid_operand_size
	cmpb $0xe9, base_code
	jne jmp_no8bit
	cmp $0x80, %eax
	jb jmp_8bit
	cmp $-0x80, %eax
	jae jmp_8bit
      jmp_no8bit:
	cmpb $32, code_type
	je jmp_32bit
      jmp_16bit:
	dec %eax
	mov %eax, %edx
	mov %edi, %ecx
	call operand_16bit_prefix
	sub %edi, %edx
	add %ecx, %edx
	movb base_code, %al
	stosb
	mov %edx, %eax
	stosw
	cmp $0x10000, %eax
	jge jump_out_of_range
	cmp $-0x10000, %eax
	jl jump_out_of_range
	jmp instruction_assembled
      jmp_32bit:
	sub $3, %eax
	mov %eax, %edx
	mov %edi, %ecx
	call operand_32bit_prefix
	sub %edi, %edx
	add %ecx, %edx
	movb base_code, %al
	stosb
	mov %edx, %eax
	stosl
	jmp instruction_assembled
      jmp_8bit:
	cmpb $0xe9, base_code
	jne invalid_operand_size
	mov %eax, %edx
	movb %al, %ah
	movb $0xeb, %al
	stosw
	cmp $0x80, %edx
	jge jump_out_of_range
	cmp $-0x80, %edx
	jl jump_out_of_range
	jmp instruction_assembled
      jmp_far:
	cmpb $1, jump_type
	je invalid_operand
	mov %ebx, %esi
	call get_word_value
	movw %ax, %dx
	movb operand_size, %bl
	cmpb $4, %bl
	je jmp_far_16bit
	cmpb $6, %bl
	je jmp_far_32bit
	orb %bl, %bl
	jnz invalid_operand_size
	cmpb $32, code_type
	je jmp_far_32bit
      jmp_far_16bit:
	inc %esi
	lodsb
	cmpb $0x28, %al
	jne invalid_operand
	movb value_type, %al
	pushw %ax
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_word_value
	mov %eax, %ebx
	call operand_16bit_prefix
	movb extended_code, %al
	stosb
	movw %bx, %ax
	call mark_relocation
	stosw
	popw %ax
	.byte 0xa2  /*WORKAROUNDL movb %al, value_type */
	.long value_type
	movw %dx, %ax
	call mark_relocation
	stosw
	jmp instruction_assembled
      jmp_far_32bit:
	inc %esi
	lodsb
	cmpb $0x28, %al
	jne invalid_operand
	movb value_type, %al
	pushw %ax
	cmpb $0x2e, (%esi)
	je invalid_value
	call get_dword_value
	mov %eax, %ebx
	call operand_32bit_prefix
	movb extended_code, %al
	stosb
	mov %ebx, %eax
	call mark_relocation
	stosl
	popw %ax
	.byte 0xa2  /*WORKAROUNDL movb %al, value_type */
	.long value_type
	movw %dx, %ax
	call mark_relocation
	stosw
	jmp instruction_assembled
ins_instruction:
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	orb %ch, %bl
	jnz invalid_address
	cmpb $0x27, %bh
	je ins_16bit
	cmpb $0x47, %bh
	jne invalid_address
	call address_32bit_prefix
	jmp ins_store
      ins_16bit:
	call address_16bit_prefix
      ins_store:
	cmpb $1, segment_register
	ja invalid_address
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	cmpb $0x22, %al
	jne invalid_operand
	movb $0x6c, %al
	cmpb $1, operand_size
	je simple_instruction
	incb %al
	cmpb $2, operand_size
	je simple_instruction_16bit
	cmpb $4, operand_size
	je simple_instruction_32bit
	cmpb $0, operand_size
	je operand_size_not_specified
	jmp invalid_operand_size
outs_instruction:
	lodsb
	cmpb $0x10, %al
	jne invalid_operand
	lodsb
	cmpb $0x22, %al
	jne invalid_operand
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	orb %ch, %bl
	jnz invalid_address
	cmpb $0x26, %bh
	je outs_16bit
	cmpb $0x46, %bh
	jne invalid_address
	call address_32bit_prefix
	jmp outs_store
      outs_16bit:
	call address_16bit_prefix
      outs_store:
	cmpb $4, segment_register
	je outs_segment_ok
	call store_segment_prefix
      outs_segment_ok:
	movb $0x6e, %al
	cmpb $1, operand_size
	je simple_instruction
	incb %al
	cmpb $2, operand_size
	je simple_instruction_16bit
	cmpb $4, operand_size
	je simple_instruction_32bit
	cmpb $0, operand_size
	je operand_size_not_specified
	jmp invalid_operand_size
movs_instruction:
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	orb %ch, %bl
	jnz invalid_address
	cmpb $1, segment_register
	ja invalid_address
	pushw %bx
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	popw %dx
	or %eax, %eax
	jnz invalid_address
	orb %ch, %bl
	jnz invalid_address
	movb %dh, %al
	movb %bh, %ah
	shrb $4, %al
	shrb $4, %ah
	cmpb %ah, %al
	jne address_sizes_do_not_agree
	andb $7, %bh
	andb $7, %dh
	cmpb $6, %bh
	jne invalid_address
	cmpb $7, %dh
	jne invalid_address
	cmpb $2, %al
	je movs_16bit
	cmpb $4, %al
	jne invalid_address
	call address_32bit_prefix
	jmp movs_store
      movs_16bit:
	call address_16bit_prefix
      movs_store:
	cmpb $4, segment_register
	je movs_segment_ok
	call store_segment_prefix
      movs_segment_ok:
	movb $0xa4, %al
	movb operand_size, %bl
	cmpb $1, %bl
	je simple_instruction
	incb %al
	cmpb $2, %bl
	je simple_instruction_16bit
	cmpb $4, %bl
	je simple_instruction_32bit
	orb %bl, %bl
	jz operand_size_not_specified
	jmp invalid_operand_size
lods_instruction:
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	orb %ch, %bl
	jnz invalid_address
	cmpb $0x26, %bh
	je lods_16bit
	cmpb $0x46, %bh
	jne invalid_address
	call address_32bit_prefix
	jmp lods_store
      lods_16bit:
	call address_16bit_prefix
      lods_store:
	cmpb $4, segment_register
	je lods_segment_ok
	call store_segment_prefix
      lods_segment_ok:
	movb $0xac, %al
	cmpb $1, operand_size
	je simple_instruction
	incb %al
	cmpb $2, operand_size
	je simple_instruction_16bit
	cmpb $4, operand_size
	je simple_instruction_32bit
	cmpb $0, operand_size
	je operand_size_not_specified
	jmp invalid_operand_size
stos_instruction:
	.byte 0xa2  /*WORKAROUNDL movb %al, base_code */
	.long base_code
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	orb %ch, %bl
	jnz invalid_address
	cmpb $0x27, %bh
	je stos_16bit
	cmpb $0x47, %bh
	jne invalid_address
	call address_32bit_prefix
	jmp stos_store
      stos_16bit:
	call address_16bit_prefix
      stos_store:
	cmpb $1, segment_register
	ja invalid_address
	movb base_code, %al
	cmpb $1, operand_size
	je simple_instruction
	incb %al
	cmpb $2, operand_size
	je simple_instruction_16bit
	cmpb $4, operand_size
	je simple_instruction_32bit
	cmpb $0, operand_size
	je operand_size_not_specified
	jmp invalid_operand_size
cmps_instruction:
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	orb %ch, %bl
	jnz invalid_address
	movb segment_register, %al
	pushw %ax
	pushw %bx
	lodsb
	cmpb $44, %al
	jne invalid_operand
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	orb %ch, %bl
	jnz invalid_address
	popw %dx
	popw %ax
	cmpb $1, segment_register
	ja invalid_address
	.byte 0xa2  /*WORKAROUNDL movb %al, segment_register */
	.long segment_register
	movb %dh, %al
	movb %bh, %ah
	shrb $4, %al
	shrb $4, %ah
	cmpb %ah, %al
	jne address_sizes_do_not_agree
	andb $7, %bh
	andb $7, %dh
	cmpb $7, %bh
	jne invalid_address
	cmpb $6, %dh
	jne invalid_address
	cmpb $2, %al
	je cmps_16bit
	cmpb $4, %al
	jne invalid_address
	call address_32bit_prefix
	jmp cmps_store
      cmps_16bit:
	call address_16bit_prefix
      cmps_store:
	cmpb $4, segment_register
	je cmps_segment_ok
	call store_segment_prefix
      cmps_segment_ok:
	movb $0xa6, %al
	movb operand_size, %bl
	cmpb $1, %bl
	je simple_instruction
	incb %al
	cmpb $2, %bl
	je simple_instruction_16bit
	cmpb $4, %bl
	je simple_instruction_32bit
	orb %bl, %bl
	jz operand_size_not_specified
	jmp invalid_operand_size
xlat_instruction:
	lodsb
	call get_size_operator
	cmpb $0x5b, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	orb %ch, %bl
	jnz invalid_address
	cmpb $0x23, %bh
	je xlat_16bit
	cmpb $0x43, %bh
	jne invalid_address
	call address_32bit_prefix
	jmp xlat_store
      xlat_16bit:
	call address_16bit_prefix
      xlat_store:
	call store_segment_prefix_if_necessary
	movb $0xd7, %al
	cmpb $1, operand_size
	jbe simple_instruction
	jmp invalid_operand_size
cmpsd_instruction:
	movb $0xa7, %al
	movb (%esi), %ah
	orb %ah, %ah
	jmp simple_instruction_32bit
movsd_instruction:
	movb $0xa5, %al
	movb (%esi), %ah
	orb %ah, %ah
	jmp simple_instruction_32bit
convert_register:
	movb %al, %ah
	shrb $4, %ah
	andb $7, %al
	cmpb $4, %ah
	ja invalid_operand
      match_register_size:
	cmpb operand_size, %ah
	je register_size_ok
	cmpb $0, operand_size
	jne operand_sizes_do_not_match
	movb %ah, operand_size
      register_size_ok:
	ret
get_size_operator:
	xorb %ah, %ah
	cmpb $0x11, %al
	jne operand_size_ok
	lodsw
	xchgb %ah, %al
	movb $1, forced_size
	cmpb operand_size, %ah
	je forced_ok
	cmpb $0, operand_size
	jne operand_sizes_do_not_match
	movb %ah, operand_size
      forced_ok:
	ret
      operand_size_ok:
	cmpb $0x5b, %al
	jne forced_ok
	movb $0, forced_size
	ret
get_jump_operator:
	movb $0, jump_type
	cmpb $0x12, %al
	jne jump_operator_ok
	lodsw
	.byte 0xa2  /*WORKAROUNDL movb %al, jump_type */
	.long jump_type
	movb %ah, %al
      jump_operator_ok:
	ret
operand_16bit_prefix:
	cmpb $16, code_type
	je size_prefix_ok
	movb $0x66, %al
	stosb
	ret
operand_32bit_prefix:
	cmpb $32, code_type
	je size_prefix_ok
	movb $0x66, %al
	stosb
      size_prefix_ok:
	ret
store_segment_prefix_if_necessary:
	movb segment_register, %al
	orb %al, %al
	jz segment_prefix_ok
	cmpb $3, %al
	je ss_prefix
	cmpb $4, %al
	ja segment_prefix_386
	jb segment_prefix
	cmpb $0x25, %bh
	je segment_prefix
	cmpb $0x45, %bh
	je segment_prefix
	cmpb $0x44, %bh
	je segment_prefix
	ret
      ss_prefix:
	cmpb $0x25, %bh
	je segment_prefix_ok
	cmpb $0x45, %bh
	je segment_prefix_ok
	cmpb $0x44, %bh
	je segment_prefix_ok
	jmp segment_prefix
store_segment_prefix:
	movb segment_register, %al
	orb %al, %al
	jz segment_prefix_ok
	cmpb $5, %al
	jae segment_prefix_386
      segment_prefix:
	decb %al
	shlb $3, %al
	addb $0x26, %al
	stosb
	jmp segment_prefix_ok
      segment_prefix_386:
	addb $0x64-5, %al
	stosb
      segment_prefix_ok:
	ret
store_instruction:
	call store_segment_prefix_if_necessary
      store_instruction_main:
	orw %bx, %bx
	jz address_immediate
	movb %bl, %al
	orb %bh, %al
	andb $240, %al
	cmpb $0x40, %al
	je postbyte_32bit
	call address_16bit_prefix
	call store_instruction_code
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2326, %bx */
	.value 0x2326
	je address_bx_si
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2623, %bx */
	.value 0x2623
	je address_bx_si
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2327, %bx */
	.value 0x2327
	je address_bx_di
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2723, %bx */
	.value 0x2723
	je address_bx_di
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2526, %bx */
	.value 0x2526
	je address_bp_si
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2625, %bx */
	.value 0x2625
	je address_bp_si
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2527, %bx */
	.value 0x2527
	je address_bp_di
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2725, %bx */
	.value 0x2725
	je address_bp_di
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2600, %bx */
	.value 0x2600
	je address_si
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2700, %bx */
	.value 0x2700
	je address_di
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2300, %bx */
	.value 0x2300
	je address_bx
	.byte 0x66, 0x81, 0xfb  /*WORKAROUNDW cmp $0x2500, %bx */
	.value 0x2500
	je address_bp
	jmp invalid_address
      address_bx_si:
	xorb %al, %al
	jmp postbyte_16bit
      address_bx_di:
	movb $1, %al
	jmp postbyte_16bit
      address_bp_si:
	movb $2, %al
	jmp postbyte_16bit
      address_bp_di:
	movb $3, %al
	jmp postbyte_16bit
      address_si:
	movb $4, %al
	jmp postbyte_16bit
      address_di:
	movb $5, %al
	jmp postbyte_16bit
      address_bx:
	movb $7, %al
	jmp postbyte_16bit
      address_bp:
	movb $6, %al
      postbyte_16bit:
	cmpb $1, %ch
	je address_8bit_value
	cmpb $2, %ch
	je address_16bit_value
	orb %ch, %ch
	jnz address_sizes_do_not_agree
	or %edx, %edx
	jz address
	cmp $0x80, %edx
	jb address_8bit_value
	cmp $-0x80, %edx
	jae address_8bit_value
      address_16bit_value:
	orb $128, %al
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosb
	mov %edx, %eax
	stosw
	cmp $0x10000, %edx
	jge value_out_of_range
	cmp $-0x8000, %edx
	jl value_out_of_range
	ret
      address_8bit_value:
	orb $64, %al
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosb
	movb %dl, %al
	stosb
	cmp $0x80, %edx
	jge value_out_of_range
	cmp $-0x80, %edx
	jl value_out_of_range
	ret
      address:
	cmpb $6, %al
	je address_8bit_value
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosb
	ret
      postbyte_32bit:
	call address_32bit_prefix
	call store_instruction_code
	cmpb $0x44, %bl
	je invalid_address
	orb %cl, %cl
	jz only_base_register
      base_and_index:
	movb $4, %al
	xorb %ah, %ah
	cmpb $1, %cl
	je scale_ok
	cmpb $2, %cl
	je scale_1
	cmpb $4, %cl
	je scale_2
	orb $192, %ah
	jmp scale_ok
      scale_2:
	orb $128, %ah
	jmp scale_ok
      scale_1:
	orb $64, %ah
      scale_ok:
	orb %bh, %bh
	jz only_index_register
	andb $7, %bl
	shlb $3, %bl
	orb %bl, %ah
	andb $7, %bh
	orb %bh, %ah
	cmpb $1, %ch
	je sib_address_8bit_value
	testb $4, %ch
	jnz sib_address_32bit_value
	cmpb $2, %ch
	je address_sizes_do_not_agree
	cmpb $5, %bh
	je address_value
	or %edx, %edx
	jz sib_address
      address_value:
	cmp $0x80, %edx
	jb sib_address_8bit_value
	cmp $-0x80, %edx
	jae sib_address_8bit_value
      sib_address_32bit_value:
	orb $128, %al
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosw
	jmp store_address_32bit_value
      sib_address_8bit_value:
	orb $64, %al
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosw
	movb %dl, %al
	stosb
	cmp $0x80, %edx
	jge value_out_of_range
	cmp $-0x80, %edx
	jl value_out_of_range
	ret
      sib_address:
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosw
	ret
      only_index_register:
	orb $5, %ah
	andb $7, %bl
	shlb $3, %bl
	orb %bl, %ah
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosw
	testb $4, %ch
	jnz store_address_32bit_value
	orb %ch, %ch
	jnz invalid_address_size
	jmp store_address_32bit_value
      zero_index_register:
	movb $4, %bl
	movb $1, %cl
	jmp base_and_index
      only_base_register:
	movb %bh, %al
	andb $7, %al
	cmpb $4, %al
	je zero_index_register
	cmpb $1, %ch
	je simple_address_8bit_value
	testb $4, %ch
	jnz simple_address_32bit_value
	cmpb $2, %ch
	je address_sizes_do_not_agree
	or %edx, %edx
	jz simple_address
	cmp $0x80, %edx
	jb simple_address_8bit_value
	cmp $-0x80, %edx
	jae simple_address_8bit_value
      simple_address_32bit_value:
	orb $128, %al
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosb
	jmp store_address_32bit_value
      simple_address_8bit_value:
	orb $64, %al
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosb
	movb %dl, %al
	stosb
	cmp $0x80, %edx
	jge value_out_of_range
	cmp $-0x80, %edx
	jl value_out_of_range
	ret
      simple_address:
	cmpb $5, %al
	je simple_address_8bit_value
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosb
	ret
      address_immediate:
	testb $4, %ch
	jnz address_immediate_32bit
	cmpb $2, %ch
	je address_immediate_16bit
	orb %ch, %ch
	jnz invalid_address_size
	cmpb $16, code_type
	je addressing_16bit
      address_immediate_32bit:
	call address_32bit_prefix
	call store_instruction_code
	movb $5, %al
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosb
      store_address_32bit_value:
	testb $0x80, %ch
	jz address_relocation_ok
	pushw value_type
	movb $2, value_type
	call mark_relocation
	popw %ax
	.byte 0xa2  /*WORKAROUNDL movb %al, value_type */
	.long value_type
      address_relocation_ok:
	mov %edx, %eax
	stosl
	ret
      addressing_16bit:
	cmp $0x10000, %edx
	jge address_immediate_32bit
	cmp $-0x8000, %edx
	jl address_immediate_32bit
	movzwl %dx, %edx
      address_immediate_16bit:
	call address_16bit_prefix
	call store_instruction_code
	movb $6, %al
	movb postbyte_register, %cl
	shlb $3, %cl
	orb %cl, %al
	stosb
	mov %edx, %eax
	stosw
	cmp $0x10000, %edx
	jge value_out_of_range
	cmp $-0x8000, %edx
	jl value_out_of_range
	ret
      store_instruction_code:
	movb base_code, %al
	stosb
	cmpb $0xf, %al
	jne instruction_code_ok
      store_extended_code:
	movb extended_code, %al
	stosb
      instruction_code_ok:
	ret
      address_16bit_prefix:
	cmpb $16, code_type
	je instruction_prefix_ok
	movb $0x67, %al
	stosb
	ret
      address_32bit_prefix:
	cmpb $32, code_type
	je instruction_prefix_ok
	movb $0x67, %al
	stosb
      instruction_prefix_ok:
	ret

/* %include '../formats.inc' */

/*  flat assembler source */
/*  Copyright (c) 1999-2001, Tomasz Grysztar */
/*  All rights reserved. */

format_directive:
	cmp code_start, %edi
	jne unexpected_instruction
	cmpb $0, output_format
	jne unexpected_instruction
	lodsb
	cmpb $0x18, %al
	jne invalid_argument
	lodsb
	.byte 0xa2  /*WORKAROUNDL movb %al, output_format */
	.long output_format
	jmp instruction_assembled
entry_directive:
	btsl $1, format_flags
	jc symbol_already_defined
	jmp illegal_instruction
stack_directive:
	btsl $2, format_flags
	jc symbol_already_defined
	jmp illegal_instruction
heap_directive:
	btsl $3, format_flags
	jc symbol_already_defined
	jmp illegal_instruction
mark_relocation:
	ret

/* %include '../tables.inc' */

/*  flat assembler source */
/*  Copyright (c) 1999-2001, Tomasz Grysztar */
/*  All rights reserved. */

get_operator:
	push %esi
	push %ebp
	mov $1, %ebp
	cmpb $0x1a, (%esi)
	jne operator_start
	inc %esi
	lodsb
	movzbl %al, %ebp
      operator_start:
	mov %esi, %edx
      check_operator:
	mov %edx, %esi
	movzbl (%edi), %ecx
	jcxz no_operator
	inc %edi
	mov %edi, %ebx
	add %ecx, %ebx
	cmp %ebp, %ecx
	jne next_operator
	repz
	cmpsb
	je operator_found
      next_operator:
	mov %ebx, %edi
	inc %edi
	jmp check_operator
      no_operator:
	xorb %al, %al
	pop %ebp
	pop %esi
	ret
      operator_found:
	pop %ebp
	pop %eax
	movb (%edi), %al
	ret

get_symbol:
	mov %esi, %edx
	mov %ecx, %ebp
      scan_symbols:
	mov %edx, %esi
	movzbl (%edi), %eax
	orb %al, %al
	jz no_symbol
	mov %ebp, %ecx
	inc %edi
	mov %edi, %ebx
	add %eax, %ebx
	movb (%esi), %ah
	cmpb (%edi), %ah
	jb no_symbol
	ja next_symbol
	cmpb %al, %cl
	jne next_symbol
	repz
	cmpsb
	jb no_symbol
	je symbol_ok
      next_symbol:
	mov %ebx, %edi
	add $2, %edi
	jmp scan_symbols
      no_symbol:
	mov %edx, %esi
	mov %ebp, %ecx
	stc
	ret
      symbol_ok:
	movw (%ebx), %ax
	clc
	ret

get_instruction:
	mov %esi, %edx
	mov %ecx, %ebp
	cmp $11, %ecx
	ja no_instruction
	subb $2, %cl
	jc no_instruction
	movzwl instructions(,%ecx,2), %edi
	add $instructions, %edi
      scan_instructions:
	mov %edx, %esi
	movb (%edi), %al
	orb %al, %al
	jz no_instruction
	mov %ebp, %ecx
	mov %edi, %ebx
	add %ecx, %ebx
	repz
	cmpsb
	jb no_instruction
	je instruction_ok
      next_instruction:
	mov %ebx, %edi
	add $3, %edi
	jmp scan_instructions
      no_instruction:
	mov %edx, %esi
	mov %ebp, %ecx
	stc
	ret
      instruction_ok:
	movb (%ebx), %al
	movw 1(%ebx), %bx
	clc
	ret

get_label_id:
	cmp $0x100, %ecx
	jae name_too_long
	cmpb $0x2e, (%esi)
	jne standard_label
	cmpb $0x2e, 1(%esi)
	je standard_label
	cmpl $0, current_locals_prefix
	je standard_label
	push %edi
	push %ecx
	push %esi
	mov additional_memory, %edi
	xorb %al, %al
	stosb
	mov current_locals_prefix, %esi
	mov %edi, %ebx
	lodsb
	movzbl %al, %ecx
	lea (%edi,%ecx,1), %ebp
	cmp additional_memory_end, %ebp
	jae out_of_memory
	rep
	movsb
	pop %esi
	pop %ecx
	addb %cl, %al
	jc name_too_long
	lea (%edi,%ecx,1), %ebp
	cmp additional_memory_end, %ebp
	jae out_of_memory
	rep
	movsb
	movl %edi, additional_memory
	pop %edi
	push %esi
	movzbl %al, %ecx
	mov %ebx, %esi
	call get_label_id
	pop %esi
	ret
      standard_label:
	cmp $1, %ecx
	jne find_label
	lodsb
	cmpb $0x24, %al
	je get_current_offset_id
	cmpb $0x25, %al
	je get_counter_id
	dec %esi
	jmp find_label
      get_current_offset_id:
	xor %eax, %eax
	ret
      get_counter_id:
	mov $1, %eax
	ret
      find_label:
	xor %ebx, %ebx
	xor %eax, %eax
	xor %ebp, %ebp
      hash_label:
	movzbl (%esi,%ebx,1), %eax
	add %eax, %ebp
	incb %bl
	cmpb %cl, %bl
	jb hash_label
	shl $24, %ebx
	or %ebx, %ebp
	movl %ebp, label_hash
	push %edi
	push %esi
	mov %esi, %ebx
	mov %ecx, %edx
	mov labels_list, %eax
      check_label:
	mov %ebx, %esi
	mov %edx, %ecx
	cmp memory_end, %eax
	je add_label
	cmp (%eax), %ebp
	jne next_label
	mov 4(%eax), %edi
	repz
	cmpsb
	je label_found
      next_label:
	add $16, %eax
	jmp check_label
      label_found:
	add $4, %esp
	pop %edi
	ret
      add_label:
	pop %esi
	cmpb $0, -1(%esi)
	je label_name_ok
	movb (%esi), %al
	cmpb $0x30, %al
	jb name_first_char_ok
	cmpb $0x39, %al
	jbe invalid_name
      name_first_char_ok:
	cmp $1, %ecx
	jne check_for_reserved_word
	cmpb $0x24, %al
	je reserved_word_used_as_symbol
      check_for_reserved_word:
	call get_instruction
	jnc reserved_word_used_as_symbol
	mov $data_directives, %edi
	call get_symbol
	jnc reserved_word_used_as_symbol
	mov $symbols, %edi
	call get_symbol
	jnc reserved_word_used_as_symbol
	mov $formatter_symbols, %edi
	call get_symbol
	jnc reserved_word_used_as_symbol
      label_name_ok:
	mov labels_list, %eax
	sub $16, %eax
	movl %eax, labels_list
	mov %esi, 4(%eax)
	add %ecx, %esi
	mov label_hash, %edx
	mov %edx, (%eax)
	pop %edi
	cmp %edi, %eax
	jbe out_of_memory
	ret

CASE_INSENSITIVE = 0
CASE_SENSITIVE = 1

symbol_characters:
.byte 25
 .byte 9, 0xa, 0xd, 0x1a, 0x20
 .byte 0x2b, 0x2d, 0x2f, 0x2a, 0x3a, 0x3d, 0x7c, 0x26, 0x7e, 0x28, 0x29, 0x5b, 0x5d, 0x3c, 0x3e, 0x7b, 0x7d, 0x2c, 0x3b, 0x5c


















operators:
 .byte 1
 .byte 0x2b
 .byte 0x80
 .byte 1
 .byte 0x2d
 .byte 0x81
 .byte 1
 .byte 0x2a
 .byte 0x90
 .byte 1
 .byte 0x2f
 .byte 0x91
 .byte 3
 .byte 0x6d, 0x6f, 0x64
 .byte 0xa0
 .byte 3
 .byte 0x61, 0x6e, 0x64
 .byte 0xb0
 .byte 2
 .byte 0x6f, 0x72
 .byte 0xb1
 .byte 3
 .byte 0x78, 0x6f, 0x72
 .byte 0xb2
 .byte 3
 .byte 0x73, 0x68, 0x6c
 .byte 0xc0
 .byte 3
 .byte 0x73, 0x68, 0x72
 .byte 0xc1
 .byte 0

single_operand_operators:
 .byte 3
 .byte 0x6e, 0x6f, 0x74
 .byte 0xd0
 .byte 3
 .byte 0x72, 0x76, 0x61
 .byte 0xe0
 .byte 0

directive_operators:
 .byte 2
 .byte 0x61, 0x74
 .byte 0x80
 .byte 2
 .byte 0x65, 0x71
 .byte 0x81
 .byte 4
 .byte 0x66, 0x72, 0x6f, 0x6d
 .byte 0x82
 .byte 2
 .byte 0x69, 0x6e
 .byte 0x83
 .byte 2
 .byte 0x6f, 0x6e
 .byte 0x84
 .byte 0

address_registers:
 .byte 2
 .byte 0x62, 0x70
 .byte 0, 0x25
 .byte 2
 .byte 0x62, 0x78
 .byte 0, 0x23
 .byte 2
 .byte 0x64, 0x69
 .byte 0, 0x27
 .byte 3
 .byte 0x65, 0x61, 0x78
 .byte 0, 0x40
 .byte 3
 .byte 0x65, 0x62, 0x70
 .byte 0, 0x45
 .byte 3
 .byte 0x65, 0x62, 0x78
 .byte 0, 0x43
 .byte 3
 .byte 0x65, 0x63, 0x78
 .byte 0, 0x41
 .byte 3
 .byte 0x65, 0x64, 0x69
 .byte 0, 0x47
 .byte 3
 .byte 0x65, 0x64, 0x78
 .byte 0, 0x42
 .byte 3
 .byte 0x65, 0x73, 0x69
 .byte 0, 0x46
 .byte 3
 .byte 0x65, 0x73, 0x70
 .byte 0, 0x44
 .byte 2
 .byte 0x73, 0x69
 .byte 0, 0x26
 .byte 0

address_sizes:
 .byte 4
 .byte 0x62, 0x79, 0x74, 0x65
 .byte 0, 1
 .byte 5
 .byte 0x64, 0x77, 0x6f, 0x72, 0x64
 .byte 0, 4
 .byte 4
 .byte 0x77, 0x6f, 0x72, 0x64
 .byte 0, 2
 .byte 0

symbols:
 .byte 2
 .byte 0x61, 0x68
 .byte 0x10, 0x14
 .byte 2
 .byte 0x61, 0x6c
 .byte 0x10, 0x10
 .byte 2
 .byte 0x61, 0x78
 .byte 0x10, 0x20
 .byte 2
 .byte 0x62, 0x68
 .byte 0x10, 0x17
 .byte 2
 .byte 0x62, 0x6c
 .byte 0x10, 0x13
 .byte 2
 .byte 0x62, 0x70
 .byte 0x10, 0x25
 .byte 2
 .byte 0x62, 0x78
 .byte 0x10, 0x23
 .byte 4
 .byte 0x62, 0x79, 0x74, 0x65
 .byte 0x11, 1
 .byte 2
 .byte 0x63, 0x68
 .byte 0x10, 0x15
 .byte 2
 .byte 0x63, 0x6c
 .byte 0x10, 0x11
 .byte 3
 .byte 0x63, 0x72, 0x30
 .byte 0x10, 0x50
 .byte 3
 .byte 0x63, 0x72, 0x32
 .byte 0x10, 0x52
 .byte 3
 .byte 0x63, 0x72, 0x33
 .byte 0x10, 0x53
 .byte 3
 .byte 0x63, 0x72, 0x34
 .byte 0x10, 0x54
 .byte 2
 .byte 0x63, 0x73
 .byte 0x10, 0x62
 .byte 2
 .byte 0x63, 0x78
 .byte 0x10, 0x21
 .byte 2
 .byte 0x64, 0x68
 .byte 0x10, 0x16
 .byte 2
 .byte 0x64, 0x69
 .byte 0x10, 0x27
 .byte 2
 .byte 0x64, 0x6c
 .byte 0x10, 0x12
 .byte 6
 .byte 0x64, 0x71, 0x77, 0x6f, 0x72, 0x64
 .byte 0x11, 16
 .byte 3
 .byte 0x64, 0x72, 0x30
 .byte 0x10, 0x70
 .byte 3
 .byte 0x64, 0x72, 0x31
 .byte 0x10, 0x71
 .byte 3
 .byte 0x64, 0x72, 0x32
 .byte 0x10, 0x72
 .byte 3
 .byte 0x64, 0x72, 0x33
 .byte 0x10, 0x73
 .byte 3
 .byte 0x64, 0x72, 0x35
 .byte 0x10, 0x75
 .byte 3
 .byte 0x64, 0x72, 0x36
 .byte 0x10, 0x76
 .byte 3
 .byte 0x64, 0x72, 0x37
 .byte 0x10, 0x77
 .byte 2
 .byte 0x64, 0x73
 .byte 0x10, 0x64
 .byte 5
 .byte 0x64, 0x77, 0x6f, 0x72, 0x64
 .byte 0x11, 4
 .byte 2
 .byte 0x64, 0x78
 .byte 0x10, 0x22
 .byte 3
 .byte 0x65, 0x61, 0x78
 .byte 0x10, 0x40
 .byte 3
 .byte 0x65, 0x62, 0x70
 .byte 0x10, 0x45
 .byte 3
 .byte 0x65, 0x62, 0x78
 .byte 0x10, 0x43
 .byte 3
 .byte 0x65, 0x63, 0x78
 .byte 0x10, 0x41
 .byte 3
 .byte 0x65, 0x64, 0x69
 .byte 0x10, 0x47
 .byte 3
 .byte 0x65, 0x64, 0x78
 .byte 0x10, 0x42
 .byte 2
 .byte 0x65, 0x73
 .byte 0x10, 0x61
 .byte 3
 .byte 0x65, 0x73, 0x69
 .byte 0x10, 0x46
 .byte 3
 .byte 0x65, 0x73, 0x70
 .byte 0x10, 0x44
 .byte 3
 .byte 0x66, 0x61, 0x72
 .byte 0x12, 2
 .byte 2
 .byte 0x66, 0x73
 .byte 0x10, 0x65
 .byte 5
 .byte 0x66, 0x77, 0x6f, 0x72, 0x64
 .byte 0x11, 6
 .byte 2
 .byte 0x67, 0x73
 .byte 0x10, 0x66
 .byte 3
 .byte 0x6d, 0x6d, 0x30
 .byte 0x10, 0x80
 .byte 3
 .byte 0x6d, 0x6d, 0x31
 .byte 0x10, 0x81
 .byte 3
 .byte 0x6d, 0x6d, 0x32
 .byte 0x10, 0x82
 .byte 3
 .byte 0x6d, 0x6d, 0x33
 .byte 0x10, 0x83
 .byte 3
 .byte 0x6d, 0x6d, 0x34
 .byte 0x10, 0x84
 .byte 3
 .byte 0x6d, 0x6d, 0x35
 .byte 0x10, 0x85
 .byte 3
 .byte 0x6d, 0x6d, 0x36
 .byte 0x10, 0x86
 .byte 3
 .byte 0x6d, 0x6d, 0x37
 .byte 0x10, 0x87
 .byte 4
 .byte 0x6e, 0x65, 0x61, 0x72
 .byte 0x12, 1
 .byte 5
 .byte 0x70, 0x77, 0x6f, 0x72, 0x64
 .byte 0x11, 6
 .byte 5
 .byte 0x71, 0x77, 0x6f, 0x72, 0x64
 .byte 0x11, 8
 .byte 2
 .byte 0x73, 0x69
 .byte 0x10, 0x26
 .byte 2
 .byte 0x73, 0x70
 .byte 0x10, 0x24
 .byte 2
 .byte 0x73, 0x73
 .byte 0x10, 0x63
 .byte 2
 .byte 0x73, 0x74
 .byte 0x10, 0xa0
 .byte 3
 .byte 0x73, 0x74, 0x30
 .byte 0x10, 0xa0
 .byte 3
 .byte 0x73, 0x74, 0x31
 .byte 0x10, 0xa1
 .byte 3
 .byte 0x73, 0x74, 0x32
 .byte 0x10, 0xa2
 .byte 3
 .byte 0x73, 0x74, 0x33
 .byte 0x10, 0xa3
 .byte 3
 .byte 0x73, 0x74, 0x34
 .byte 0x10, 0xa4
 .byte 3
 .byte 0x73, 0x74, 0x35
 .byte 0x10, 0xa5
 .byte 3
 .byte 0x73, 0x74, 0x36
 .byte 0x10, 0xa6
 .byte 3
 .byte 0x73, 0x74, 0x37
 .byte 0x10, 0xa7
 .byte 5
 .byte 0x74, 0x77, 0x6f, 0x72, 0x64
 .byte 0x11, 0xa
 .byte 5
 .byte 0x75, 0x73, 0x65, 0x31, 0x36
 .byte 0x13, 0x10
 .byte 5
 .byte 0x75, 0x73, 0x65, 0x33, 0x32
 .byte 0x13, 0x20
 .byte 4
 .byte 0x77, 0x6f, 0x72, 0x64
 .byte 0x11, 2
 .byte 4
 .byte 0x78, 0x6d, 0x6d, 0x30
 .byte 0x10, 0x90
 .byte 4
 .byte 0x78, 0x6d, 0x6d, 0x31
 .byte 0x10, 0x91
 .byte 4
 .byte 0x78, 0x6d, 0x6d, 0x32
 .byte 0x10, 0x92
 .byte 4
 .byte 0x78, 0x6d, 0x6d, 0x33
 .byte 0x10, 0x93
 .byte 4
 .byte 0x78, 0x6d, 0x6d, 0x34
 .byte 0x10, 0x94
 .byte 4
 .byte 0x78, 0x6d, 0x6d, 0x35
 .byte 0x10, 0x95
 .byte 4
 .byte 0x78, 0x6d, 0x6d, 0x36
 .byte 0x10, 0x96
 .byte 4
 .byte 0x78, 0x6d, 0x6d, 0x37
 .byte 0x10, 0x97
 .byte 0

formatter_symbols:
 /*.if !CASE_SENSITIVE*/

/**/
 /*.byte 6*/
 /*.ascii "binary"*/
 /*.byte 0x18, 1*/
 /*.byte 4*/
 /*.ascii "code"*/
 /*.byte 0x19, 5*/
 /*.byte 7*/
 /*.ascii "console"*/
 /*.byte 0x1b, 3*/
 /*.byte 4*/
 /*.ascii "data"*/
 /*.byte 0x19, 6*/
 /*.byte 11*/
 /*.ascii "discardable"*/
 /*.byte 0x19, 25*/
 /*.byte 3*/
 /*.ascii "dll"*/
 /*.byte 0x1b, 0x80*/
 /*.byte 10*/
 /*.ascii "executable"*/
 /*.byte 0x19, 29*/
 /*.byte 6*/
 /*.ascii "export"*/
 /*.byte 0x1a, 0*/
 /*.byte 6*/
 /*.ascii "fixups"*/
 /*.byte 0x1a, 5*/
 /*.byte 3*/
 /*.ascii "gui"*/
 /*.byte 0x1b, 2*/
 /*.byte 4*/
 /*.ascii "i386"*/
 /*.byte 0x1b, 0x43*/
 /*.byte 4*/
 /*.ascii "i486"*/
 /*.byte 0x1b, 0x44*/
 /*.byte 4*/
 /*.ascii "i586"*/
 /*.byte 0x1b, 0x45*/
 /*.byte 6*/
 /*.ascii "import"*/
 /*.byte 0x1a, 1*/
 /*.byte 2*/
 /*.ascii "mz"*/
 /*.byte 0x18, 2*/
 /*.byte 6*/
 /*.ascii "native"*/
 /*.byte 0x1b, 1*/
 /*.byte 2*/
 /*.ascii "pe"*/
 /*.byte 0x18, 3*/
 /*.byte 8*/
 /*.ascii "readable"*/
 /*.byte 0x19, 30*/
 /*.byte 8*/
 /*.ascii "resource"*/
 /*.byte 0x1a, 2*/
 /*.byte 9*/
 /*.ascii "shareable"*/
 /*.byte 0x19, 28*/
 /*.byte 5*/
 /*.ascii "udata"*/
 /*.byte 0x19, 7*/
 /*.byte 9*/
 /*.ascii "writeable"*/
 /*.byte 0x19, 31*/
 /*.else*/
 .byte 3
 .byte 0x44, 0x4c, 0x4c
 .byte 0x1b, 0x80
 .byte 3
 .byte 0x47, 0x55, 0x49
 .byte 0x1b, 2
 .byte 2
 .byte 0x4d, 0x5a
 .byte 0x18, 2
 .byte 2
 .byte 0x50, 0x45
 .byte 0x18, 3
 .byte 6
 .byte 0x62, 0x69, 0x6e, 0x61, 0x72, 0x79
 .byte 0x18, 1
 .byte 4
 .byte 0x63, 0x6f, 0x64, 0x65
 .byte 0x19, 5
 .byte 7
 .byte 0x63, 0x6f, 0x6e, 0x73, 0x6f, 0x6c, 0x65
 .byte 0x1b, 3
 .byte 4
 .byte 0x64, 0x61, 0x74, 0x61
 .byte 0x19, 6
 .byte 11
 .byte 0x64, 0x69, 0x73, 0x63, 0x61, 0x72, 0x64, 0x61, 0x62, 0x6c, 0x65
 .byte 0x19, 25
 .byte 10
 .byte 0x65, 0x78, 0x65, 0x63, 0x75, 0x74, 0x61, 0x62, 0x6c, 0x65
 .byte 0x19, 29
 .byte 6
 .byte 0x65, 0x78, 0x70, 0x6f, 0x72, 0x74
 .byte 0x1a, 0
 .byte 6
 .byte 0x66, 0x69, 0x78, 0x75, 0x70, 0x73
 .byte 0x1a, 5
 .byte 4
 .byte 0x69, 0x33, 0x38, 0x36
 .byte 0x1b, 0x43
 .byte 4
 .byte 0x69, 0x34, 0x38, 0x36
 .byte 0x1b, 0x44
 .byte 4
 .byte 0x69, 0x35, 0x38, 0x36
 .byte 0x1b, 0x45
 .byte 6
 .byte 0x69, 0x6d, 0x70, 0x6f, 0x72, 0x74
 .byte 0x1a, 1
 .byte 6
 .byte 0x6e, 0x61, 0x74, 0x69, 0x76, 0x65
 .byte 0x1b, 1
 .byte 8
 .byte 0x72, 0x65, 0x61, 0x64, 0x61, 0x62, 0x6c, 0x65
 .byte 0x19, 30
 .byte 8
 .byte 0x72, 0x65, 0x73, 0x6f, 0x75, 0x72, 0x63, 0x65
 .byte 0x1a, 2
 .byte 9
 .byte 0x73, 0x68, 0x61, 0x72, 0x65, 0x61, 0x62, 0x6c, 0x65
 .byte 0x19, 28
 .byte 5
 .byte 0x75, 0x64, 0x61, 0x74, 0x61
 .byte 0x19, 7
 .byte 9
 .byte 0x77, 0x72, 0x69, 0x74, 0x65, 0x61, 0x62, 0x6c, 0x65
 .byte 0x19, 31
 /*.endif*/
 .byte 0

preprocessor_directives:
 .byte 7
 .byte 0x69, 0x6e, 0x63, 0x6c, 0x75, 0x64, 0x65
 .value include_file-preprocessor
 .byte 5
 .byte 0x6d, 0x61, 0x63, 0x72, 0x6f
 .value define_macro-preprocessor
 .byte 5
 .byte 0x70, 0x75, 0x72, 0x67, 0x65
 .value purge_macro-preprocessor
 .byte 5
 .byte 0x73, 0x74, 0x72, 0x75, 0x63
 .value define_struc-preprocessor
 .byte 0

macro_directives:
 .byte 6
 .byte 0x63, 0x6f, 0x6d, 0x6d, 0x6f, 0x6e
 .value common_block-preprocessor
 .byte 7
 .byte 0x66, 0x6f, 0x72, 0x77, 0x61, 0x72, 0x64
 .value forward_block-preprocessor
 .byte 5
 .byte 0x6c, 0x6f, 0x63, 0x61, 0x6c
 .value local_symbols-preprocessor
 .byte 7
 .byte 0x72, 0x65, 0x76, 0x65, 0x72, 0x73, 0x65
 .value reverse_block-preprocessor
 .byte 0

data_handlers:
 .value --data_bytes___assembler
 .value --data_file___assembler
 .value --reserve_bytes___assembler
 .value --data_words___assembler
 .value --data_unicode___assembler
 .value --reserve_words___assembler
 .value --data_dwords___assembler
 .value --reserve_dwords___assembler
 .value --data_pwords___assembler
 .value --reserve_pwords___assembler
 .value --data_qwords___assembler
 .value --reserve_qwords___assembler
 .value --data_twords___assembler
 .value --reserve_twords___assembler

data_directives:
 .byte 2
 .byte 0x64, 0x62
 .byte 1, 0
 .byte 2
 .byte 0x64, 0x64
 .byte 4, 6
 .byte 2
 .byte 0x64, 0x70
 .byte 6, 8
 .byte 2
 .byte 0x64, 0x71
 .byte 8, 10
 .byte 2
 .byte 0x64, 0x74
 .byte 10, 12
 .byte 2
 .byte 0x64, 0x75
 .byte 2, 4
 .byte 2
 .byte 0x64, 0x77
 .byte 2, 3
 .byte 4
 .byte 0x66, 0x69, 0x6c, 0x65
 .byte 1, 1
 .byte 2
 .byte 0x72, 0x62
 .byte 1, 2
 .byte 2
 .byte 0x72, 0x64
 .byte 4, 7
 .byte 2
 .byte 0x72, 0x70
 .byte 6, 9
 .byte 2
 .byte 0x72, 0x71
 .byte 8, 11
 .byte 2
 .byte 0x72, 0x74
 .byte 10, 13
 .byte 2
 .byte 0x72, 0x77
 .byte 2, 5
 .byte 0

instructions:
 .value --instructions_2___instructions
 .value --instructions_3___instructions
 .value --instructions_4___instructions
 .value --instructions_5___instructions
 .value --instructions_6___instructions
 .value --instructions_7___instructions
 .value --instructions_8___instructions
 .value --instructions_9___instructions
 .value --instructions_10___instructions
 .value --instructions_11___instructions

/*%macro dbw 3 */
/*db %1, %2 */
/*dw %3 */
/*%endm */


instructions_2:
 .byte 0x62, 0x74
 .byte 4
 .value --bt_instruction___assembler
 .string "if"
 .value --if_directive___assembler
 .string "in"
 .value --in_instruction___assembler
 .byte 0x6a, 0x61
 .byte 0x77
 .value --conditional_jump___assembler
 .byte 0x6a, 0x62
 .byte 0x72
 .value --conditional_jump___assembler
 .byte 0x6a, 0x63
 .byte 0x72
 .value --conditional_jump___assembler
 .byte 0x6a, 0x65
 .byte 0x74
 .value --conditional_jump___assembler
 .byte 0x6a, 0x67
 .byte 0x7f
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6c
 .byte 0x7c
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6f
 .byte 0x70
 .value --conditional_jump___assembler
 .byte 0x6a, 0x70
 .byte 0x7a
 .value --conditional_jump___assembler
 .byte 0x6a, 0x73
 .byte 0x78
 .value --conditional_jump___assembler
 .byte 0x6a, 0x7a
 .byte 0x74
 .value --conditional_jump___assembler
 .byte 0x6f, 0x72
 .byte 0x8
 .value --basic_instruction___assembler
 .byte 0
instructions_3:
 .byte 0x61, 0x61, 0x61
 .byte 0x37
 .value --simple_instruction___assembler
 .byte 0x61, 0x61, 0x64
 .byte 0xd5
 .value --aa_instruction___assembler
 .byte 0x61, 0x61, 0x6d
 .byte 0xd4
 .value --aa_instruction___assembler
 .byte 0x61, 0x61, 0x73
 .byte 0x3f
 .value --simple_instruction___assembler
 .byte 0x61, 0x64, 0x63
 .byte 0x10
 .value --basic_instruction___assembler
 .byte 0x61, 0x64, 0x64
 .byte 0x0
 .value --basic_instruction___assembler
 .byte 0x61, 0x6e, 0x64
 .byte 0x20
 .value --basic_instruction___assembler
 .byte 0x62, 0x73, 0x66
 .byte 0xbc
 .value --bs_instruction___assembler
 .byte 0x62, 0x73, 0x72
 .byte 0xbd
 .value --bs_instruction___assembler
 .byte 0x62, 0x74, 0x63
 .byte 7
 .value --bt_instruction___assembler
 .byte 0x62, 0x74, 0x72
 .byte 6
 .value --bt_instruction___assembler
 .byte 0x62, 0x74, 0x73
 .byte 5
 .value --bt_instruction___assembler
 .byte 0x63, 0x62, 0x77
 .byte 0x98
 .value --simple_instruction_16bit___assembler
 .byte 0x63, 0x64, 0x71
 .byte 0x99
 .value --simple_instruction_32bit___assembler
 .byte 0x63, 0x6c, 0x63
 .byte 0xf8
 .value --simple_instruction___assembler
 .byte 0x63, 0x6c, 0x64
 .byte 0xfc
 .value --simple_instruction___assembler
 .byte 0x63, 0x6c, 0x69
 .byte 0xfa
 .value --simple_instruction___assembler
 .byte 0x63, 0x6d, 0x63
 .byte 0xf5
 .value --simple_instruction___assembler
 .byte 0x63, 0x6d, 0x70
 .byte 0x38
 .value --basic_instruction___assembler
 .byte 0x63, 0x77, 0x64
 .byte 0x99
 .value --simple_instruction_16bit___assembler
 .byte 0x64, 0x61, 0x61
 .byte 0x27
 .value --simple_instruction___assembler
 .byte 0x64, 0x61, 0x73
 .byte 0x2f
 .value --simple_instruction___assembler
 .byte 0x64, 0x65, 0x63
 .byte 1
 .value --inc_instruction___assembler
 .byte 0x64, 0x69, 0x76
 .byte 6
 .value --single_operand_instruction___assembler
 .string "end"
 .value --end_directive___assembler
 .byte 0x68, 0x6c, 0x74
 .byte 0xf4
 .value --simple_instruction___assembler
 .string "inc"
 .value --inc_instruction___assembler
 .string "ins"
 .value --ins_instruction___assembler
 .byte 0x69, 0x6e, 0x74
 .byte 0xcd
 .value --int_instruction___assembler
 .byte 0x6a, 0x61, 0x65
 .byte 0x73
 .value --conditional_jump___assembler
 .byte 0x6a, 0x62, 0x65
 .byte 0x76
 .value --conditional_jump___assembler
 .byte 0x6a, 0x67, 0x65
 .byte 0x7d
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6c, 0x65
 .byte 0x7e
 .value --conditional_jump___assembler
 .string "jmp"
 .value --jmp_instruction___assembler
 .byte 0x6a, 0x6e, 0x61
 .byte 0x76
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x62
 .byte 0x73
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x63
 .byte 0x73
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x65
 .byte 0x75
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x67
 .byte 0x7e
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x6c
 .byte 0x7d
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x6f
 .byte 0x71
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x70
 .byte 0x7b
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x73
 .byte 0x79
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x7a
 .byte 0x75
 .value --conditional_jump___assembler
 .byte 0x6a, 0x70, 0x65
 .byte 0x7a
 .value --conditional_jump___assembler
 .byte 0x6a, 0x70, 0x6f
 .byte 0x7b
 .value --conditional_jump___assembler
 .byte 0x6c, 0x61, 0x72
 .byte 2
 .value --lar_instruction___assembler
 .byte 0x6c, 0x64, 0x73
 .byte 3
 .value --ls_instruction___assembler
 .string "lea"
 .value --lea_instruction___assembler
 .string "les"
 .value --ls_instruction___assembler
 .byte 0x6c, 0x66, 0x73
 .byte 4
 .value --ls_instruction___assembler
 .byte 0x6c, 0x67, 0x73
 .byte 5
 .value --ls_instruction___assembler
 .byte 0x6c, 0x73, 0x6c
 .byte 3
 .value --lar_instruction___assembler
 .byte 0x6c, 0x73, 0x73
 .byte 2
 .value --ls_instruction___assembler
 .string "mov"
 .value --mov_instruction___assembler
 .byte 0x6d, 0x75, 0x6c
 .byte 4
 .value --single_operand_instruction___assembler
 .byte 0x6e, 0x65, 0x67
 .byte 3
 .value --single_operand_instruction___assembler
 .byte 0x6e, 0x6f, 0x70
 .byte 0x90
 .value --simple_instruction___assembler
 .byte 0x6e, 0x6f, 0x74
 .byte 2
 .value --single_operand_instruction___assembler
 .string "org"
 .value --org_directive___assembler
 .string "out"
 .value --out_instruction___assembler
 .string "pop"
 .value --pop_instruction___assembler
 .byte 0x72, 0x63, 0x6c
 .byte 2
 .value --sh_instruction___assembler
 .byte 0x72, 0x63, 0x72
 .byte 3
 .value --sh_instruction___assembler
 .byte 0x72, 0x65, 0x70
 .byte 0xf3
 .value --prefix_instruction___assembler
 .byte 0x72, 0x65, 0x74
 .byte 0xc2
 .value --ret_instruction___assembler
 .string "rol"
 .value --sh_instruction___assembler
 .byte 0x72, 0x6f, 0x72
 .byte 1
 .value --sh_instruction___assembler
 .byte 0x72, 0x73, 0x6d
 .byte 0xaa
 .value --simple_extended_instruction___assembler
 .byte 0x73, 0x61, 0x6c
 .byte 6
 .value --sh_instruction___assembler
 .byte 0x73, 0x61, 0x72
 .byte 7
 .value --sh_instruction___assembler
 .byte 0x73, 0x62, 0x62
 .byte 0x18
 .value --basic_instruction___assembler
 .byte 0x73, 0x68, 0x6c
 .byte 4
 .value --sh_instruction___assembler
 .byte 0x73, 0x68, 0x72
 .byte 5
 .value --sh_instruction___assembler
 .byte 0x73, 0x74, 0x63
 .byte 0xf9
 .value --simple_instruction___assembler
 .byte 0x73, 0x74, 0x64
 .byte 0xfd
 .value --simple_instruction___assembler
 .byte 0x73, 0x74, 0x69
 .byte 0xfb
 .value --simple_instruction___assembler
 .byte 0x73, 0x75, 0x62
 .byte 0x28
 .value --basic_instruction___assembler
 .byte 0x75, 0x64, 0x32
 .byte 0xb
 .value --simple_extended_instruction___assembler
 .byte 0x78, 0x6f, 0x72
 .byte 0x30
 .value --basic_instruction___assembler
 .byte 0
instructions_4:
 .string "arpl"
 .value --arpl_instruction___assembler
 .string "call"
 .value --call_instruction___assembler
 .byte 0x63, 0x6c, 0x74, 0x73
 .byte 6
 .value --simple_extended_instruction___assembler
 .string "cmps"
 .value --cmps_instruction___assembler
 .byte 0x63, 0x77, 0x64, 0x65
 .byte 0x98
 .value --simple_instruction_32bit___assembler
 .string "else"
 .value --else_directive___assembler
 .string "heap"
 .value --heap_directive___assembler
 .byte 0x69, 0x64, 0x69, 0x76
 .byte 7
 .value --single_operand_instruction___assembler
 .string "imul"
 .value --imul_instruction___assembler
 .byte 0x69, 0x6e, 0x74, 0x33
 .byte 0xcc
 .value --simple_instruction___assembler
 .byte 0x69, 0x6e, 0x74, 0x6f
 .byte 0xce
 .value --simple_instruction___assembler
 .byte 0x69, 0x6e, 0x76, 0x64
 .byte 8
 .value --simple_extended_instruction___assembler
 .byte 0x69, 0x72, 0x65, 0x74
 .byte 0xcf
 .value --simple_instruction___assembler
 .byte 0x6a, 0x63, 0x78, 0x7a
 .byte 0xe3
 .value --loop_instruction_16bit___assembler
 .byte 0x6a, 0x6e, 0x61, 0x65
 .byte 0x72
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x62, 0x65
 .byte 0x77
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x67, 0x65
 .byte 0x7c
 .value --conditional_jump___assembler
 .byte 0x6a, 0x6e, 0x6c, 0x65
 .byte 0x7f
 .value --conditional_jump___assembler
 .byte 0x6c, 0x61, 0x68, 0x66
 .byte 0x9f
 .value --simple_instruction___assembler
 .string "load"
 .value --load_directive___assembler
 .byte 0x6c, 0x6f, 0x63, 0x6b
 .byte 0xf0
 .value --prefix_instruction___assembler
 .string "lods"
 .value --lods_instruction___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70
 .byte 0xe2
 .value --loop_instruction___assembler
 .string "movs"
 .value --movs_instruction___assembler
 .string "outs"
 .value --outs_instruction___assembler
 .byte 0x70, 0x6f, 0x70, 0x61
 .byte 0x61
 .value --simple_instruction___assembler
 .byte 0x70, 0x6f, 0x70, 0x66
 .byte 0x9d
 .value --simple_instruction___assembler
 .string "push"
 .value --push_instruction___assembler
 .byte 0x72, 0x65, 0x70, 0x65
 .byte 0xf3
 .value --prefix_instruction___assembler
 .byte 0x72, 0x65, 0x70, 0x7a
 .byte 0xf3
 .value --prefix_instruction___assembler
 .byte 0x72, 0x65, 0x74, 0x64
 .byte 0xc2
 .value --ret_instruction_32bit___assembler
 .byte 0x72, 0x65, 0x74, 0x66
 .byte 0xca
 .value --ret_instruction___assembler
 .byte 0x72, 0x65, 0x74, 0x6e
 .byte 0xc2
 .value --ret_instruction___assembler
 .byte 0x72, 0x65, 0x74, 0x77
 .byte 0xc2
 .value --ret_instruction_16bit___assembler
 .byte 0x73, 0x61, 0x68, 0x66
 .byte 0x9e
 .value --simple_instruction___assembler
 .byte 0x73, 0x63, 0x61, 0x73
 .byte 0xae
 .value --stos_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x61
 .byte 0x97
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x62
 .byte 0x92
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x63
 .byte 0x92
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x65
 .byte 0x94
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x67
 .byte 0x9f
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6c
 .byte 0x9c
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6f
 .byte 0x90
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x70
 .byte 0x9a
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x73
 .byte 0x98
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x7a
 .byte 0x94
 .value --set_instruction___assembler
 .byte 0x73, 0x68, 0x6c, 0x64
 .byte 0xa4
 .value --shd_instruction___assembler
 .byte 0x73, 0x68, 0x72, 0x64
 .byte 0xac
 .value --shd_instruction___assembler
 .byte 0x73, 0x74, 0x6f, 0x73
 .byte 0xaa
 .value --stos_instruction___assembler
 .string "test"
 .value --test_instruction___assembler
 .byte 0x77, 0x61, 0x69, 0x74
 .byte 0x9b
 .value --simple_instruction___assembler
 .byte 0x78, 0x61, 0x64, 0x64
 .byte 0xc0
 .value --basic_486_instruction___assembler
 .string "xchg"
 .value --xchg_instruction___assembler
 .byte 0x78, 0x6c, 0x61, 0x74
 .byte 0xd7
 .value --xlat_instruction___assembler
 .byte 0
instructions_5:
 .string "bound"
 .value --bound_instruction___assembler
 .string "bswap"
 .value --bswap_instruction___assembler
 .byte 0x63, 0x6d, 0x70, 0x73, 0x62
 .byte 0xa6
 .value --simple_instruction___assembler
 .string "cmpsd"
 .value --cmpsd_instruction___assembler
 .byte 0x63, 0x6d, 0x70, 0x73, 0x77
 .byte 0xa7
 .value --simple_instruction_16bit___assembler
 .byte 0x63, 0x70, 0x75, 0x69, 0x64
 .byte 0xa2
 .value --simple_extended_instruction___assembler
 .string "enter"
 .value --enter_instruction___assembler
 .string "entry"
 .value --entry_directive___assembler
 .byte 0x66, 0x77, 0x61, 0x69, 0x74
 .byte 0x9b
 .value --simple_instruction___assembler
 .byte 0x69, 0x72, 0x65, 0x74, 0x64
 .byte 0xcf
 .value --simple_instruction_32bit___assembler
 .byte 0x69, 0x72, 0x65, 0x74, 0x77
 .byte 0xcf
 .value --simple_instruction_16bit___assembler
 .byte 0x6a, 0x65, 0x63, 0x78, 0x7a
 .byte 0xe3
 .value --loop_instruction_32bit___assembler
 .string "label"
 .value --label_directive___assembler
 .byte 0x6c, 0x65, 0x61, 0x76, 0x65
 .byte 0xc9
 .value --simple_instruction___assembler
 .byte 0x6c, 0x6f, 0x64, 0x73, 0x62
 .byte 0xac
 .value --simple_instruction___assembler
 .byte 0x6c, 0x6f, 0x64, 0x73, 0x64
 .byte 0xad
 .value --simple_instruction_32bit___assembler
 .byte 0x6c, 0x6f, 0x64, 0x73, 0x77
 .byte 0xad
 .value --simple_instruction_16bit___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x64
 .byte 0xe2
 .value --loop_instruction_32bit___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x65
 .byte 0xe1
 .value --loop_instruction___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x77
 .byte 0xe2
 .value --loop_instruction_16bit___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x7a
 .byte 0xe1
 .value --loop_instruction___assembler
 .byte 0x6d, 0x6f, 0x76, 0x73, 0x62
 .byte 0xa4
 .value --simple_instruction___assembler
 .string "movsd"
 .value --movsd_instruction___assembler
 .byte 0x6d, 0x6f, 0x76, 0x73, 0x77
 .byte 0xa5
 .value --simple_instruction_16bit___assembler
 .byte 0x6d, 0x6f, 0x76, 0x73, 0x78
 .byte 0xbe
 .value --movx_instruction___assembler
 .byte 0x6d, 0x6f, 0x76, 0x7a, 0x78
 .byte 0xb6
 .value --movx_instruction___assembler
 .byte 0x70, 0x6f, 0x70, 0x61, 0x64
 .byte 0x61
 .value --simple_instruction_32bit___assembler
 .byte 0x70, 0x6f, 0x70, 0x61, 0x77
 .byte 0x61
 .value --simple_instruction_16bit___assembler
 .byte 0x70, 0x6f, 0x70, 0x66, 0x64
 .byte 0x9d
 .value --simple_instruction_32bit___assembler
 .byte 0x70, 0x6f, 0x70, 0x66, 0x77
 .byte 0x9d
 .value --simple_instruction_16bit___assembler
 .byte 0x70, 0x75, 0x73, 0x68, 0x61
 .byte 0x60
 .value --simple_instruction___assembler
 .byte 0x70, 0x75, 0x73, 0x68, 0x66
 .byte 0x9c
 .value --simple_instruction___assembler
 .byte 0x72, 0x65, 0x70, 0x6e, 0x65
 .byte 0xf2
 .value --prefix_instruction___assembler
 .byte 0x72, 0x65, 0x70, 0x6e, 0x7a
 .byte 0xf2
 .value --prefix_instruction___assembler
 .byte 0x72, 0x65, 0x74, 0x66, 0x64
 .byte 0xca
 .value --ret_instruction_32bit___assembler
 .byte 0x72, 0x65, 0x74, 0x66, 0x77
 .byte 0xca
 .value --ret_instruction_16bit___assembler
 .byte 0x72, 0x65, 0x74, 0x6e, 0x64
 .byte 0xc2
 .value --ret_instruction_32bit___assembler
 .byte 0x72, 0x65, 0x74, 0x6e, 0x77
 .byte 0xc2
 .value --ret_instruction_16bit___assembler
 .byte 0x73, 0x63, 0x61, 0x73, 0x62
 .byte 0xae
 .value --simple_instruction___assembler
 .byte 0x73, 0x63, 0x61, 0x73, 0x64
 .byte 0xaf
 .value --simple_instruction_32bit___assembler
 .byte 0x73, 0x63, 0x61, 0x73, 0x77
 .byte 0xaf
 .value --simple_instruction_16bit___assembler
 .byte 0x73, 0x65, 0x74, 0x61, 0x65
 .byte 0x93
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x62, 0x65
 .byte 0x96
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x67, 0x65
 .byte 0x9d
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6c, 0x65
 .byte 0x9e
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x61
 .byte 0x96
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x62
 .byte 0x93
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x63
 .byte 0x93
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x65
 .byte 0x95
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x67
 .byte 0x9e
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x6c
 .byte 0x9d
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x6f
 .byte 0x91
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x70
 .byte 0x9b
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x73
 .byte 0x99
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x7a
 .byte 0x95
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x70, 0x65
 .byte 0x9a
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x70, 0x6f
 .byte 0x9b
 .value --set_instruction___assembler
 .string "stack"
 .value --stack_directive___assembler
 .byte 0x73, 0x74, 0x6f, 0x73, 0x62
 .byte 0xaa
 .value --simple_instruction___assembler
 .byte 0x73, 0x74, 0x6f, 0x73, 0x64
 .byte 0xab
 .value --simple_instruction_32bit___assembler
 .byte 0x73, 0x74, 0x6f, 0x73, 0x77
 .byte 0xab
 .value --simple_instruction_16bit___assembler
 .string "times"
 .value --times_directive___assembler
 .byte 0x78, 0x6c, 0x61, 0x74, 0x62
 .byte 0xd7
 .value --simple_instruction___assembler
 .byte 0
instructions_6:
 .string "format"
 .value --format_directive___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x65, 0x64
 .byte 0xe1
 .value --loop_instruction_32bit___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x65, 0x77
 .byte 0xe1
 .value --loop_instruction_16bit___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x6e, 0x65
 .byte 0xe0
 .value --loop_instruction___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x6e, 0x7a
 .byte 0xe0
 .value --loop_instruction___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x7a, 0x64
 .byte 0xe1
 .value --loop_instruction_32bit___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x7a, 0x77
 .byte 0xe1
 .value --loop_instruction_16bit___assembler
 .byte 0x70, 0x75, 0x73, 0x68, 0x61, 0x64
 .byte 0x60
 .value --simple_instruction_32bit___assembler
 .byte 0x70, 0x75, 0x73, 0x68, 0x61, 0x77
 .byte 0x60
 .value --simple_instruction_16bit___assembler
 .byte 0x70, 0x75, 0x73, 0x68, 0x66, 0x64
 .byte 0x9c
 .value --simple_instruction_32bit___assembler
 .byte 0x70, 0x75, 0x73, 0x68, 0x66, 0x77
 .byte 0x9c
 .value --simple_instruction_16bit___assembler
 .string "repeat"
 .value --repeat_directive___assembler
 .byte 0x73, 0x65, 0x74, 0x61, 0x6c, 0x63
 .byte 0xd6
 .value --simple_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x61, 0x65
 .byte 0x92
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x62, 0x65
 .byte 0x97
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x67, 0x65
 .byte 0x9c
 .value --set_instruction___assembler
 .byte 0x73, 0x65, 0x74, 0x6e, 0x6c, 0x65
 .byte 0x9f
 .value --set_instruction___assembler
 .byte 0
instructions_7:
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x6e, 0x65, 0x64
 .byte 0xe0
 .value --loop_instruction_32bit___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x6e, 0x65, 0x77
 .byte 0xe0
 .value --loop_instruction_16bit___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x6e, 0x7a, 0x64
 .byte 0xe0
 .value --loop_instruction_32bit___assembler
 .byte 0x6c, 0x6f, 0x6f, 0x70, 0x6e, 0x7a, 0x77
 .byte 0xe0
 .value --loop_instruction_16bit___assembler
 .string "virtual"
 .value --virtual_directive___assembler
 .byte 0
instructions_8:
 .byte 0
instructions_9:
 .byte 0
instructions_10:
 .byte 0
instructions_11:
 .byte 0

/* %include done */

_copyright:
.string "Copyright (c) 1999-2002, Tomasz Grysztar"

_logo:
.byte 0x66, 0x6c, 0x61, 0x74, 0x20, 0x61, 0x73, 0x73, 0x65, 0x6d, 0x62, 0x6c, 0x65, 0x72, 0x20, 0x20, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x20
.byte 0x31, 0x2e, 0x33, 0x30, 0x2d, 0x62, 0x6f, 0x6f, 0x74, 0x73, 0x74, 0x72, 0x61, 0x70
.byte 0xa
.byte 0



_usage:
.byte 0x75, 0x73, 0x61, 0x67, 0x65, 0x3a, 0x20, 0x66, 0x61, 0x73, 0x6d, 0x20, 0x73, 0x6f, 0x75, 0x72, 0x63, 0x65, 0x20, 0x6f, 0x75, 0x74, 0x70, 0x75, 0x74
.byte 0xa, 0

_passes_suffix:
.string " passes, "
_seconds_suffix:
.string " seconds, "
_bytes_suffix:
.byte 0x20, 0x62, 0x79, 0x74, 0x65, 0x73, 0x2e
.byte 0xa, 0

_counter:
.byte 4
.byte 0x30, 0x30, 0x30, 0x30

prebss:
.align 4
/* bss_align = 0 */
.bss  /*  We could use `absolute $' here instead, but that's broken (breaks address calculation in program_end-bss+prebss-file_header) in NASM 0.95--0.97. */
bss:
/* .fill bss_align, 1, 0  #  Uninitialized data follows. */

memory_start:
. = . + [4]
memory_end:
. = . + [4]
additional_memory:
. = . + [4]
additional_memory_end:
. = . + [4]
input_file:
. = . + [4]
output_file:
. = . + [4]
source_start:
. = . + [4]
code_start:
. = . + [4]
code_size:
. = . + [4]
real_code_size:
. = . + [4]
start_time:
. = . + [4]
written_size:
. = . + [4]

current_line:
. = . + [4]
macros_list:
. = . + [4]
macro_constants:
. = . + [4]
macro_block:
. = . + [4]
macro_block_line_number:
. = . + [4]
struc_name:
. = . + [4]
current_locals_prefix:
. = . + [4]
labels_list:
. = . + [4]
label_hash:
. = . + [4]
org_start:
. = . + [4]
org_sib:
. = . + [4]
undefined_data_start:
. = . + [4]
undefined_data_end:
. = . + [4]
counter:
. = . + [4]
counter_limit:
. = . + [4]
error_line:
. = . + [4]
error:
. = . + [4]
display_buffer:
. = . + [4]
structures_buffer:
. = . + [4]
number_start:
. = . + [4]
current_offset:
. = . + [4]
value:
. = . + [8]
fp_value:
. = . + [8]
format_flags:
. = . + [4]
number_of_relocations:
. = . + [4]
number_of_sections:
. = . + [4]
stub_size:
. = . + [4]
header_data:
. = . + [4]
sections_data:
. = . + [4]
current_section:
. = . + [4]
machine:
. = . + [2]
subsystem:
. = . + [2]
subsystem_version:
. = . + [4]

macro_status:
. = . + [1]
parenthesis_stack:
. = . + [1]
output_format:
. = . + [1]
code_type:
. = . + [1]
current_pass:
. = . + [1]
next_pass_needed:
. = . + [1]
reloc_labels:
. = . + [1]
times_working:
. = . + [1]
virtual_data:
. = . + [1]
fp_sign:
. = . + [1]
fp_format:
. = . + [1]  /*  TODO(pts): Remove unused variables. */
value_size:
. = . + [1]
forced_size:
. = . + [1]
value_type:
. = . + [1]
address_size:
. = . + [1]
compare_type:
. = . + [1]
base_code:
. = . + [1]
extended_code:
. = . + [1]
postbyte_register:
. = . + [1]
segment_register:
. = . + [1]
operand_size:
. = . + [1]
imm_sized:
. = . + [1]
jump_type:
. = . + [1]
mmx_size:
. = . + [1]
mmx_prefix:
. = . + [1]
nextbyte:
. = . + [1]

characters:
. = . + [0x100]
converted:
. = . + [0x100]
available_memory:
. = . + [4]

program_end:

/*  __END__ */
aa_instruction___assembler = aa_instruction-assembler
arpl_instruction___assembler = arpl_instruction-assembler
basic_486_instruction___assembler = basic_486_instruction-assembler
basic_instruction___assembler = basic_instruction-assembler
bound_instruction___assembler = bound_instruction-assembler
bs_instruction___assembler = bs_instruction-assembler
bswap_instruction___assembler = bswap_instruction-assembler
bt_instruction___assembler = bt_instruction-assembler
call_instruction___assembler = call_instruction-assembler
cmps_instruction___assembler = cmps_instruction-assembler
cmpsd_instruction___assembler = cmpsd_instruction-assembler
conditional_jump___assembler = conditional_jump-assembler
data_bytes___assembler = data_bytes-assembler
data_dwords___assembler = data_dwords-assembler
data_file___assembler = data_file-assembler
data_pwords___assembler = data_pwords-assembler
data_qwords___assembler = data_qwords-assembler
data_twords___assembler = data_twords-assembler
data_unicode___assembler = data_unicode-assembler
data_words___assembler = data_words-assembler
else_directive___assembler = else_directive-assembler
end_directive___assembler = end_directive-assembler
enter_instruction___assembler = enter_instruction-assembler
entry_directive___assembler = entry_directive-assembler
format_directive___assembler = format_directive-assembler
heap_directive___assembler = heap_directive-assembler
if_directive___assembler = if_directive-assembler
imul_instruction___assembler = imul_instruction-assembler
in_instruction___assembler = in_instruction-assembler
inc_instruction___assembler = inc_instruction-assembler
ins_instruction___assembler = ins_instruction-assembler
instructions_10___instructions = instructions_10-instructions
instructions_11___instructions = instructions_11-instructions
instructions_2___instructions = instructions_2-instructions
instructions_3___instructions = instructions_3-instructions
instructions_4___instructions = instructions_4-instructions
instructions_5___instructions = instructions_5-instructions
instructions_6___instructions = instructions_6-instructions
instructions_7___instructions = instructions_7-instructions
instructions_8___instructions = instructions_8-instructions
instructions_9___instructions = instructions_9-instructions
int_instruction___assembler = int_instruction-assembler
jmp_instruction___assembler = jmp_instruction-assembler
label_directive___assembler = label_directive-assembler
lar_instruction___assembler = lar_instruction-assembler
lea_instruction___assembler = lea_instruction-assembler
load_directive___assembler = load_directive-assembler
lods_instruction___assembler = lods_instruction-assembler
loop_instruction___assembler = loop_instruction-assembler
loop_instruction_16bit___assembler = loop_instruction_16bit-assembler
loop_instruction_32bit___assembler = loop_instruction_32bit-assembler
ls_instruction___assembler = ls_instruction-assembler
mov_instruction___assembler = mov_instruction-assembler
movs_instruction___assembler = movs_instruction-assembler
movsd_instruction___assembler = movsd_instruction-assembler
movx_instruction___assembler = movx_instruction-assembler
org_directive___assembler = org_directive-assembler
out_instruction___assembler = out_instruction-assembler
outs_instruction___assembler = outs_instruction-assembler
pop_instruction___assembler = pop_instruction-assembler
prefix_instruction___assembler = prefix_instruction-assembler
push_instruction___assembler = push_instruction-assembler
repeat_directive___assembler = repeat_directive-assembler
reserve_bytes___assembler = reserve_bytes-assembler
reserve_dwords___assembler = reserve_dwords-assembler
reserve_pwords___assembler = reserve_pwords-assembler
reserve_qwords___assembler = reserve_qwords-assembler
reserve_twords___assembler = reserve_twords-assembler
reserve_words___assembler = reserve_words-assembler
ret_instruction___assembler = ret_instruction-assembler
ret_instruction_16bit___assembler = ret_instruction_16bit-assembler
ret_instruction_32bit___assembler = ret_instruction_32bit-assembler
set_instruction___assembler = set_instruction-assembler
sh_instruction___assembler = sh_instruction-assembler
shd_instruction___assembler = shd_instruction-assembler
simple_extended_instruction___assembler = simple_extended_instruction-assembler
simple_instruction___assembler = simple_instruction-assembler
simple_instruction_16bit___assembler = simple_instruction_16bit-assembler
simple_instruction_32bit___assembler = simple_instruction_32bit-assembler
single_operand_instruction___assembler = single_operand_instruction-assembler
stack_directive___assembler = stack_directive-assembler
stos_instruction___assembler = stos_instruction-assembler
test_instruction___assembler = test_instruction-assembler
times_directive___assembler = times_directive-assembler
virtual_directive___assembler = virtual_directive-assembler
xchg_instruction___assembler = xchg_instruction-assembler
xlat_instruction___assembler = xlat_instruction-assembler
