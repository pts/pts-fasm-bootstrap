##  #  by pts@fazekas.hu at Thu Mar 21 07:44:40 CET 2024
##  #
##  #  This is a subset of the source code of fasm 1.30 (with some CPU instructions,
##  #  `format MZ' and `format PE' removed), ported to NASM syntax, for Linux i386
##  #  only. It's useful for bootstrapping fasm.
##  #
##  #  Compile with: nasm-0.98.39 -O0 -w+orphan-labels -f bin -o fbsasm fbsasm.asm && chmod +x fbsasm  # Fast.
##  #  Compile with: nasm-0.98.39 -O1 -Dnear_o0= -w+orphan-labels -f bin -o fbsasm fbsasm.asm && chmod +x fbsasm
##  #  Compile with: nasm-0.98.39 -O999999999 -Dnear_o0= -w+orphan-labels -f bin -o fbsasm fbsasm.asm && chmod +x fbsasm
##  #
##  #  Compile the GNU as(1) version (fbsasm.s) with: as --32 -march=i386 -o fbsasm.o fbsasm && ld -m elf_i386 -N -s -o fbsasm fbsasm.o
##  #  Compile the GNU as(1) version (fbsasm.s) with earlier versions of GNU as(1) with: as -o fbsasm.o fbsasm && ld -m elf_i386 -N -s -o fbsasm fbsasm.o
##  #
##  #  Lines in fbsasm.nasm and fbsasm.fasm correspond to each other.
##  #
# This file is autognerated by nasm2as.pl
##
##  #  flat assembler 0.37 source, fasm.asm
##  #  Copyright (c) 1999-2002, Tomasz Grysztar
##  #  All rights reserved.
##
	##program_base equ 0x700000
##%ifndef near_o0
##%define near_o0 near  #  For `nasm -O0'.
##%endif
##
	##org	program_base
	##use32
	##cpu 386
##
##  # 	macro	align value { rb (value-1) - ($ + value-1) mod value }
##
##file_header:
	##db	0x7F,'ELF',1,1,1
	##times	file_header+0x10-$ db 0
	##dw	2,3
	##dd	1,start
	##dd	program_header-file_header,0,0
	##dw	program_header-file_header,0x20,1,0,0,0
##
##program_header:
	##dd	1,0,program_base,0
	##dd	prebss-file_header,program_end-bss+prebss-file_header,7,0x1000
##
.globl _start
_start:  # Program entry point.
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
	xor %al, %al
      make_characters_table:
	stosb %al, %es:(%edi)
	inc %al
	loop make_characters_table
	mov $characters+'a, %esi
	mov $characters+'A, %edi
	mov $26, %ecx
	rep
	movsb %ds:(%esi), %es:(%edi)
	mov $characters, %edi
	mov $symbol_characters+1, %esi
	movzbl -1(%esi), %ecx
	xor %ebx, %ebx
      convert_table:
	lodsb %ds:(%esi), %al
	mov %al, %bl
	movb $0, (%edi,%ebx,1)
	loop convert_table

	push %eax
	push %eax  #  alloca(8) for the gettimeofday buffer.
	mov $78, %eax  #  SYS_gettimeofday.
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
	pop %ecx  #  Free the gettimeofday buffer.
	add %ebx, %eax
	movl %eax, start_time

	call preprocessor
	call parser
	call assembler

	movzbl current_pass, %eax
	inc %al
	call display_number
	mov $_passes_suffix, %esi
	call display_string
	push %eax
	push %eax  #  alloca(8) for the gettimeofday buffer.
	mov $78, %eax  #  SYS_gettimeofday.
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
	pop %ecx  #  Free the gettimeofday buffer.
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
	mov $'., %dl
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
	xor %al, %al
	jmp exit_program

information:
	mov $_usage, %esi
	call display_string
	mov $1, %al
	jmp exit_program

# %include 'system.inc'

#  flat assembler 0.37 source, system.inc
#  Copyright (c) 1999-2002, Tomasz Grysztar
#  All rights reserved.

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
	mov $45, %eax  #  SYS_brk.
	int $0x80
	movl %eax, additional_memory
# mov	ebx,syscall_buffer
# mov	eax,116  ; SYS_sysinfo. We are interested only the sysinfo.freeram field ([syscall_buffer+14h]), but on modern Linux it's not bytes anymore (see mem_unit in sysinfo(2)), so it's meaningless below.
# int	0x80
# mov dword [available_memory],0x100000  ; Hardcode allocating maximum 1 MiB. 1 MiB enough, but 0.75 MiB is not enough to compile fasm 1.30.
	movl $0x280000, available_memory  #  Hardcode allocating maximum 2.5 MiB. 1 MiB enough, but 0.75 MiB is not enough to compile fasm 1.30. 2.5 MiB is enough to compile fasm 1.73.32.
    allocate_memory:
	mov additional_memory, %ebx
	add available_memory, %ebx
	mov $45, %eax  #  SYS_brk.
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
	mov $1, %eax  #  SYS_exit.
	int $0x80

open:
	push %edx
	push %esi
	push %edi
	push %ebp
	mov %edx, %ebx
	mov $5, %eax  #  SYS_open.
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
	mov $5, %eax  #  SYS_open.
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
	mov $6, %eax  #  SYS_close.
	int $0x80
	ret
read:
	push %ecx
	push %edx
	push %esi
	push %edi
	push %ebp
	mov $3, %eax  #  SYS_read.
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
	mov $4, %eax  #  SYS_write.
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
	mov %al, %dl
	mov $19, %eax  #  SYS_lseek.
	int $0x80
	clc
	ret

display_string:
	push %ebx
	mov %esi, %edi
	mov %esi, %edx
	or $-1, %ecx
	xor %al, %al
	repne
	scasb %es:(%edi), %al
	neg %ecx
	sub $2, %ecx
	mov $4, %eax  #  SYS_write.
	mov $1, %ebx
	xchg %edx, %ecx
	int $0x80
	pop %ebx
	ret
display_block:
	push %ebx
	mov $4, %eax  #  SYS_write.
	mov $1, %ebx
	mov %ecx, %edx
	mov %esi, %ecx
	int $0x80
	pop %ebx
	ret
display_character:
	push %ebx
	mov %dl, character
	mov $4, %eax  #  SYS_write.
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
	xor %bl, %bl
      display_loop:
	div %ecx
	push %edx
	cmp $1, %ecx
	je display_digit
	or %bl, %bl
	jnz display_digit
	or %al, %al
	jz digit_ok
	not %bl
      display_digit:
	mov %al, %dl
	add $0x30, %dl
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
	mov $0xff, %al
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
	mov $'], %dl
	call display_character
	cmp current_line, %ebx
	je line_number_ok
	mov $0x20, %dl
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
	mov $'], %dl
	call display_character
      line_number_ok:
	mov $line_data_start, %esi
	call display_string
	mov %ebx, %esi
	mov (%esi), %edx
	call open
	mov $2, %al
	xor %edx, %edx
	call lseek
	mov 8(%esi), %edx
	sub %edx, %eax
	push %eax
	xor %al, %al
	call lseek
	mov (%esp), %ecx
	mov memory_start, %edx
	call read
	call close
	pop %ecx
	mov memory_start, %esi
      get_line_data:
	mov (%esi), %al
	cmp $0xa, %al
	je display_line_data
	cmp $0xd, %al
	je display_line_data
	cmp $0x1a, %al
	je display_line_data
	or %al, %al
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
.ascii "error: "
.byte 0
error_suffix:
.ascii "."
lf:
.byte 0xa, 0
line_number_start:
.ascii " ["
.byte 0
line_data_start:
.ascii ":"
.byte 0xa, 0

##%macro dm 1
  ##db %1, 0
##%endm

# %include '../version.inc'

#  flat assembler  version 1.30
#  Copyright (c) 1999-2002, Tomasz Grysztar
#  All rights reserved.
#
#  This programs is free for commercial and non-commercial use as long as
#  the following conditions are aheared to.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are
#  met:
#
#  1. Redistributions of source code must retain the above copyright notice,
#     this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
#  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
#  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR
#  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
#  The licence and distribution terms for any publically available
#  version or derivative of this code cannot be changed. i.e. this code
#  cannot simply be copied and put under another distribution licence
#  (including the GNU Public Licence).

##%define VERSION_STRING '1.30-bootstrap'



VERSION_MAJOR = 1
VERSION_MINOR = 30

# %include '../errors.inc'

#  flat assembler source
#  Copyright (c) 1999-2001, Tomasz Grysztar
#  All rights reserved.

out_of_memory:
	call fatal_error
	.asciz "out of memory"
main_file_not_found:
	call fatal_error
	.asciz "source file not found"
write_failed:
	call fatal_error
	.asciz "write failed"
code_cannot_be_generated:
	call fatal_error
	.asciz "code cannot be generated"
unexpected_end_of_file:
	call fatal_error
	.asciz "unexpected end of file"
file_not_found:
	call assembler_error
	.asciz "file not found"
error_reading_file:
	call assembler_error
	.asciz "error reading file"
invalid_macro_arguments:
	call assembler_error
	.asciz "invalid macro arguments"
unexpected_characters:
	call assembler_error
	.asciz "unexpected characters"
invalid_argument:
	call assembler_error
	.asciz "invalid argument"
illegal_instruction:
	call assembler_error
	.asciz "illegal instruction"
unexpected_instruction:
	call assembler_error
	.asciz "unexpected instruction"
invalid_operand:
	call assembler_error
	.asciz "invalid operand"
invalid_operand_size:
	call assembler_error
	.asciz "invalid size of operand"
operand_size_not_specified:
	call assembler_error
	.asciz "operand size not specified"
operand_sizes_do_not_match:
	call assembler_error
	.asciz "operand sizes do not match"
invalid_address_size:
	call assembler_error
	.asciz "invalid size of address value"
address_sizes_do_not_agree:
	call assembler_error
	.asciz "address sizes do not agree"
invalid_expression:
	call assembler_error
	.asciz "invalid expression"
invalid_address:
	call assembler_error
	.asciz "invalid address"
invalid_value:
	call assembler_error
	.asciz "invalid value"
value_out_of_range:
	call assembler_error
	.asciz "value out of range"
invalid_use_of_symbol:
	call assembler_error
	.asciz "invalid use of symbol"
relative_jump_out_of_range:
	call assembler_error
	.asciz "relative jump out of range"
extra_characters_on_line:
	call assembler_error
	.asciz "extra characters on line"
name_too_long:
	call assembler_error
	.asciz "name too long"
invalid_name:
	call assembler_error
	.asciz "invalid name"
reserved_word_used_as_symbol:
	call assembler_error
	.asciz "reserved word used as symbol"
symbol_already_defined:
	call assembler_error
	.asciz "symbol already defined"
missing_end_quote:
	call assembler_error
	.asciz "missing end quote"

# %include '../expressi.inc'

#  flat assembler source
#  Copyright (c) 1999-2001, Tomasz Grysztar
#  All rights reserved.

convert_expression:
	push %ebp
	mov %esp, %ebp
	push %edi
	mov $operators, %edi
	call get_operator
	pop %edi
	or %al, %al
	jz expression_loop
	push %ebp
	cmp $0x80, %al
	je init_positive
	cmp $0x81, %al
	je init_negative
	jmp invalid_expression
      init_positive:
	xor %al, %al
	jmp expression_number
      init_negative:
	mov $0xd1, %al
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
	or %al, %al
	jz expression_operator
	stosb %al, %es:(%edi)
      expression_operator:
	push %edi
	mov $operators, %edi
	call get_operator
	pop %edi
	pop %ebp
	or %al, %al
	jz expression_end
      operators_loop:
	cmp %ebp, %esp
	je push_operator
	mov %al, %bl
	and $0xf0, %bl
	mov (%esp), %bh
	and $0xf0, %bh
	cmp %bh, %bl
	ja push_operator
	pop %bx
	movb %bl, (%edi)
	inc %edi
	jmp operators_loop
      push_operator:
	push %ax
	jmp expression_loop
      expression_end:
	cmp %ebp, %esp
	je expression_converted
	pop %ax
	stosb %al, %es:(%edi)
	jmp expression_end
      expression_converted:
	pop %ebp
	ret

convert_number:
	cmpb $'(, (%esi)
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
	scasl %es:(%edi), %eax
	scasl %es:(%edi), %eax
	ret
      dword_number:
	movb $4, -1(%edi)
	scasl %es:(%edi), %eax
	ret
      word_number:
	movb $2, -1(%edi)
	scasw %es:(%edi), %ax
	ret
      expression_value:
	inc %esi
	call convert_expression
	lodsb %ds:(%esi), %al
	cmp $'), %al
	jne invalid_expression
	ret
      symbol_value:
	lodsb %ds:(%esi), %al
	cmp $0x1a, %al
	jne invalid_value
	lodsb %ds:(%esi), %al
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
	stosl %eax, %es:(%edi)
	ret
      register_value:
	pop %edi
	add $8, %esp
	movb $0x10, -1(%edi)
	mov %ah, %al
	stosb %al, %es:(%edi)
	ret

get_number:
	xor %ebp, %ebp
	lodsb %ds:(%esi), %al
	cmp $0x22, %al
	je get_text_number
	cmp $0x1a, %al
	jne not_number
	lodsb %ds:(%esi), %al
	movzbl %al, %ecx
	movl %esi, number_start
	mov (%esi), %al
	sub $0x30, %al
	jb invalid_number
	cmp $9, %al
	ja invalid_number
	mov %esi, %eax
	add %ecx, %esi
	push %esi
	sub $2, %esi
	movl $0, (%edi)
	movl $0, 4(%edi)
	inc %esi
	cmpw $('0|'x<<8), (%eax)  #  Same multibyte character constant order in fasm and NASM.
	je get_hex_number
	dec %esi
	cmpb $'h, 1(%esi)
	je get_hex_number
	cmpb $'o, 1(%esi)
	je get_oct_number
	cmpb $'b, 1(%esi)
	je get_bin_number
	cmpb $'d, 1(%esi)
	je get_dec_number
	inc %esi
	cmpb $'0, (%eax)
	je get_oct_number
      get_dec_number:
	xor %edx, %edx
	mov $1, %ebx
      get_dec_digit:
	cmp number_start, %esi
	jb number_ok
	movzbl (%esi), %eax
	sub $0x30, %al
	jc bad_number
	cmp $9, %al
	ja bad_number
	mov %eax, %ecx
	jecxz next_dec_digit
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
	imul $10, %ecx
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
	xor %bl, %bl
      get_bin_digit:
	cmp number_start, %esi
	jb number_ok
	movzbl (%esi), %eax
	sub $0x30, %al
	jc bad_number
	cmp $1, %al
	ja bad_number
	xor %edx, %edx
	mov %bl, %cl
	dec %esi
	cmp $64, %bl
	je bin_out_of_range
	inc %bl
	cmp $32, %cl
	jae bin_digit_high
	shl %cl, %eax
	orl %eax, (%edi)
	jmp get_bin_digit
      bin_digit_high:
	sub $32, %cl
	shl %cl, %eax
	orl %eax, 4(%edi)
	jmp get_bin_digit
      bin_out_of_range:
	or $1, %ebp
	jmp get_bin_digit
      get_hex_number:
	xor %bl, %bl
      get_hex_digit:
	cmp number_start, %esi
	jb number_ok
	movzbl (%esi), %eax
	cmp $'x, %al
	je hex_number_ok
	sub $0x30, %al
	jc bad_number
	cmp $9, %al
	jbe hex_digit_ok
	sub $7, %al
	cmp $15, %al
	jbe hex_digit_ok
	sub $0x20, %al
	jc bad_number
	cmp $15, %al
	ja bad_number
      hex_digit_ok:
	xor %edx, %edx
	mov %bl, %cl
	dec %esi
	cmp $64, %bl
	je hex_out_of_range
	add $4, %bl
	cmp $32, %cl
	jae hex_digit_high
	shl %cl, %eax
	orl %eax, (%edi)
	jmp get_hex_digit
      hex_digit_high:
	sub $32, %cl
	shl %cl, %eax
	orl %eax, 4(%edi)
	jmp get_hex_digit
      hex_out_of_range:
	or $1, %ebp
	jmp get_hex_digit
      get_oct_number:
	xor %bl, %bl
      get_oct_digit:
	cmp number_start, %esi
	jb number_ok
	movzbl (%esi), %eax
	sub $0x30, %al
	jc bad_number
	cmp $7, %al
	ja bad_number
      oct_digit_ok:
	xor %edx, %edx
	mov %bl, %cl
	dec %esi
	cmp $64, %bl
	jae oct_out_of_range
	add $3, %bl
	cmp $32, %cl
	jae oct_digit_high
	shl %cl, %eax
	orl %eax, (%edi)
	jmp get_oct_digit
      oct_digit_high:
	sub $32, %cl
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
	lodsl %ds:(%esi), %eax
	mov %eax, %edx
	xor %bl, %bl
	movl $0, (%edi)
	movl $0, 4(%edi)
      get_text_character:
	sub $1, %edx
	jc number_done
	movzbl (%esi), %eax
	inc %esi
	mov %bl, %cl
	cmp $64, %bl
	je text_out_of_range
	add $8, %bl
	cmp $32, %cl
	jae text_character_high
	shl %cl, %eax
	orl %eax, (%edi)
	jmp get_text_character
      text_character_high:
	sub $32, %cl
	shl %cl, %eax
	orl %eax, 4(%edi)
	jmp get_text_character
      text_out_of_range:
	or $1, %ebp
	jmp get_text_character

calculate_expression:
	lodsb %ds:(%esi), %al
	or %al, %al
	jz get_string_value
	cmp $'., %al
	je convert_fp
	cmp $1, %al
	je get_byte_number
	cmp $2, %al
	je get_word_number
	cmp $4, %al
	je get_dword_number
	cmp $8, %al
	je get_qword_number
	cmp $0xf, %al
	je value_out_of_range
	cmp $0x10, %al
	je get_register
	cmp $0x11, %al
	je get_label
	cmp $'), %al
	je expression_calculated
	cmp $'], %al
	je expression_calculated
	sub $0x10, %edi
	mov %edi, %ebx
	sub $0x10, %ebx
	mov 8(%ebx), %dx
	or 8(%edi), %dx
	cmp $0xe0, %al
	je calculate_rva
	cmp $0xd0, %al
	je calculate_not
	cmp $0xd1, %al
	je calculate_neg
	cmp $0x80, %al
	je calculate_add
	cmp $0x81, %al
	je calculate_sub
	mov 12(%ebx), %ah
	or 12(%edi), %ah
	jnz invalid_use_of_symbol
	cmp $0x90, %al
	je calculate_mul
	cmp $0x91, %al
	je calculate_div
	or %dx, %dx
	jnz invalid_expression
	cmp $0xa0, %al
	je calculate_mod
	cmp $0xb0, %al
	je calculate_and
	cmp $0xb1, %al
	je calculate_or
	cmp $0xb2, %al
	je calculate_xor
	cmp $0xc0, %al
	je calculate_shl
	cmp $0xc1, %al
	je calculate_shr
	jmp invalid_expression
      expression_calculated:
	sub $0x10, %edi
	ret
      get_byte_number:
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	xor %eax, %eax
	lodsb %ds:(%esi), %al
	stosl %eax, %es:(%edi)
	xor %al, %al
	stosl %eax, %es:(%edi)
	scasl %es:(%edi), %eax
	scasl %es:(%edi), %eax
	jmp calculate_expression
      get_word_number:
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	xor %eax, %eax
	lodsw %ds:(%esi), %ax
	stosl %eax, %es:(%edi)
	xor %ax, %ax
	stosl %eax, %es:(%edi)
	scasl %es:(%edi), %eax
	scasl %es:(%edi), %eax
	jmp calculate_expression
      get_dword_number:
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	movsl %ds:(%esi), %es:(%edi)
	xor %eax, %eax
	stosl %eax, %es:(%edi)
	scasl %es:(%edi), %eax
	scasl %es:(%edi), %eax
	jmp calculate_expression
      get_qword_number:
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	movsl %ds:(%esi), %es:(%edi)
	movsl %ds:(%esi), %es:(%edi)
	scasl %es:(%edi), %eax
	scasl %es:(%edi), %eax
	jmp calculate_expression
      get_register:
	movb $0, 9(%edi)
	movb $0, 12(%edi)
	lodsb %ds:(%esi), %al
	mov %al, 8(%edi)
	movb $1, 10(%edi)
	xor %eax, %eax
	stosl %eax, %es:(%edi)
	stosl %eax, %es:(%edi)
	scasl %es:(%edi), %eax
	scasl %es:(%edi), %eax
	jmp calculate_expression
      get_label:
	movw $0, 8(%edi)
	movb $0, 12(%edi)
	lodsl %ds:(%esi), %eax
	or %eax, %eax
	jz current_offset_label
	cmp $1, %eax
	je counter_label
	mov %eax, %ebx
	testb $1, 8(%ebx)
	jz label_undefined
	testb $4, 8(%ebx)
	jz label_defined
	mov current_pass, %al
	cmp 9(%ebx), %al
	jne label_undefined
      label_defined:
	mov 11(%ebx), %al
	cmpb $0, next_pass_needed
	je label_type_ok
	cmpb $0, current_pass
	jne label_type_ok
	xor %al, %al
      label_type_ok:
	mov %al, 12(%edi)
	mov 12(%ebx), %eax
	mov %eax, 8(%edi)
	mov (%ebx), %eax
	stosl %eax, %es:(%edi)
	mov 4(%ebx), %eax
	stosl %eax, %es:(%edi)
	scasl %es:(%edi), %eax
	scasl %es:(%edi), %eax
	mov 10(%ebx), %al
	or %al, %al
	jz calculate_expression
	cmpb $2, forced_size
	je calculate_expression
	cmpb $1, forced_size
	jne check_size
	cmpb $0, operand_size
	jne calculate_expression
	.byte 0xa2  # movb %al, operand_size
	.long operand_size
	jmp calculate_expression
      check_size:
	xchgb %al, operand_size
	or %al, %al
	jz calculate_expression
	cmp operand_size, %al
	jne operand_sizes_do_not_match
	jmp calculate_expression
      current_offset_label:
	cmpb $0, reloc_labels
	je get_current_offset
	movb $2, 12(%edi)
      get_current_offset:
	mov current_offset, %eax
	sub org_start, %eax
	cdq
	stosl %eax, %es:(%edi)
	mov %edx, %eax
	stosl %eax, %es:(%edi)
	mov org_sib, %eax
	stosl %eax, %es:(%edi)
	scasl %es:(%edi), %eax
	jmp calculate_expression
      counter_label:
	mov counter, %eax
	stosl %eax, %es:(%edi)
	xor %eax, %eax
	stosl %eax, %es:(%edi)
	scasl %es:(%edi), %eax
	scasl %es:(%edi), %eax
	jmp calculate_expression
      label_undefined:
	cmpb $0, current_pass
	jne invalid_value
	orb $-1, next_pass_needed
	movb $0, 12(%edi)
	xor %eax, %eax
	stosl %eax, %es:(%edi)
	stosl %eax, %es:(%edi)
	scasl %es:(%edi), %eax
	scasl %es:(%edi), %eax
	jmp calculate_expression
      calculate_add:
	cmpb $0, next_pass_needed
	jne add_values
	cmpb $0, 12(%edi)
	je add_values
	cmpb $0, 12(%ebx)
	jne invalid_use_of_symbol
      add_values:
	mov 12(%edi), %al
	or %al, 12(%ebx)
	mov (%edi), %eax
	add %eax, (%ebx)
	mov 4(%edi), %eax
	adc %eax, 4(%ebx)
	or %dx, %dx
	jz calculate_expression
	push %esi
	mov %ebx, %esi
	lea 10(%edi), %ebx
	mov 8(%edi), %cl
	call add_register
	lea 11(%edi), %ebx
	mov 9(%edi), %cl
	call add_register
	pop %esi
	jmp calculate_expression
      add_register:
	or %cl, %cl
	jz add_register_done
      add_register_start:
	cmp %cl, 8(%esi)
	jne add_in_second_slot
	mov (%ebx), %al
	add %al, 10(%esi)
	jnz add_register_done
	movb $0, 8(%esi)
	ret
      add_in_second_slot:
	cmp %cl, 9(%esi)
	jne create_in_first_slot
	mov (%ebx), %al
	add %al, 11(%esi)
	jnz add_register_done
	movb $0, 9(%esi)
	ret
      create_in_first_slot:
	cmpb $0, 8(%esi)
	jne create_in_second_slot
	mov %cl, 8(%esi)
	mov (%ebx), %al
	mov %al, 10(%esi)
	ret
      create_in_second_slot:
	cmpb $0, 9(%esi)
	jne invalid_expression
	mov %cl, 9(%esi)
	mov (%ebx), %al
	mov %al, 11(%esi)
      add_register_done:
	ret
      calculate_sub:
	xor %ah, %ah
	cmpb $0, next_pass_needed
	jne sub_values
	mov 12(%ebx), %ah
	mov 12(%edi), %al
	or %al, %al
	jz sub_values
	cmp %ah, %al
	jne invalid_use_of_symbol
	xor %ah, %ah
      sub_values:
	movb %ah, 12(%ebx)
	mov (%edi), %eax
	sub %eax, (%ebx)
	mov 4(%edi), %eax
	sbb %eax, 4(%ebx)
	or %dx, %dx
	jz calculate_expression
	push %esi
	mov %ebx, %esi
	lea 10(%edi), %ebx
	mov 8(%edi), %cl
	call sub_register
	lea 11(%edi), %ebx
	mov 9(%edi), %cl
	call sub_register
	pop %esi
	jmp calculate_expression
      sub_register:
	or %cl, %cl
	jz add_register_done
	negb (%ebx)
	jmp add_register_start
      calculate_mul:
	or %dx, %dx
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
	push %dx
	mov %ebx, %esi
	xor %bl, %bl
	testl $1 << 31, 4(%esi)
	jz mul_first_sign_ok
	notl (%esi)
	notl 4(%esi)
	addl $1, (%esi)
	adcl $0, 4(%esi)
	not %bl
      mul_first_sign_ok:
	testl $1 << 31, 4(%edi)
	jz mul_second_sign_ok
	notl (%edi)
	notl 4(%edi)
	addl $1, (%edi)
	adcl $0, 4(%edi)
	not %bl
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
	or %bl, %bl
	jz mul_ok
	notl (%esi)
	notl 4(%esi)
	addl $1, (%esi)
	adcl $0, 4(%esi)
      mul_ok:
	pop %dx
	or %dx, %dx
	jz mul_calculated
	cmpw $0, 8(%edi)
	jne invalid_value
	cmpb $0, 8(%esi)
	je mul_first_register_ok
	mov (%edi), %al
	cbw
	cwde
	cdq
	cmp 4(%edi), %edx
	jne value_out_of_range
	cmp (%edi), %eax
	jne value_out_of_range
	imulb 10(%esi)
	mov %ah, %dl
	cbw
	cmp %dl, %ah
	jne value_out_of_range
	mov %al, 10(%esi)
      mul_first_register_ok:
	cmpb $0, 9(%esi)
	je mul_calculated
	mov (%edi), %al
	cbw
	cwde
	cdq
	cmp 4(%edi), %edx
	jne value_out_of_range
	cmp (%edi), %eax
	jne value_out_of_range
	imulb 11(%esi)
	mov %ah, %dl
	cbw
	cmp %dl, %ah
	jne value_out_of_range
	mov %al, 11(%esi)
      mul_calculated:
	pop %esi
	jmp calculate_expression
      calculate_div:
	push %esi
	push %dx
	mov %ebx, %esi
	call div_64
	pop %dx
	or %dx, %dx
	jz div_calculated
	cmpb $0, 8(%esi)
	je div_first_register_ok
	mov (%edi), %al
	cbw
	cwde
	cdq
	cmp 4(%edi), %edx
	jne value_out_of_range
	cmp (%edi), %eax
	jne value_out_of_range
	or %al, %al
	jz value_out_of_range
	mov 10(%esi), %al
	cbw
	idivb (%edi)
	mov %al, 10(%esi)
      div_first_register_ok:
	cmpb $0, 9(%esi)
	je div_calculated
	mov (%edi), %al
	cbw
	cwde
	cdq
	cmp 4(%edi), %edx
	jne value_out_of_range
	cmp (%edi), %eax
	jne value_out_of_range
	or %al, %al
	jz value_out_of_range
	mov 11(%esi), %al
	cbw
	idivb (%edi)
	mov %al, 11(%esi)
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
	mov (%edi), %al
	xor %al, (%ebx)
	jmp calculate_expression
      xor_word:
	cmpl $0, 4(%edi)
	jne xor_qword
	cmpw $0, 2(%edi)
	jne xor_qword
	mov (%edi), %ax
	xor %ax, (%ebx)
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
	mov 4(%edi), %ax
	xor %ax, 4(%ebx)
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
	shld %cl, %eax, %edx
	shl %cl, %eax
	mov %eax, (%ebx)
	mov %edx, 4(%ebx)
	jmp calculate_expression
      shl_high:
	sub $32, %cl
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
	shrd %cl, %edx, %eax
	shr %cl, %edx
	mov %eax, (%ebx)
	mov %edx, 4(%ebx)
	jmp calculate_expression
      shr_high:
	sub $32, %cl
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
	mov 12(%edi), %al
	cmp $2, %al
	je rva_ok
	or %al, %al
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
	xor %bl, %bl
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
	not %bl
      div_first_sign_ok:
	testl $1 << 31, 4(%edi)
	jz div_second_sign_ok
	notl (%edi)
	notl 4(%edi)
	addl $1, (%edi)
	adcl $0, 4(%edi)
	not %bl
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
	or %bl, %bl
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
	mov value_size, %al
	cmp $4, %al
	je convert_fp_dword
	cmp $8, %al
	je convert_fp_qword
	jmp invalid_value
      convert_fp_dword:
	xor %eax, %eax
	cmpw $0x8000, 8(%esi)
	je fp_dword_store
	mov 8(%esi), %bx
	mov 4(%esi), %eax
	shl %eax
	shr $9, %eax
	jnc fp_dword_ok
	inc %eax
	test $1 << 23, %eax
	jz fp_dword_ok
	and $(1 << 23) - 1, %eax
	inc %bx
	shr %eax
      fp_dword_ok:
	add $0x7f, %bx
	.byte 0x66, 0x81, 0xfb  # cmp $0x100, %bx
	.word 0x100
	jae value_out_of_range
	shl $23, %ebx
	or %ebx, %eax
	mov 11(%esi), %bl
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
	mov 8(%esi), %bx
	mov (%esi), %eax
	mov 4(%esi), %edx
	shl %eax
	rcl %edx
	mov %edx, %ecx
	shr $12, %edx
	shrd $12, %ecx, %eax
	jnc fp_qword_ok
	add $1, %eax
	adc $0, %edx
	test $1 << 20, %edx
	jz fp_qword_ok
	and $(1 << 20) - 1, %edx
	inc %bx
	shr %edx
	rcr %eax
      fp_qword_ok:
	add $0x3ff, %bx
	.byte 0x66, 0x81, 0xfb  # cmp $0x800, %bx
	.word 0x800
	jae value_out_of_range
	shl $20, %ebx
	or %ebx, %edx
	mov 11(%esi), %bl
	shl $31, %ebx
	or %ebx, %edx
      fp_qword_store:
	mov %eax, (%edi)
	mov %edx, 4(%edi)
	add $12, %esi
	ret
      get_string_value:
	lodsl %ds:(%esi), %eax
	mov %eax, %ecx
	cmp $8, %ecx
	ja value_out_of_range
	mov %edi, %edx
	xor %eax, %eax
	stosl %eax, %es:(%edi)
	stosl %eax, %es:(%edi)
	mov %edx, %edi
	rep
	movsb %ds:(%esi), %es:(%edi)
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
	mov 12(%edi), %al
	cmp $2, %al
	je invalid_use_of_symbol
	.byte 0xa2  # movb %al, value_type
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
	mov 12(%edi), %al
	.byte 0xa2  # movb %al, value_type
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
	mov 12(%edi), %al
	.byte 0xa2  # movb %al, value_type
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
	mov 12(%edi), %al
	.byte 0xa2  # movb %al, value_type
	.long value_type
	mov (%edi), %eax
	mov 4(%edi), %edx
	ret
get_value:
	movb $0, operand_size
	movb $0, forced_size
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	jne invalid_value
	mov operand_size, %al
	cmp $1, %al
	je value_byte
	cmp $2, %al
	je value_word
	cmp $4, %al
	je value_dword
	cmp $6, %al
	je value_pword
	cmp $8, %al
	je value_qword
	or %al, %al
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
	mov (%esi), %al
	and $240, %al
	cmp $0x60, %al
	jne get_size_prefix
	lodsb %ds:(%esi), %al
	sub $0x60, %al
	.byte 0xa2  # movb %al, segment_register
	.long segment_register
	mov (%esi), %al
	and $240, %al
      get_size_prefix:
	cmp $0x70, %al
	jne calculate_address
	lodsb %ds:(%esi), %al
	sub $0x70, %al
	cmp $4, %al
	ja invalid_address_size
	.byte 0xa2  # movb %al, address_size
	.long address_size
	.byte 0xa2  # movb %al, value_size
	.long value_size
	jmp calculate_address
get_address_value:
	movb $0, address_size
	movb $4, value_size
	pushl $address_ok
      calculate_address:
	movl %edi, current_offset
	call calculate_expression
	mov 12(%edi), %al
	.byte 0xa2  # movb %al, value_type
	.long value_type
	cmp $1, %al
	je invalid_use_of_symbol
	or %al, %al
	jz address_symbol_ok
	mov $0x84, %al
	xchgb %al, address_size
	or %al, %al
	jz address_symbol_ok
	cmp $4, %al
	jne address_sizes_do_not_agree
      address_symbol_ok:
	xor %bx, %bx
	xor %cl, %cl
	mov address_size, %ch
	cmpw $0, 8(%edi)
	je check_dword_value
	mov 8(%edi), %al
	mov 10(%edi), %dl
	call get_address_register
	mov 9(%edi), %al
	mov 11(%edi), %dl
	call get_address_register
	mov %bx, %ax
	shr $4, %ah
	shr $4, %al
	or %bh, %bh
	jz check_address_registers
	or %bl, %bl
	jz check_address_registers
	cmp %ah, %al
	jne invalid_address
      check_address_registers:
	or %ah, %al
	cmp $2, %al
	je address_16bit
	cmp $4, %al
	jne invalid_address
	or %bh, %bh
	jnz check_index_scale
	cmp $2, %cl
	je special_index_scale
	cmp $3, %cl
	je special_index_scale
	cmp $5, %cl
	je special_index_scale
	cmp $9, %cl
	je special_index_scale
      check_index_scale:
	or %cl, %cl
	jz address_registers_ok
	cmp $1, %cl
	je address_registers_ok
	cmp $2, %cl
	je address_registers_ok
	cmp $4, %cl
	je address_registers_ok
	cmp $8, %cl
	je address_registers_ok
	jmp invalid_address
      special_index_scale:
	mov %bl, %bh
	dec %cl
      address_registers_ok:
	jmp check_dword_value
      address_16bit:
	or %cl, %cl
	jz check_word_value
	cmp $1, %cl
	je check_word_value
	jmp invalid_address
      get_address_register:
	or %al, %al
	jz address_register_ok
	cmp $1, %dl
	jne scaled_register
	or %bh, %bh
	jnz scaled_register
	mov %al, %bh
      address_register_ok:
	ret
      scaled_register:
	or %bl, %bl
	jnz invalid_address
	mov %al, %bl
	mov %dl, %cl
	jmp address_register_ok
      address_ok:
	mov %eax, %edx
	ret

calculate_logical_expression:
	call get_logical_value
      logical_loop:
	push %ax
	lodsb %ds:(%esi), %al
	cmp $'|, %al
	je logical_or
	cmp $'&, %al
	je logical_and
	dec %esi
	pop %ax
	ret
      logical_or:
	call get_logical_value
	pop %bx
	or %bl, %al
	jmp logical_loop
      logical_and:
	call get_logical_value
	pop %bx
	and %bl, %al
	jmp logical_loop

get_logical_value:
	xor %al, %al
	cmpb $'~, (%esi)
	jne negation_ok
	inc %esi
	or $-1, %al
      negation_ok:
	push %ax
	cmpb $'{, (%esi)
	je logical_expression
	push %esi
	cmpb $0x11, (%esi)
	jne check_for_values
	add $2, %esi
      check_for_values:
	xor %bl, %bl
	cmpb $'(, (%esi)
	jne find_eq_symbol
	call skip_symbol
	lodsb %ds:(%esi), %al
	cmp $'=, %al
	je compare_values
	cmp $'>, %al
	je compare_values
	cmp $'<, %al
	je compare_values
	cmp $0xf2, %al
	je compare_values
	cmp $0xf3, %al
	je compare_values
	cmp $0xf6, %al
	je compare_values
	dec %esi
      find_eq_symbol:
	cmpb $0x81, (%esi)
	je compare_symbols
	cmpb $0x83, (%esi)
	je scan_symbols_list
	call check_character
	jc logical_number
	cmp $44, %al
	jne next_eq_symbol
	mov $1, %bl
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
	repe
	cmpsb %es:(%edi), %ds:(%esi)
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
	or %bl, %bl
	jnz invalid_expression
	xor %bp, %bp
	inc %esi
	lodsb %ds:(%esi), %al
	cmp $'<, %al
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
	repe
	cmpsb %es:(%edi), %ds:(%esi)
	pop %edi
	pop %ecx
	jne not_equal_in_list
	cmpb $44, (%esi)
	je skip_rest_of_list
	cmpb $'>, (%esi)
	jne not_equal_in_list
      skip_rest_of_list:
	call check_character
	jc invalid_expression
	cmp $'>, %al
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
	cmp $'>, %al
	je list_return_false
	cmp $44, %al
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
	mov (%esi), %al
	or %al, %al
	jz stop
	cmp $0xf, %al
	je stop
	cmp $'}, %al
	je stop
	cmp $'|, %al
	je stop
	cmp $'&, %al
	je stop
	clc
	ret
      stop:
	stc
	ret
      compare_values:
	pop %esi
	call get_value
	mov value_type, %bl
	push %eax
	push %edx
	push %bx
	lodsb %ds:(%esi), %al
	.byte 0xa2  # movb %al, compare_type
	.long compare_type
	call get_value
	pop %bx
	cmpb $0, next_pass_needed
	jne values_ok
	cmp value_type, %bl
	jne invalid_use_of_symbol
      values_ok:
	pop %ecx
	pop %ebx
	cmpb $'=, compare_type
	je check_equal
	cmpb $'>, compare_type
	je check_greater
	cmpb $'<, compare_type
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
	xor %al, %al
	jmp logical_value_ok
      return_true:
	or $-1, %al
	jmp logical_value_ok
      logical_expression:
	inc %esi
	call calculate_logical_expression
	push %ax
	lodsb %ds:(%esi), %al
	cmp $'}, %al
	jne invalid_expression
	pop %ax
      logical_value_ok:
	pop %bx
	xor %bl, %al
	ret

# %include '../preproce.inc'

#  flat assembler source
#  Copyright (c) 1999-2001, Tomasz Grysztar
#  All rights reserved.

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
	mov $2, %al
	xor %edx, %edx
	call lseek
	push %eax
	xor %al, %al
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
	stosl %eax, %es:(%edi)
	mov %ecx, %eax
	stosl %eax, %es:(%edi)
	mov %esi, %eax
	sub %ebx, %eax
	stosl %eax, %es:(%edi)
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
	.byte 0x66, 0xb8  # mov $0x3b, %ax
	.word 0x3b
	stosw %ax, %es:(%edi)
      convert_line_data:
	cmp memory_end, %edi
	jae out_of_memory
	lodsb %ds:(%esi), %al
	cmp $0x20, %al
	je convert_line_data
	cmp $9, %al
	je convert_line_data
	dec %esi
	lodsb %ds:(%esi), %al
	mov %al, %ah
	mov $characters, %ebx
	xlatb
	or %al, %al
	jz convert_separator
	cmp $0x27, %ah
	je convert_string
	cmp $0x22, %ah
	je convert_string
	movb $0x1a, (%edi)
	scasw %es:(%edi), %ax
	stosb %al, %es:(%edi)
	mov $characters, %ebx
	xor %ecx, %ecx
      convert_symbol:
	lodsb %ds:(%esi), %al
	xlatb
	stosb %al, %es:(%edi)
	or %al, %al
	loopnz convert_symbol
	neg %ecx
	cmp $255, %ecx
	ja name_too_long
	dec %edi
	mov %edi, %ebx
	sub %ecx, %ebx
	movb %cl, -1(%ebx)
	mov -1(%esi), %ah
      convert_separator:
	xchg %ah, %al
	cmp $0x20, %al
	jb control_character
	je convert_line_data
      symbol_character:
	cmp $0x3b, %al
	je ignore_comment
	cmp $0x5c, %al
	je concate_lines
	stosb %al, %es:(%edi)
	jmp convert_line_data
      control_character:
	cmp $0x1a, %al
	je line_end
	cmp $0xd, %al
	je cr_character
	cmp $0xa, %al
	je lf_character
	cmp $9, %al
	je convert_line_data
	or %al, %al
	jnz symbol_character
	jmp line_end
      lf_character:
	lodsb %ds:(%esi), %al
	cmp $0xd, %al
	je line_end
	dec %esi
	jmp line_end
      cr_character:
	lodsb %ds:(%esi), %al
	cmp $0xa, %al
	je line_end
	dec %esi
	jmp line_end
      convert_string:
	mov $0x22, %al
	stosb %al, %es:(%edi)
	scasl %es:(%edi), %eax
	mov %edi, %ebx
      copy_string:
	lodsb %ds:(%esi), %al
	stosb %al, %es:(%edi)
	cmp $0xa, %al
	je missing_end_quote
	cmp $0xd, %al
	je missing_end_quote
	or %al, %al
	jz missing_end_quote
	cmp $0x1a, %al
	je missing_end_quote
	cmp %ah, %al
	jne copy_string
	lodsb %ds:(%esi), %al
	cmp %ah, %al
	je copy_string
	dec %esi
	dec %edi
	mov %edi, %eax
	sub %ebx, %eax
	mov %eax, -4(%ebx)
	jmp convert_line_data
      concate_lines:
	lodsb %ds:(%esi), %al
	cmp $0x20, %al
	je concate_lines
	cmp $9, %al
	je concate_lines
	cmp $0x1a, %al
	je unexpected_end_of_file
	cmp $0xa, %al
	je concate_lf
	cmp $0xd, %al
	je concate_cr
	cmp $0x3b, %al
	jne extra_characters_on_line
      find_concated_line:
	lodsb %ds:(%esi), %al
	cmp $0xa, %al
	je concate_lf
	cmp $0xd, %al
	je concate_cr
	or %al, %al
	jz concate_ok
	cmp $0x1a, %al
	jne find_concated_line
	jmp unexpected_end_of_file
      concate_lf:
	lodsb %ds:(%esi), %al
	cmp $0xd, %al
	je concate_ok
	dec %esi
	jmp concate_ok
      concate_cr:
	lodsb %ds:(%esi), %al
	cmp $0xa, %al
	je concate_ok
	dec %esi
      concate_ok:
	incl (%esp)
	jmp convert_line_data
      ignore_comment:
	lodsb %ds:(%esi), %al
	cmp $0xa, %al
	je lf_character
	cmp $0xd, %al
	je cr_character
	or %al, %al
	jz line_end
	cmp $0x1a, %al
	jne ignore_comment
      line_end:
	xor %al, %al
	stosb %al, %es:(%edi)
	pop %ecx
	ret

preprocess_line:
	pushl struc_name
	push %ecx
	push %esi
	mov current_line, %esi
	add $12, %esi
	mov macro_status, %al
	dec %al
	jz find_macro_block
	dec %al
	jz skip_macro_block
      preprocess_instruction:
	lodsb %ds:(%esi), %al
	cmp $':, %al
	je preprocess_instruction
	movzbl (%esi), %ecx
	inc %esi
	cmp $0x1a, %al
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
	mov %cl, %al
	xor %ah, %ah
	call get_macro
	jc not_macro
	movb $0x3b, -2(%edx)
	movl $0, struc_name
	jmp use_macro
      not_macro:
	movl %esi, struc_name
	add %ecx, %esi
	lodsb %ds:(%esi), %al
	cmp $':, %al
	je preprocess_instruction
	cmp $0x1a, %al
	jne not_preprocessor_symbol
	cmpl $3+(('e|'q<<8|'u<<16) << 8), (%esi)  #  Same multibyte character constant order in fasm and NASM.
	je define_symbolic_constant
	lodsb %ds:(%esi), %al
	mov $1, %ah
	call get_macro
	jc not_preprocessor_symbol
	movb $':, -2(%edx)
	mov $0x3b, %al
	xchg -1(%edx), %al
	dec %al
	mov %al, (%edx)
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
	mov %al, %cl
	cmp macros_list, %ebx
	je no_macro_found
	sub $8, %ebx
	cmp (%ebx), %ax
	jne check_macro
	mov 4(%ebx), %edi
	repe
	cmpsb %es:(%edi), %ds:(%esi)
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
	lodsb %ds:(%esi), %al
	cmp $0x1a, %al
	je check_symbol
	cmp $0x22, %al
	je ignore_string
	or %al, %al
	jnz process_symbolic_constants
	dec %esi
	ret
      ignore_string:
	lodsl %ds:(%esi), %eax
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
	cmp (%edx), %al
	jne next_symbolic_constant
	mov 4(%edx), %edi
	repe
	cmpsb %es:(%edi), %ds:(%esi)
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
	rcl %al
	shr %ecx
	rcl %ah
	rep
	movsl %ds:(%esi), %es:(%edi)
	mov %ah, %cl
	rep
	movsw %ds:(%esi), %es:(%edi)
	mov %al, %cl
	rep
	movsb %ds:(%esi), %es:(%edi)
	mov %edx, %esi
	clc
	ret
      process_after_replaced:
	lodsb %ds:(%esi), %al
	cmp $0x1a, %al
	je symbol_after_replaced
	stosb %al, %es:(%edi)
	cmp $0x22, %al
	je string_after_replaced
	or %al, %al
	jnz process_after_replaced
	mov %edi, %ecx
	sub %esi, %ecx
	mov %ebp, %edi
	xor %eax, %eax
	shr %ecx
	rcl %al
	shr %ecx
	rcl %ah
	rep
	movsl %ds:(%esi), %es:(%edi)
	mov %ah, %cl
	rep
	movsw %ds:(%esi), %es:(%edi)
	mov %al, %cl
	rep
	movsb %ds:(%esi), %es:(%edi)
	ret
      string_after_replaced:
	lodsl %ds:(%esi), %eax
	stosl %eax, %es:(%edi)
	mov %eax, %ecx
	rep
	movsb %ds:(%esi), %es:(%edi)
	jmp process_after_replaced
      symbol_after_replaced:
	movzbl (%esi), %ecx
	inc %esi
	call replace_symbolic_constant
	jnc process_after_replaced
	mov $0x1a, %al
	mov %cl, %ah
	stosw %ax, %es:(%edi)
	rep
	movsb %ds:(%esi), %es:(%edi)
	jmp process_after_replaced
include_file:
	lodsb %ds:(%esi), %al
	cmp $0x22, %al
	jne invalid_argument
	lodsl %ds:(%esi), %eax
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
	mov -1(%ebx), %al
	mov %al, (%edx)
	mov %ebx, 4(%edx)
	jmp line_preprocessed
define_struc:
	or $1, %ah
define_macro:
	cmpb $0, macro_status
	jne unexpected_instruction
	lodsb %ds:(%esi), %al
	cmp $0x1a, %al
	jne invalid_name
	lodsb %ds:(%esi), %al
	mov additional_memory, %ebx
	mov %ax, (%ebx)
	mov %esi, 4(%ebx)
	add $8, %ebx
	cmp labels_list, %ebx
	jae out_of_memory
	movl %ebx, additional_memory
	movzbl %al, %eax
	add %eax, %esi
	movb $1, macro_status
	xor %bl, %bl
	lodsb %ds:(%esi), %al
	or %al, %al
	jz line_preprocessed
	cmp $'{, %al
	je found_macro_block
	dec %esi
      skip_macro_arguments:
	lodsb %ds:(%esi), %al
	cmp $0x1a, %al
	je skip_macro_argument
	cmp $'[, %al
	jne invalid_macro_arguments
	xor $-1, %bl
	jz invalid_macro_arguments
	lodsb %ds:(%esi), %al
	cmp $0x1a, %al
	jne invalid_macro_arguments
      skip_macro_argument:
	movzbl (%esi), %eax
	inc %esi
	add %eax, %esi
	lodsb %ds:(%esi), %al
	cmp $44, %al
	je skip_macro_arguments
	cmp $'], %al
	jne end_macro_arguments
	lodsb %ds:(%esi), %al
	not %bl
      end_macro_arguments:
	or %bl, %bl
	jnz invalid_macro_arguments
	or %al, %al
	jz line_preprocessed
	cmp $'{, %al
	je found_macro_block
	jmp invalid_macro_arguments
      find_macro_block:
	add $2, %esi
	lodsb %ds:(%esi), %al
	or %al, %al
	jz line_preprocessed
	cmp $'{, %al
	jne unexpected_characters
      found_macro_block:
	movb $2, macro_status
      skip_macro_block:
	lodsb %ds:(%esi), %al
	cmp $0x1a, %al
	je skip_macro_symbol
	cmp $0x3b, %al
	je skip_macro_symbol
	cmp $0x22, %al
	je skip_macro_string
	or %al, %al
	jz line_preprocessed
	cmp $'}, %al
	jne skip_macro_block
	lodsb %ds:(%esi), %al
	or %al, %al
	jnz extra_characters_on_line
	movb $0, macro_status
	jmp line_preprocessed
      skip_macro_symbol:
	movzbl (%esi), %eax
	inc %esi
	add %eax, %esi
	jmp skip_macro_block
      skip_macro_string:
	lodsl %ds:(%esi), %eax
	add %eax, %esi
	jmp skip_macro_block
purge_macro:
	lodsb %ds:(%esi), %al
	cmp $0x1a, %al
	jne invalid_name
	lodsb %ds:(%esi), %al
	xor %ah, %ah
	call get_macro
	jc macro_purged
	orb $0x80, 1(%ebx)
      macro_purged:
	lodsb %ds:(%esi), %al
	cmp $44, %al
	je purge_macro
	or %al, %al
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
	lodsb %ds:(%esi), %al
	or %al, %al
	jz find_macro_instructions
	cmp $'{, %al
	je macro_instructions_start
	cmp $'[, %al
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
	lodsb %ds:(%esi), %al
	or %al, %al
	jz argument_value_end
	cmp $44, %al
	je argument_value_end
	cmp $0x22, %al
	je argument_string
	cmp $0x1a, %al
	jne get_argument_value
	movzbl (%esi), %eax
	inc %esi
	add %eax, %esi
	jmp get_argument_value
      argument_string:
	lodsl %ds:(%esi), %eax
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
	lodsb %ds:(%esi), %al
	cmp $44, %al
	je next_argument
	cmp $'], %al
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
	lodsb %ds:(%esi), %al
	cmp $'{, %al
	je macro_instructions_start
      find_macro_instructions:
	add $14, %esi
	lodsb %ds:(%esi), %al
	or %al, %al
	jz find_macro_instructions
	cmp $'{, %al
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
	stosl %eax, %es:(%edi)
	mov %ecx, %eax
	stosl %eax, %es:(%edi)
	mov (%esp), %eax
	stosl %eax, %es:(%edi)
	orb $0x40, macro_status
	push %ebx
	push %ecx
      process_macro:
	lodsb %ds:(%esi), %al
	cmp $'}, %al
	je macro_line_processed
	or %al, %al
	jz macro_line_processed
	cmp $0x1a, %al
	je process_macro_symbol
	andb $~0x40, macro_status
	stosb %al, %es:(%edi)
	cmp $0x22, %al
	jne process_macro
      copy_macro_string:
	mov (%esi), %ecx
	add $4, %ecx
	rep
	movsb %ds:(%esi), %es:(%edi)
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
	andb $~0x40, macro_status
	mov counter, %eax
	or %eax, %eax
	jnz check_for_macro_constant
	inc %eax
      check_for_macro_constant:
	shl $8, %eax
	mov (%esi), %al
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
	repe
	cmpsb %es:(%edi), %ds:(%esi)
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
	movsb %ds:(%esi), %es:(%edi)
	movb $44, (%edi)
	inc %edi
	mov %ebx, %esi
	inc %eax
	shl $8, %eax
	mov -1(%esi), %al
	push %edi
	jmp scan_macro_constants
      replace_macro_constant:
	pop %edi
	pop %eax
	mov 8(%edx), %ecx
	mov 12(%edx), %edx
	xchg %edx, %esi
	rep
	movsb %ds:(%esi), %es:(%edi)
	mov %edx, %esi
	jmp process_macro
      not_macro_constant:
	pop %edi
	pop %esi
	mov $0x1a, %al
	stosb %al, %es:(%edi)
	mov (%esi), %al
	inc %esi
	stosb %al, %es:(%edi)
	cmpb $'., (%esi)
	jne copy_macro_symbol
	mov struc_name, %ebx
	or %ebx, %ebx
	jz copy_macro_symbol
	xchg %ebx, %esi
	movzbl -1(%esi), %ecx
	add %cl, -1(%edi)
	jc name_too_long
	rep
	movsb %ds:(%esi), %es:(%edi)
	xchg %ebx, %esi
      copy_macro_symbol:
	movzbl %al, %ecx
	rep
	movsb %ds:(%esi), %es:(%edi)
	jmp process_macro
      macro_line_processed:
	movb $0, (%edi)
	inc %edi
	push %eax
	call preprocess_line
	pop %eax
	pop %ecx
	pop %ebx
	cmp $'}, %al
	je macro_block_processed
      process_next_line:
	inc %ecx
	add $14, %esi
	jmp process_macro_line
      local_symbols:
	lodsb %ds:(%esi), %al
	cmp $0x1a, %al
	jne invalid_argument
	push %edi
	push %ecx
	movzbl (%esi), %ecx
	inc %esi
	mov additional_memory, %edx
	mov counter, %eax
	shl $8, %eax
	mov %cl, %al
	mov %eax, (%edx)
	mov %esi, 4(%edx)
	movzbl _counter, %eax
	mov memory_end, %edi
	sub %eax, %edi
	sub %ecx, %edi
	sub $3, %edi
	movl %edi, memory_end
	mov %edi, 12(%edx)
	add %cl, %al
	jc name_too_long
	inc %al
	jz name_too_long
	movb $0x1a, (%edi)
	inc %edi
	mov %al, (%edi)
	inc %edi
	add $2, %eax
	mov %eax, 8(%edx)
	add $16, %edx
	cmp labels_list, %edx
	jae out_of_memory
	movl %edx, additional_memory
	rep
	movsb %ds:(%esi), %es:(%edi)
	mov $'?, %al
	stosb %al, %es:(%edi)
	movzbl _counter, %ecx
	push %esi
	mov $_counter+1, %esi
	rep
	movsb %ds:(%esi), %es:(%edi)
	pop %esi
	pop %ecx
	pop %edi
	cmp memory_end, %edi
	jae out_of_memory
	lodsb %ds:(%esi), %al
	cmp $44, %al
	je local_symbols
	cmp $'}, %al
	je macro_block_processed
	or %al, %al
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
	andb $~0x80, 1(%ebx)
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
	movb $'0, (%eax,%ecx,1)
	loop counter_loop
      counter_ok:
	ret
      increase_digit:
	incb (%eax,%ecx,1)
	cmpb $':, (%eax,%ecx,1)
	jb digit_increased
	je letter_digit
	cmpb $'f, (%eax,%ecx,1)
	jbe digit_increased
	stc
	ret
      letter_digit:
	movb $'a, (%eax,%ecx,1)
      digit_increased:
	clc
	ret

# %include '../parser.inc'

#  flat assembler source
#  Copyright (c) 1999-2001, Tomasz Grysztar
#  All rights reserved.

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
	mov $0xf, %al
	stosb %al, %es:(%edi)
	mov %esi, %eax
	stosl %eax, %es:(%edi)
	add $12, %esi
	call parse_line
	cmp code_start, %esi
	jb parser_loop
	xor %al, %al
	stosb %al, %es:(%edi)
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
	cmpb $':, (%esi,%ecx,1)
	je simple_label
	push %esi
	push %ecx
	add %ecx, %esi
	cmpb $0x1a, (%esi)
	je check_for_data_label
	cmpb $'=, (%esi)
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
	stosw %ax, %es:(%edi)
	jmp parse_arguments
      data_instruction:
	movzbl %ah, %ebx
	mov data_handlers(,%ebx,2), %bx
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
	stosl %eax, %es:(%edi)
	xor %al, %al
	stosb %al, %es:(%edi)
	inc %esi
	jmp parse_arguments
      data_label:
	pop %ecx
	pop %ebx
	pop %edi
	push %ax
	push %esi
	mov %ebx, %esi
	call identify_label
	movb $2, (%edi)
	inc %edi
	stosl %eax, %es:(%edi)
	pop %esi
	pop %ax
	stosb %al, %es:(%edi)
	push %edi
	jmp data_instruction
      simple_label:
	pop %edi
	call identify_label
	movb $2, (%edi)
	inc %edi
	stosl %eax, %es:(%edi)
	inc %esi
	xor %al, %al
	stosb %al, %es:(%edi)
	jmp instruction_start
      identify_label:
	cmpb $'., (%esi)
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
	lodsb %ds:(%esi), %al
	cmp $0x1a, %al
	jne invalid_argument
	movzbl (%esi), %ecx
	lodsb %ds:(%esi), %al
	pop %edi
	mov $2, %al
	stosb %al, %es:(%edi)
	call identify_label
	stosl %eax, %es:(%edi)
	xor %al, %al
	stosb %al, %es:(%edi)
	jmp parse_arguments
      parse_instruction:
	pop %edi
	mov %al, %dl
	mov $1, %al
	stosb %al, %es:(%edi)
	mov %bx, %ax
	stosw %ax, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	.byte 0x66, 0x81, 0xfb  # cmp $prefix_instruction-assembler, %bx
	.word prefix_instruction-assembler
	je parse_prefix_instruction
	.byte 0x66, 0x81, 0xfb  # cmp $end_directive-assembler, %bx
	.word end_directive-assembler
	je parse_prefix_instruction
	.byte 0x66, 0x81, 0xfb  # cmp $label_directive-assembler, %bx
	.word label_directive-assembler
	je parse_label_directive
	.byte 0x66, 0x81, 0xfb  # cmp $load_directive-assembler, %bx
	.word load_directive-assembler
	je parse_label_directive
      parse_arguments:
	lodsb %ds:(%esi), %al
	cmp $':, %al
	je instruction_separator
	cmp $44, %al
	je separator
	cmp $'=, %al
	je separator
	cmp $'|, %al
	je separator
	cmp $'&, %al
	je separator
	cmp $'~, %al
	je separator
	cmp $'>, %al
	je greater
	cmp $'<, %al
	je less
	cmp $'), %al
	je close_expression
	or %al, %al
	jz line_parsed
	cmp $'[, %al
	je address_argument
	cmp $'], %al
	je separator
	dec %esi
	cmp $0x1a, %al
	jne expression_argument
	push %edi
	mov $directive_operators, %edi
	call get_operator
	or %al, %al
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
	cmpb $'?, (%esi)
	jne check_argument
	pop %edi
	movsb %ds:(%esi), %es:(%edi)
	jmp argument_parsed
      symbol_argument:
	pop %edi
	stosw %ax, %es:(%edi)
	jmp argument_parsed
      operator_argument:
	pop %edi
	stosb %al, %es:(%edi)
	cmp $0x80, %al
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
	or %al, %al
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
	mov $'(, %al
	stosb %al, %es:(%edi)
	call convert_expression
	mov $'), %al
	stosb %al, %es:(%edi)
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
	.byte 0x66, 0xb8  # mov $'(, %ax
	.word '(
	stosw %ax, %es:(%edi)
	lodsl %ds:(%esi), %eax
	mov %eax, %ecx
	stosl %eax, %es:(%edi)
	shr %ecx
	jnc string_movsb_ok
	movsb %ds:(%esi), %es:(%edi)
      string_movsb_ok:
	shr %ecx
	jnc string_movsw_ok
	movsw %ds:(%esi), %es:(%edi)
      string_movsw_ok:
	rep
	movsl %ds:(%esi), %es:(%edi)
	xor %al, %al
	stosb %al, %es:(%edi)
	jmp argument_parsed
      not_string:
	cmpb $'(, (%esi)
	jne parse_expression
	push %esi
	push %edi
	inc %esi
	mov $'{, %al
	stosb %al, %es:(%edi)
	incb parenthesis_stack
	jmp parse_arguments
      parse_expression:
      forced_expression:
	mov $'(, %al
	stosb %al, %es:(%edi)
      expression:
	call convert_expression
	mov $'), %al
	stosb %al, %es:(%edi)
	jmp expression_parsed
      address_argument:
	mov $'[, %al
	stosb %al, %es:(%edi)
	cmpw $0x21a, (%esi)
	jne convert_address
	push %esi
	add $4, %esi
	lea 1(%esi), %ebx
	cmpb $':, (%esi)
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
	cmp $0x10, %al
	jne invalid_address
	mov %ah, %al
	and $240, %ah
	cmp $0x60, %ah
	jne invalid_address
	stosb %al, %es:(%edi)
      convert_address:
	cmpb $0x1a, (%esi)
	jne convert_address_value
	push %esi
	lodsw %ds:(%esi), %ax
	movzbl %ah, %ecx
	push %edi
	mov $address_sizes, %edi
	call get_symbol
	pop %edi
	jc no_size_prefix
	mov %ah, %al
	add $0x70, %al
	stosb %al, %es:(%edi)
	add $4, %esp
	jmp convert_address_value
      no_size_prefix:
	pop %esi
      convert_address_value:
	call convert_expression
	lodsb %ds:(%esi), %al
	cmp $'], %al
	jne invalid_address
	stosb %al, %es:(%edi)
	jmp argument_parsed
      close_expression:
	mov $'}, %al
      separator:
	stosb %al, %es:(%edi)
	jmp argument_parsed
      instruction_separator:
	stosb %al, %es:(%edi)
	jmp instruction_start
      greater:
	cmpb $'=, (%esi)
	jne separator
	inc %esi
	mov $0xf2, %al
	jmp separator
      less:
	cmpb $0x83, -1(%edi)
	je separator
	cmpb $'>, (%esi)
	je not_equal
	cmpb $'=, (%esi)
	jne separator
	inc %esi
	mov $0xf3, %al
	jmp separator
      not_equal:
	inc %esi
	mov $0xf6, %al
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
	cmpb $'), (%esi)
	jne argument_parsed
	decb parenthesis_stack
	pop %edi
	pop %esi
	jmp parse_expression
      empty_instruction:
	lodsb %ds:(%esi), %al
	or %al, %al
	jz line_parsed
	cmp $':, %al
	je empty_label
	cmp $0x3b, %al
	je skip_preprocessed_symbol
	dec %esi
	jmp parse_arguments
      empty_label:
	mov $_counter, %eax
	call increase_counter
	movl %eax, current_locals_prefix
	jmp instruction_start
      skip_preprocessed_symbol:
	lodsb %ds:(%esi), %al
	movzbl %al, %eax
	add %eax, %esi
      skip_next:
	lodsb %ds:(%esi), %al
	or %al, %al
	jz line_parsed
	cmp $0x1a, %al
	je skip_preprocessed_symbol
	cmp $0x22, %al
	je skip_preprocessed_string
	jmp skip_next
      skip_preprocessed_string:
	lodsl %ds:(%esi), %eax
	add %eax, %esi
	jmp skip_next
      line_parsed:
	cmpb $0, parenthesis_stack
	jne invalid_expression
	ret

# %include '../assemble.inc'

#  flat assembler source
#  Copyright (c) 1999-2001, Tomasz Grysztar
#  All rights reserved.

assembler:
	mov labels_list, %edi
	mov memory_end, %ecx
	sub %edi, %ecx
	shr $2, %ecx
	xor %eax, %eax
	rep
	stosl %eax, %es:(%edi)
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
	jmpl *error
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
	lodsb %ds:(%esi), %al
	or %al, %al
	jz source_end
	cmp $1, %al
	je assemble_instruction
	cmp $2, %al
	je define_label
	cmp $3, %al
	je define_constant
	cmp $0xf, %al
	je new_line
	cmp $0x13, %al
	je code_type_setting
	cmp $0x10, %al
	jne illegal_instruction
	lodsb %ds:(%esi), %al
	mov %al, %ah
	shr $4, %ah
	cmp $6, %ah
	jne illegal_instruction
	and $15, %al
	.byte 0xa2  # movb %al, segment_register
	.long segment_register
	call store_segment_prefix
	jmp assemble_line
      code_type_setting:
	lodsb %ds:(%esi), %al
	.byte 0xa2  # movb %al, code_type
	.long code_type
	jmp line_assembled
      new_line:
	lodsl %ds:(%esi), %eax
	movl %eax, current_line
	jmp assemble_line
      define_label:
	lodsl %ds:(%esi), %eax
	mov %eax, %ebx
	lodsb %ds:(%esi), %al
	mov %al, %dl
	xor %ch, %ch
	cmpb $0, reloc_labels
	je label_reloc_ok
	mov $2, %ch
      label_reloc_ok:
	xchg 11(%ebx), %ch
	mov current_pass, %al
	testb $1, 8(%ebx)
	jz new_label
	cmp 9(%ebx), %al
	je symbol_already_defined
	mov %al, 9(%ebx)
	mov %edi, %eax
	sub org_start, %eax
	xchg %eax, (%ebx)
	cdq
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
	cmp 11(%ebx), %ch
	jne changed_label
	jmp assemble_line
      changed_label:
	orb $-1, next_pass_needed
	jmp assemble_line
      new_label:
	orb $1, 8(%ebx)
	mov %al, 9(%ebx)
	movb %dl, 10(%ebx)
	mov %edi, %eax
	sub org_start, %eax
	mov %eax, (%ebx)
	cdq
	movl %edx, 4(%ebx)
	mov org_sib, %eax
	mov %eax, 12(%ebx)
	jmp assemble_line
      define_constant:
	lodsl %ds:(%esi), %eax
	push %eax
	lodsb %ds:(%esi), %al
	push %ax
	call get_value
	pop %bx
	mov %bl, %ch
	pop %ebx
      make_constant:
	mov current_pass, %cl
	testb $1, 8(%ebx)
	jz new_constant
	cmp 9(%ebx), %cl
	jne redefine_constant
	testb $2, 8(%ebx)
	jz symbol_already_defined
	orb $4, 8(%ebx)
      redefine_constant:
	mov %cl, 9(%ebx)
	xchg %eax, (%ebx)
	xchg %edx, 4(%ebx)
	mov value_type, %cl
	xchg %cl, 11(%ebx)
	cmpb $0, current_pass
	je assemble_line
	cmp (%ebx), %eax
	jne changed_constant
	cmp 4(%ebx), %edx
	jne changed_constant
	cmp 11(%ebx), %cl
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
	mov value_type, %cl
	mov %cl, 11(%ebx)
	jmp assemble_line
      assemble_instruction:
	movb $0, operand_size
	movb $0, forced_size
	lodsw %ds:(%esi), %ax
	movzwl %ax, %ebx
	add $assembler, %ebx
	lodsb %ds:(%esi), %al
	jmp *%ebx
      instruction_assembled:
	mov (%esi), %al
	cmp $0xf, %al
	je line_assembled
	or %al, %al
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
	lodsb %ds:(%esi), %al
	or %al, %al
	jz nothing_to_skip
	cmp $0xf, %al
	je nothing_to_skip
	cmp $1, %al
	je skip_instruction
	cmp $2, %al
	je skip_label
	cmp $3, %al
	je skip_label
	cmp $0x20, %al
	jb skip_assembler_symbol
	cmp $'(, %al
	je skip_expression
	cmp $'[, %al
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
	mov (%esi), %al
	and $240, %al
	cmp $0x60, %al
	jb skip_expression
	cmp $0x70, %al
	ja skip_expression
	inc %esi
	jmp skip_address
      skip_expression:
	lodsb %ds:(%esi), %al
	or %al, %al
	jz skip_string
	cmp $'., %al
	je skip_fp_value
	cmp $'), %al
	je skip_done
	cmp $'], %al
	je skip_done
	cmp $0xf, %al
	je skip_expression
	cmp $0x10, %al
	je skip_register
	cmp $0x11, %al
	je skip_label_value
	cmp $0x80, %al
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
	lodsl %ds:(%esi), %eax
	add %eax, %esi
	inc %esi
	jmp skip_done
      nothing_to_skip:
	dec %esi
	stc
	ret

org_directive:
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	cmpb $'., (%esi)
	je invalid_value
	call get_dword_value
	movb $0, reloc_labels
	mov value_type, %dl
	or %dl, %dl
	jz org_ok
	cmp $2, %dl
	jne invalid_use_of_symbol
	orb $-1, reloc_labels
      org_ok:
	mov %edi, %ecx
	sub %eax, %ecx
	movl %ecx, org_start
	movl $0, org_sib
	jmp instruction_assembled
label_directive:
	lodsb %ds:(%esi), %al
	cmp $2, %al
	jne invalid_argument
	lodsl %ds:(%esi), %eax
	inc %esi
	mov %eax, %ebx
	xor %ch, %ch
	cmpb $0x11, (%esi)
	jne label_size_ok
	lodsw %ds:(%esi), %ax
	mov %ah, %ch
      label_size_ok:
	mov %edi, %eax
	sub org_start, %eax
	mov org_sib, %ebp
	cmpb $0x80, (%esi)
	jne define_free_label
	inc %esi
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	movb $0, 11(%ebx)
	push %ebx
	push %cx
	cmpb $'., (%esi)
	je invalid_value
	call get_address_value
	or %bh, %bh
	setnz %ch
	xchg %cl, %ch
	mov %cx, %bp
	shl $16, %ebp
	mov %bh, %bl
	mov %bx, %bp
	pop %cx
	pop %ebx
	mov %al, %dl
	mov value_type, %dh
	cmp $1, %dh
	je invalid_use_of_symbol
	jb free_label_reloc_ok
      define_free_label:
	xor %dh, %dh
	cmpb $0, reloc_labels
	je free_label_reloc_ok
	mov $2, %dh
      free_label_reloc_ok:
	xchg 11(%ebx), %dh
	mov current_pass, %cl
	testb $1, 8(%ebx)
	jz new_free_label
	cmp 9(%ebx), %cl
	je symbol_already_defined
	mov %dh, %ch
	mov %cl, 9(%ebx)
	xchg %eax, (%ebx)
	cdq
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
	cmp 11(%ebx), %ch
	jne changed_free_label
	jmp instruction_assembled
      changed_free_label:
	orb $-1, next_pass_needed
	jmp instruction_assembled
      new_free_label:
	orb $1, 8(%ebx)
	mov %cl, 9(%ebx)
	movb %ch, 10(%ebx)
	mov %eax, (%ebx)
	cdq
	movl %edx, 4(%ebx)
	mov %ebp, 12(%ebx)
	jmp instruction_assembled
load_directive:
	lodsb %ds:(%esi), %al
	cmp $2, %al
	jne invalid_argument
	lodsl %ds:(%esi), %eax
	inc %esi
	push %eax
	mov $1, %al
	cmpb $0x11, (%esi)
	jne load_size_ok
	lodsb %ds:(%esi), %al
	lodsb %ds:(%esi), %al
      load_size_ok:
	cmp $8, %al
	ja invalid_value
	.byte 0xa2  # movb %al, operand_size
	.long operand_size
	lodsb %ds:(%esi), %al
	cmp $0x82, %al
	jne invalid_argument
	lodsw %ds:(%esi), %ax
	.byte 0x66, 0x3d  # cmp $'(, %ax
	.word '(
	jne invalid_argument
	lea 4(%esi), %edx
	mov (%esi), %eax
	lea (4)+(1)(%esi,%eax,1), %esi
	call open
	jc file_not_found
	mov $2, %al
	xor %edx, %edx
	call lseek
	xor %edx, %edx
	cmpb $':, (%esi)
	jne load_position_ok
	inc %esi
	cmpb $'(, (%esi)
	jne invalid_argument
	inc %esi
	cmpb $'., (%esi)
	je invalid_value
	push %ebx
	call get_dword_value
	pop %ebx
	mov %eax, %edx
      load_position_ok:
	xor %al, %al
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
	xor %ch, %ch
	movb $0, value_type
	jmp make_constant
display_directive:
	push %esi
	push %edi
      prepare_display:
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	cmpb $0, (%esi)
	jne display_byte
	inc %esi
	lodsl %ds:(%esi), %eax
	mov %eax, %ecx
	rep
	movsb %ds:(%esi), %es:(%edi)
	inc %esi
	jmp display_next
      display_byte:
	call get_byte_value
	stosb %al, %es:(%edi)
      display_next:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb %ds:(%esi), %al
	or %al, %al
	jz do_display
	cmp $0xf, %al
	je do_display
	cmp $44, %al
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
	movsb %ds:(%esi), %es:(%edi)
	stosl %eax, %es:(%edi)
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
	mov value, %ax
	jecxz last_bytes_ok
	mov %ah, %al
	mov -1(%esi), %ah
	cmp $1, %ecx
	je last_bytes_ok
	mov -2(%esi), %al
      last_bytes_ok:
	movw %ax, value
	sub %ecx, %esi
	push %esi
	call display_block
	pop %esi
	cmp display_buffer, %esi
	jne display_messages
	.byte 0x66, 0xb8  # mov $0xa0d, %ax
	.word 0xa0d
	cmpw %ax, value
	je display_ok
	mov $value, %esi
	mov %ax, (%esi)
	mov $2, %ecx
	call display_block
      display_ok:
	mov labels_list, %eax
	movl %eax, display_buffer
      display_done:
	ret
times_directive:
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	cmpb $'., (%esi)
	je invalid_value
	call get_dword_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	or %eax, %eax
	jz zero_times
	cmpb $':, (%esi)
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
	lodsb %ds:(%esi), %al
	cmp $0x80, %al
	jne virtual_at_current
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	cmpb $'., (%esi)
	je invalid_value
	call get_address_value
	xor %ch, %ch
	or %bh, %bh
	jz set_virtual
	mov $1, %ch
	jmp set_virtual
      virtual_at_current:
	dec %esi
	mov %edi, %eax
	sub org_start, %eax
	xor %bx, %bx
	xor %cx, %cx
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
	movw $virtual_directive-assembler, (%ebx)
	neg %eax
	add %edi, %eax
	xchgl %eax, org_start
	mov %eax, 4(%ebx)
	mov %edx, 8(%ebx)
	mov virtual_data, %al
	mov %al, 2(%ebx)
	mov reloc_labels, %al
	mov %al, 3(%ebx)
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
	cmp (%ebx), %ax
	jne next_structure
	clc
	ret
      next_structure:
	.byte 0x66, 0x3d  # cmp $repeat_directive-assembler, %ax
	.word repeat_directive-assembler
	jne if_structure_ok
	cmpw $if_directive-assembler, (%ebx)
	je no_such_structure
      if_structure_ok:
	.byte 0x66, 0x3d  # cmp $if_directive-assembler, %ax
	.word if_directive-assembler
	jne repeat_structure_ok
	cmpw $repeat_directive-assembler, (%ebx)
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
	mov 2(%ebx), %al
	.byte 0xa2  # movb %al, virtual_data
	.long virtual_data
	mov 3(%ebx), %al
	.byte 0xa2  # movb %al, reloc_labels
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
	movsl %ds:(%esi), %es:(%edi)
	pop %edi
	pop %esi
	jmp instruction_assembled
repeat_directive:
	cmpb $0, times_working
	jne unexpected_instruction
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	cmpb $'., (%esi)
	je invalid_value
	call get_dword_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	or %eax, %eax
	jz zero_repeat
	call allocate_structure_data
	movw $repeat_directive-assembler, (%ebx)
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
	mov (%esi), %al
	or %al, %al
	jz unexpected_end_of_file
	cmp $0xf, %al
	jne extra_characters_on_line
	call find_end_repeat
	jmp instruction_assembled
      find_end_repeat:
	call find_structure_end
	.byte 0x66, 0x3d  # cmp $repeat_directive-assembler, %ax
	.word repeat_directive-assembler
	jne unexpected_instruction
	ret
      find_structure_end:
	call skip_line
	lodsb %ds:(%esi), %al
	cmp $0xf, %al
	jne unexpected_end_of_file
	lodsl %ds:(%esi), %eax
	movl %eax, current_line
      skip_labels:
	cmpb $2, (%esi)
	jne labels_ok
	add $6, %esi
	jmp skip_labels
      labels_ok:
	cmpb $1, (%esi)
	jne find_structure_end
	mov 1(%esi), %ax
	.byte 0x66, 0x3d  # cmp $prefix_instruction-assembler, %ax
	.word prefix_instruction-assembler
	je find_structure_end
	add $4, %esi
	.byte 0x66, 0x3d  # cmp $repeat_directive-assembler, %ax
	.word repeat_directive-assembler
	je skip_repeat
	.byte 0x66, 0x3d  # cmp $if_directive-assembler, %ax
	.word if_directive-assembler
	je skip_if
	.byte 0x66, 0x3d  # cmp $else_directive-assembler, %ax
	.word else_directive-assembler
	je structure_end
	.byte 0x66, 0x3d  # cmp $end_directive-assembler, %ax
	.word end_directive-assembler
	jne find_structure_end
	cmpb $1, (%esi)
	jne find_structure_end
	mov 1(%esi), %ax
	add $4, %esi
	.byte 0x66, 0x3d  # cmp $repeat_directive-assembler, %ax
	.word repeat_directive-assembler
	je structure_end
	.byte 0x66, 0x3d  # cmp $if_directive-assembler, %ax
	.word if_directive-assembler
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
	mov %al, %dl
	mov (%esi), %al
	or %al, %al
	jz unexpected_end_of_file
	cmp $0xf, %al
	jne extra_characters_on_line
	or %dl, %dl
	jnz if_true
	call find_else
	jc instruction_assembled
	mov (%esi), %al
	cmp $1, %al
	jne else_true
	cmpw $if_directive-assembler, 1(%esi)
	jne else_true
	add $4, %esi
	jmp if_directive
      if_true:
	call allocate_structure_data
	movw $if_directive-assembler, (%ebx)
	movb $0, 2(%ebx)
	jmp instruction_assembled
      else_true:
	or %al, %al
	jz unexpected_end_of_file
	cmp $0xf, %al
	jne extra_characters_on_line
	call allocate_structure_data
	movw $if_directive-assembler, (%ebx)
	orb $-1, 2(%ebx)
	jmp instruction_assembled
      else_directive:
	cmpb $0, times_working
	jne unexpected_instruction
	.byte 0x66, 0xb8  # mov $if_directive-assembler, %ax
	.word if_directive-assembler
	call find_structure_data
	jc unexpected_instruction
	cmpb $0, 2(%ebx)
	jne unexpected_instruction
      found_else:
	mov (%esi), %al
	cmp $1, %al
	jne skip_else
	cmpw $if_directive-assembler, 1(%esi)
	jne skip_else
	add $4, %esi
	call find_else
	jnc found_else
	jmp remove_structure_data
      skip_else:
	or %al, %al
	jz unexpected_end_of_file
	cmp $0xf, %al
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
	cmpw $if_directive-assembler, 1(%esi)
	jne skip_after_else
	add $4, %esi
	jmp skip_if
      skip_after_else:
	call find_end_if
	jmp find_structure_end
      find_else:
	call find_structure_end
	.byte 0x66, 0x3d  # cmp $else_directive-assembler, %ax
	.word else_directive-assembler
	je else_found
	.byte 0x66, 0x3d  # cmp $if_directive-assembler, %ax
	.word if_directive-assembler
	jne unexpected_instruction
	stc
	ret
      else_found:
	clc
	ret
      find_end_if:
	call find_structure_end
	.byte 0x66, 0x3d  # cmp $if_directive-assembler, %ax
	.word if_directive-assembler
	jne unexpected_instruction
	ret
end_directive:
	lodsb %ds:(%esi), %al
	cmp $1, %al
	jne invalid_argument
	lodsw %ds:(%esi), %ax
	inc %esi
	.byte 0x66, 0x3d  # cmp $virtual_directive-assembler, %ax
	.word virtual_directive-assembler
	je end_virtual
	.byte 0x66, 0x3d  # cmp $repeat_directive-assembler, %ax
	.word repeat_directive-assembler
	je end_repeat
	.byte 0x66, 0x3d  # cmp $if_directive-assembler, %ax
	.word if_directive-assembler
	je end_if
	jmp invalid_argument

data_bytes:
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	je get_byte
	cmp $'?, %al
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
	stosb %al, %es:(%edi)
      byte_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb %ds:(%esi), %al
	or %al, %al
	jz data_end
	cmp $0xf, %al
	je data_end
	cmp $44, %al
	jne extra_characters_on_line
	jmp data_bytes
      data_end:
	dec %esi
	jmp instruction_assembled
      get_string:
	inc %esi
	lodsl %ds:(%esi), %eax
	mov %eax, %ecx
	rep
	movsb %ds:(%esi), %es:(%edi)
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
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	je get_word
	cmp $'?, %al
	jne invalid_argument
	mov %edi, %eax
	movw $0, (%edi)
	scasw %es:(%edi), %ax
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
	stosw %ax, %es:(%edi)
      word_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb %ds:(%esi), %al
	or %al, %al
	jz data_end
	cmp $0xf, %al
	je data_end
	cmp $44, %al
	jne extra_characters_on_line
	jmp get_words_data
      word_string:
	inc %esi
	lodsl %ds:(%esi), %eax
	mov %eax, %ecx
	jecxz word_string_ok
	xor %ah, %ah
      copy_word_string:
	lodsb %ds:(%esi), %al
	stosw %ax, %es:(%edi)
	loop copy_word_string
      word_string_ok:
	inc %esi
	jmp word_ok
data_dwords:
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	je get_dword
	cmp $'?, %al
	jne invalid_argument
	mov %edi, %eax
	movl $0, (%edi)
	scasl %es:(%edi), %eax
	call undefined_data
	jmp dword_ok
      get_dword:
	push %esi
	call get_dword_value
	pop %ebx
	cmpb $':, (%esi)
	je complex_dword
	call mark_relocation
	stosl %eax, %es:(%edi)
	jmp dword_ok
      complex_dword:
	mov %ebx, %esi
	cmpb $'., (%esi)
	je invalid_value
	call get_word_value
	mov %ax, %dx
	inc %esi
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_operand
	mov value_type, %al
	push %ax
	cmpb $'., (%esi)
	je invalid_value
	call get_word_value
	call mark_relocation
	stosw %ax, %es:(%edi)
	pop %ax
	.byte 0xa2  # movb %al, value_type
	.long value_type
	mov %dx, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
      dword_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb %ds:(%esi), %al
	or %al, %al
	jz data_end
	cmp $0xf, %al
	je data_end
	cmp $44, %al
	jne extra_characters_on_line
	jmp data_dwords
data_pwords:
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	je get_pword
	cmp $'?, %al
	jne invalid_argument
	mov %edi, %eax
	movl $0, (%edi)
	scasl %es:(%edi), %eax
	movw $0, (%edi)
	scasw %es:(%edi), %ax
	call undefined_data
	jmp pword_ok
      get_pword:
	push %esi
	call get_pword_value
	pop %ebx
	cmpb $':, (%esi)
	je complex_pword
	call mark_relocation
	stosl %eax, %es:(%edi)
	mov %dx, %ax
	stosw %ax, %es:(%edi)
	jmp pword_ok
      complex_pword:
	mov %ebx, %esi
	cmpb $'., (%esi)
	je invalid_value
	call get_word_value
	mov %ax, %dx
	inc %esi
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_operand
	mov value_type, %al
	push %ax
	cmpb $'., (%esi)
	je invalid_value
	call get_dword_value
	call mark_relocation
	stosl %eax, %es:(%edi)
	pop %ax
	.byte 0xa2  # movb %al, value_type
	.long value_type
	mov %dx, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
      pword_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb %ds:(%esi), %al
	or %al, %al
	jz data_end
	cmp $0xf, %al
	je data_end
	cmp $44, %al
	jne extra_characters_on_line
	jmp data_pwords
data_qwords:
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	je get_qword
	cmp $'?, %al
	jne invalid_argument
	mov %edi, %eax
	movl $0, (%edi)
	scasl %es:(%edi), %eax
	movl $0, (%edi)
	scasl %es:(%edi), %eax
	call undefined_data
	jmp qword_ok
      get_qword:
	call get_qword_value
	call mark_relocation
	stosl %eax, %es:(%edi)
	mov %edx, %eax
	stosl %eax, %es:(%edi)
      qword_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb %ds:(%esi), %al
	or %al, %al
	jz data_end
	cmp $0xf, %al
	je data_end
	cmp $44, %al
	jne extra_characters_on_line
	jmp data_qwords
data_twords:
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	je get_tbyte
	cmp $'?, %al
	jne invalid_argument
	mov %edi, %eax
	movl $0, (%edi)
	scasl %es:(%edi), %eax
	movl $0, (%edi)
	scasl %es:(%edi), %eax
	movw $0, (%edi)
	scasw %es:(%edi), %ax
	call undefined_data
	jmp tbyte_ok
      get_tbyte:
	lodsb %ds:(%esi), %al
	cmp $'., %al
	jne invalid_value
	cmpw $0x8000, 8(%esi)
	je fp_zero_tbyte
	mov (%esi), %eax
	stosl %eax, %es:(%edi)
	mov 4(%esi), %eax
	stosl %eax, %es:(%edi)
	mov 8(%esi), %ax
	add $0x3fff, %ax
	.byte 0x66, 0x3d  # cmp $0x8000, %ax
	.word 0x8000
	jae value_out_of_range
	mov 11(%esi), %bl
	shl $15, %bx
	or %bx, %ax
	stosw %ax, %es:(%edi)
	add $12, %esi
	jmp tbyte_ok
      fp_zero_tbyte:
	xor %eax, %eax
	stosl %eax, %es:(%edi)
	stosl %eax, %es:(%edi)
	stosw %ax, %es:(%edi)
	add $12, %esi
      tbyte_ok:
	cmp display_buffer, %edi
	jae out_of_memory
	lodsb %ds:(%esi), %al
	or %al, %al
	jz data_end
	cmp $0xf, %al
	je data_end
	cmp $44, %al
	jne extra_characters_on_line
	jmp data_twords
data_file:
	lodsw %ds:(%esi), %ax
	.byte 0x66, 0x3d  # cmp $'(, %ax
	.word '(
	jne invalid_argument
	lea 4(%esi), %edx
	mov (%esi), %eax
	lea (4)+(1)(%esi,%eax,1), %esi
	call open
	jc file_not_found
	mov $2, %al
	xor %edx, %edx
	call lseek
	push %eax
	xor %edx, %edx
	cmpb $':, (%esi)
	jne position_ok
	inc %esi
	cmpb $'(, (%esi)
	jne invalid_argument
	inc %esi
	cmpb $'., (%esi)
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
	cmpb $'(, (%esi)
	jne invalid_argument
	inc %esi
	cmpb $'., (%esi)
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
	xor %al, %al
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
	lodsb %ds:(%esi), %al
	cmp $44, %al
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
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	cmpb $'., (%esi)
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
	stosb %al, %es:(%edi)
      bytes_stosb_ok:
	shr %ecx
	jnc bytes_stosw_ok
	stosw %ax, %es:(%edi)
      bytes_stosw_ok:
	rep
	stosl %eax, %es:(%edi)
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
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	cmpb $'., (%esi)
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
	stosw %ax, %es:(%edi)
      words_stosw_ok:
	rep
	stosl %eax, %es:(%edi)
	jmp reserved_data
reserve_dwords:
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	cmpb $'., (%esi)
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
	stosl %eax, %es:(%edi)
	jmp reserved_data
reserve_pwords:
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	cmpb $'., (%esi)
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
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	cmpb $'., (%esi)
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
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_argument
	cmpb $'., (%esi)
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
	stosb %al, %es:(%edi)
	jmp instruction_assembled
simple_instruction_16bit:
	cmpb $32, code_type
	je size_prefix
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      size_prefix:
	mov %al, %ah
	mov $0x66, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
simple_instruction_32bit:
	cmpb $16, code_type
	je size_prefix
	stosb %al, %es:(%edi)
	jmp instruction_assembled
simple_extended_instruction:
	mov %al, %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
prefix_instruction:
	stosb %al, %es:(%edi)
	jmp assemble_line
int_instruction:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $1, %ah
	ja invalid_operand_size
	cmp $'(, %al
	jne invalid_operand
	call get_byte_value
	mov %al, %ah
	mov $0xcd, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
aa_instruction:
	push %ax
	mov $10, %bl
	cmpb $'(, (%esi)
	jne aa_instruction.store
	inc %esi
	xor %al, %al
	xchg operand_size, %al
	cmp $1, %al
	ja invalid_operand_size
	call get_byte_value
	mov %al, %bl
      aa_instruction.store:
	cmpb $0, operand_size
	jne invalid_operand
	pop %ax
	mov %bl, %ah
	stosw %ax, %es:(%edi)
	jmp instruction_assembled

basic_instruction:
	.byte 0xa2  # movb %al, base_code
	.long base_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je basic_reg
	cmp $'[, %al
	jne invalid_operand
      basic_mem:
	call get_address
	push %edx
	push %bx
	push %cx
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	cmpb $0x11, (%esi)
	sete %al
	.byte 0xa2  # movb %al, imm_sized
	.long imm_sized
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	je basic_mem_imm
	cmp $0x10, %al
	jne invalid_operand
      basic_mem_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	pop %cx
	pop %bx
	pop %edx
	mov %ah, %al
	cmp $1, %al
	je basic_mem_reg_8bit
	cmp $2, %al
	je basic_mem_reg_16bit
	cmp $4, %al
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
	mov operand_size, %al
	cmp $1, %al
	je basic_mem_imm_8bit
	cmp $2, %al
	je basic_mem_imm_16bit
	cmp $4, %al
	je basic_mem_imm_32bit
	or %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp basic_mem_imm_32bit
      basic_mem_imm_8bit:
	call get_byte_value
	.byte 0xa2  # movb %al, value
	.long value
	mov base_code, %al
	shr $3, %al
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	pop %cx
	pop %bx
	pop %edx
	movb $0x80, base_code
	call store_instruction
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      basic_mem_imm_16bit:
	call get_word_value
	movw %ax, value
	mov base_code, %al
	shr $3, %al
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	call operand_16bit_prefix
	pop %cx
	pop %bx
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
	mov value, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      basic_mem_simm_8bit:
	movb $0x83, base_code
	call store_instruction
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      basic_mem_imm_32bit:
	call get_dword_value
	movl %eax, value
	mov base_code, %al
	shr $3, %al
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	call operand_32bit_prefix
	pop %cx
	pop %bx
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
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
      basic_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	cmpb $0x11, (%esi)
	sete %al
	.byte 0xa2  # movb %al, imm_sized
	.long imm_sized
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je basic_reg_reg
	cmp $'(, %al
	je basic_reg_imm
	cmp $'[, %al
	jne invalid_operand
      basic_reg_mem:
	call get_address
	mov operand_size, %al
	cmp $1, %al
	je basic_reg_mem_8bit
	cmp $2, %al
	je basic_reg_mem_16bit
	cmp $4, %al
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
	lodsb %ds:(%esi), %al
	call convert_register
	shl $3, %al
	mov postbyte_register, %bl
	or %al, %bl
	or $192, %bl
	mov %ah, %al
	cmp $1, %al
	je basic_reg_reg_8bit
	cmp $2, %al
	je basic_reg_reg_16bit
	cmp $4, %al
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
	mov base_code, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      basic_reg_imm:
	mov operand_size, %al
	cmp $1, %al
	je basic_reg_imm_8bit
	cmp $2, %al
	je basic_reg_imm_16bit
	cmp $4, %al
	je basic_reg_imm_32bit
	or %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp basic_reg_imm_32bit
      basic_reg_imm_8bit:
	call get_byte_value
	mov %al, %dl
	mov base_code, %ah
	or $192, %ah
	mov postbyte_register, %bl
	and $7, %bl
	or %bl, %bl
	jz basic_al_imm
	or %bl, %ah
	mov $0x80, %al
	stosw %ax, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      basic_al_imm:
	mov base_code, %al
	add $4, %al
	stosb %al, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      basic_reg_imm_16bit:
	call get_word_value
	mov %ax, %dx
	call operand_16bit_prefix
	mov base_code, %ah
	or $192, %ah
	mov postbyte_register, %bl
	and $7, %bl
	or %bl, %ah
	cmpb $0, value_type
	jne basic_reg_imm_16bit.store
	cmpb $0, imm_sized
	jne basic_reg_imm_16bit.store
	cmp $0x80, %dx
	jb basic_reg_simm_8bit
	cmp $-0x80, %dx
	jae basic_reg_simm_8bit
      basic_reg_imm_16bit.store:
	or %bl, %bl
	jz basic_ax_imm
	mov $0x81, %al
	stosw %ax, %es:(%edi)
	mov %dx, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      basic_reg_simm_8bit:
	mov $0x83, %al
	stosw %ax, %es:(%edi)
	mov %dx, %ax
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      basic_ax_imm:
	mov base_code, %al
	add $5, %al
	stosb %al, %es:(%edi)
	mov %dx, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      basic_reg_imm_32bit:
	call get_dword_value
	mov %eax, %edx
	call operand_32bit_prefix
	mov base_code, %ah
	or $192, %ah
	mov postbyte_register, %bl
	and $7, %bl
	or %bl, %ah
	cmpb $0, value_type
	jne basic_reg_imm_32bit.store
	cmpb $0, imm_sized
	jne basic_reg_imm_32bit.store
	cmp $0x80, %edx
	jb basic_reg_simm_8bit
	cmp $-0x80, %edx
	jae basic_reg_simm_8bit
      basic_reg_imm_32bit.store:
	or %bl, %bl
	jz basic_eax_imm
	mov $0x81, %al
	stosw %ax, %es:(%edi)
	mov %edx, %eax
	call mark_relocation
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
      basic_eax_imm:
	mov base_code, %al
	add $5, %al
	stosb %al, %es:(%edi)
	mov %edx, %eax
	call mark_relocation
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
single_operand_instruction:
	movb $0xf6, base_code
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je single_reg
	cmp $'[, %al
	jne invalid_operand
      single_mem:
	call get_address
	mov operand_size, %al
	cmp $1, %al
	je single_mem_8bit
	cmp $2, %al
	je single_mem_16bit
	cmp $4, %al
	je single_mem_32bit
	or %al, %al
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
	lodsb %ds:(%esi), %al
	call convert_register
	mov postbyte_register, %bl
	shl $3, %bl
	or %al, %bl
	or $192, %bl
	mov %ah, %al
	cmp $1, %al
	je single_reg_8bit
	cmp $2, %al
	je single_reg_16bit
	cmp $4, %al
	je single_reg_32bit
	jmp invalid_operand_size
      single_reg_8bit:
	mov %bl, %ah
	mov $0xf6, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      single_reg_16bit:
	call operand_16bit_prefix
	mov %bl, %ah
	mov $0xf7, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      single_reg_32bit:
	call operand_32bit_prefix
	mov %bl, %ah
	mov $0xf7, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
mov_instruction:
	movb $0x88, base_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je mov_reg
	cmp $'[, %al
	jne invalid_operand
      mov_mem:
	call get_address
	push %edx
	push %bx
	push %cx
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	je mov_mem_imm
	cmp $0x10, %al
	jne invalid_operand
      mov_mem_reg:
	lodsb %ds:(%esi), %al
	cmp $0x60, %al
	jae mov_mem_sreg
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	pop %cx
	pop %bx
	pop %edx
	cmp $1, %ah
	je mov_mem_reg_8bit
	cmp $2, %ah
	je mov_mem_reg_16bit
	cmp $4, %ah
	je mov_mem_reg_32bit
	jmp invalid_operand_size
      mov_mem_reg_8bit:
	or %bl, %al
	or %bh, %al
	jz mov_mem_al
	call store_instruction
	jmp instruction_assembled
      mov_mem_al:
	cmp $2, %ch
	je mov_mem_address16_al
	test $4, %ch
	jnz mov_mem_address32_al
	or %ch, %ch
	jnz invalid_address_size
	cmpb $32, code_type
	je mov_mem_address32_al
	cmp $0x10000, %edx
	jb mov_mem_address16_al
      mov_mem_address32_al:
	call address_32bit_prefix
	call store_segment_prefix_if_necessary
	mov $0xa2, %al
      store_mov_address32:
	stosb %al, %es:(%edi)
	pushl $instruction_assembled
	jmp store_address_32bit_value
      mov_mem_address16_al:
	call address_16bit_prefix
	call store_segment_prefix_if_necessary
	mov $0xa2, %al
      store_mov_address16:
	stosb %al, %es:(%edi)
	mov %edx, %eax
	stosw %ax, %es:(%edi)
	cmp $0x10000, %edx
	jge value_out_of_range
	jmp instruction_assembled
      mov_mem_reg_16bit:
	call operand_16bit_prefix
	mov postbyte_register, %al
	or %bl, %al
	or %bh, %al
	jz mov_mem_ax
	incb base_code
	call store_instruction
	jmp instruction_assembled
      mov_mem_ax:
	cmp $2, %ch
	je mov_mem_address16_ax
	test $4, %ch
	jnz mov_mem_address32_ax
	or %ch, %ch
	jnz invalid_address_size
	cmpb $32, code_type
	je mov_mem_address32_ax
	cmp $0x10000, %edx
	jb mov_mem_address16_ax
      mov_mem_address32_ax:
	call address_32bit_prefix
	call store_segment_prefix_if_necessary
	mov $0xa3, %al
	jmp store_mov_address32
      mov_mem_address16_ax:
	call address_16bit_prefix
	call store_segment_prefix_if_necessary
	mov $0xa3, %al
	jmp store_mov_address16
      mov_mem_reg_32bit:
	call operand_32bit_prefix
	mov postbyte_register, %al
	or %bl, %al
	or %bh, %al
	jz mov_mem_ax
	incb base_code
	call store_instruction
	jmp instruction_assembled
      mov_mem_sreg:
	cmp $0x70, %al
	jae invalid_operand
	sub $0x61, %al
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	pop %cx
	pop %bx
	pop %edx
	mov operand_size, %ah
	or %ah, %ah
	jz mov_mem_sreg_size_ok
	cmp $2, %ah
	je mov_mem16_sreg
	cmp $4, %ah
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
	mov operand_size, %al
	cmp $1, %al
	je mov_mem_imm_8bit
	cmp $2, %al
	je mov_mem_imm_16bit
	cmp $4, %al
	je mov_mem_imm_32bit
	or %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp mov_mem_imm_32bit
      mov_mem_imm_8bit:
	call get_byte_value
	.byte 0xa2  # movb %al, value
	.long value
	movb $0, postbyte_register
	movb $0xc6, base_code
	pop %cx
	pop %bx
	pop %edx
	call store_instruction
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      mov_mem_imm_16bit:
	call get_word_value
	movw %ax, value
	movb $0, postbyte_register
	movb $0xc7, base_code
	call operand_16bit_prefix
	pop %cx
	pop %bx
	pop %edx
	call store_instruction
	mov value, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      mov_mem_imm_32bit:
	call get_dword_value
	movl %eax, value
	movb $0, postbyte_register
	movb $0xc7, base_code
	call operand_32bit_prefix
	pop %cx
	pop %bx
	pop %edx
	call store_instruction
	mov value, %eax
	call mark_relocation
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
      mov_reg:
	lodsb %ds:(%esi), %al
	cmp $0x50, %al
	jae mov_sreg
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	je mov_reg_mem
	cmp $'(, %al
	je mov_reg_imm
	cmp $0x10, %al
	jne invalid_operand
      mov_reg_reg:
	lodsb %ds:(%esi), %al
	cmp $0x50, %al
	jae mov_reg_sreg
	call convert_register
	shl $3, %al
	mov postbyte_register, %bl
	or %al, %bl
	or $192, %bl
	mov %ah, %al
	cmp $1, %al
	je mov_reg_reg_8bit
	cmp $2, %al
	je mov_reg_reg_16bit
	cmp $4, %al
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
	mov base_code, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      mov_reg_sreg:
	mov %al, %ah
	shr $4, %ah
	cmp $5, %ah
	je mov_reg_creg
	cmp $7, %ah
	je mov_reg_dreg
	ja invalid_operand
	sub $0x61, %al
	mov postbyte_register, %bl
	shl $3, %al
	or %al, %bl
	or $192, %bl
	cmpb $4, operand_size
	je mov_reg_sreg32
	cmpb $2, operand_size
	jne invalid_operand_size
	call operand_16bit_prefix
	jmp mov_reg_sreg_store
     mov_reg_sreg32:
	call operand_32bit_prefix
     mov_reg_sreg_store:
	mov $0x8c, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      mov_reg_creg:
	mov $0x20, %bh
	jmp mov_reg_xrx
      mov_reg_dreg:
	mov $0x21, %bh
      mov_reg_xrx:
	and $7, %al
	mov postbyte_register, %bl
	shl $3, %al
	or %al, %bl
	or $192, %bl
	cmpb $4, operand_size
	jne invalid_operand_size
	mov %bh, %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      mov_reg_mem:
	call get_address
	mov operand_size, %al
	cmp $1, %al
	je mov_reg_mem_8bit
	cmp $2, %al
	je mov_reg_mem_16bit
	cmp $4, %al
	je mov_reg_mem_32bit
	jmp invalid_operand_size
      mov_reg_mem_8bit:
	mov postbyte_register, %al
	or %bl, %al
	or %bh, %al
	jz mov_al_mem
	addb $2, base_code
	call store_instruction
	jmp instruction_assembled
      mov_al_mem:
	cmp $2, %ch
	je mov_al_mem_address16
	test $4, %ch
	jnz mov_al_mem_address32
	or %ch, %ch
	jnz invalid_address_size
	cmpb $32, code_type
	je mov_al_mem_address32
	cmp $0x10000, %edx
	jb mov_al_mem_address16
      mov_al_mem_address32:
	call address_32bit_prefix
	call store_segment_prefix_if_necessary
	mov $0xa0, %al
	jmp store_mov_address32
      mov_al_mem_address16:
	call address_16bit_prefix
	call store_segment_prefix_if_necessary
	mov $0xa0, %al
	jmp store_mov_address16
      mov_reg_mem_16bit:
	call operand_16bit_prefix
	mov postbyte_register, %al
	or %bl, %al
	or %bh, %al
	jz mov_ax_mem
	addb $3, base_code
	call store_instruction
	jmp instruction_assembled
      mov_ax_mem:
	cmp $2, %ch
	je mov_ax_mem_address16
	test $4, %ch
	jnz mov_ax_mem_address32
	or %ch, %ch
	jnz invalid_address_size
	cmpb $32, code_type
	je mov_ax_mem_address32
	cmp $0x10000, %edx
	jb mov_ax_mem_address16
      mov_ax_mem_address32:
	call address_32bit_prefix
	call store_segment_prefix_if_necessary
	mov $0xa1, %al
	jmp store_mov_address32
      mov_ax_mem_address16:
	call address_16bit_prefix
	call store_segment_prefix_if_necessary
	mov $0xa1, %al
	jmp store_mov_address16
      mov_reg_mem_32bit:
	call operand_32bit_prefix
	mov postbyte_register, %al
	or %bl, %al
	or %bh, %al
	jz mov_ax_mem
	addb $3, base_code
	call store_instruction
	jmp instruction_assembled
      mov_reg_imm:
	mov operand_size, %al
	cmp $1, %al
	je mov_reg_imm_8bit
	cmp $2, %al
	je mov_reg_imm_16bit
	cmp $4, %al
	je mov_reg_imm_32bit
	or %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp mov_reg_imm_32bit
      mov_reg_imm_8bit:
	call get_byte_value
	mov %al, %ah
	mov postbyte_register, %al
	and $7, %al
	add $0xb0, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      mov_reg_imm_16bit:
	call get_word_value
	mov %ax, %dx
	call operand_16bit_prefix
	mov postbyte_register, %al
	and $7, %al
	add $0xb8, %al
	stosb %al, %es:(%edi)
	mov %dx, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      mov_reg_imm_32bit:
	call get_dword_value
	mov %eax, %edx
	call operand_32bit_prefix
	mov postbyte_register, %al
	and $7, %al
	add $0xb8, %al
	stosb %al, %es:(%edi)
	mov %edx, %eax
	call mark_relocation
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
      mov_sreg:
	mov %al, %ah
	shr $4, %ah
	cmp $5, %ah
	je mov_creg
	cmp $7, %ah
	je mov_dreg
	ja invalid_operand
	sub $0x61, %al
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	je mov_sreg_mem
	cmp $0x10, %al
	jne invalid_operand
      mov_sreg_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	or %ah, %ah
	jz mov_sreg_reg_size_ok
	cmp $4, %ah
	je mov_sreg_reg32
	cmp $2, %ah
	je mov_sreg_reg16
	jmp invalid_operand_size
      mov_sreg_reg32:
	mov %al, %ah
	call operand_32bit_prefix
	mov %ah, %al
	jmp mov_sreg_reg_size_ok
      mov_sreg_reg16:
	mov %al, %ah
	call operand_16bit_prefix
	mov %ah, %al
      mov_sreg_reg_size_ok:
	mov $192, %bl
	or %al, %bl
	mov postbyte_register, %al
	shl $3, %al
	or %al, %bl
	mov $0x8e, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      mov_sreg_mem:
	call get_address
	mov operand_size, %al
	or %al, %al
	jz mov_sreg_mem_size_ok
	cmp $2, %al
	je mov_sreg_mem16
	cmp $4, %al
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
	mov $0x22, %dl
	jmp mov_xrx
      mov_dreg:
	mov $0x23, %dl
      mov_xrx:
	and $7, %al
	mov %al, %bh
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	cmp $4, %ah
	jne invalid_operand_size
	mov $192, %bl
	or %al, %bl
	mov %bh, %al
	shl $3, %al
	or %al, %bl
	mov $0xf, %al
	mov %dl, %ah
	stosw %ax, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
test_instruction:
	movb $0x84, base_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je test_reg
	cmp $'[, %al
	jne invalid_operand
      test_mem:
	call get_address
	push %edx
	push %bx
	push %cx
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	je test_mem_imm
	cmp $0x10, %al
	jne invalid_operand
      test_mem_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	pop %cx
	pop %bx
	pop %edx
	mov %ah, %al
	cmp $1, %al
	je test_mem_reg_8bit
	cmp $2, %al
	je test_mem_reg_16bit
	cmp $4, %al
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
	mov operand_size, %al
	cmp $1, %al
	je test_mem_imm_8bit
	cmp $2, %al
	je test_mem_imm_16bit
	cmp $4, %al
	je test_mem_imm_32bit
	or %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp test_mem_imm_32bit
      test_mem_imm_8bit:
	call get_byte_value
	.byte 0xa2  # movb %al, value
	.long value
	movb $0, postbyte_register
	movb $0xf6, base_code
	pop %cx
	pop %bx
	pop %edx
	call store_instruction
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      test_mem_imm_16bit:
	call get_word_value
	movw %ax, value
	movb $0, postbyte_register
	movb $0xf7, base_code
	call operand_16bit_prefix
	pop %cx
	pop %bx
	pop %edx
	call store_instruction
	mov value, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      test_mem_imm_32bit:
	call get_dword_value
	movl %eax, value
	movb $0, postbyte_register
	movb $0xf7, base_code
	call operand_32bit_prefix
	pop %cx
	pop %bx
	pop %edx
	call store_instruction
	mov value, %eax
	call mark_relocation
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
      test_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	je test_reg_imm
	cmp $0x10, %al
	jne invalid_operand
      test_reg_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	shl $3, %al
	mov postbyte_register, %bl
	or %al, %bl
	or $192, %bl
	mov %ah, %al
	cmp $1, %al
	je test_reg_reg_8bit
	cmp $2, %al
	je test_reg_reg_16bit
	cmp $4, %al
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
	mov base_code, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      test_reg_imm:
	mov operand_size, %al
	cmp $1, %al
	je test_reg_imm_8bit
	cmp $2, %al
	je test_reg_imm_16bit
	cmp $4, %al
	je test_reg_imm_32bit
	or %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp test_reg_imm_32bit
      test_reg_imm_8bit:
	call get_byte_value
	mov %al, %dl
	mov $192, %ah
	mov postbyte_register, %bl
	and $7, %bl
	or %bl, %bl
	jz test_al_imm
	or %bl, %ah
	mov $0xf6, %al
	stosw %ax, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      test_al_imm:
	mov $0xa8, %al
	stosb %al, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      test_reg_imm_16bit:
	call get_word_value
	mov %ax, %dx
	call operand_16bit_prefix
	mov $192, %ah
	mov postbyte_register, %bl
	and $7, %bl
	or %bl, %bl
	jz test_ax_imm
	or %bl, %ah
	mov $0xf7, %al
	stosw %ax, %es:(%edi)
	mov %dx, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      test_ax_imm:
	mov $0xa9, %al
	stosb %al, %es:(%edi)
	mov %dx, %ax
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      test_reg_imm_32bit:
	call get_dword_value
	mov %eax, %edx
	call operand_32bit_prefix
	mov $192, %ah
	mov postbyte_register, %bl
	and $7, %bl
	or %bl, %bl
	jz test_eax_imm
	or %bl, %ah
	mov $0xf7, %al
	stosw %ax, %es:(%edi)
	mov %edx, %eax
	call mark_relocation
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
      test_eax_imm:
	mov $0xa9, %al
	stosb %al, %es:(%edi)
	mov %edx, %eax
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
xchg_instruction:
	movb $0x86, base_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je xchg_reg
	cmp $'[, %al
	jne invalid_operand
      xchg_mem:
	call get_address
	push %edx
	push %bx
	push %cx
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
      xchg_mem_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	pop %cx
	pop %bx
	pop %edx
	mov %ah, %al
	cmp $1, %al
	je xchg_mem_reg_8bit
	cmp $2, %al
	je xchg_mem_reg_16bit
	cmp $4, %al
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
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	je xchg_reg_mem
	cmp $0x10, %al
	jne invalid_operand
      xchg_reg_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	mov %al, %bh
	mov postbyte_register, %bl
	shlb $3, postbyte_register
	or $192, %al
	orb %al, postbyte_register
	mov %ah, %al
	cmp $1, %al
	je xchg_reg_reg_8bit
	cmp $2, %al
	je xchg_reg_reg_16bit
	cmp $4, %al
	je xchg_reg_reg_32bit
	jmp invalid_operand_size
      xchg_reg_reg_32bit:
	call operand_32bit_prefix
	or %bh, %bh
	jz xchg_ax_reg
	xchg %bl, %bh
	or %bh, %bh
	jz xchg_ax_reg
	incb base_code
	jmp xchg_reg_reg_8bit
      xchg_reg_reg_16bit:
	call operand_16bit_prefix
	or %bh, %bh
	jz xchg_ax_reg
	xchg %bl, %bh
	or %bh, %bh
	jz xchg_ax_reg
	incb base_code
      xchg_reg_reg_8bit:
	mov base_code, %al
	mov postbyte_register, %ah
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      xchg_ax_reg:
	mov $0x90, %al
	add %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      xchg_reg_mem:
	call get_address
	mov operand_size, %al
	cmp $1, %al
	je xchg_reg_mem_8bit
	cmp $2, %al
	je xchg_reg_mem_16bit
	cmp $4, %al
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
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je push_reg
	cmp $'(, %al
	je push_imm
	cmp $'[, %al
	jne invalid_operand
      push_mem:
	call get_address
	mov operand_size, %al
	cmp $2, %al
	je push_mem_16bit
	cmp $4, %al
	je push_mem_32bit
	or %al, %al
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
	lodsb %ds:(%esi), %al
	cmp $0x60, %al
	jae push_sreg
	call convert_register
	mov %al, %dl
	add $0x50, %dl
	mov %ah, %al
	cmp $2, %al
	je push_reg_16bit
	cmp $4, %al
	je push_reg_32bit
	jmp invalid_operand_size
      push_reg_16bit:
	call operand_16bit_prefix
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp push_done
      push_reg_32bit:
	call operand_32bit_prefix
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp push_done
      push_sreg:
	mov operand_size, %bl
	cmp $4, %bl
	je push_sreg32
	cmp $2, %bl
	je push_sreg16
	or %bl, %bl
	jz push_sreg_store
	jmp invalid_operand_size
      push_sreg16:
	mov %al, %bl
	call operand_16bit_prefix
	mov %bl, %al
	jmp push_sreg_store
      push_sreg32:
	mov %al, %bl
	call operand_32bit_prefix
	mov %bl, %al
      push_sreg_store:
	cmp $0x70, %al
	jae invalid_operand
	sub $0x61, %al
	cmp $4, %al
	jae push_sreg_386
	shl $3, %al
	add $6, %al
	stosb %al, %es:(%edi)
	jmp push_done
      push_sreg_386:
	sub $4, %al
	shl $3, %al
	mov $0xa0, %ah
	add %al, %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	jmp push_done
      push_imm:
	mov operand_size, %al
	cmp $2, %al
	je push_imm_16bit
	cmp $4, %al
	je push_imm_32bit
	or %al, %al
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
	mov %al, %ah
	mov $0x6a, %al
	stosw %ax, %es:(%edi)
	jmp push_done
      push_imm_optimized_16bit:
	call get_word_value
	mov %ax, %dx
	cmpb $0, value_type
	jne push_imm_16bit_forced
	.byte 0x66, 0x3d  # cmp $-0x80, %ax
	.word -0x80
	jl push_imm_16bit_forced
	.byte 0x66, 0x3d  # cmp $0x80, %ax
	.word 0x80
	jge push_imm_16bit_forced
	jmp push_imm_8bit
      push_imm_16bit:
	call get_word_value
	mov %ax, %dx
	call operand_16bit_prefix
      push_imm_16bit_forced:
	mov $0x68, %al
	stosb %al, %es:(%edi)
	mov %dx, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp push_done
      push_imm_32bit:
	call get_dword_value
	mov %eax, %edx
	call operand_32bit_prefix
      push_imm_32bit_forced:
	mov $0x68, %al
	stosb %al, %es:(%edi)
	mov %edx, %eax
	call mark_relocation
	stosl %eax, %es:(%edi)
      push_done:
	lodsb %ds:(%esi), %al
	dec %esi
	cmp $0xf, %al
	je instruction_assembled
	or %al, %al
	jz instruction_assembled
	movb $0, operand_size
	movb $0, forced_size
	jmp push_instruction
pop_instruction:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je pop_reg
	cmp $'[, %al
	jne invalid_operand
      pop_mem:
	call get_address
	mov operand_size, %al
	cmp $2, %al
	je pop_mem_16bit
	cmp $4, %al
	je pop_mem_32bit
	or %al, %al
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
	lodsb %ds:(%esi), %al
	cmp $0x60, %al
	jae pop_sreg
	call convert_register
	mov %al, %dl
	add $0x58, %dl
	mov %ah, %al
	cmp $2, %al
	je pop_reg_16bit
	cmp $4, %al
	je pop_reg_32bit
	jmp invalid_operand_size
      pop_reg_16bit:
	call operand_16bit_prefix
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp pop_done
      pop_reg_32bit:
	call operand_32bit_prefix
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp pop_done
      pop_sreg:
	mov operand_size, %bl
	cmp $4, %bl
	je pop_sreg32
	cmp $2, %bl
	je pop_sreg16
	or %bl, %bl
	jz pop_sreg_store
	jmp invalid_operand_size
      pop_sreg16:
	mov %al, %bl
	call operand_16bit_prefix
	mov %bl, %al
	jmp pop_sreg_store
      pop_sreg32:
	mov %al, %bl
	call operand_32bit_prefix
	mov %bl, %al
      pop_sreg_store:
	cmp $0x70, %al
	jae invalid_operand
	sub $0x61, %al
	cmp $1, %al
	je illegal_instruction
	cmp $4, %al
	jae pop_sreg_386
	shl $3, %al
	add $7, %al
	stosb %al, %es:(%edi)
	jmp pop_done
      pop_sreg_386:
	sub $4, %al
	shl $3, %al
	mov $0xa1, %ah
	add %al, %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
      pop_done:
	lodsb %ds:(%esi), %al
	dec %esi
	cmp $0xf, %al
	je instruction_assembled
	or %al, %al
	jz instruction_assembled
	movb $0, operand_size
	movb $0, forced_size
	jmp pop_instruction
inc_instruction:
	.byte 0xa2  # movb %al, base_code
	.long base_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je inc_reg
	cmp $'[, %al
	je inc_mem
	jne invalid_operand
      inc_mem:
	call get_address
	mov operand_size, %al
	cmp $1, %al
	je inc_mem_8bit
	cmp $2, %al
	je inc_mem_16bit
	cmp $4, %al
	je inc_mem_32bit
	or %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
      inc_mem_8bit:
	mov $0xfe, %al
	xchg base_code, %al
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	call store_instruction
	jmp instruction_assembled
      inc_mem_16bit:
	call operand_16bit_prefix
	mov $0xff, %al
	xchg base_code, %al
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	call store_instruction
	jmp instruction_assembled
      inc_mem_32bit:
	call operand_32bit_prefix
	mov $0xff, %al
	xchg base_code, %al
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	call store_instruction
	jmp instruction_assembled
      inc_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	mov %al, %dl
	shr $4, %al
	mov %ah, %al
	cmp $1, %al
	je inc_reg_8bit
	mov base_code, %dh
	shl $3, %dh
	add %dh, %dl
	add $0x40, %dl
	cmp $2, %al
	je inc_reg_16bit
	cmp $4, %al
	je inc_reg_32bit
	jmp invalid_operand_size
      inc_reg_8bit:
	mov $0xfe, %al
	mov base_code, %ah
	shl $3, %ah
	or %dl, %ah
	or $192, %ah
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      inc_reg_16bit:
	call operand_16bit_prefix
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      inc_reg_32bit:
	call operand_32bit_prefix
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
arpl_instruction:
	.byte 0xa2  # movb %al, base_code
	.long base_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je arpl_reg
	cmp $'[, %al
	jne invalid_operand
	call get_address
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	cmp $2, %ah
	jne invalid_operand_size
	movb $0x63, base_code
	call store_instruction
	jmp instruction_assembled
      arpl_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	cmp $2, %ah
	jne invalid_operand_size
	mov %al, %dl
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	cmp $2, %ah
	jne invalid_operand_size
	mov %al, %ah
	shl $3, %ah
	or %dl, %ah
	or $192, %ah
	mov $0x63, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
bound_instruction:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $'[, %al
	jne invalid_operand
	call get_address
	mov operand_size, %al
	cmp $2, %al
	je bound_16bit
	cmp $4, %al
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
	.byte 0xa2  # movb %al, extended_code
	.long extended_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je set_reg
	cmp $'[, %al
	jne invalid_operand
      set_mem:
	call get_address
	cmpb $1, operand_size
	ja invalid_operand_size
	movb $0, postbyte_register
	call store_instruction
	jmp instruction_assembled
      set_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	mov %al, %bl
	cmp $1, %ah
	jne invalid_operand_size
	mov extended_code, %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	mov $192, %al
	or %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
ret_instruction_16bit:
	mov %al, %ah
	call operand_16bit_prefix
	mov %ah, %al
	jmp ret_instruction
ret_instruction_32bit:
	mov %al, %ah
	call operand_32bit_prefix
	mov %ah, %al
ret_instruction:
	.byte 0xa2  # movb %al, base_code
	.long base_code
	lodsb %ds:(%esi), %al
	dec %esi
	or %al, %al
	jz simple_ret
	cmp $0xf, %al
	je simple_ret
	lodsb %ds:(%esi), %al
	call get_size_operator
	or %ah, %ah
	jz ret_imm
	cmp $2, %ah
	je ret_imm
	jmp invalid_operand_size
      ret_imm:
	cmp $'(, %al
	jne invalid_operand
	call get_word_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	mov %ax, %dx
	mov base_code, %al
	stosb %al, %es:(%edi)
	mov %dx, %ax
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      simple_ret:
	mov base_code, %al
	inc %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
lea_instruction:
	movb $0x8d, base_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	mov operand_size, %al
	push %ax
	movb $0, operand_size
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	pop %ax
	cmp $2, %al
	je lea_16bit
	cmp $4, %al
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
	or %al, %al
	jz les_instruction
	cmp $3, %al
	jz lds_instruction
	add $0xb0, %al
	.byte 0xa2  # movb %al, extended_code
	.long extended_code
	movb $0xf, base_code
	jmp ls_code_ok
      les_instruction:
	movb $0xc4, base_code
	jmp ls_code_ok
      lds_instruction:
	movb $0xc5, base_code
      ls_code_ok:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	addb $2, operand_size
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	mov operand_size, %al
	cmp $4, %al
	je ls_16bit
	cmp $6, %al
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
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $2, %ah
	je enter_imm16_size_ok
	or %ah, %ah
	jnz invalid_operand_size
      enter_imm16_size_ok:
	cmp $'(, %al
	jne invalid_operand
	call get_word_value
	cmpb $0, value_type
	jne invalid_use_of_symbol
	push %ax
	movb $0, operand_size
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $1, %ah
	je enter_imm8_size_ok
	or %ah, %ah
	jnz invalid_operand_size
      enter_imm8_size_ok:
	cmp $'(, %al
	jne invalid_operand
	call get_byte_value
	mov %al, %dl
	pop %bx
	mov $0xc8, %al
	stosb %al, %es:(%edi)
	mov %bx, %ax
	stosw %ax, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
sh_instruction:
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je sh_reg
	cmp $'[, %al
	jne invalid_operand
      sh_mem:
	call get_address
	push %edx
	push %bx
	push %cx
	mov operand_size, %al
	push %ax
	movb $0, operand_size
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	je sh_mem_imm
	cmp $0x10, %al
	jne invalid_operand
      sh_mem_reg:
	lodsb %ds:(%esi), %al
	cmp $0x11, %al
	jne invalid_operand
	pop %ax
	pop %cx
	pop %bx
	pop %edx
	cmp $1, %al
	je sh_mem_cl_8bit
	cmp $2, %al
	je sh_mem_cl_16bit
	cmp $4, %al
	je sh_mem_cl_32bit
	or %ah, %ah
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
	mov operand_size, %al
	or %al, %al
	jz sh_mem_imm_size_ok
	cmp $1, %al
	jne invalid_operand_size
      sh_mem_imm_size_ok:
	call get_byte_value
	.byte 0xa2  # movb %al, value
	.long value
	pop %ax
	pop %cx
	pop %bx
	pop %edx
	cmp $1, %al
	je sh_mem_imm_8bit
	cmp $2, %al
	je sh_mem_imm_16bit
	cmp $4, %al
	je sh_mem_imm_32bit
	or %al, %al
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
	mov value, %al
	stosb %al, %es:(%edi)
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
	mov value, %al
	stosb %al, %es:(%edi)
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
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      sh_mem_1_32bit:
	movb $0xd1, base_code
	call operand_32bit_prefix
	call store_instruction
	jmp instruction_assembled
      sh_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	shlb $3, postbyte_register
	or $192, %al
	orb %al, postbyte_register
	mov %ah, %al
	push %ax
	movb $0, operand_size
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	je sh_reg_imm
	cmp $0x10, %al
	jne invalid_operand
      sh_reg_reg:
	lodsb %ds:(%esi), %al
	cmp $0x11, %al
	jne invalid_operand
	pop %ax
	mov postbyte_register, %bl
	cmp $1, %al
	je sh_reg_cl_8bit
	cmp $2, %al
	je sh_reg_cl_16bit
	cmp $4, %al
	je sh_reg_cl_32bit
	jmp invalid_operand_size
      sh_reg_cl_8bit:
	mov $0xd2, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      sh_reg_cl_16bit:
	call operand_16bit_prefix
	mov $0xd3, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      sh_reg_cl_32bit:
	call operand_32bit_prefix
	mov $0xd3, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      sh_reg_imm:
	mov operand_size, %al
	or %al, %al
	jz sh_reg_imm_size_ok
	cmp $1, %al
	jne invalid_operand_size
      sh_reg_imm_size_ok:
	call get_byte_value
	.byte 0xa2  # movb %al, value
	.long value
	pop %ax
	mov postbyte_register, %bl
	cmp $1, %al
	je sh_reg_imm_8bit
	cmp $2, %al
	je sh_reg_imm_16bit
	cmp $4, %al
	je sh_reg_imm_32bit
	jmp invalid_operand_size
      sh_reg_imm_8bit:
	cmpb $1, value
	je sh_reg_1_8bit
	mov $0xc0, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	mov value, %ah
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      sh_reg_1_8bit:
	mov $0xd0, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      sh_reg_imm_16bit:
	cmpb $1, value
	je sh_reg_1_16bit
	call operand_16bit_prefix
	mov $0xc1, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	mov value, %ah
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      sh_reg_1_16bit:
	call operand_16bit_prefix
	mov $0xd1, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      sh_reg_imm_32bit:
	cmpb $1, value
	je sh_reg_1_32bit
	call operand_32bit_prefix
	mov $0xc1, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	mov value, %ah
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      sh_reg_1_32bit:
	call operand_32bit_prefix
	mov $0xd1, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
shd_instruction:
	movb $0xf, base_code
	.byte 0xa2  # movb %al, extended_code
	.long extended_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je shd_reg
	cmp $'[, %al
	jne invalid_operand
      shd_mem:
	call get_address
	push %edx
	push %bx
	push %cx
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	xor %al, %al
	xchg operand_size, %al
	push %ax
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	je shd_mem_reg_imm
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $0x11, %al
	jne invalid_operand
	pop %ax
	pop %cx
	pop %bx
	pop %edx
	cmp $2, %al
	je shd_mem_reg_cl_16bit
	cmp $4, %al
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
	mov operand_size, %al
	or %al, %al
	jz shd_mem_reg_imm_size_ok
	cmp $1, %al
	jne invalid_operand_size
      shd_mem_reg_imm_size_ok:
	call get_byte_value
	.byte 0xa2  # movb %al, value
	.long value
	pop %ax
	pop %cx
	pop %bx
	pop %edx
	cmp $2, %al
	je shd_mem_reg_imm_16bit
	cmp $4, %al
	je shd_mem_reg_imm_32bit
	jmp invalid_operand_size
      shd_mem_reg_imm_16bit:
	call operand_16bit_prefix
	call store_instruction
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      shd_mem_reg_imm_32bit:
	call operand_32bit_prefix
	call store_instruction
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      shd_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	mov postbyte_register, %bl
	shl $3, %al
	or %al, %bl
	or $192, %bl
	mov %ah, %al
	push %ax
	push %bx
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	movb $0, operand_size
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	je shd_reg_reg_imm
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $0x11, %al
	jne invalid_operand
	pop %bx
	pop %ax
	cmp $2, %al
	je shd_reg_reg_cl_16bit
	cmp $4, %al
	je shd_reg_reg_cl_32bit
	jmp invalid_operand_size
      shd_reg_reg_cl_16bit:
	call operand_16bit_prefix
	jmp shd_reg_reg_cl_store
      shd_reg_reg_cl_32bit:
	call operand_32bit_prefix
      shd_reg_reg_cl_store:
	mov extended_code, %ah
	inc %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      shd_reg_reg_imm:
	mov operand_size, %al
	or %al, %al
	jz shd_reg_reg_imm_size_ok
	cmp $1, %al
	jne invalid_operand_size
      shd_reg_reg_imm_size_ok:
	call get_byte_value
	.byte 0xa2  # movb %al, value
	.long value
	pop %bx
	pop %ax
	cmp $2, %al
	je shd_reg_reg_imm_16bit
	cmp $4, %al
	je shd_reg_reg_imm_32bit
	jmp invalid_operand_size
      shd_reg_reg_imm_16bit:
	call operand_16bit_prefix
	jmp shd_reg_reg_imm_store
      shd_reg_reg_imm_32bit:
	call operand_32bit_prefix
      shd_reg_reg_imm_store:
	mov extended_code, %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
movx_instruction:
	movb $0xf, base_code
	.byte 0xa2  # movb %al, extended_code
	.long extended_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	mov %ah, %al
	cmp $2, %al
	je movx_16bit
	cmp $4, %al
	je movx_32bit
	jmp invalid_operand_size
      movx_16bit:
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	movb $0, operand_size
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je movx_16bit_reg
	cmp $'[, %al
	jne invalid_operand
	call get_address
	mov operand_size, %al
	cmp $1, %al
	je movx_16bit_mem_8bit
	or %al, %al
	jnz invalid_operand_size
      movx_16bit_mem_8bit:
	call operand_16bit_prefix
	call store_instruction
	jmp instruction_assembled
      movx_16bit_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	mov postbyte_register, %bl
	shl $3, %bl
	or %al, %bl
	or $192, %bl
	cmp $1, %ah
	jne invalid_operand_size
	call operand_16bit_prefix
	mov $0xf, %al
	stosb %al, %es:(%edi)
	mov extended_code, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      movx_32bit:
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	movb $0, operand_size
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je movx_32bit_reg
	cmp $'[, %al
	jne invalid_operand
	call get_address
	mov operand_size, %al
	cmp $1, %al
	je movx_32bit_mem_8bit
	cmp $2, %al
	je movx_32bit_mem_16bit
	or %al, %al
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
	lodsb %ds:(%esi), %al
	call convert_register
	mov postbyte_register, %bl
	shl $3, %bl
	or %al, %bl
	or $192, %bl
	mov %ah, %al
	cmp $1, %al
	je movx_32bit_reg_8bit
	cmp $2, %al
	je movx_32bit_reg_16bit
	jmp invalid_operand_size
      movx_32bit_reg_8bit:
	call operand_32bit_prefix
	mov $0xf, %al
	stosb %al, %es:(%edi)
	mov extended_code, %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      movx_32bit_reg_16bit:
	call operand_32bit_prefix
	mov $0xf, %al
	stosb %al, %es:(%edi)
	mov extended_code, %al
	inc %al
	stosb %al, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
bt_instruction:
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	shl $3, %al
	add $0x83, %al
	.byte 0xa2  # movb %al, extended_code
	.long extended_code
	movb $0xf, base_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je bt_reg
	cmp $'[, %al
	jne invalid_operand
	call get_address
	push %eax
	push %bx
	push %cx
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	cmpb $'(, (%esi)
	je bt_mem_imm
	cmpb $0x11, (%esi)
	jne bt_mem_reg
	cmpb $'(, 2(%esi)
	je bt_mem_imm
      bt_mem_reg:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	pop %cx
	pop %bx
	pop %edx
	mov %ah, %al
	cmp $2, %al
	je bt_mem_reg_16bit
	cmp $4, %al
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
	xor %al, %al
	xchg operand_size, %al
	push %ax
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	jne invalid_operand
	mov operand_size, %al
	or %al, %al
	jz bt_mem_imm_size_ok
	cmp $1, %al
	jne invalid_operand_size
      bt_mem_imm_size_ok:
	movb $0xba, extended_code
	call get_byte_value
	.byte 0xa2  # movb %al, value
	.long value
	pop %ax
	cmp $2, %al
	je bt_mem_imm_16bit
	cmp $4, %al
	je bt_mem_imm_32bit
	or %al, %al
	jnz invalid_operand_size
	cmpb $0, current_pass
	jne operand_size_not_specified
	cmpb $0, next_pass_needed
	je operand_size_not_specified
	jmp bt_mem_imm_32bit
      bt_mem_imm_16bit:
	call operand_16bit_prefix
	pop %cx
	pop %bx
	pop %edx
	call store_instruction
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      bt_mem_imm_32bit:
	call operand_32bit_prefix
	pop %cx
	pop %bx
	pop %edx
	call store_instruction
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      bt_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	cmpb $'(, (%esi)
	je bt_reg_imm
	cmpb $0x11, (%esi)
	jne bt_reg_reg
	cmpb $'(, 2(%esi)
	je bt_reg_imm
      bt_reg_reg:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	mov postbyte_register, %bl
	shl $3, %al
	or %al, %bl
	or $192, %bl
	mov %ah, %al
	cmp $2, %al
	je bt_reg_reg_16bit
	cmp $4, %al
	je bt_reg_reg_32bit
	jmp invalid_operand_size
      bt_reg_reg_16bit:
	call operand_16bit_prefix
	mov extended_code, %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      bt_reg_reg_32bit:
	call operand_32bit_prefix
	mov extended_code, %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      bt_reg_imm:
	xor %al, %al
	xchg operand_size, %al
	push %ax
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	jne invalid_operand
	mov operand_size, %al
	or %al, %al
	jz bt_reg_imm_size_ok
	cmp $1, %al
	jne invalid_operand_size
      bt_reg_imm_size_ok:
	call get_byte_value
	.byte 0xa2  # movb %al, value
	.long value
	pop %ax
	cmp $2, %al
	je bt_reg_imm_16bit
	cmp $4, %al
	je bt_reg_imm_32bit
	or %al, %al
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
	.byte 0x66, 0xb8  # mov $0xba0f, %ax
	.word 0xba0f
	stosw %ax, %es:(%edi)
	mov $192, %al
	or postbyte_register, %al
	mov extended_code, %ah
	sub $0x83, %ah
	or %ah, %al
	stosb %al, %es:(%edi)
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
bs_instruction:
	.byte 0xa2  # movb %al, extended_code
	.long extended_code
	movb $0xf, base_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $0x10, %al
	je bs_reg_reg
	cmp $'[, %al
	jne invalid_argument
	call get_address
	mov operand_size, %al
	cmp $2, %al
	je bs_reg_mem_16bit
	cmp $4, %al
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
	lodsb %ds:(%esi), %al
	call convert_register
	mov postbyte_register, %bl
	shl $3, %bl
	or %al, %bl
	or $192, %bl
	mov %ah, %al
	cmp $2, %al
	je bs_reg_reg_16bit
	cmp $4, %al
	je bs_reg_reg_32bit
	jmp invalid_operand_size
      bs_reg_reg_16bit:
	call operand_16bit_prefix
	jmp bs_reg_reg_store
      bs_reg_reg_32bit:
	call operand_32bit_prefix
      bs_reg_reg_store:
	mov extended_code, %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
imul_instruction:
	movb $0xf6, base_code
	movb $5, postbyte_register
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je imul_reg
	cmp $'[, %al
	jne invalid_operand
      imul_mem:
	call get_address
	mov operand_size, %al
	cmp $1, %al
	je imul_mem_8bit
	cmp $2, %al
	je imul_mem_16bit
	cmp $4, %al
	je imul_mem_32bit
	or %al, %al
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
	lodsb %ds:(%esi), %al
	call convert_register
	cmpb $44, (%esi)
	je imul_reg_
	mov postbyte_register, %bl
	shl $3, %bl
	or %al, %bl
	or $192, %bl
	cmp $1, %ah
	je imul_reg_8bit
	cmp $2, %ah
	je imul_reg_16bit
	cmp $4, %ah
	je imul_reg_32bit
	jmp invalid_operand_size
      imul_reg_8bit:
	mov %bl, %ah
	mov $0xf6, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      imul_reg_16bit:
	call operand_16bit_prefix
	mov %bl, %ah
	mov $0xf7, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      imul_reg_32bit:
	call operand_32bit_prefix
	mov %bl, %ah
	mov $0xf7, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      imul_reg_:
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	inc %esi
	cmpb $'(, (%esi)
	je imul_reg_imm
	cmpb $0x11, (%esi)
	jne imul_reg__
	cmpb $'(, 2(%esi)
	je imul_reg_imm
      imul_reg__:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je imul_reg_reg
	cmp $'[, %al
	je imul_reg_mem
	jne invalid_operand
      imul_reg_mem:
	call get_address
	push %edx
	push %bx
	push %cx
	cmpb $44, (%esi)
	je imul_reg_mem_imm
	mov operand_size, %al
	cmp $2, %al
	je imul_reg_mem_16bit
	cmp $4, %al
	je imul_reg_mem_32bit
	jmp invalid_operand_size
      imul_reg_mem_16bit:
	call operand_16bit_prefix
	jmp imul_reg_mem_store
      imul_reg_mem_32bit:
	call operand_32bit_prefix
      imul_reg_mem_store:
	pop %cx
	pop %bx
	pop %edx
	movb $0xf, base_code
	movb $0xaf, extended_code
	call store_instruction
	jmp instruction_assembled
      imul_reg_mem_imm:
	inc %esi
	xor %cl, %cl
	xchg operand_size, %cl
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	jne invalid_operand
	mov operand_size, %al
	movb %cl, operand_size
	cmp $1, %al
	je imul_reg_mem_imm_8bit
	cmp $2, %al
	je imul_reg_mem_imm_16bit
	cmp $4, %al
	je imul_reg_mem_imm_32bit
	or %al, %al
	jnz invalid_operand_size
	cmp $2, %cl
	je imul_reg_mem_imm_16bit
	cmp $4, %cl
	je imul_reg_mem_imm_32bit
	jmp invalid_operand_size
      imul_reg_mem_imm_8bit:
	call get_byte_value
	.byte 0xa2  # movb %al, value
	.long value
	pop %cx
	pop %bx
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
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      imul_reg_mem_32bit_imm_8bit:
	call operand_32bit_prefix
	call store_instruction
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      imul_reg_mem_imm_16bit:
	call get_word_value
	movw %ax, value
	pop %cx
	pop %bx
	pop %edx
	movb $0x69, base_code
	cmpb $2, operand_size
	jne invalid_operand_size
	call operand_16bit_prefix
	call store_instruction
	mov value, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      imul_reg_mem_imm_32bit:
	call get_dword_value
	movl %eax, value
	pop %cx
	pop %bx
	pop %edx
	movb $0x69, base_code
	cmpb $4, operand_size
	jne invalid_operand_size
	call operand_32bit_prefix
	call store_instruction
	mov value, %eax
	call mark_relocation
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
      imul_reg_imm:
	mov postbyte_register, %dl
	mov %dl, %bl
	dec %esi
	jmp imul_reg_reg_imm
      imul_reg_reg:
	lodsb %ds:(%esi), %al
	call convert_register
	mov postbyte_register, %bl
	mov %al, %dl
	cmpb $44, (%esi)
	je imul_reg_reg_imm
	mov %ah, %al
	cmp $2, %al
	je imul_reg_reg_16bit
	cmp $4, %al
	je imul_reg_reg_32bit
	jmp invalid_operand_size
      imul_reg_reg_16bit:
	call operand_16bit_prefix
	jmp imul_reg_reg_store
      imul_reg_reg_32bit:
	call operand_32bit_prefix
      imul_reg_reg_store:
	.byte 0x66, 0xb8  # mov $0xaf0f, %ax
	.word 0xaf0f
	stosw %ax, %es:(%edi)
	mov %dl, %al
	shl $3, %bl
	or %bl, %al
	or $192, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      imul_reg_reg_imm:
	inc %esi
	xor %cl, %cl
	xchg operand_size, %cl
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	jne invalid_operand
	mov operand_size, %al
	movb %cl, operand_size
	cmp $1, %al
	je imul_reg_reg_imm_8bit
	cmp $2, %al
	je imul_reg_reg_imm_16bit
	cmp $4, %al
	je imul_reg_reg_imm_32bit
	or %al, %al
	jnz invalid_operand_size
	cmp $2, %cl
	je imul_reg_reg_imm_16bit
	cmp $4, %cl
	je imul_reg_reg_imm_32bit
	jmp invalid_operand_size
      imul_reg_reg_imm_8bit:
	push %bx
	push %dx
	call get_byte_value
	pop %dx
	pop %bx
      imul_reg_reg_imm_8bit_store:
	.byte 0xa2  # movb %al, value
	.long value
	cmpb $2, operand_size
	je imul_reg_reg_16bit_imm_8bit
	cmpb $4, operand_size
	je imul_reg_reg_32bit_imm_8bit
	jmp invalid_operand_size
      imul_reg_reg_16bit_imm_8bit:
	call operand_16bit_prefix
	mov $0x6b, %al
	stosb %al, %es:(%edi)
	mov %dl, %al
	shl $3, %bl
	or %bl, %al
	or $192, %al
	stosb %al, %es:(%edi)
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      imul_reg_reg_32bit_imm_8bit:
	call operand_32bit_prefix
	mov $0x6b, %al
	stosb %al, %es:(%edi)
	mov %dl, %al
	shl $3, %bl
	or %bl, %al
	or $192, %al
	stosb %al, %es:(%edi)
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      imul_reg_reg_imm_16bit:
	push %bx
	push %dx
	call get_word_value
	pop %dx
	pop %bx
	cmpb $0, value_type
	jne imul_reg_reg_imm_16bit_forced
	.byte 0x66, 0x3d  # cmp $-0x80, %ax
	.word -0x80
	jl imul_reg_reg_imm_16bit_forced
	.byte 0x66, 0x3d  # cmp $0x80, %ax
	.word 0x80
	jl imul_reg_reg_imm_8bit_store
      imul_reg_reg_imm_16bit_forced:
	movw %ax, value
	call operand_16bit_prefix
	mov $0x69, %al
	stosb %al, %es:(%edi)
	mov %dl, %al
	shl $3, %bl
	or %bl, %al
	or $192, %al
	stosb %al, %es:(%edi)
	mov value, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      imul_reg_reg_imm_32bit:
	push %bx
	push %dx
	call get_dword_value
	pop %dx
	pop %bx
	cmpb $0, value_type
	jne imul_reg_reg_imm_32bit_forced
	.byte 0x66, 0x3d  # cmp $-0x80, %ax
	.word -0x80
	jl imul_reg_reg_imm_32bit_forced
	.byte 0x66, 0x3d  # cmp $0x80, %ax
	.word 0x80
	jl imul_reg_reg_imm_8bit_store
      imul_reg_reg_imm_32bit_forced:
	movl %eax, value
	call operand_32bit_prefix
	mov $0x69, %al
	stosb %al, %es:(%edi)
	mov %dl, %al
	shl $3, %bl
	or %bl, %al
	or $192, %al
	stosb %al, %es:(%edi)
	mov value, %eax
	call mark_relocation
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
in_instruction:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	or %al, %al
	jnz invalid_operand
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	mov %ah, %al
	push %ax
	movb $0, operand_size
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	je in_imm
	cmp $0x10, %al
	je in_reg
	jmp invalid_operand
      in_reg:
	lodsb %ds:(%esi), %al
	cmp $0x22, %al
	jne invalid_operand
	pop %ax
	cmp $1, %al
	je in_al_dx
	cmp $2, %al
	je in_ax_dx
	cmp $4, %al
	je in_eax_dx
	jmp invalid_operand_size
      in_al_dx:
	mov $0xec, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      in_ax_dx:
	call operand_16bit_prefix
	mov $0xed, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      in_eax_dx:
	call operand_32bit_prefix
	mov $0xed, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      in_imm:
	mov operand_size, %al
	or %al, %al
	jz in_imm_size_ok
	cmp $1, %al
	jne invalid_operand_size
      in_imm_size_ok:
	call get_byte_value
	mov %al, %dl
	pop %ax
	cmp $1, %al
	je in_al_imm
	cmp $2, %al
	je in_ax_imm
	cmp $4, %al
	je in_eax_imm
	jmp invalid_operand_size
      in_al_imm:
	mov $0xe4, %al
	stosb %al, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      in_ax_imm:
	call operand_16bit_prefix
	mov $0xe5, %al
	stosb %al, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      in_eax_imm:
	call operand_32bit_prefix
	mov $0xe5, %al
	stosb %al, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
out_instruction:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'(, %al
	je out_imm
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $0x22, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	movb $0, operand_size
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	or %al, %al
	jnz invalid_operand
	mov %ah, %al
	cmp $1, %al
	je out_dx_al
	cmp $2, %al
	je out_dx_ax
	cmp $4, %al
	je out_dx_eax
	jmp invalid_operand_size
      out_dx_al:
	mov $0xee, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      out_dx_ax:
	call operand_16bit_prefix
	mov $0xef, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      out_dx_eax:
	call operand_32bit_prefix
	mov $0xef, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      out_imm:
	mov operand_size, %al
	or %al, %al
	jz out_imm_size_ok
	cmp $1, %al
	jne invalid_operand_size
      out_imm_size_ok:
	call get_byte_value
	.byte 0xa2  # movb %al, value
	.long value
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	movb $0, operand_size
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	or %al, %al
	jnz invalid_operand
	mov %ah, %al
	cmp $1, %al
	je out_imm_al
	cmp $2, %al
	je out_imm_ax
	cmp $4, %al
	je out_imm_eax
	jmp invalid_operand_size
      out_imm_al:
	mov $0xe6, %al
	stosb %al, %es:(%edi)
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      out_imm_ax:
	call operand_16bit_prefix
	mov $0xe7, %al
	stosb %al, %es:(%edi)
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
      out_imm_eax:
	call operand_32bit_prefix
	mov $0xe7, %al
	stosb %al, %es:(%edi)
	mov value, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
lar_instruction:
	.byte 0xa2  # movb %al, extended_code
	.long extended_code
	movb $0xf, base_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je lar_reg_reg
	cmp $'[, %al
	jne invalid_operand
	call get_address
	mov operand_size, %al
	cmp $2, %al
	je lar_16bit
	cmp $4, %al
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
	lodsb %ds:(%esi), %al
	call convert_register
	mov postbyte_register, %bl
	shl $3, %bl
	or %al, %bl
	or $192, %bl
	mov %ah, %al
	cmp $2, %al
	je lar_reg_reg_16bit
	cmp $4, %al
	je lar_reg_reg_32bit
	jmp invalid_operand_size
      lar_reg_reg_32bit:
	call operand_32bit_prefix
	jmp lar_reg_reg_store
      lar_reg_reg_16bit:
	call operand_16bit_prefix
      lar_reg_reg_store:
	mov $0xf, %al
	mov extended_code, %ah
	stosw %ax, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
invlpg_instruction:
	movb $0xf, base_code
	movb $1, extended_code
	movb $7, postbyte_register
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	call store_instruction
	jmp instruction_assembled
basic_486_instruction:
	movb $0xf, base_code
	.byte 0xa2  # movb %al, extended_code
	.long extended_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	je basic_486_reg
	cmp $'[, %al
	jne invalid_operand
	call get_address
	push %edx
	push %bx
	push %cx
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	pop %cx
	pop %bx
	pop %edx
	mov %ah, %al
	cmp $1, %al
	je basic_486_mem_reg_8bit
	cmp $2, %al
	je basic_486_mem_reg_16bit
	cmp $4, %al
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
	lodsb %ds:(%esi), %al
	call convert_register
	.byte 0xa2  # movb %al, postbyte_register
	.long postbyte_register
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	mov postbyte_register, %bl
	shl $3, %al
	or %al, %bl
	or $192, %bl
	mov %ah, %al
	cmp $1, %al
	je basic_486_reg_reg_8bit
	cmp $2, %al
	je basic_486_reg_reg_16bit
	cmp $4, %al
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
	mov $0xf, %al
	mov extended_code, %ah
	stosw %ax, %es:(%edi)
	mov %bl, %al
	stosb %al, %es:(%edi)
	jmp instruction_assembled
bswap_instruction:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call convert_register
	mov %al, %ah
	add $0xc8, %ah
	cmp $4, %ah
	jne invalid_operand_size
	call operand_32bit_prefix
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
conditional_jump:
	.byte 0xa2  # movb %al, base_code
	.long base_code
	lodsb %ds:(%esi), %al
	call get_jump_operator
	cmpb $2, jump_type
	je invalid_operand
	call get_size_operator
	cmp $'(, %al
	jne invalid_operand
	cmpb $'., (%esi)
	je invalid_value
	call get_dword_value
	cmpb $1, value_type
	je invalid_use_of_symbol
	sub %edi, %eax
	add org_start, %eax
	sub $2, %eax
	cmpl $0, org_sib
	jne invalid_use_of_symbol
	mov operand_size, %bl
	cmp $1, %bl
	je conditional_jump_8bit
	cmp $2, %bl
	je conditional_jump_16bit
	cmp $4, %bl
	je conditional_jump_32bit
	or %bl, %bl
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
	mov base_code, %ah
	add $0x10, %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	mov %edx, %eax
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
      conditional_jump_16bit:
	sub $2, %eax
	mov %eax, %edx
	mov %edi, %ecx
	call operand_16bit_prefix
	sub %edi, %edx
	add %ecx, %edx
	mov base_code, %ah
	add $0x10, %ah
	mov $0xf, %al
	stosw %ax, %es:(%edi)
	mov %edx, %eax
	stosw %ax, %es:(%edi)
	cmp $0x10000, %eax
	jge jump_out_of_range
	cmp $-0x10000, %eax
	jl jump_out_of_range
	jmp instruction_assembled
      conditional_jump_8bit:
	mov %eax, %edx
	mov %al, %ah
	mov base_code, %al
	stosw %ax, %es:(%edi)
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
	mov %al, %cl
	call address_16bit_prefix
	mov %cl, %al
	jmp loop_instruction
loop_instruction_32bit:
	mov %al, %cl
	call address_32bit_prefix
	mov %cl, %al
loop_instruction:
	.byte 0xa2  # movb %al, base_code
	.long base_code
	lodsb %ds:(%esi), %al
	call get_jump_operator
	cmpb $2, jump_type
	je invalid_operand
	call get_size_operator
	cmp $'(, %al
	jne invalid_operand
	cmpb $'., (%esi)
	je invalid_value
	call get_dword_value
	cmpb $1, value_type
	je invalid_use_of_symbol
	sub %edi, %eax
	add org_start, %eax
	cmpl $0, org_sib
	jne invalid_use_of_symbol
	mov operand_size, %bl
	cmp $1, %bl
	je loop_8bit
	or %bl, %bl
	jnz invalid_operand_size
      loop_8bit:
	sub $2, %eax
	mov %eax, %edx
	mov base_code, %al
	stosb %al, %es:(%edi)
	mov %edx, %eax
	stosb %al, %es:(%edi)
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
	lodsb %ds:(%esi), %al
	call get_jump_operator
	call get_size_operator
	cmp $0x10, %al
	je jmp_reg
	cmp $'(, %al
	je jmp_imm
	cmp $'[, %al
	jne invalid_operand
      jmp_mem:
	call get_address
	movb $0xff, base_code
	mov %eax, %edx
	mov operand_size, %al
	or %al, %al
	jz jmp_mem_size_not_specified
	cmp $2, %al
	je jmp_mem_16bit
	cmp $4, %al
	je jmp_mem_32bit
	cmp $6, %al
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
	lodsb %ds:(%esi), %al
	call convert_register
	mov %al, %bl
	or $192, %bl
	mov %ah, %al
	cmp $2, %al
	je jmp_reg_16bit
	cmp $4, %al
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
	mov postbyte_register, %al
	shl $3, %al
	or %al, %bl
	mov %bl, %ah
	mov $0xff, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      jmp_reg_far32bit:
	call operand_32bit_prefix
	mov postbyte_register, %al
	inc %al
	shl $3, %al
	or %al, %bl
	mov %bl, %ah
	mov $0xff, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      jmp_reg_16bit:
	cmpb $2, jump_type
	je invalid_operand_size
	call operand_16bit_prefix
	mov postbyte_register, %al
	shl $3, %al
	or %al, %bl
	mov %bl, %ah
	mov $0xff, %al
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      jmp_imm:
	push %esi
	cmpb $'., (%esi)
	je invalid_value
	call get_dword_value
	pop %ebx
	cmpb $':, (%esi)
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
	mov operand_size, %bl
	cmp $1, %bl
	je jmp_8bit
	cmp $2, %bl
	je jmp_16bit
	cmp $4, %bl
	je jmp_32bit
	or %bl, %bl
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
	mov base_code, %al
	stosb %al, %es:(%edi)
	mov %edx, %eax
	stosw %ax, %es:(%edi)
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
	mov base_code, %al
	stosb %al, %es:(%edi)
	mov %edx, %eax
	stosl %eax, %es:(%edi)
	jmp instruction_assembled
      jmp_8bit:
	cmpb $0xe9, base_code
	jne invalid_operand_size
	mov %eax, %edx
	mov %al, %ah
	mov $0xeb, %al
	stosw %ax, %es:(%edi)
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
	mov %ax, %dx
	mov operand_size, %bl
	cmp $4, %bl
	je jmp_far_16bit
	cmp $6, %bl
	je jmp_far_32bit
	or %bl, %bl
	jnz invalid_operand_size
	cmpb $32, code_type
	je jmp_far_32bit
      jmp_far_16bit:
	inc %esi
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_operand
	mov value_type, %al
	push %ax
	cmpb $'., (%esi)
	je invalid_value
	call get_word_value
	mov %eax, %ebx
	call operand_16bit_prefix
	mov extended_code, %al
	stosb %al, %es:(%edi)
	mov %bx, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	pop %ax
	.byte 0xa2  # movb %al, value_type
	.long value_type
	mov %dx, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
      jmp_far_32bit:
	inc %esi
	lodsb %ds:(%esi), %al
	cmp $'(, %al
	jne invalid_operand
	mov value_type, %al
	push %ax
	cmpb $'., (%esi)
	je invalid_value
	call get_dword_value
	mov %eax, %ebx
	call operand_32bit_prefix
	mov extended_code, %al
	stosb %al, %es:(%edi)
	mov %ebx, %eax
	call mark_relocation
	stosl %eax, %es:(%edi)
	pop %ax
	.byte 0xa2  # movb %al, value_type
	.long value_type
	mov %dx, %ax
	call mark_relocation
	stosw %ax, %es:(%edi)
	jmp instruction_assembled
ins_instruction:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	or %ch, %bl
	jnz invalid_address
	cmp $0x27, %bh
	je ins_16bit
	cmp $0x47, %bh
	jne invalid_address
	call address_32bit_prefix
	jmp ins_store
      ins_16bit:
	call address_16bit_prefix
      ins_store:
	cmpb $1, segment_register
	ja invalid_address
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $0x22, %al
	jne invalid_operand
	mov $0x6c, %al
	cmpb $1, operand_size
	je simple_instruction
	inc %al
	cmpb $2, operand_size
	je simple_instruction_16bit
	cmpb $4, operand_size
	je simple_instruction_32bit
	cmpb $0, operand_size
	je operand_size_not_specified
	jmp invalid_operand_size
outs_instruction:
	lodsb %ds:(%esi), %al
	cmp $0x10, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $0x22, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	or %ch, %bl
	jnz invalid_address
	cmp $0x26, %bh
	je outs_16bit
	cmp $0x46, %bh
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
	mov $0x6e, %al
	cmpb $1, operand_size
	je simple_instruction
	inc %al
	cmpb $2, operand_size
	je simple_instruction_16bit
	cmpb $4, operand_size
	je simple_instruction_32bit
	cmpb $0, operand_size
	je operand_size_not_specified
	jmp invalid_operand_size
movs_instruction:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	or %ch, %bl
	jnz invalid_address
	cmpb $1, segment_register
	ja invalid_address
	push %bx
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	pop %dx
	or %eax, %eax
	jnz invalid_address
	or %ch, %bl
	jnz invalid_address
	mov %dh, %al
	mov %bh, %ah
	shr $4, %al
	shr $4, %ah
	cmp %ah, %al
	jne address_sizes_do_not_agree
	and $7, %bh
	and $7, %dh
	cmp $6, %bh
	jne invalid_address
	cmp $7, %dh
	jne invalid_address
	cmp $2, %al
	je movs_16bit
	cmp $4, %al
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
	mov $0xa4, %al
	mov operand_size, %bl
	cmp $1, %bl
	je simple_instruction
	inc %al
	cmp $2, %bl
	je simple_instruction_16bit
	cmp $4, %bl
	je simple_instruction_32bit
	or %bl, %bl
	jz operand_size_not_specified
	jmp invalid_operand_size
lods_instruction:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	or %ch, %bl
	jnz invalid_address
	cmp $0x26, %bh
	je lods_16bit
	cmp $0x46, %bh
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
	mov $0xac, %al
	cmpb $1, operand_size
	je simple_instruction
	inc %al
	cmpb $2, operand_size
	je simple_instruction_16bit
	cmpb $4, operand_size
	je simple_instruction_32bit
	cmpb $0, operand_size
	je operand_size_not_specified
	jmp invalid_operand_size
stos_instruction:
	.byte 0xa2  # movb %al, base_code
	.long base_code
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	or %ch, %bl
	jnz invalid_address
	cmp $0x27, %bh
	je stos_16bit
	cmp $0x47, %bh
	jne invalid_address
	call address_32bit_prefix
	jmp stos_store
      stos_16bit:
	call address_16bit_prefix
      stos_store:
	cmpb $1, segment_register
	ja invalid_address
	mov base_code, %al
	cmpb $1, operand_size
	je simple_instruction
	inc %al
	cmpb $2, operand_size
	je simple_instruction_16bit
	cmpb $4, operand_size
	je simple_instruction_32bit
	cmpb $0, operand_size
	je operand_size_not_specified
	jmp invalid_operand_size
cmps_instruction:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	or %ch, %bl
	jnz invalid_address
	mov segment_register, %al
	push %ax
	push %bx
	lodsb %ds:(%esi), %al
	cmp $44, %al
	jne invalid_operand
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	or %ch, %bl
	jnz invalid_address
	pop %dx
	pop %ax
	cmpb $1, segment_register
	ja invalid_address
	.byte 0xa2  # movb %al, segment_register
	.long segment_register
	mov %dh, %al
	mov %bh, %ah
	shr $4, %al
	shr $4, %ah
	cmp %ah, %al
	jne address_sizes_do_not_agree
	and $7, %bh
	and $7, %dh
	cmp $7, %bh
	jne invalid_address
	cmp $6, %dh
	jne invalid_address
	cmp $2, %al
	je cmps_16bit
	cmp $4, %al
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
	mov $0xa6, %al
	mov operand_size, %bl
	cmp $1, %bl
	je simple_instruction
	inc %al
	cmp $2, %bl
	je simple_instruction_16bit
	cmp $4, %bl
	je simple_instruction_32bit
	or %bl, %bl
	jz operand_size_not_specified
	jmp invalid_operand_size
xlat_instruction:
	lodsb %ds:(%esi), %al
	call get_size_operator
	cmp $'[, %al
	jne invalid_operand
	call get_address
	or %eax, %eax
	jnz invalid_address
	or %ch, %bl
	jnz invalid_address
	cmp $0x23, %bh
	je xlat_16bit
	cmp $0x43, %bh
	jne invalid_address
	call address_32bit_prefix
	jmp xlat_store
      xlat_16bit:
	call address_16bit_prefix
      xlat_store:
	call store_segment_prefix_if_necessary
	mov $0xd7, %al
	cmpb $1, operand_size
	jbe simple_instruction
	jmp invalid_operand_size
cmpsd_instruction:
	mov $0xa7, %al
	mov (%esi), %ah
	or %ah, %ah
	jmp simple_instruction_32bit
movsd_instruction:
	mov $0xa5, %al
	mov (%esi), %ah
	or %ah, %ah
	jmp simple_instruction_32bit
convert_register:
	mov %al, %ah
	shr $4, %ah
	and $7, %al
	cmp $4, %ah
	ja invalid_operand
      match_register_size:
	cmp operand_size, %ah
	je register_size_ok
	cmpb $0, operand_size
	jne operand_sizes_do_not_match
	movb %ah, operand_size
      register_size_ok:
	ret
get_size_operator:
	xor %ah, %ah
	cmp $0x11, %al
	jne operand_size_ok
	lodsw %ds:(%esi), %ax
	xchg %ah, %al
	movb $1, forced_size
	cmp operand_size, %ah
	je forced_ok
	cmpb $0, operand_size
	jne operand_sizes_do_not_match
	movb %ah, operand_size
      forced_ok:
	ret
      operand_size_ok:
	cmp $'[, %al
	jne forced_ok
	movb $0, forced_size
	ret
get_jump_operator:
	movb $0, jump_type
	cmp $0x12, %al
	jne jump_operator_ok
	lodsw %ds:(%esi), %ax
	.byte 0xa2  # movb %al, jump_type
	.long jump_type
	mov %ah, %al
      jump_operator_ok:
	ret
operand_16bit_prefix:
	cmpb $16, code_type
	je size_prefix_ok
	mov $0x66, %al
	stosb %al, %es:(%edi)
	ret
operand_32bit_prefix:
	cmpb $32, code_type
	je size_prefix_ok
	mov $0x66, %al
	stosb %al, %es:(%edi)
      size_prefix_ok:
	ret
store_segment_prefix_if_necessary:
	mov segment_register, %al
	or %al, %al
	jz segment_prefix_ok
	cmp $3, %al
	je ss_prefix
	cmp $4, %al
	ja segment_prefix_386
	jb segment_prefix
	cmp $0x25, %bh
	je segment_prefix
	cmp $0x45, %bh
	je segment_prefix
	cmp $0x44, %bh
	je segment_prefix
	ret
      ss_prefix:
	cmp $0x25, %bh
	je segment_prefix_ok
	cmp $0x45, %bh
	je segment_prefix_ok
	cmp $0x44, %bh
	je segment_prefix_ok
	jmp segment_prefix
store_segment_prefix:
	mov segment_register, %al
	or %al, %al
	jz segment_prefix_ok
	cmp $5, %al
	jae segment_prefix_386
      segment_prefix:
	dec %al
	shl $3, %al
	add $0x26, %al
	stosb %al, %es:(%edi)
	jmp segment_prefix_ok
      segment_prefix_386:
	add $0x64-5, %al
	stosb %al, %es:(%edi)
      segment_prefix_ok:
	ret
store_instruction:
	call store_segment_prefix_if_necessary
      store_instruction_main:
	or %bx, %bx
	jz address_immediate
	mov %bl, %al
	or %bh, %al
	and $240, %al
	cmp $0x40, %al
	je postbyte_32bit
	call address_16bit_prefix
	call store_instruction_code
	.byte 0x66, 0x81, 0xfb  # cmp $0x2326, %bx
	.word 0x2326
	je address_bx_si
	.byte 0x66, 0x81, 0xfb  # cmp $0x2623, %bx
	.word 0x2623
	je address_bx_si
	.byte 0x66, 0x81, 0xfb  # cmp $0x2327, %bx
	.word 0x2327
	je address_bx_di
	.byte 0x66, 0x81, 0xfb  # cmp $0x2723, %bx
	.word 0x2723
	je address_bx_di
	.byte 0x66, 0x81, 0xfb  # cmp $0x2526, %bx
	.word 0x2526
	je address_bp_si
	.byte 0x66, 0x81, 0xfb  # cmp $0x2625, %bx
	.word 0x2625
	je address_bp_si
	.byte 0x66, 0x81, 0xfb  # cmp $0x2527, %bx
	.word 0x2527
	je address_bp_di
	.byte 0x66, 0x81, 0xfb  # cmp $0x2725, %bx
	.word 0x2725
	je address_bp_di
	.byte 0x66, 0x81, 0xfb  # cmp $0x2600, %bx
	.word 0x2600
	je address_si
	.byte 0x66, 0x81, 0xfb  # cmp $0x2700, %bx
	.word 0x2700
	je address_di
	.byte 0x66, 0x81, 0xfb  # cmp $0x2300, %bx
	.word 0x2300
	je address_bx
	.byte 0x66, 0x81, 0xfb  # cmp $0x2500, %bx
	.word 0x2500
	je address_bp
	jmp invalid_address
      address_bx_si:
	xor %al, %al
	jmp postbyte_16bit
      address_bx_di:
	mov $1, %al
	jmp postbyte_16bit
      address_bp_si:
	mov $2, %al
	jmp postbyte_16bit
      address_bp_di:
	mov $3, %al
	jmp postbyte_16bit
      address_si:
	mov $4, %al
	jmp postbyte_16bit
      address_di:
	mov $5, %al
	jmp postbyte_16bit
      address_bx:
	mov $7, %al
	jmp postbyte_16bit
      address_bp:
	mov $6, %al
      postbyte_16bit:
	cmp $1, %ch
	je address_8bit_value
	cmp $2, %ch
	je address_16bit_value
	or %ch, %ch
	jnz address_sizes_do_not_agree
	or %edx, %edx
	jz address
	cmp $0x80, %edx
	jb address_8bit_value
	cmp $-0x80, %edx
	jae address_8bit_value
      address_16bit_value:
	or $128, %al
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosb %al, %es:(%edi)
	mov %edx, %eax
	stosw %ax, %es:(%edi)
	cmp $0x10000, %edx
	jge value_out_of_range
	cmp $-0x8000, %edx
	jl value_out_of_range
	ret
      address_8bit_value:
	or $64, %al
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosb %al, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	cmp $0x80, %edx
	jge value_out_of_range
	cmp $-0x80, %edx
	jl value_out_of_range
	ret
      address:
	cmp $6, %al
	je address_8bit_value
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosb %al, %es:(%edi)
	ret
      postbyte_32bit:
	call address_32bit_prefix
	call store_instruction_code
	cmp $0x44, %bl
	je invalid_address
	or %cl, %cl
	jz only_base_register
      base_and_index:
	mov $4, %al
	xor %ah, %ah
	cmp $1, %cl
	je scale_ok
	cmp $2, %cl
	je scale_1
	cmp $4, %cl
	je scale_2
	or $192, %ah
	jmp scale_ok
      scale_2:
	or $128, %ah
	jmp scale_ok
      scale_1:
	or $64, %ah
      scale_ok:
	or %bh, %bh
	jz only_index_register
	and $7, %bl
	shl $3, %bl
	or %bl, %ah
	and $7, %bh
	or %bh, %ah
	cmp $1, %ch
	je sib_address_8bit_value
	test $4, %ch
	jnz sib_address_32bit_value
	cmp $2, %ch
	je address_sizes_do_not_agree
	cmp $5, %bh
	je address_value
	or %edx, %edx
	jz sib_address
      address_value:
	cmp $0x80, %edx
	jb sib_address_8bit_value
	cmp $-0x80, %edx
	jae sib_address_8bit_value
      sib_address_32bit_value:
	or $128, %al
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosw %ax, %es:(%edi)
	jmp store_address_32bit_value
      sib_address_8bit_value:
	or $64, %al
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosw %ax, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	cmp $0x80, %edx
	jge value_out_of_range
	cmp $-0x80, %edx
	jl value_out_of_range
	ret
      sib_address:
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosw %ax, %es:(%edi)
	ret
      only_index_register:
	or $5, %ah
	and $7, %bl
	shl $3, %bl
	or %bl, %ah
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosw %ax, %es:(%edi)
	test $4, %ch
	jnz store_address_32bit_value
	or %ch, %ch
	jnz invalid_address_size
	jmp store_address_32bit_value
      zero_index_register:
	mov $4, %bl
	mov $1, %cl
	jmp base_and_index
      only_base_register:
	mov %bh, %al
	and $7, %al
	cmp $4, %al
	je zero_index_register
	cmp $1, %ch
	je simple_address_8bit_value
	test $4, %ch
	jnz simple_address_32bit_value
	cmp $2, %ch
	je address_sizes_do_not_agree
	or %edx, %edx
	jz simple_address
	cmp $0x80, %edx
	jb simple_address_8bit_value
	cmp $-0x80, %edx
	jae simple_address_8bit_value
      simple_address_32bit_value:
	or $128, %al
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosb %al, %es:(%edi)
	jmp store_address_32bit_value
      simple_address_8bit_value:
	or $64, %al
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosb %al, %es:(%edi)
	mov %dl, %al
	stosb %al, %es:(%edi)
	cmp $0x80, %edx
	jge value_out_of_range
	cmp $-0x80, %edx
	jl value_out_of_range
	ret
      simple_address:
	cmp $5, %al
	je simple_address_8bit_value
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosb %al, %es:(%edi)
	ret
      address_immediate:
	test $4, %ch
	jnz address_immediate_32bit
	cmp $2, %ch
	je address_immediate_16bit
	or %ch, %ch
	jnz invalid_address_size
	cmpb $16, code_type
	je addressing_16bit
      address_immediate_32bit:
	call address_32bit_prefix
	call store_instruction_code
	mov $5, %al
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosb %al, %es:(%edi)
      store_address_32bit_value:
	test $0x80, %ch
	jz address_relocation_ok
	pushw value_type
	movb $2, value_type
	call mark_relocation
	pop %ax
	.byte 0xa2  # movb %al, value_type
	.long value_type
      address_relocation_ok:
	mov %edx, %eax
	stosl %eax, %es:(%edi)
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
	mov $6, %al
	mov postbyte_register, %cl
	shl $3, %cl
	or %cl, %al
	stosb %al, %es:(%edi)
	mov %edx, %eax
	stosw %ax, %es:(%edi)
	cmp $0x10000, %edx
	jge value_out_of_range
	cmp $-0x8000, %edx
	jl value_out_of_range
	ret
      store_instruction_code:
	mov base_code, %al
	stosb %al, %es:(%edi)
	cmp $0xf, %al
	jne instruction_code_ok
      store_extended_code:
	mov extended_code, %al
	stosb %al, %es:(%edi)
      instruction_code_ok:
	ret
      address_16bit_prefix:
	cmpb $16, code_type
	je instruction_prefix_ok
	mov $0x67, %al
	stosb %al, %es:(%edi)
	ret
      address_32bit_prefix:
	cmpb $32, code_type
	je instruction_prefix_ok
	mov $0x67, %al
	stosb %al, %es:(%edi)
      instruction_prefix_ok:
	ret

# %include '../formats.inc'

#  flat assembler source
#  Copyright (c) 1999-2001, Tomasz Grysztar
#  All rights reserved.

format_directive:
	cmp code_start, %edi
	jne unexpected_instruction
	cmpb $0, output_format
	jne unexpected_instruction
	lodsb %ds:(%esi), %al
	cmp $0x18, %al
	jne invalid_argument
	lodsb %ds:(%esi), %al
	.byte 0xa2  # movb %al, output_format
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

# %include '../tables.inc'

#  flat assembler source
#  Copyright (c) 1999-2001, Tomasz Grysztar
#  All rights reserved.

get_operator:
	push %esi
	push %ebp
	mov $1, %ebp
	cmpb $0x1a, (%esi)
	jne operator_start
	inc %esi
	lodsb %ds:(%esi), %al
	movzbl %al, %ebp
      operator_start:
	mov %esi, %edx
      check_operator:
	mov %edx, %esi
	movzbl (%edi), %ecx
	jecxz no_operator
	inc %edi
	mov %edi, %ebx
	add %ecx, %ebx
	cmp %ebp, %ecx
	jne next_operator
	repe
	cmpsb %es:(%edi), %ds:(%esi)
	je operator_found
      next_operator:
	mov %ebx, %edi
	inc %edi
	jmp check_operator
      no_operator:
	xor %al, %al
	pop %ebp
	pop %esi
	ret
      operator_found:
	pop %ebp
	pop %eax
	mov (%edi), %al
	ret

get_symbol:
	mov %esi, %edx
	mov %ecx, %ebp
      scan_symbols:
	mov %edx, %esi
	movzbl (%edi), %eax
	or %al, %al
	jz no_symbol
	mov %ebp, %ecx
	inc %edi
	mov %edi, %ebx
	add %eax, %ebx
	mov (%esi), %ah
	cmp (%edi), %ah
	jb no_symbol
	ja next_symbol
	cmp %al, %cl
	jne next_symbol
	repe
	cmpsb %es:(%edi), %ds:(%esi)
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
	mov (%ebx), %ax
	clc
	ret

get_instruction:
	mov %esi, %edx
	mov %ecx, %ebp
	cmp $11, %ecx
	ja no_instruction
	sub $2, %cl
	jc no_instruction
	movzwl instructions(,%ecx,2), %edi
	add $instructions, %edi
      scan_instructions:
	mov %edx, %esi
	mov (%edi), %al
	or %al, %al
	jz no_instruction
	mov %ebp, %ecx
	mov %edi, %ebx
	add %ecx, %ebx
	repe
	cmpsb %es:(%edi), %ds:(%esi)
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
	mov (%ebx), %al
	mov 1(%ebx), %bx
	clc
	ret

get_label_id:
	cmp $0x100, %ecx
	jae name_too_long
	cmpb $'., (%esi)
	jne standard_label
	cmpb $'., 1(%esi)
	je standard_label
	cmpl $0, current_locals_prefix
	je standard_label
	push %edi
	push %ecx
	push %esi
	mov additional_memory, %edi
	xor %al, %al
	stosb %al, %es:(%edi)
	mov current_locals_prefix, %esi
	mov %edi, %ebx
	lodsb %ds:(%esi), %al
	movzbl %al, %ecx
	lea (%edi,%ecx,1), %ebp
	cmp additional_memory_end, %ebp
	jae out_of_memory
	rep
	movsb %ds:(%esi), %es:(%edi)
	pop %esi
	pop %ecx
	add %cl, %al
	jc name_too_long
	lea (%edi,%ecx,1), %ebp
	cmp additional_memory_end, %ebp
	jae out_of_memory
	rep
	movsb %ds:(%esi), %es:(%edi)
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
	lodsb %ds:(%esi), %al
	cmp $'$, %al
	je get_current_offset_id
	cmp $'%, %al
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
	inc %bl
	cmp %cl, %bl
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
	repe
	cmpsb %es:(%edi), %ds:(%esi)
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
	mov (%esi), %al
	cmp $0x30, %al
	jb name_first_char_ok
	cmp $0x39, %al
	jbe invalid_name
      name_first_char_ok:
	cmp $1, %ecx
	jne check_for_reserved_word
	cmp $'$, %al
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

symbol_characters:
.byte 25
 .byte 9, 0xa, 0xd, 0x1a, 0x20
 .ascii "+-/*:=|&~()[]<>{},;\\"

operators:
 .byte 1
 .ascii "+"
 .byte 0x80
 .byte 1
 .ascii "-"
 .byte 0x81
 .byte 1
 .ascii "*"
 .byte 0x90
 .byte 1
 .ascii "/"
 .byte 0x91
 .byte 3
 .ascii "mod"
 .byte 0xa0
 .byte 3
 .ascii "and"
 .byte 0xb0
 .byte 2
 .ascii "or"
 .byte 0xb1
 .byte 3
 .ascii "xor"
 .byte 0xb2
 .byte 3
 .ascii "shl"
 .byte 0xc0
 .byte 3
 .ascii "shr"
 .byte 0xc1
 .byte 0

single_operand_operators:
 .byte 3
 .ascii "not"
 .byte 0xd0
 .byte 3
 .ascii "rva"
 .byte 0xe0
 .byte 0

directive_operators:
 .byte 2
 .ascii "at"
 .byte 0x80
 .byte 2
 .ascii "eq"
 .byte 0x81
 .byte 4
 .ascii "from"
 .byte 0x82
 .byte 2
 .ascii "in"
 .byte 0x83
 .byte 2
 .ascii "on"
 .byte 0x84
 .byte 0

address_registers:
 .byte 2
 .ascii "bp"
 .byte 0, 0x25
 .byte 2
 .ascii "bx"
 .byte 0, 0x23
 .byte 2
 .ascii "di"
 .byte 0, 0x27
 .byte 3
 .ascii "eax"
 .byte 0, 0x40
 .byte 3
 .ascii "ebp"
 .byte 0, 0x45
 .byte 3
 .ascii "ebx"
 .byte 0, 0x43
 .byte 3
 .ascii "ecx"
 .byte 0, 0x41
 .byte 3
 .ascii "edi"
 .byte 0, 0x47
 .byte 3
 .ascii "edx"
 .byte 0, 0x42
 .byte 3
 .ascii "esi"
 .byte 0, 0x46
 .byte 3
 .ascii "esp"
 .byte 0, 0x44
 .byte 2
 .ascii "si"
 .byte 0, 0x26
 .byte 0

address_sizes:
 .byte 4
 .ascii "byte"
 .byte 0, 1
 .byte 5
 .ascii "dword"
 .byte 0, 4
 .byte 4
 .ascii "word"
 .byte 0, 2
 .byte 0

symbols:
 .byte 2
 .ascii "ah"
 .byte 0x10, 0x14
 .byte 2
 .ascii "al"
 .byte 0x10, 0x10
 .byte 2
 .ascii "ax"
 .byte 0x10, 0x20
 .byte 2
 .ascii "bh"
 .byte 0x10, 0x17
 .byte 2
 .ascii "bl"
 .byte 0x10, 0x13
 .byte 2
 .ascii "bp"
 .byte 0x10, 0x25
 .byte 2
 .ascii "bx"
 .byte 0x10, 0x23
 .byte 4
 .ascii "byte"
 .byte 0x11, 1
 .byte 2
 .ascii "ch"
 .byte 0x10, 0x15
 .byte 2
 .ascii "cl"
 .byte 0x10, 0x11
 .byte 3
 .ascii "cr0"
 .byte 0x10, 0x50
 .byte 3
 .ascii "cr2"
 .byte 0x10, 0x52
 .byte 3
 .ascii "cr3"
 .byte 0x10, 0x53
 .byte 3
 .ascii "cr4"
 .byte 0x10, 0x54
 .byte 2
 .ascii "cs"
 .byte 0x10, 0x62
 .byte 2
 .ascii "cx"
 .byte 0x10, 0x21
 .byte 2
 .ascii "dh"
 .byte 0x10, 0x16
 .byte 2
 .ascii "di"
 .byte 0x10, 0x27
 .byte 2
 .ascii "dl"
 .byte 0x10, 0x12
 .byte 6
 .ascii "dqword"
 .byte 0x11, 16
 .byte 3
 .ascii "dr0"
 .byte 0x10, 0x70
 .byte 3
 .ascii "dr1"
 .byte 0x10, 0x71
 .byte 3
 .ascii "dr2"
 .byte 0x10, 0x72
 .byte 3
 .ascii "dr3"
 .byte 0x10, 0x73
 .byte 3
 .ascii "dr5"
 .byte 0x10, 0x75
 .byte 3
 .ascii "dr6"
 .byte 0x10, 0x76
 .byte 3
 .ascii "dr7"
 .byte 0x10, 0x77
 .byte 2
 .ascii "ds"
 .byte 0x10, 0x64
 .byte 5
 .ascii "dword"
 .byte 0x11, 4
 .byte 2
 .ascii "dx"
 .byte 0x10, 0x22
 .byte 3
 .ascii "eax"
 .byte 0x10, 0x40
 .byte 3
 .ascii "ebp"
 .byte 0x10, 0x45
 .byte 3
 .ascii "ebx"
 .byte 0x10, 0x43
 .byte 3
 .ascii "ecx"
 .byte 0x10, 0x41
 .byte 3
 .ascii "edi"
 .byte 0x10, 0x47
 .byte 3
 .ascii "edx"
 .byte 0x10, 0x42
 .byte 2
 .ascii "es"
 .byte 0x10, 0x61
 .byte 3
 .ascii "esi"
 .byte 0x10, 0x46
 .byte 3
 .ascii "esp"
 .byte 0x10, 0x44
 .byte 3
 .ascii "far"
 .byte 0x12, 2
 .byte 2
 .ascii "fs"
 .byte 0x10, 0x65
 .byte 5
 .ascii "fword"
 .byte 0x11, 6
 .byte 2
 .ascii "gs"
 .byte 0x10, 0x66
 .byte 3
 .ascii "mm0"
 .byte 0x10, 0x80
 .byte 3
 .ascii "mm1"
 .byte 0x10, 0x81
 .byte 3
 .ascii "mm2"
 .byte 0x10, 0x82
 .byte 3
 .ascii "mm3"
 .byte 0x10, 0x83
 .byte 3
 .ascii "mm4"
 .byte 0x10, 0x84
 .byte 3
 .ascii "mm5"
 .byte 0x10, 0x85
 .byte 3
 .ascii "mm6"
 .byte 0x10, 0x86
 .byte 3
 .ascii "mm7"
 .byte 0x10, 0x87
 .byte 4
 .ascii "near"
 .byte 0x12, 1
 .byte 5
 .ascii "pword"
 .byte 0x11, 6
 .byte 5
 .ascii "qword"
 .byte 0x11, 8
 .byte 2
 .ascii "si"
 .byte 0x10, 0x26
 .byte 2
 .ascii "sp"
 .byte 0x10, 0x24
 .byte 2
 .ascii "ss"
 .byte 0x10, 0x63
 .byte 2
 .ascii "st"
 .byte 0x10, 0xa0
 .byte 3
 .ascii "st0"
 .byte 0x10, 0xa0
 .byte 3
 .ascii "st1"
 .byte 0x10, 0xa1
 .byte 3
 .ascii "st2"
 .byte 0x10, 0xa2
 .byte 3
 .ascii "st3"
 .byte 0x10, 0xa3
 .byte 3
 .ascii "st4"
 .byte 0x10, 0xa4
 .byte 3
 .ascii "st5"
 .byte 0x10, 0xa5
 .byte 3
 .ascii "st6"
 .byte 0x10, 0xa6
 .byte 3
 .ascii "st7"
 .byte 0x10, 0xa7
 .byte 5
 .ascii "tword"
 .byte 0x11, 0xa
 .byte 5
 .ascii "use16"
 .byte 0x13, 0x10
 .byte 5
 .ascii "use32"
 .byte 0x13, 0x20
 .byte 4
 .ascii "word"
 .byte 0x11, 2
 .byte 4
 .ascii "xmm0"
 .byte 0x10, 0x90
 .byte 4
 .ascii "xmm1"
 .byte 0x10, 0x91
 .byte 4
 .ascii "xmm2"
 .byte 0x10, 0x92
 .byte 4
 .ascii "xmm3"
 .byte 0x10, 0x93
 .byte 4
 .ascii "xmm4"
 .byte 0x10, 0x94
 .byte 4
 .ascii "xmm5"
 .byte 0x10, 0x95
 .byte 4
 .ascii "xmm6"
 .byte 0x10, 0x96
 .byte 4
 .ascii "xmm7"
 .byte 0x10, 0x97
 .byte 0

formatter_symbols:
 .if CASE_INSENSITIVE
 .byte 6
 .ascii "binary"
 .byte 0x18, 1
 .byte 4
 .ascii "code"
 .byte 0x19, 5
 .byte 7
 .ascii "console"
 .byte 0x1b, 3
 .byte 4
 .ascii "data"
 .byte 0x19, 6
 .byte 11
 .ascii "discardable"
 .byte 0x19, 25
 .byte 3
 .ascii "dll"
 .byte 0x1b, 0x80
 .byte 10
 .ascii "executable"
 .byte 0x19, 29
 .byte 6
 .ascii "export"
 .byte 0x1a, 0
 .byte 6
 .ascii "fixups"
 .byte 0x1a, 5
 .byte 3
 .ascii "gui"
 .byte 0x1b, 2
 .byte 4
 .ascii "i386"
 .byte 0x1b, 0x43
 .byte 4
 .ascii "i486"
 .byte 0x1b, 0x44
 .byte 4
 .ascii "i586"
 .byte 0x1b, 0x45
 .byte 6
 .ascii "import"
 .byte 0x1a, 1
 .byte 2
 .ascii "mz"
 .byte 0x18, 2
 .byte 6
 .ascii "native"
 .byte 0x1b, 1
 .byte 2
 .ascii "pe"
 .byte 0x18, 3
 .byte 8
 .ascii "readable"
 .byte 0x19, 30
 .byte 8
 .ascii "resource"
 .byte 0x1a, 2
 .byte 9
 .ascii "shareable"
 .byte 0x19, 28
 .byte 5
 .ascii "udata"
 .byte 0x19, 7
 .byte 9
 .ascii "writeable"
 .byte 0x19, 31
 .else
 .byte 3
 .ascii "DLL"
 .byte 0x1b, 0x80
 .byte 3
 .ascii "GUI"
 .byte 0x1b, 2
 .byte 2
 .ascii "MZ"
 .byte 0x18, 2
 .byte 2
 .ascii "PE"
 .byte 0x18, 3
 .byte 6
 .ascii "binary"
 .byte 0x18, 1
 .byte 4
 .ascii "code"
 .byte 0x19, 5
 .byte 7
 .ascii "console"
 .byte 0x1b, 3
 .byte 4
 .ascii "data"
 .byte 0x19, 6
 .byte 11
 .ascii "discardable"
 .byte 0x19, 25
 .byte 10
 .ascii "executable"
 .byte 0x19, 29
 .byte 6
 .ascii "export"
 .byte 0x1a, 0
 .byte 6
 .ascii "fixups"
 .byte 0x1a, 5
 .byte 4
 .ascii "i386"
 .byte 0x1b, 0x43
 .byte 4
 .ascii "i486"
 .byte 0x1b, 0x44
 .byte 4
 .ascii "i586"
 .byte 0x1b, 0x45
 .byte 6
 .ascii "import"
 .byte 0x1a, 1
 .byte 6
 .ascii "native"
 .byte 0x1b, 1
 .byte 8
 .ascii "readable"
 .byte 0x19, 30
 .byte 8
 .ascii "resource"
 .byte 0x1a, 2
 .byte 9
 .ascii "shareable"
 .byte 0x19, 28
 .byte 5
 .ascii "udata"
 .byte 0x19, 7
 .byte 9
 .ascii "writeable"
 .byte 0x19, 31
 .endif
 .byte 0

preprocessor_directives:
 .byte 7
 .ascii "include"
 .word include_file-preprocessor
 .byte 5
 .ascii "macro"
 .word define_macro-preprocessor
 .byte 5
 .ascii "purge"
 .word purge_macro-preprocessor
 .byte 5
 .ascii "struc"
 .word define_struc-preprocessor
 .byte 0

macro_directives:
 .byte 6
 .ascii "common"
 .word common_block-preprocessor
 .byte 7
 .ascii "forward"
 .word forward_block-preprocessor
 .byte 5
 .ascii "local"
 .word local_symbols-preprocessor
 .byte 7
 .ascii "reverse"
 .word reverse_block-preprocessor
 .byte 0

data_handlers:
 .word data_bytes-assembler
 .word data_file-assembler
 .word reserve_bytes-assembler
 .word data_words-assembler
 .word data_unicode-assembler
 .word reserve_words-assembler
 .word data_dwords-assembler
 .word reserve_dwords-assembler
 .word data_pwords-assembler
 .word reserve_pwords-assembler
 .word data_qwords-assembler
 .word reserve_qwords-assembler
 .word data_twords-assembler
 .word reserve_twords-assembler

data_directives:
 .byte 2
 .ascii "db"
 .byte 1, 0
 .byte 2
 .ascii "dd"
 .byte 4, 6
 .byte 2
 .ascii "dp"
 .byte 6, 8
 .byte 2
 .ascii "dq"
 .byte 8, 10
 .byte 2
 .ascii "dt"
 .byte 10, 12
 .byte 2
 .ascii "du"
 .byte 2, 4
 .byte 2
 .ascii "dw"
 .byte 2, 3
 .byte 4
 .ascii "file"
 .byte 1, 1
 .byte 2
 .ascii "rb"
 .byte 1, 2
 .byte 2
 .ascii "rd"
 .byte 4, 7
 .byte 2
 .ascii "rp"
 .byte 6, 9
 .byte 2
 .ascii "rq"
 .byte 8, 11
 .byte 2
 .ascii "rt"
 .byte 10, 13
 .byte 2
 .ascii "rw"
 .byte 2, 5
 .byte 0

instructions:
 .word instructions_2-instructions
 .word instructions_3-instructions
 .word instructions_4-instructions
 .word instructions_5-instructions
 .word instructions_6-instructions
 .word instructions_7-instructions
 .word instructions_8-instructions
 .word instructions_9-instructions
 .word instructions_10-instructions
 .word instructions_11-instructions

##%macro dbw 3
  ##db %1, %2
  ##dw %3
##%endm

instructions_2:
 .ascii "bt"
 .byte 4
 .word bt_instruction-assembler
 .ascii "if"
 .byte 0
 .word if_directive-assembler
 .ascii "in"
 .byte 0
 .word in_instruction-assembler
 .ascii "ja"
 .byte 0x77
 .word conditional_jump-assembler
 .ascii "jb"
 .byte 0x72
 .word conditional_jump-assembler
 .ascii "jc"
 .byte 0x72
 .word conditional_jump-assembler
 .ascii "je"
 .byte 0x74
 .word conditional_jump-assembler
 .ascii "jg"
 .byte 0x7f
 .word conditional_jump-assembler
 .ascii "jl"
 .byte 0x7c
 .word conditional_jump-assembler
 .ascii "jo"
 .byte 0x70
 .word conditional_jump-assembler
 .ascii "jp"
 .byte 0x7a
 .word conditional_jump-assembler
 .ascii "js"
 .byte 0x78
 .word conditional_jump-assembler
 .ascii "jz"
 .byte 0x74
 .word conditional_jump-assembler
 .ascii "or"
 .byte 0x8
 .word basic_instruction-assembler
 .byte 0
instructions_3:
 .ascii "aaa"
 .byte 0x37
 .word simple_instruction-assembler
 .ascii "aad"
 .byte 0xd5
 .word aa_instruction-assembler
 .ascii "aam"
 .byte 0xd4
 .word aa_instruction-assembler
 .ascii "aas"
 .byte 0x3f
 .word simple_instruction-assembler
 .ascii "adc"
 .byte 0x10
 .word basic_instruction-assembler
 .ascii "add"
 .byte 0x0
 .word basic_instruction-assembler
 .ascii "and"
 .byte 0x20
 .word basic_instruction-assembler
 .ascii "bsf"
 .byte 0xbc
 .word bs_instruction-assembler
 .ascii "bsr"
 .byte 0xbd
 .word bs_instruction-assembler
 .ascii "btc"
 .byte 7
 .word bt_instruction-assembler
 .ascii "btr"
 .byte 6
 .word bt_instruction-assembler
 .ascii "bts"
 .byte 5
 .word bt_instruction-assembler
 .ascii "cbw"
 .byte 0x98
 .word simple_instruction_16bit-assembler
 .ascii "cdq"
 .byte 0x99
 .word simple_instruction_32bit-assembler
 .ascii "clc"
 .byte 0xf8
 .word simple_instruction-assembler
 .ascii "cld"
 .byte 0xfc
 .word simple_instruction-assembler
 .ascii "cli"
 .byte 0xfa
 .word simple_instruction-assembler
 .ascii "cmc"
 .byte 0xf5
 .word simple_instruction-assembler
 .ascii "cmp"
 .byte 0x38
 .word basic_instruction-assembler
 .ascii "cwd"
 .byte 0x99
 .word simple_instruction_16bit-assembler
 .ascii "daa"
 .byte 0x27
 .word simple_instruction-assembler
 .ascii "das"
 .byte 0x2f
 .word simple_instruction-assembler
 .ascii "dec"
 .byte 1
 .word inc_instruction-assembler
 .ascii "div"
 .byte 6
 .word single_operand_instruction-assembler
 .ascii "end"
 .byte 0
 .word end_directive-assembler
 .ascii "hlt"
 .byte 0xf4
 .word simple_instruction-assembler
 .ascii "inc"
 .byte 0
 .word inc_instruction-assembler
 .ascii "ins"
 .byte 0
 .word ins_instruction-assembler
 .ascii "int"
 .byte 0xcd
 .word int_instruction-assembler
 .ascii "jae"
 .byte 0x73
 .word conditional_jump-assembler
 .ascii "jbe"
 .byte 0x76
 .word conditional_jump-assembler
 .ascii "jge"
 .byte 0x7d
 .word conditional_jump-assembler
 .ascii "jle"
 .byte 0x7e
 .word conditional_jump-assembler
 .ascii "jmp"
 .byte 0
 .word jmp_instruction-assembler
 .ascii "jna"
 .byte 0x76
 .word conditional_jump-assembler
 .ascii "jnb"
 .byte 0x73
 .word conditional_jump-assembler
 .ascii "jnc"
 .byte 0x73
 .word conditional_jump-assembler
 .ascii "jne"
 .byte 0x75
 .word conditional_jump-assembler
 .ascii "jng"
 .byte 0x7e
 .word conditional_jump-assembler
 .ascii "jnl"
 .byte 0x7d
 .word conditional_jump-assembler
 .ascii "jno"
 .byte 0x71
 .word conditional_jump-assembler
 .ascii "jnp"
 .byte 0x7b
 .word conditional_jump-assembler
 .ascii "jns"
 .byte 0x79
 .word conditional_jump-assembler
 .ascii "jnz"
 .byte 0x75
 .word conditional_jump-assembler
 .ascii "jpe"
 .byte 0x7a
 .word conditional_jump-assembler
 .ascii "jpo"
 .byte 0x7b
 .word conditional_jump-assembler
 .ascii "lar"
 .byte 2
 .word lar_instruction-assembler
 .ascii "lds"
 .byte 3
 .word ls_instruction-assembler
 .ascii "lea"
 .byte 0
 .word lea_instruction-assembler
 .ascii "les"
 .byte 0
 .word ls_instruction-assembler
 .ascii "lfs"
 .byte 4
 .word ls_instruction-assembler
 .ascii "lgs"
 .byte 5
 .word ls_instruction-assembler
 .ascii "lsl"
 .byte 3
 .word lar_instruction-assembler
 .ascii "lss"
 .byte 2
 .word ls_instruction-assembler
 .ascii "mov"
 .byte 0
 .word mov_instruction-assembler
 .ascii "mul"
 .byte 4
 .word single_operand_instruction-assembler
 .ascii "neg"
 .byte 3
 .word single_operand_instruction-assembler
 .ascii "nop"
 .byte 0x90
 .word simple_instruction-assembler
 .ascii "not"
 .byte 2
 .word single_operand_instruction-assembler
 .ascii "org"
 .byte 0
 .word org_directive-assembler
 .ascii "out"
 .byte 0
 .word out_instruction-assembler
 .ascii "pop"
 .byte 0
 .word pop_instruction-assembler
 .ascii "rcl"
 .byte 2
 .word sh_instruction-assembler
 .ascii "rcr"
 .byte 3
 .word sh_instruction-assembler
 .ascii "rep"
 .byte 0xf3
 .word prefix_instruction-assembler
 .ascii "ret"
 .byte 0xc2
 .word ret_instruction-assembler
 .ascii "rol"
 .byte 0
 .word sh_instruction-assembler
 .ascii "ror"
 .byte 1
 .word sh_instruction-assembler
 .ascii "rsm"
 .byte 0xaa
 .word simple_extended_instruction-assembler
 .ascii "sal"
 .byte 6
 .word sh_instruction-assembler
 .ascii "sar"
 .byte 7
 .word sh_instruction-assembler
 .ascii "sbb"
 .byte 0x18
 .word basic_instruction-assembler
 .ascii "shl"
 .byte 4
 .word sh_instruction-assembler
 .ascii "shr"
 .byte 5
 .word sh_instruction-assembler
 .ascii "stc"
 .byte 0xf9
 .word simple_instruction-assembler
 .ascii "std"
 .byte 0xfd
 .word simple_instruction-assembler
 .ascii "sti"
 .byte 0xfb
 .word simple_instruction-assembler
 .ascii "sub"
 .byte 0x28
 .word basic_instruction-assembler
 .ascii "ud2"
 .byte 0xb
 .word simple_extended_instruction-assembler
 .ascii "xor"
 .byte 0x30
 .word basic_instruction-assembler
 .byte 0
instructions_4:
 .ascii "arpl"
 .byte 0
 .word arpl_instruction-assembler
 .ascii "call"
 .byte 0
 .word call_instruction-assembler
 .ascii "clts"
 .byte 6
 .word simple_extended_instruction-assembler
 .ascii "cmps"
 .byte 0
 .word cmps_instruction-assembler
 .ascii "cwde"
 .byte 0x98
 .word simple_instruction_32bit-assembler
 .ascii "else"
 .byte 0
 .word else_directive-assembler
 .ascii "heap"
 .byte 0
 .word heap_directive-assembler
 .ascii "idiv"
 .byte 7
 .word single_operand_instruction-assembler
 .ascii "imul"
 .byte 0
 .word imul_instruction-assembler
 .ascii "int3"
 .byte 0xcc
 .word simple_instruction-assembler
 .ascii "into"
 .byte 0xce
 .word simple_instruction-assembler
 .ascii "invd"
 .byte 8
 .word simple_extended_instruction-assembler
 .ascii "iret"
 .byte 0xcf
 .word simple_instruction-assembler
 .ascii "jcxz"
 .byte 0xe3
 .word loop_instruction_16bit-assembler
 .ascii "jnae"
 .byte 0x72
 .word conditional_jump-assembler
 .ascii "jnbe"
 .byte 0x77
 .word conditional_jump-assembler
 .ascii "jnge"
 .byte 0x7c
 .word conditional_jump-assembler
 .ascii "jnle"
 .byte 0x7f
 .word conditional_jump-assembler
 .ascii "lahf"
 .byte 0x9f
 .word simple_instruction-assembler
 .ascii "load"
 .byte 0
 .word load_directive-assembler
 .ascii "lock"
 .byte 0xf0
 .word prefix_instruction-assembler
 .ascii "lods"
 .byte 0
 .word lods_instruction-assembler
 .ascii "loop"
 .byte 0xe2
 .word loop_instruction-assembler
 .ascii "movs"
 .byte 0
 .word movs_instruction-assembler
 .ascii "outs"
 .byte 0
 .word outs_instruction-assembler
 .ascii "popa"
 .byte 0x61
 .word simple_instruction-assembler
 .ascii "popf"
 .byte 0x9d
 .word simple_instruction-assembler
 .ascii "push"
 .byte 0
 .word push_instruction-assembler
 .ascii "repe"
 .byte 0xf3
 .word prefix_instruction-assembler
 .ascii "repz"
 .byte 0xf3
 .word prefix_instruction-assembler
 .ascii "retd"
 .byte 0xc2
 .word ret_instruction_32bit-assembler
 .ascii "retf"
 .byte 0xca
 .word ret_instruction-assembler
 .ascii "retn"
 .byte 0xc2
 .word ret_instruction-assembler
 .ascii "retw"
 .byte 0xc2
 .word ret_instruction_16bit-assembler
 .ascii "sahf"
 .byte 0x9e
 .word simple_instruction-assembler
 .ascii "scas"
 .byte 0xae
 .word stos_instruction-assembler
 .ascii "seta"
 .byte 0x97
 .word set_instruction-assembler
 .ascii "setb"
 .byte 0x92
 .word set_instruction-assembler
 .ascii "setc"
 .byte 0x92
 .word set_instruction-assembler
 .ascii "sete"
 .byte 0x94
 .word set_instruction-assembler
 .ascii "setg"
 .byte 0x9f
 .word set_instruction-assembler
 .ascii "setl"
 .byte 0x9c
 .word set_instruction-assembler
 .ascii "seto"
 .byte 0x90
 .word set_instruction-assembler
 .ascii "setp"
 .byte 0x9a
 .word set_instruction-assembler
 .ascii "sets"
 .byte 0x98
 .word set_instruction-assembler
 .ascii "setz"
 .byte 0x94
 .word set_instruction-assembler
 .ascii "shld"
 .byte 0xa4
 .word shd_instruction-assembler
 .ascii "shrd"
 .byte 0xac
 .word shd_instruction-assembler
 .ascii "stos"
 .byte 0xaa
 .word stos_instruction-assembler
 .ascii "test"
 .byte 0
 .word test_instruction-assembler
 .ascii "wait"
 .byte 0x9b
 .word simple_instruction-assembler
 .ascii "xadd"
 .byte 0xc0
 .word basic_486_instruction-assembler
 .ascii "xchg"
 .byte 0
 .word xchg_instruction-assembler
 .ascii "xlat"
 .byte 0xd7
 .word xlat_instruction-assembler
 .byte 0
instructions_5:
 .ascii "bound"
 .byte 0
 .word bound_instruction-assembler
 .ascii "bswap"
 .byte 0
 .word bswap_instruction-assembler
 .ascii "cmpsb"
 .byte 0xa6
 .word simple_instruction-assembler
 .ascii "cmpsd"
 .byte 0
 .word cmpsd_instruction-assembler
 .ascii "cmpsw"
 .byte 0xa7
 .word simple_instruction_16bit-assembler
 .ascii "cpuid"
 .byte 0xa2
 .word simple_extended_instruction-assembler
 .ascii "enter"
 .byte 0
 .word enter_instruction-assembler
 .ascii "entry"
 .byte 0
 .word entry_directive-assembler
 .ascii "fwait"
 .byte 0x9b
 .word simple_instruction-assembler
 .ascii "iretd"
 .byte 0xcf
 .word simple_instruction_32bit-assembler
 .ascii "iretw"
 .byte 0xcf
 .word simple_instruction_16bit-assembler
 .ascii "jecxz"
 .byte 0xe3
 .word loop_instruction_32bit-assembler
 .ascii "label"
 .byte 0
 .word label_directive-assembler
 .ascii "leave"
 .byte 0xc9
 .word simple_instruction-assembler
 .ascii "lodsb"
 .byte 0xac
 .word simple_instruction-assembler
 .ascii "lodsd"
 .byte 0xad
 .word simple_instruction_32bit-assembler
 .ascii "lodsw"
 .byte 0xad
 .word simple_instruction_16bit-assembler
 .ascii "loopd"
 .byte 0xe2
 .word loop_instruction_32bit-assembler
 .ascii "loope"
 .byte 0xe1
 .word loop_instruction-assembler
 .ascii "loopw"
 .byte 0xe2
 .word loop_instruction_16bit-assembler
 .ascii "loopz"
 .byte 0xe1
 .word loop_instruction-assembler
 .ascii "movsb"
 .byte 0xa4
 .word simple_instruction-assembler
 .ascii "movsd"
 .byte 0
 .word movsd_instruction-assembler
 .ascii "movsw"
 .byte 0xa5
 .word simple_instruction_16bit-assembler
 .ascii "movsx"
 .byte 0xbe
 .word movx_instruction-assembler
 .ascii "movzx"
 .byte 0xb6
 .word movx_instruction-assembler
 .ascii "popad"
 .byte 0x61
 .word simple_instruction_32bit-assembler
 .ascii "popaw"
 .byte 0x61
 .word simple_instruction_16bit-assembler
 .ascii "popfd"
 .byte 0x9d
 .word simple_instruction_32bit-assembler
 .ascii "popfw"
 .byte 0x9d
 .word simple_instruction_16bit-assembler
 .ascii "pusha"
 .byte 0x60
 .word simple_instruction-assembler
 .ascii "pushf"
 .byte 0x9c
 .word simple_instruction-assembler
 .ascii "repne"
 .byte 0xf2
 .word prefix_instruction-assembler
 .ascii "repnz"
 .byte 0xf2
 .word prefix_instruction-assembler
 .ascii "retfd"
 .byte 0xca
 .word ret_instruction_32bit-assembler
 .ascii "retfw"
 .byte 0xca
 .word ret_instruction_16bit-assembler
 .ascii "retnd"
 .byte 0xc2
 .word ret_instruction_32bit-assembler
 .ascii "retnw"
 .byte 0xc2
 .word ret_instruction_16bit-assembler
 .ascii "scasb"
 .byte 0xae
 .word simple_instruction-assembler
 .ascii "scasd"
 .byte 0xaf
 .word simple_instruction_32bit-assembler
 .ascii "scasw"
 .byte 0xaf
 .word simple_instruction_16bit-assembler
 .ascii "setae"
 .byte 0x93
 .word set_instruction-assembler
 .ascii "setbe"
 .byte 0x96
 .word set_instruction-assembler
 .ascii "setge"
 .byte 0x9d
 .word set_instruction-assembler
 .ascii "setle"
 .byte 0x9e
 .word set_instruction-assembler
 .ascii "setna"
 .byte 0x96
 .word set_instruction-assembler
 .ascii "setnb"
 .byte 0x93
 .word set_instruction-assembler
 .ascii "setnc"
 .byte 0x93
 .word set_instruction-assembler
 .ascii "setne"
 .byte 0x95
 .word set_instruction-assembler
 .ascii "setng"
 .byte 0x9e
 .word set_instruction-assembler
 .ascii "setnl"
 .byte 0x9d
 .word set_instruction-assembler
 .ascii "setno"
 .byte 0x91
 .word set_instruction-assembler
 .ascii "setnp"
 .byte 0x9b
 .word set_instruction-assembler
 .ascii "setns"
 .byte 0x99
 .word set_instruction-assembler
 .ascii "setnz"
 .byte 0x95
 .word set_instruction-assembler
 .ascii "setpe"
 .byte 0x9a
 .word set_instruction-assembler
 .ascii "setpo"
 .byte 0x9b
 .word set_instruction-assembler
 .ascii "stack"
 .byte 0
 .word stack_directive-assembler
 .ascii "stosb"
 .byte 0xaa
 .word simple_instruction-assembler
 .ascii "stosd"
 .byte 0xab
 .word simple_instruction_32bit-assembler
 .ascii "stosw"
 .byte 0xab
 .word simple_instruction_16bit-assembler
 .ascii "times"
 .byte 0
 .word times_directive-assembler
 .ascii "xlatb"
 .byte 0xd7
 .word simple_instruction-assembler
 .byte 0
instructions_6:
 .ascii "format"
 .byte 0
 .word format_directive-assembler
 .ascii "looped"
 .byte 0xe1
 .word loop_instruction_32bit-assembler
 .ascii "loopew"
 .byte 0xe1
 .word loop_instruction_16bit-assembler
 .ascii "loopne"
 .byte 0xe0
 .word loop_instruction-assembler
 .ascii "loopnz"
 .byte 0xe0
 .word loop_instruction-assembler
 .ascii "loopzd"
 .byte 0xe1
 .word loop_instruction_32bit-assembler
 .ascii "loopzw"
 .byte 0xe1
 .word loop_instruction_16bit-assembler
 .ascii "pushad"
 .byte 0x60
 .word simple_instruction_32bit-assembler
 .ascii "pushaw"
 .byte 0x60
 .word simple_instruction_16bit-assembler
 .ascii "pushfd"
 .byte 0x9c
 .word simple_instruction_32bit-assembler
 .ascii "pushfw"
 .byte 0x9c
 .word simple_instruction_16bit-assembler
 .ascii "repeat"
 .byte 0
 .word repeat_directive-assembler
 .ascii "setalc"
 .byte 0xd6
 .word simple_instruction-assembler
 .ascii "setnae"
 .byte 0x92
 .word set_instruction-assembler
 .ascii "setnbe"
 .byte 0x97
 .word set_instruction-assembler
 .ascii "setnge"
 .byte 0x9c
 .word set_instruction-assembler
 .ascii "setnle"
 .byte 0x9f
 .word set_instruction-assembler
 .byte 0
instructions_7:
 .ascii "loopned"
 .byte 0xe0
 .word loop_instruction_32bit-assembler
 .ascii "loopnew"
 .byte 0xe0
 .word loop_instruction_16bit-assembler
 .ascii "loopnzd"
 .byte 0xe0
 .word loop_instruction_32bit-assembler
 .ascii "loopnzw"
 .byte 0xe0
 .word loop_instruction_16bit-assembler
 .ascii "virtual"
 .byte 0
 .word virtual_directive-assembler
 .byte 0
instructions_8:
 .byte 0
instructions_9:
 .byte 0
instructions_10:
 .byte 0
instructions_11:
 .byte 0

# %include done

_copyright:
.ascii "Copyright (c) 1999-2002, Tomasz Grysztar"
.byte 0

_logo:
.ascii "flat assembler  version "
.ascii "1.30-bootstrap"
.byte 0xa
.byte 0


_usage:
.ascii "usage: fasm source output"
.byte 0xa, 0

_passes_suffix:
.ascii " passes, "
.byte 0
_seconds_suffix:
.ascii " seconds, "
.byte 0
_bytes_suffix:
.ascii " bytes."
.byte 0xa, 0

_counter:
.byte 4
.ascii "0000"

prebss:
# bss_align = 0
.section .bss  #  We could use `absolute $' here instead, but that's broken (breaks address calculation in program_end-bss+prebss-file_header) in NASM 0.95--0.97.
bss:
# .fill bss_align, 1, 0  #  Uninitialized data follows.

memory_start:
.fill 4, 1, 0
memory_end:
.fill 4, 1, 0
additional_memory:
.fill 4, 1, 0
additional_memory_end:
.fill 4, 1, 0
input_file:
.fill 4, 1, 0
output_file:
.fill 4, 1, 0
source_start:
.fill 4, 1, 0
code_start:
.fill 4, 1, 0
code_size:
.fill 4, 1, 0
real_code_size:
.fill 4, 1, 0
start_time:
.fill 4, 1, 0
written_size:
.fill 4, 1, 0

current_line:
.fill 4, 1, 0
macros_list:
.fill 4, 1, 0
macro_constants:
.fill 4, 1, 0
macro_block:
.fill 4, 1, 0
macro_block_line_number:
.fill 4, 1, 0
struc_name:
.fill 4, 1, 0
current_locals_prefix:
.fill 4, 1, 0
labels_list:
.fill 4, 1, 0
label_hash:
.fill 4, 1, 0
org_start:
.fill 4, 1, 0
org_sib:
.fill 4, 1, 0
undefined_data_start:
.fill 4, 1, 0
undefined_data_end:
.fill 4, 1, 0
counter:
.fill 4, 1, 0
counter_limit:
.fill 4, 1, 0
error_line:
.fill 4, 1, 0
error:
.fill 4, 1, 0
display_buffer:
.fill 4, 1, 0
structures_buffer:
.fill 4, 1, 0
number_start:
.fill 4, 1, 0
current_offset:
.fill 4, 1, 0
value:
.fill 8, 1, 0
fp_value:
.fill 8, 1, 0
format_flags:
.fill 4, 1, 0
number_of_relocations:
.fill 4, 1, 0
number_of_sections:
.fill 4, 1, 0
stub_size:
.fill 4, 1, 0
header_data:
.fill 4, 1, 0
sections_data:
.fill 4, 1, 0
current_section:
.fill 4, 1, 0
machine:
.fill 2, 1, 0
subsystem:
.fill 2, 1, 0
subsystem_version:
.fill 4, 1, 0

macro_status:
.fill 1, 1, 0
parenthesis_stack:
.fill 1, 1, 0
output_format:
.fill 1, 1, 0
code_type:
.fill 1, 1, 0
current_pass:
.fill 1, 1, 0
next_pass_needed:
.fill 1, 1, 0
reloc_labels:
.fill 1, 1, 0
times_working:
.fill 1, 1, 0
virtual_data:
.fill 1, 1, 0
fp_sign:
.fill 1, 1, 0
fp_format:
.fill 1, 1, 0  #  TODO(pts): Remove unused variables.
value_size:
.fill 1, 1, 0
forced_size:
.fill 1, 1, 0
value_type:
.fill 1, 1, 0
address_size:
.fill 1, 1, 0
compare_type:
.fill 1, 1, 0
base_code:
.fill 1, 1, 0
extended_code:
.fill 1, 1, 0
postbyte_register:
.fill 1, 1, 0
segment_register:
.fill 1, 1, 0
operand_size:
.fill 1, 1, 0
imm_sized:
.fill 1, 1, 0
jump_type:
.fill 1, 1, 0
mmx_size:
.fill 1, 1, 0
mmx_prefix:
.fill 1, 1, 0
nextbyte:
.fill 1, 1, 0

characters:
.fill 0x100, 1, 0
converted:
.fill 0x100, 1, 0
available_memory:
.fill 4, 1, 0

program_end:

#  __END__
