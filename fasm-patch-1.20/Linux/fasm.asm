
; flat assembler source
; Copyright (c) 1999-2001, Tomasz Grysztar
; All rights reserved.

	program_base = 0x700000

	org	program_base
	use32

	macro	align value { rb (value-1) - ($ + value-1) mod value }

file_header:
	db	0x7F,'ELF',1,1,1
	rb	file_header+0x10-$
	dw	2,3
	dd	1,start
	dd	program_header-file_header,0,0
	dw	program_header-file_header,0x20,1,0,0,0

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
