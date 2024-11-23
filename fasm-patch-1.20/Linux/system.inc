
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
