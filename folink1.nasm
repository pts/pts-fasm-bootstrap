;
; folink1.nasm: flat OMF linker implemented in NASM 0.98.39 targeting DOS 8086
; by pts@fazekas.hu at Mon Mar 25 04:35:37 CET 2024
;
; Compile with: nasm-0.98.39 -O999999999 -w+orphan-labels -f bin -o folink1.com folink1.nasm
;

	bits 16
	cpu 8086
	org 100h  ; DOS .com file is loaded at CS:0x100.

___section_startup_text:

;___stack_size	equ 0x140  ; To estimate, specify -sc to dosmc, and run it to get the `max st:HHHH' value printed, and round up 0xHHHH to here. Typical value: 0x200.

_start:  ; Entry point of the DOS .com program.
		cld
		;mov sp, ___initial_sp
		mov di, ___section_c_bss
		mov cx, (___section_startup_ubss-___section_c_bss+1)>>1
		xor ax, ax
		rep stosw
		mov di, argv_bytes
		mov bp, argv_pointers
		push bp
		push es
		lds si, [0x2c-2]  ; Environment segment within PSP.
		
		xor si, si
		lodsb
.next_entry:	test al, al
		jz .end_entries
.next_char:	test al, al
		lodsb
		jnz .next_char
		jmp short .next_entry
.end_entries:	inc si  ; Skip over a single byte.
		inc si  ; Skip over '\0'.
		; Now ds:si points to the program name as an uppercase, absolute pathname with extension (e.g. .EXE or .COM). We will use it as argv.
		
		; Copy program name to argv[0].
		mov [bp], di  ; argv[0] pointer.
		inc bp
		inc bp
		mov cx, 144  ; To avoid overflowing argv_bytes. See above why 144.
.next_copy:	dec cx
		jnz .argv0_limit_not_reached
		xor al, al
		stosb
		jmp short .after_copy
.argv0_limit_not_reached:
		lodsb
		stosb
		test al, al
		jnz .next_copy
.after_copy:
		
		; Now copy cmdline.
		pop ds  ; PSP.
		mov si, 0x80  ; Command-line size byte within PSP, usually space. 0..127, we trust it.
		lodsb
		xor ah, ah
		xchg bx, ax  ; bx := ax.
		mov byte [si+bx], 0
.scan_for_arg:	lodsb
		test al, al
		jz .after_cmdline
		cmp al, ' '
		je .scan_for_arg
		cmp al, 9  ; Tab.
		je .scan_for_arg
		mov [bp], di  ; Start new argv[...] element. Uses ss by default, good.
		inc bp
		inc bp
		stosb  ; First byte of argv[...].
.next_argv_byte:
		lodsb
		stosb
		test al, al
		jz .after_cmdline
		cmp al, ' '
		je .end_arg
		cmp al, 9  ; Tab.
		jne .next_argv_byte
.end_arg:	dec di
		xor al, al
		stosb  ; Replace whitespace with terminating '\0'.
		jmp short .scan_for_arg
		
.after_cmdline:	mov word [bp], 0  ; NULL at the end of argv.
		pop dx  ; argv_pointers. Final return value of dx.
		sub bp, dx
		xchg ax, bp  ; ax := bp.
		shr ax, 1  ; Set ax to argc, it's final return value.
		; Fall through.

main_:
		push	bx
		push	cx
		push	si
		push	di
		push	bp
		mov	bp,sp
		sub	sp,6
		xor	dx,dx
		mov	word [_ledata_size],dx
		mov	word [_wrfd],1
.10:
		or	byte [_rsize],3
		call	r8_
		mov	bx,ax
		call	r16_
		mov	word [_rsize],ax
		test	ax,ax
		ja	.11
		mov	ax,2
		call	fatal_
.11:
		dec	word [_rsize]
		cmp	bx,8ah
		je	.13
		cmp	bx,88h
		je	.12
		cmp	bx,9ah
		je	.12
		cmp	bx,96h
		je	.12
		cmp	bx,98h
		je	.12
		cmp	bx,80h
		jne	.14
.12:
		cmp	word [_rsize],0
		jbe	.18
		call	r8_
		jmp	.12
.13:
		jmp	.40
.14:
		cmp	bx,0a0h
		je	.15
		cmp	bx,0a2h
		jne	.19
.15:
		call	flush_ledata_
		call	r8_
		call	r16_
		cmp	ax,dx
		je	.16
		mov	ax,4
		call	fatal_
.16:
		cmp	bx,0a0h
		jne	.20
		mov	ax,word [_rsize]
		mov	word [_ledata_size],ax
		add	dx,ax
		mov	bx,_ledata
.17:
		cmp	word [_rsize],0
		jbe	.18
		call	r8_
		mov	byte [bx],al
		inc	bx
		jmp	.17
.18:
		jmp	.37
.19:
		jmp	.28
.20:
		call	r16_
		mov	word [bp-6],ax
		call	r16_
		mov	cx,ax
		test	ax,ax
		jne	.21
		mov	ax,6
		call	fatal_
.21:
		test	cx,cx
		jbe	.18
		call	r16_
		mov	di,ax
		call	r16_
		test	ax,ax
		je	.22
		mov	ax,8
		call	fatal_
.22:
		call	r8_
		mov	bx,ax
		call	r8_
		mov	word [bp-4],ax
		cmp	bx,1
		je	.23
		mov	ax,9
		call	fatal_
.23:
		mov	bx,word [bp-6]
.24:
		test	bx,bx
		jbe	.27
		add	dx,di
		mov	si,di
.25:
		test	si,si
		jbe	.26
		mov	al,byte [bp-4]
		mov	byte [bp-2],al
		mov	byte [bp-1],0
		mov	ax,word [bp-2]
		call	w8_
		dec	si
		jmp	.25
.26:
		dec	bx
		jmp	.24
.27:
		dec	cx
		jmp	.21
.28:
		cmp	bx,9dh
		je	.29
		jmp	.36
.29:
		cmp	word [_rsize],0
		jbe	.18
		call	r8_
		mov	bx,ax
		test	al,80h
		jne	.30
		mov	ax,0ch
		call	fatal_
.30:
		mov	cx,bx
		mov	ch,bl
		and	ch,3
		xor	cl,bl
		call	r8_
		or	cx,ax
		shr	bx,1
		shr	bx,1
		cmp	bx,39h
		je	.31
		mov	ax,0dh
		call	fatal_
.31:
		call	r8_
		cmp	ax,14h
		je	.32
		mov	ax,0eh
		call	fatal_
.32:
		call	r8_
		cmp	ax,1
		je	.33
		mov	ax,0fh
		call	fatal_
.33:
		call	r8_
		cmp	ax,1
		je	.34
		mov	ax,10h
		call	fatal_
.34:
		mov	ax,cx
		add	ax,4
		cmp	ax,word [_ledata_size]
		jbe	.35
		mov	ax,11h
		call	fatal_
.35:
		mov	bx,_ledata
		add	bx,cx
		_org equ 8048000h  ; !! Make it configurable, or read it from the ELF header.
		add	word [bx],_org & 0xffff
		adc	word [bx+2],_org >> 16
		jmp	.29
.36:
		mov	ax,12h
		call	fatal_
.37:
		inc	word [_rsize]
		call	r8_
		cmp	word [_rsize],0
		jne	.39
.38:
		jmp	.10
.39:
		mov	ax,0ah
		call	fatal_
		jmp	.38
.40:
		call	flush_ledata_
		call	wflush_
		mov	ax,4c00h  ; EXIT_SUCCESS.
		int	21h

; ssize_t write(int fd, const void *buf, size_t count);
; Optimized for size. AX == fd, DX == buf, BX == count.
write_:		push cx
		xchg ax, bx		; AX := count; BX := fd.
		xchg ax, cx		; CX := count; AX := junk.
		mov ah, 0x40
		int 0x21
		jnc .ok
		sbb ax, ax		; AX := -1.
.ok:		pop cx
		ret

; ssize_t read(int fd, void *buf, size_t count);
; Optimized for size. AX == fd, DX == buf, BX == count.
read_:		push cx
		xchg ax, bx		; AX := count; BX := fd.
		xchg ax, cx		; CX := count; AX := junk.
		mov ah, 0x3f
		int 0x21
		jnc .ok
		sbb ax, ax		; AX := -1.
.ok:		pop cx
		ret

fatal_:
		push	bx
		push	cx
		push	dx
		mov	cx,ax
		mov	bx,15h
		mov	dx,fatal_msg
		mov	ax,2
		call	write_
		mov	ax,cx
		mov	ah,4ch
		int	21h

r8_:
		push	bx
		push	dx
		cmp	word [_rsize],0
		jne	.1
		mov	ax,3
		call	fatal_
.1:
		mov	ax,word [_rdi]
		cmp	ax,word [_rdlimit]
		jne	.3
		mov	ax,word [_rdfd]
		mov	bx,200h
		mov	dx,_rdbuf
		call	read_
		mov	bx,ax
		test	ax,ax
		jge	.2
		mov	ax,5
		call	fatal_
.2:
		mov	word [_rdlimit],bx
		xor	ax,ax
		mov	word [_rdi],ax
.3:
		mov	bx,word [_rdi]
		mov	al,byte [_rdbuf+bx]
		xor	ah,ah
		dec	word [_rsize]
		inc	bx
		mov	word [_rdi],bx
		pop	dx
		pop	bx
		ret

wflush_:
		push	bx
		push	dx
		mov	ax,word [_wri]
		test	ax,ax
		jbe	.6
		mov	bx,ax
		mov	ax,word [_wrfd]
		mov	dx,_wrbuf
		call	write_
		test	ax,ax
		jle	.4
		cmp	ax,word [_wri]
		je	.5
.4:
		mov	ax,7
		call	fatal_
.5:
		xor	ax,ax
		mov	word [_wri],ax
.6:
		pop	dx
		pop	bx
		ret

w8_:
		push	bx
		push	dx
		mov	dl,al
		cmp	word [_wri],200h
		jne	.7
		call	wflush_
.7:
		mov	bx,word [_wri]
		mov	byte [_wrbuf+bx],dl
		inc	bx
		mov	word [_wri],bx
		pop	dx
		pop	bx
		ret

r16_:
		push	dx
		call	r8_
		mov	dx,ax
		call	r8_
		mov	ah,al
		xor	al,al
		or	ax,dx
		pop	dx
		ret

flush_ledata_:
		push	bx
		push	dx
		mov	bx,_ledata
		mov	dx,bx
		add	dx,word [_ledata_size]
.8:
		cmp	bx,dx
		je	.9
		mov	al,byte [bx]
		xor	ah,ah
		call	w8_
		inc	bx
		jmp	.8
.9:
		xor	ax,ax
		mov	word [_ledata_size],ax
		pop	dx
		pop	bx
		ret

fatal_msg:	db 'folink1 fatal error', 13, 10, 0

; --- Variables initialized to 0 by _start.
___section_nobss_end:
		absolute $
		resb ($$-$)&1  ; align 2
		;section .bss align=1
___section_bss:
		;resb (___section_startup_text-___section_nobss_end)&(2-1)  ; Align to multiple of 2. We don't do it.

___section_c_bss:
_ledata:	resb 0x400
_wrbuf:		resb 0x200
_rdbuf:		resb 0x200
_ledata_size:	resb 2
_wrfd:		resb 2
_rdfd:		resb 2
_wri:		resb 2
_rdlimit:	resb 2
_rsize:		resb 2
_rdi:		resb 2

; --- Uninitialized .bss used by _start.    ___section_startup_ubss:
___section_startup_ubss:

argv_bytes	resb 270
argv_pointers	resb 130

___section_ubss_end:

;___initial_sp	equ ___section_startup_text+((___section_ubss_end-___section_bss+___section_nobss_end-___section_startup_text+___stack_size+1)&~1)  ; Word-align stack for speed.
;___sd_top__	equ 0x10+((___initial_sp-___section_startup_text+0xf)>>4)  ; Round top of stack to next para, use para (16-byte).
