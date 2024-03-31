;
; folink2l.nasm: flat OMF linker implemented in NASM 0.98.39, targeting Linux i386
; by pts@fazekas.hu at Wed Mar 27 03:01:38 CET 2024
;
; This linker just dumps the data in the LEDATA, LEDATA386, LIDATA and
; LIDATA386 records in the input OMF file. For the LIDATA and LIDATA386
; records, it expands the repeated bytes. It fails if it encounters
; multiple segments or any relocations (fixupps).
;
; Compile with: nasm-0.98.39 -O999999999 -w+orphan-labels -f bin -o folink2 folink2l.nasm && chmod +x folink2
;
; Implementation based on folink2.c and on the disassembly of the
; folink2.obj file created by `owcc -v -blinux -s -Os -W -Wall
; -fno-stack-check -fsigned-char -Duu=unsigned -c -o folink2.obj folink2.c'.
;
; The program has been heavily optimized manually for executable byte size:
; from 1120 bytes to 818 bytes.
;

bits 32
cpu 386
program_base equ 8048000h  ; Typical Linux i386 executable program.
org program_base

file_header:
Elf32_Ehdr:
		OSABI_Linux equ 3
		db 7Fh,'ELF',1,1,1,OSABI_Linux,0,0,0,0,0,0,0,0,2,0,3,0
		dd 1,_start,Elf32_Phdr-file_header,0,0
		dw Elf32_Phdr-file_header,20h,1,0,0,0
Elf32_Phdr:	dd 1,0,program_base,0,prebss-program_base,mem_end-program_base,7,1000h

_start:  ; Entry point of the Linux i386 program. Stack: top is argc, then argv[0], argv[1] etc., then NULL, then envp[0], envp[1] etc., then NULL, then ELF aux table.
		pop eax  ; argc.
		mov edi, esp  ; argv.
		;push exit_  ; No need for this if main never returns.
		; Fall through to main_.

; int __watcall __declspec(noreturn) main(int argc, char **argv);
main_:
		mov esi, bss  ; Use [esi+...] effective addresses throughout the program for shorter encoding.
		xor eax, eax
		scasd  ; EDI += 4.
		scasd  ; argv[1] == NULL?
		je .8
		scasd  ; argv[2] == NULL?
		je .8
		scasd  ; argv[3] == NULL?
		je .9
.8:
		xor ebx, ebx
		mov bl, msg_usage.end-msg_usage
		lea edx, [esi-bss+msg_usage]
		mov eax, 2
		call write_
		xor eax, eax
		inc eax  ; EAX := 1.
		jmp exit_
.9:
		mov eax, [edi-0x10+4]  ; argv[1].
		xor edx, edx  ; O_RDONLY.
		call open_
		mov dword [esi-bss+_rdfd], eax
		test eax, eax
		jge .10
		push 14h
		jmp .jfatal1
.10:
		mov ebx, 666o
		mov eax, [edi-0x10+8]  ; argv[2].
		mov edx, 1 | 100o | 01000o  ; O_WRONLY | O_CREAT | O_TRUNC | O_BINARY.
		call open_
		mov dword [esi-bss+_wrfd], eax
		test eax, eax
		jge .11
		push 15h
.jfatal1:	jmp fatal_
.11:
		xor ebx, ebx
		mov byte [esi-bss+main_4], 0
.12:
		or dword [esi-bss+_rsize], 3  ; Any value at least 3 will do.
		call r8_
		mov edx, eax
		call r16_le_
		mov dword [esi-bss+_rsize], eax
		test eax, eax
		ja .13
		push 2
		jmp .jfatal1
.13:
		dec dword [esi-bss+_rsize]
		mov al, dl
		cmp al, 8ah
		je .j36
		cmp al, 8bh
.j36:		je .36
		cmp al, 88h
		je .14
		cmp al, 9ah
		je .14
		cmp al, 96h
		je .14
		cmp al, 98h
		je .14
		cmp al, 99h
		je .14
		cmp al, 80h
		jne .15
.14:
		cmp dword [esi-bss+_rsize], 0
		jbe .j35
		call r8_
		jmp .14
.15:
		cmp al, 0a0h
		jb .34
		cmp al, 0a3h
		jna .16
.34:
		push 12h
		jmp .jfatal2
.16:
		call r8_
		cmp eax, 1
		je .17
		push 0bh
		jmp .jfatal2
.17:
		call r16_le_
		mov ecx, eax
		test dl, 1
		jz .19  ; Not 0a1h or 0a3h.
.18:
		call r16_le_
.19:
		cmp byte [esi-bss+main_4], 0
		je .20
		cmp ecx, ebx
		je .21
		push 4
.jfatal2:	jmp fatal_
		; Not reached.
.20:
		mov ebx, ecx
		inc byte [esi-bss+main_4]
.21:
		test dl, 2
		jnz .24  ; Not 0a0h or 0a2h.
.22:
		add ebx, dword [esi-bss+_rsize]
.23:
		cmp dword [esi-bss+_rsize], 0
.j35:		jbe .35
		call r8_
		movsx eax, al
		call w8_
		jmp .23
.24:
		call r16_le_
		mov ecx, eax
		mov [esi-bss+main_0ch], eax
		cmp dl, 0a3h
		jne .25
		call r16_le_
		shl eax, 10h
		or ecx, eax
		mov [esi-bss+main_0ch], ecx
.25:
		call r16_le_
		mov [esi-bss+main_14h], eax
		test eax, eax
		jnz .26
		push 6
		jmp .jfatal2
.26:		call r16_le_
		mov ecx, eax
		mov [esi-bss+main_8], eax
		cmp dl, 0a3h
		jne .27
		call r16_le_
		shl eax, 10h
		or ecx, eax
		mov [esi-bss+main_8], ecx
.27:
		call r16_le_
		test eax, eax
		je .28
		push 8
		jmp .jfatal2
.28:
		call r8_
		mov ecx, eax
		call r8_
		mov [esi-bss+main_10h], eax
		cmp ecx, 1
		je .29
		push 9
		jmp .jfatal3
.29:
		mov edi, [esi-bss+main_0ch]
.30:
		test edi, edi
		jbe .33
		mov ecx, [esi-bss+main_8]
		add ebx, ecx
.31:
		test ecx, ecx
		jbe .32
		movsx eax, byte [esi-bss+main_10h]
		call w8_
		dec ecx
		jmp .31
.32:
		dec edi
		jmp .30
.33:
		dec dword [esi-bss+main_14h]
		jz .35
		jmp .26
.35:
		inc dword [esi-bss+_rsize]
		call r8_
		cmp dword [esi-bss+_rsize], 0
		je .12
		push 0ah
.jfatal3:	jmp fatal_
		; Not reached.
.36:
		call wflush_
		xor eax, eax  ; EAX := 0.
		jmp exit_
		; Not reached.

; void __watcall wflush(void);
wflush_:
		push ebx
		push edx
		mov ebx, [esi-bss+_wri]
		test ebx, ebx
		jbe .6
		push ebx
		mov eax, [esi-bss+_wrfd]
		lea edx, [esi-bss+_wrbuf]
		call write_
		test eax, eax
		jle .4
		pop ebx  ; EBX := [esi-bss+_wri].
		cmp eax, ebx
		je .5
.4:
		push 7
		jmp main_.jfatal3
.5:
		and dword [esi-bss+_wri], 0
.6:
		pop edx
		pop ebx
		ret

; void __watcall w8(char c);  ; Writes a byte to the output raw binary file.
w8_:
		push edx
		xchg eax, edx  ; DL := AL; EAX := junk; rest of EDX := junk.
		cmp byte [esi-bss+_wri+1], 20h  ; Same as: cmp dword [esi-bss+_wri], 2000h
		jne .7
		call wflush_
.7:
		mov eax, dword [esi-bss+_wri]
		mov byte [esi-bss+_wrbuf+eax], dl
		inc dword [esi-bss+_wri]
		pop edx
		ret


; uu __watcall r8(void);  ; Reads a byte from the input OMF file.
r8_:
		push edx
		cmp dword [esi-bss+_rsize], 0
		jne .1
		push 3
		jmp fatal_
.1:
		mov eax, dword [esi-bss+_rdi]
		cmp eax, dword [esi-bss+_rdlimit]
		jne .3
		push ebx
		mov eax, dword [esi-bss+_rdfd]
		mov ebx, 2000h
		mov edx, _rdbuf  ; Don't access it through esi, because it's relative address is >7h.
		call read_
		pop ebx
		test eax, eax
		jge .2
		push 5
		jmp fatal_
.2:
		mov dword [esi-bss+_rdlimit], eax
		xor eax, eax
.3:
		movzx edx, byte [_rdbuf+eax]  ; Don't access it through esi, because it's relative address is >7h.
		dec dword [esi-bss+_rsize]
		inc eax
		mov dword [esi-bss+_rdi], eax
		xchg eax, edx  ; EAX := EDX; EDX := junk.
		pop edx
		ret

; uu __watcall r16_le(void);  ; Reads 2 bytes in little-endian order from the input OMF file.
r16_le_:
		push edx
		call r8_
		mov edx, eax
		call r8_
		mov dh, al
		xchg eax, edx  ; EAX := EAX; EDX := junk.
		pop edx
		ret

; ssize_t __watcall read(int fd, void *buf, size_t count);
read_:		push byte 3  ; SYS_read.
		jmp short do_syscall3

; ssize_t __watcall write(int fd, const void *buf, size_t count);
write_:		push byte 4  ; SYS_write.
		jmp short do_syscall3

; int __watcall open(const char *pathname, int flags, mode_t mode);
open_:		push byte 5  ; SYS_open.
		jmp short do_syscall3

; void __cdecl_without_return_address __declspec(noreturn) fatal(char exit_code);
fatal_:
		xor ebx, ebx
		mov bl, msg_fatal.end-msg_fatal
		lea edx, [esi-bss+msg_fatal]
		xor eax, eax
		inc eax
		inc eax  ; EAX := 2. STDERR_FILENO.
		call write_
		pop eax  ; exit_code argument of fatal_.
		; Fall through to exit_.

; void __watcall __declspec(noreturn) exit(int exit_code);
exit_:		push byte 1  ; SYS_exit.
		; Fall through to do_syscall3.

; Do Linux i386 syscall (system call) of up to 3 arguments:
;
; * in dword[ESP(+4)]: syscall number, will be popped upon return
; * maybe in EAX (__watcall): arg1
; * maybe in EDX (__watcall): arg2
; * maybe in EBX (__watcall): arg3
; * out EAX: result or -1 on error
; * out EBX: kept intact
; * out ECX: kept intact
; * out EDX: kept intact
;
; For watcall syscall0, EDX, EBX and ECX has to be kept intact.
; For watcall syscall1, EBX and ECX has to be kept intact.
; For watcall syscall2, ECX has to be kept intact.
do_syscall3:	xchg ecx, [esp]	; Keep ECX pushed.
		push edx
		push ebx
		xchg eax, ebx
		xchg eax, edx
		xchg eax, ecx
		int 80h
%if 0  ; Longer but better for other (unused) syscalls.
		cmp eax, -100h  ; Treat very large (e.g. <-0100h; with Linux 5.4.0, 85h seems to be the smallest) non-negative return values as success rather than errno. This is needed by time(2) when it returns a negative timestamp. uClibc has -1000h here.
		jna .8
%else  ; Treat any negative values as -1.
		test eax, eax
		jns .8
%endif
		or eax, byte -1  ; EAX := -1.
.8:		pop ebx
		pop edx
		pop ecx
		ret

msg_fatal:	db 'folink2 fatal error', 10, 0
.end:
msg_usage:	db 'Usage: folink2 <in.obj> <out.bin>', 10, 0
.end:

prebss:		absolute $
		resb ($$-$)&3  ; align 4
bss:

; These variables are initialized to zero by the Linux kernel.
; No extra bytes are generated for them.
main_4:		resb 4  ; Local variable of main_.
main_8:		resb 4  ; Local variable of main_.
main_0ch:	resb 4  ; Local variable of main_.
main_10h:	resb 4  ; Local variable of main_.
main_14h:	resb 4  ; Local variable of main_.
_wrfd:		resb 4
_rdfd:		resb 4
_wri:		resb 4
_rdlimit:	resb 4
_rsize:		resb 4
_rdi:		resb 4
_wrbuf:		resb 2000h
_rdbuf:		resb 2000h  ; We don't access it as [esi-bss+_rdbuf], because it's distance from bss is >7fh.

mem_end:
