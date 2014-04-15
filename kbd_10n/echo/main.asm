.model tiny

jumps ; fixes jump out of range somehow
.code
org 100h

locals @@ ; from now on each identifier beginning from @@ will only work in the scope it was declared
start:
	jmp main
len equ 7
bufferSize  db len + 1  ; 20 char + RETURN
inputLength db 0   ; number of read characters
buffer      db len + 1 DUP(0) ; actual buffer

main:
	push cx
	pop dx
	mov dx, offset bufferSize ; load our pointer to the beginning of the structure
	mov ah, 0Ah ; GetLine function
	int 21h
	mov al, '$'
	mov bh, 0
	mov bl, [inputLength]
	mov buffer[bx], al
	mov dl, 0Ah
	mov ah, 2
	int 21h
	mov dl, 0Dh
	mov ah, 2
	int 21h
	mov dx, offset buffer
	mov ah, 9
	int 21h
	ret
end start