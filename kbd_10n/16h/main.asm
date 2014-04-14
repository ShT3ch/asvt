.model tiny

jumps ; fixes jump out of range somehow
.code
org 100h

locals @@ ; from now on each identifier beginning from @@ will only work in the scope it was declared
start:
	jmp main

hexAlphabet db "0123456789ABCDEF"
header db "ascii  scan  symbol$"

printAL proc near
	push ax bx cx dx
	mov cl, al
	mov bh, 0
	mov bl, cl
	sar bx, 4
	mov dl, hexAlphabet[bx]
	mov ah, 2
	int 21h
	mov bl, cl
	and bl, 0Fh
	mov dl, hexAlphabet[bx]
	mov ah, 2
	int 21h
	pop dx cx bx ax
	ret
endp

printLine proc near
	push ax bx cx dx
	mov dl, 0Ah
	mov ah, 2
	int 21h
	mov dl, 0Dh
	mov ah, 2
	int 21h
	pop dx cx bx ax
	ret
endp

printSpaces proc near
	push ax bx cx dx
	mov cx, 6
@@loop:
	mov dl, ' '
	mov ah, 2
	int 21h
	dec cx
	jcxz @@end
	jmp @@loop
@@end:
	pop dx cx bx ax
	ret
endp
	
printSymbolFromAL proc near
	push ax bx cx dx
	mov cl, al
	mov ah, 0fh
	int 10h ; al<-mode; ah<-column number; bh<-current video page
	mov ah, 0Ah ; print char func
	mov al, cl
	mov cx, 1
	int 10h
	pop dx cx bx ax
	ret
endp

main:
	
	mov dx, offset header
	mov ah, 9
	int 21h
	call printLine
whileNotEscPressed:
	mov ax, 0
	int 16h
	mov cx, ax
	call printAL
	call printSpaces
	mov al, ah
	call printAL
	call printSpaces

	; print symbol
	mov al, cl
	call printSymbolFromAL
	
	call printLine
	cmp cl, 1Bh
	jne whileNotEscPressed
	ret
end start