.model tiny

jumps ; fixes jump out of range somehow
.code
org 100h

locals @@ ; from now on each identifier beginning from @@ will only work in the scope it was declared
start:
	jmp main
oldInt9Seg dw ?
oldInt9Off dw ?
hexAlphabet db "0123456789ABCDEF"

scanBufferLen equ 3
scanBuffer db scanBufferLen dup (?)
scanBufferHead dw (offset scanBuffer)
scanBufferTail dw (offset scanBuffer)
overflowMessage db "Overflow",0Dh,0Ah,"$"
scanBufferEnd equ (offset scanBufferHead)

isScanBufferEmpty proc near
	mov ax, [scanBufferHead]
	cmp ax, [scanBufferTail]
	je empty
	mov ax, 0
	ret
empty:
	mov ax, 1
	ret
endp

getNextAddressInBx proc near
	inc bx
	cmp bx, scanBufferEnd
	je @@resetToBeginning
	ret
@@resetToBeginning:
	mov bx, offset scanBuffer
	ret
endp

popScanCode proc near
	push bx cx dx
	cli
	mov ax, [scanBufferTail]
	cmp [scanBufferHead], ax
	je @@empty
	mov bx, [scanBufferTail]
	mov al, [bx]
	mov ah, 0
	call getNextAddressInBx
	mov [scanBufferTail], bx
	jmp @@end
@@empty:
	mov ax, 0FFFFh
@@end:
	sti
	pop dx cx bx
	ret
endp

pushScanCodeFromAl proc near
	push ax bx cx dx
	mov bx, [scanBufferHead]
	call getNextAddressInBx
	cmp bx, [scanBufferTail] ; will we meet our tail?
	je @@overflow
	mov cx, bx
	mov bx, [scanBufferHead]
	mov [bx], al
	mov bx, cx
	mov [bx], al
	mov [scanBufferHead], bx
	jmp @@end
@@overflow:
	mov ah, 09h
	mov dx, offset overflowMessage
	int 21h
@@end:
	pop dx cx bx ax
	ret
endp

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

printSpace proc near
	push ax bx cx dx
	mov dl, ' '
	mov ah, 2
	int 21h
	pop dx cx bx ax
	ret
endp

newInt9 proc far
	push ax
	cli
	in al, 60h
	call pushScanCodeFromAl
	mov al, 20h ;Send EOI (end of interrupt)
	out 20h, al ; to the 8259A PIC.
	pop ax
	iret
endp

main:
	mov ax, 3509h
	int 21h
	mov [oldInt9Seg], es
    mov [oldInt9Off], bx
	mov ax, 2509h
    mov dx, offset newInt9
    int 21h
	mov cx, 0
whileIsEmpty:
	mov ax, [scanBufferHead]
	cmp ax, [scanBufferTail]
	je whileIsEmpty
	
	call popScanCode
	
	cmp cx, 0
	jne counterAssigned
	cmp al, 0E0h
	je numKey
	cmp al, 0E1h
	je pauseKey
	mov cx, 1
	jmp counterAssigned
numKey:
	mov cx, 2
	jmp counterAssigned
pauseKey:
	mov cx, 3
	jmp counterAssigned
counterAssigned:
	call printAL
	dec cx
	cmp cx, 0
	je keyPressEnded
	call printSpace
	jmp printed
keyPressEnded:
	call printLine
printed:
	cmp al, 1 ; esc
	jne whileIsEmpty
	
    mov dx, [oldInt9Off]
	mov ax, [oldInt9Seg]
	mov ds, ax
	mov ax, 2509h
    int 21h
	
	push cs
	pop ds
	ret
end start