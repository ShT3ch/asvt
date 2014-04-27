.model tiny

jumps ; fixes jump out of range somehow
.code
org 100h

locals @@ ; from now on each identifier beginning from @@ will only work in the scope it was declared
start:
	jmp main

oldInt9Seg dw ?
oldInt9Off dw ?
hexAlphabet db "0123456789ABCDEF$"

handleNote macro key, freq
	local next
	cmp al, key
	jne next
	mov bx, freq
	call playSoundFromBx
next:
endm

wasEscapePressed db 0

playSoundFromBx proc
	push ax bx cx dx

	mov     al, 10110110b    ; the magic number (use this binary number only!)
	out     43h, al          ; send it to the initializing port 43h timer 2.

	mov     ax, bx           ; move our frequency value into ax.

	out     42h, al          ; send lsb to port 42h.
	mov     al, ah           ; move msb into al
	out     42h, al          ; send msb to port 42h.

	in      al, 61h          ; get current value of port 61h.
	or      al, 00000011b    ; or al to this value, forcing first two bits high.
	out     61h, al          ; copy it to port 61h of the ppi chip
							 ; to turn on the speaker.


	pop dx cx bx ax
	ret
endp

newInt9 proc far
	push ax bx
	cli
	in al, 60h

	handleNote 02h, 9121d
	handleNote 03h, 8611d
	
	test al, 80h
	jnz @@endSound
	
	cmp al, 01h
	je @@escape	
	jmp @@end

@@endSound:
	in      al, 61h
	and     al, 11111100b
	out     61h, al
	jmp @@end
@@escape:
	mov [wasEscapePressed], 1
	jmp @@end
@@end:
	
	mov al, 20h ;Send EOI (end of interrupt)
	out 20h, al ; to the 8259A PIC.
	pop bx ax
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
whileIsNotEsc:
	cmp [wasEscapePressed], 1
	jne whileIsNotEsc
	
    mov dx, [oldInt9Off]
	mov ax, [oldInt9Seg]
	mov ds, ax
	mov ax, 2509h
    int 21h
	
	ret
end start