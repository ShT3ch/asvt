
.model tiny

.code
org 100h        

print macro a
	local @@start, @@msg
	push ax dx ds
	jmp @@start
@@msg db a,'$'
@@start:
	mov ah,9
	lea dx, @@msg
	int 21h
	pop ds dx ax
endm

lastColor db 00h

attributesForRow db 00h

attribArray db 00h,00100100b,01100001b,10010010b,00100001b,01000001b,01100001b,01110001b,01000010b,01100010b,01110010b,00110010b,00010100b,00110100b,00100100b,01110100b,01010100b
					

widthInVideoMode dw 40,40,80,80,-1,-1,-1,80
heightInVideoMode dw 25,25,25,25,-1,-1,-1,25

middleBeginColumnIndexInVideoMode dw 4,4,24,24,-1,-1,-1,24
middleBeginRowIndexInVideoMode dw 5,5,5,5,-1,-1,-1,5

savedVideoMode db 00h
currentNumberOfColums db 00h
savedVideoPage db 00h

currentVideoMode db 00h
currentVideoPage db 00h

start:	
	mov ax,4c00h
	int 21h

end start

saveCurrentModeAndPage proc near
	push ax
	push bx
	pushf
	
	mov ah,0fh
	int 10h ; al<-mode; ah<-column number; bh<-current video page
	mov savedVideoMode, al
	mov savedVideoPage, bh
	mov currentNumberOfColums, ah
	
	popf
	pop bx
	pop ax
endp saveCurrentModeAndPage

restoreSavedVideoModeAndPage proc near
	push ax
	pushf
	
	push savedVideoMode
	call setVideoMode
	
	push savedVideoPage
	call setVideoPage
	
	popf
	pop ax
	ret
endp restoreSavedVideoModeAndPage

getAttribute proc
	arg rowNumber:byte
	push bp
	mov bp,sp
	push ax
	pushf
	
	test rowNumber,rowNumber
	jne @@otherRows
@@firstRow:
	inc lastColor
	and lastColor, 07f
	
	move bl,00001000b
	or bl, lastColor
	jmp @@exit
@@otherRows:
	move bl, attribArray[rowNumber]
@@exit:	
	popf
	pop ax
	pop bp
	ret 1
endp setVideoMode


setVideoMode proc
	arg toMode:byte
	push bp
	mov bp,sp
	push ax
	pushf
	
	xor ah,ah ;ah = 00h
	mov al, toMode
	int 10h ; al->mode

	mov currentVideoMode, toMode
	
	popf
	pop ax
	pop bp
	ret 1
endp setVideoMode

setVideoPage proc near
	arg toPage:byte
	push bp
	mov bp,sp
	push ax
	pushf
	
	mov ah,05h
	mov al, toPage
	int 10h
	
	mov currentVideoPage, toPage
	
	popf
	pop ax
	pop bp
	ret 1
endp setVideoPage