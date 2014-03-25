
.model tiny

.code
org 100h
start:
	jmp main

fromUserNewMode dw 03h
fromUserNewPage dw 03h

currentRow dw 0

lastColor db 00h

OutTemplate db 'MODE: ___   PAGE: ___'
endStringOut dw 21
modeShift db 06
modePage db 18

attributeArray db 00h,00100100b,01101001b,10010010b,00100001b,01000001b,01100001b,01110001b,01000010b,01100010b,01110010b,00110010b,00010100b,00110100b,00100100b,01110100b,01010100b

widthInVideoMode dw 40,40,80,80,-1,-1,-1,80
heightInVideoMode dw 25,25,25,25,-1,-1,-1,25

beginColumnIndex dw 4,4,24,24,-1,-1,-1,24
beginRowIndex dw 5,5,5,5,-1,-1,-1,5

savedVideoMode dw 00h
currentNumberOfColums db 00h
savedVideoPage dw 00h

currentVideoMode dw 00h
currentVideoPage dw 00h


printSymbol proc near
	arg symbol:word
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	pushf

	push currentRow
	call getAttribute ; Теперь в bl лежит байт аттрибута

	mov ah, 09h
	mov al, byte ptr symbol
	mov bh, byte ptr currentVideoPage
	mov cx, 1 ; Repeat factor
	int 10h

	call shift2Right
	popf
	pop cx
	pop bx
	pop ax
	pop bp
	ret 2
printSymbol endp

shift2Right proc near
	push ax
	push bx
	push cx
	push dx
	
	mov ah, 03h
	mov bh, byte ptr currentVideoPage
	int 10h
	
	inc dl
	
	mov ah, 02h
	mov bh, byte ptr currentVideoPage
	int 10h
	
	;Ввод: АН = 03
	;ВН = номер страницы
	;Вывод: DH, DL = строка и столбец текущей позиции курсора
	;СН, CL = первая и последняя строки курсора
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp shift2Right


printLine proc near
	arg  y:word, x:word,initialSymbol:word
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	pushf

	mov ah, 02h
	mov bh, byte ptr currentVideoPage
	mov dh, byte ptr x
	mov dl, byte ptr y
	int 10h

	mov bx, initialSymbol ; Current symbol
	mov cx, initialSymbol
	add cx, 16 ; End symbol

	push bx
	call printSymbol

@@printLineFor:
	inc bx
	cmp bx, cx
	je @@printLineEndFor

	push ' '
	call printSymbol
	push bx
	call printSymbol

	jmp @@printLineFor
@@printLineEndFor:

	popf
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 6
printLine endp

printGrid proc near
	push ax
	push bx
	push cx
	push dx
	pushf

	mov bx, currentVideoMode
	mov dx, currentRow
	add bx,bx
	mov bx, beginRowIndex[bx] ; Начальный y
	mov cx, 0 ; Начальный символ

@@printGridFor:
	mov currentRow, dx

	push bx
	push cx
	push bx
	mov bx, currentVideoMode
	add bx,bx
	push [beginColumnIndex+bx] ; Начальный x
	call printLine
	pop bx

	add cx, 16
	inc bx
	inc dx

	cmp dx, 16
	jne @@printGridFor

	popf
	pop dx
	pop cx
	pop bx
	pop ax
	ret
printGrid endp

saveCurrentModeAndPage proc near
	push ax
	push bx
	pushf
	
	mov ah,0fh
	int 10h ; al<-mode; ah<-column number; bh<-current video page
	mov byte ptr savedVideoMode, al
	mov byte ptr savedVideoPage, bh
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

getAttribute proc near
	arg rowNumber:word
	push bp
	mov bp,sp
	push ax
	pushf
	
	mov ax, rowNumber

	test ax,ax
	jne @@otherRows
@@firstRow:
	inc lastColor
	and lastColor, 07h
	
	mov bl,00001000b
	or bl, lastColor
	jmp @@exit
@@otherRows:
	mov bx,ax
	mov bl, attributeArray[bx]
@@exit:	
	popf
	pop ax
	pop bp
	ret 2
endp getAttribute


setVideoMode proc near
	arg toMode:word
	push bp
	mov bp,sp
	push ax
	pushf
	
	xor ah,ah ;ah = 00h
	mov al, byte ptr toMode
	int 10h ; al->mode

	mov ax,toMode
	mov currentVideoMode, ax
	
	popf
	pop ax
	pop bp
	ret 2
setVideoMode endp

setVideoPage proc near
	arg toPage:word
	push bp
	mov bp,sp
	push ax
	pushf
	
	mov ah,05h
	mov al,byte ptr toPage
	int 10h
	
	mov ax,toPage
	mov currentVideoPage, ax
	
	popf
	pop ax
	pop bp
	ret 2
endp setVideoPage

main:
	call saveCurrentModeAndPage

	push fromUserNewMode
	call setVideoMode

	push fromUserNewPage
	call setVideoPage

	call printGrid

	push es
	
		mov ah, 13h
		
		mov al, 00000001b
		mov cx, endStringOut
		
		push 0
		call getAttribute  ;bl <- attr
		push 0
		call getAttribute  ;bl <- attr
		
		mov bp, currentVideoMode
		add bp,bp
		mov dh, 00h
		mov dl, byte ptr beginColumnIndex[bp]
		mov bh, byte ptr currentVideoPage
		
		push cs
		pop es
		
		lea bp, OutTemplate
		int 10h
	pop es
	
	xor ax,ax
	int 16h

	call restoreSavedVideoModeAndPage

	mov ax,4c00h
	int 21h
end start