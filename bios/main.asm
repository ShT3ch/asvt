
.model tiny

.code
org 100h
start:
	jmp main

fromUserNewMode dw 07h
fromUserNewPage dw 02h

currentRow dw 0

lastColor db 00h


endStringOut dw 23

SymTable db '0123456789ABCDEF'
Buf2Out db 'MODE: ____   PAGE: ____',13,10,'$'
fPlace dw 6
sPlace dw 19

attributeArray db 00h,00101100b,11101001b,00011010b,00101001b,01001001b,01101001b,01111001b,01011010b,01101010b,01111010b,00111010b,00011100b,00111100b,00101100b,01111100b,01011100b

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

blinkDisable proc near
	push ax
	push bx
	
	mov ax, 1003h
	xor bx, bx
	int 10h

	pop bx
	pop ax
	ret
endp blinkDisable

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

outAdresForm proc near
				push bp
				mov bp, sp
				push bx
				push cx
				
				;si:di
				
				cld
				
				lea bx, SymTable
				
				lea di,Buf2Out
				add di, [fPlace]
				
				mov dx, [bp+4]
				
				mov al,dh
				shr al,4
				
				xlat 
				stosb
				
				mov al,dh
				and al,0fh
				
				xlat 
				stosb
				
				mov al,dl
				shr al,4
				
				xlat 
				stosb
				
				mov al,dl
				and al,0fh
				
				xlat 
				stosb
				
				
				lea di,Buf2Out
				add di, [sPlace]
				mov dx, [bp+6]
				
				mov al,dh
				shr al,4
				
				xlat 
				stosb
				
				mov al,dh
				and al,0fh
				
				xlat 
				stosb
				
				mov al,dl
				shr al,4
				
				xlat 
				stosb
				
				mov al,dl
				and al,0fh
				
				xlat 
				stosb
				
								
				pop cx
				pop bx
				pop bp

				ret 4
outAdresForm endp

main:
	call saveCurrentModeAndPage

	push fromUserNewMode
	call setVideoMode

	push fromUserNewPage
	call setVideoPage

	;call blinkDisable
	
	call printGrid

	
	push es
		
		push cs
		pop es
		
		push currentVideoPage
		push currentVideoMode
		call outAdresForm
		
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
		
		lea bp, Buf2Out
		int 10h
	pop es
	
	xor ax,ax
	int 16h

	call restoreSavedVideoModeAndPage

	mov ax,4c00h
	int 21h
end start