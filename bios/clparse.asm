.model tiny, c

.code
org 100h
start:
	jmp main

; enum ArgType {
ArgType_None equ 0
ArgType_HexNumber equ 1
; }
KeyWordDescription struc ; for docs on struct: http://www.kolasc.net.ru/cdo/programmes/assembler/strukt.html
	keyWord dw ?
	keyWordLength db ?
	argType db ?
	value dw ?
	wasSet db 0
KeyWordDescription ends

; enum State {
State_Error equ 0
State_SpacesBeforeKeyWord equ 1
State_KeyWordPrefixChar equ 2
State_KeyWord equ 3
State_SpacesAndAssignmentAfterKeyWord equ 4
State_Value equ 5
; }

maxKeywordLength equ 20
maxValueLength equ 20
keywordPrefixChar equ '-'

; returns if char is whiteSpace in ax
isWhitespace proc near
	arg char:byte
	mov al, char
	cmp al, ' '
	je @@whiteSpace
	cmp al, '	'
	je @@whiteSpace
@@notWhiteSpace:
	mov ax, 0
	ret 1
@@whiteSpace:
	mov ax, 1
	ret 1
endp

DFA struc
	stateType db 0
	currentKeyword db maxKeywordLength dup(0)
	currentValue db maxValueLength dup(0)
DFA ends

transition proc near
	arg char:byte, dfaRef:word
	uses ax, bx, cx, dx
	mov cl, [dfaRef.stateType]
	cmp cl, State_Error
	je @@end
@@onError:
	mov [dfaRef.stateType], State_Error
	jmp @@end
@@on
@@end:
	ret 3
endp


; returns char in al
toUpperCase proc near
	arg char:byte
	mov al, char
	cmp al, 'a'
	jb @@charIsUpperEnough
	cmp al, 'z'
	ja @@charIsUpperEnough
	add al, ('A' - 'a')
@@charIsUpperEnough:
	ret 1
endp

; parseDFA DFA <> ; <> for default constructor
parseCl proc near
	uses bx, cx, dx, di, si, es
	mov cx, 0
	push cx cx
	pop es ds ; both needed for cmpsb
	mov bx, 80h
	mov cl, [bx]
	mov si, 81h
@@forEachChar_Loop:
	; al = curr char
	mov al, [si]
	
	
@@forEachChar_LoopEnd:
	
	ret
endp


asdKey db 'asd'
asdLen equ $ - asdKey

Descriptions KeyWordDescription <asdKey, asdLen, ArgType_None,,>

main:
	
	ret

end start