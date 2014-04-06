.model tiny

.code
org 100h
locals @@
start:
	jmp main

debugOn equ 0
print macro a
	local @@start, @@msg
	push ax dx ds
	jmp @@start
@@msg db a, 0Dh, 0Ah,'$'
@@start:
	mov ah,9
	lea dx, @@msg
	int 21h
	pop ds dx ax
endm

debug macro a
if debugOn
	print a
endif
endm

maxKeywordLength equ 20
maxValueLength equ 20

; enum ArgType {
ArgType_None equ 1
ArgType_HexNum equ 2
; }
KeyWordDescription struc ; for docs on struct: http://www.kolasc.net.ru/cdo/programmes/assembler/strukt.html
	keyWord db maxKeywordLength dup(0)
	keyWordLength dw ?
	argType dw ?
	value db maxValueLength dup(0)
	valueLength dw ?
	wasSet dw 0
KeyWordDescription ends

; enum State {
State_Error equ 0
State_SpacesBeforeKeyWord equ 1
State_KeyWordPrefixChar equ 2
State_KeyWord equ 3
State_BeforeValue equ 4
State_Value equ 5
; }


keyWordPrefixChar equ '-'


isCharInRange macro char, from, to
	local @@notInRange, @@end
	mov ax, char
	cmp al, from
	jb @@notInRange
	cmp al, to
	ja @@notInRange
	mov ax, 1
	jmp @@end
@@notInRange:
	mov ax, 0
@@end:
endm

DFA struc
	currentState dw State_SpacesBeforeKeyWord
	currentKeyWord db maxKeywordLength dup(0)
	currentValue db maxValueLength dup(0)
	currentKeyWordIndex dw 0
	currentValueIndex dw 0
	currentDescription dw 0
DFA ends


errorTransition proc near
	arg char:word
	debug "error"
	ret 2
endp

beforeKeyWordSpacesTransition proc near
	arg char:word
	push bp
	mov bp, sp
	
	debug 'beforeKeyWordSpaces'
	isCharInRange char, 0Ah, ' '
	cmp ax, 1 ; if new char is whitespace too
	je @@transEnd ; then there's nothing to do here
	mov ax, char 
	cmp al, keyWordPrefixChar ; if it's not our prefix char (i.e. '-')
	jne @@onError ; we go erroneous
	mov [parsingDFA.currentState], State_KeyWordPrefixChar
	jmp @@transEnd
@@onError:
	mov [parsingDFA.currentState], State_Error
@@transEnd:

	pop bp
	ret 2
endp

readCharIntoCurrentKeyWord proc near
	arg char:word
	push bp
	mov bp, sp
	push bx
	
	mov ax, char
	mov bx, [parsingDFA.currentKeyWordIndex]
	mov parsingDFA.currentKeyWord[bx], al ; put current char at the last position of currently reading word
	inc bx
	mov [parsingDFA.currentKeyWordIndex], bx ; increment currentKeyWordIndex

	pop bx bp
	ret 2
endp

readCharIntoCurrentValue proc near
	arg char:word
	push bp
	mov bp, sp
	push bx
	
	mov ax, char
	mov bx, [parsingDFA.currentValueIndex]
	mov parsingDFA.currentValue[bx], al ; put current char at the last position of currently reading word
	inc bx
	mov [parsingDFA.currentValueIndex], bx ; increment currentKeyWordIndex

	pop bx bp
	ret 2
endp

isKeyWordish proc near
	arg char:word
	push bp
	mov bp, sp
	
	isCharInRange char,'A','Z'
	cmp ax, 1
	je @@ok
	isCharInRange char,'a','z'
	cmp ax, 1
	je @@ok
	mov ax, char
	cmp al, keyWordPrefixChar
	je @@ok
@@notOk:
	mov ax, 0
	pop bp
	ret 2
@@ok:
	mov ax, 1
	pop bp
	ret 2
endp

keyWordPrefixCharTransition proc near
	arg char:word
	push bp
	mov bp, sp
	
	debug 'keyWordPrefixChar'
	push char
	call isKeyWordish
	cmp ax, 0
	je @@onError ; if in prefix state we read non-alphabetic symbol, error
	mov [parsingDFA.currentState], State_KeyWord ; else change state cause we began reading keyword
	push char
	call readCharIntoCurrentKeyWord
	jmp @@transEnd
@@onError:
	mov [parsingDFA.currentState], State_Error
@@transEnd:
	pop bp
	ret 2
endp

findDescriptionOfCurrentKeyWord proc near
	push bx di si es cx
	
	mov bx, 0
	push cs
	pop es ; for repne cmps
@@forAlldescriptions_Loop:
	lea di, descriptions[bx].keyWord
	mov si, offset parsingDFA.currentKeyWord
	mov cx, descriptions[bx].keyWordLength
	cmp [parsingDFA.currentKeyWordIndex], cx ; if lengths differ,
	jne @@forAlldescriptions_LoopNext
	cld                     ;increment di after each character
	repe cmpsb
	jne @@forAlldescriptions_LoopNext
	jmp @@foundDescription
@@forAlldescriptions_LoopNext:
	add bx, type(KeyWordDescription)
	cmp bx, descriptionsSize * type(KeyWordDescription)
	jae @@forAlldescriptions_LoopEnd
	jmp @@forAlldescriptions_Loop
@@forAlldescriptions_LoopEnd:
	mov ax, 0
	jmp @@end
@@foundDescription:
	mov ax, bx
	add ax, offset(descriptions)
@@end:
	pop cx es si di bx
	ret
endp


keyWordTransition proc near
	arg char:word
	push bp
	mov bp, sp
	push bx
	
	debug 'keyWord'
	push char
	call isKeyWordish
	cmp ax, 0
	je @@onEndOfWord ; if in prefix state we read non-alphabetic symbol, key word ended
	push char
	call readCharIntoCurrentKeyWord
	jmp @@transEnd
@@onEndOfWord:
	call findDescriptionOfCurrentKeyWord
	cmp ax, 0 ; if not found
	je @@onError
	mov [parsingDFA.currentDescription], ax
	mov bx, [parsingDFA.currentDescription]
	mov ax, [bx.wasSet] 
	cmp ax, 1 ; if was already set
	jne @@notAlreadySet
	print "The key was already set. Ignoring new value if any."
@@notAlreadySet:
	mov ax, 1
	mov [bx.wasSet], ax
	mov bx, [bx.argType]
	sal bx, 1 ; bx indexes double words
	mov ax, statesOfArgTypes[bx]
	mov [parsingDFA.currentState], ax
	mov ax, 0
	mov [parsingDFA.currentKeyWordIndex], ax
	jmp @@transEnd
@@onError:
	mov [parsingDFA.currentState], State_Error
@@transEnd:

	pop bx bp
	ret 2
endp

isValuable proc near
	arg char:word
	push bp
	mov bp, sp
	
	isCharInRange char,'0','9'
	cmp ax, 1
	je @@ok
	isCharInRange char,'A','D'
	cmp ax, 1
	je @@ok
	isCharInRange char,'a','d'
	cmp ax, 1
	je @@ok
@@notOk:
	mov ax, 0
	pop bp
	ret 2
@@ok:
	mov ax, 1
	pop bp
	ret 2
endp

beforeValueTransition proc near
	arg char:word
	push bp
	mov bp, sp
	
	debug 'beforeValue'
	isCharInRange char, 0Ah, ' '
	cmp ax, 1 ; if new char is whitespace too
	je @@transEnd ; then there's nothing to do here
	push char
	call isValuable
	cmp ax, 1
	jne @@onError
	mov [parsingDFA.currentState], State_Value
	push char
	call readCharIntoCurrentValue
	jmp @@transEnd
@@onError:
	mov [parsingDFA.currentState], State_Error
@@transEnd:

	pop bp
	ret 2
endp

copyDescriptionFromDFA proc near
	push bx di si es cx
	mov bx, [parsingDFA.currentDescription] ; beggining of currentDescription is in bx now
	mov ax, [bx.valueLength]
	cmp ax, 0
	jne @@alreadySet
	push cs
	pop es
	mov si, offset parsingDFA.currentValue
	lea di, [bx.value]
	mov cx, [parsingDFA.currentValueIndex]
	mov [bx.valueLength], cx ; copy current value length
	cld
	rep movsb ; and copy the contents
	jmp @@end
@@alreadySet:
@@end:
	pop cx es si di bx
	ret
endp

valueTransition proc near
	arg char:word
	push bp
	mov bp, sp
	push bx
	
	debug 'value'
	push char
	call isValuable
	cmp ax, 0
	je @@onEndOfVal ; if in prefix state we read non-valuable symbol, value ended
	push char
	call readCharIntoCurrentValue
	jmp @@transEnd
@@onEndOfVal:
	call copyDescriptionFromDFA
	mov ax, 0
	mov [parsingDFA.currentValueIndex], ax
	isCharInRange char, 0Ah, ' '
	cmp ax, 1
	jne @@onError
	mov [parsingDFA.currentState], State_SpacesBeforeKeyWord
	jmp @@transEnd
@@onError:
	mov [parsingDFA.currentState], State_Error
@@transEnd:

	pop bx bp
	ret 2
endp

; transition functions of DFA related to its states
; (indices are the State enum values)
; every transition has "arg char:word" signature
transitionsOfStates dw \
						offset errorTransition, \
						offset beforeKeyWordSpacesTransition, \
						offset keyWordPrefixCharTransition, \
						offset keyWordTransition, \
						offset beforeValueTransition, \
						offset valueTransition

statesOfArgTypes dw 0, \ ; no such ArgType
					State_SpacesBeforeKeyWord , \ ;for ArgType_None
					State_BeforeValue ;for ArgType_HexNum
parsingDFA DFA <> ; <> for default constructor

;returns whether it parsed successfully
parseCl proc near
	push bx cx dx di si es
	mov cx, 0
	mov bx, 80h
	mov cl, [bx]
	inc cx ; endline at the end
	mov si, 81h
@@forEachChar_Loop:
	; al = curr char
	mov ah, 0
	mov al, [si]
	mov bx, [parsingDFA.currentState]
	sal bx, 1 ; x2
	push ax
	call transitionsOfStates[bx]
	mov ax, [parsingDFA.currentState]
	cmp ax, State_Error
	je @@error
	inc si
	dec cx
	jcxz @@forEachChar_LoopEnd
	jmp @@forEachChar_Loop
@@error:
	debug 'parse error'
@@forEachChar_LoopEnd:
	mov ax, [parsingDFA.currentState]
	cmp ax, State_SpacesBeforeKeyWord
	je @@ok
	mov ax, 0
	jmp @@end
@@ok:
	mov ax, 1
@@end:
	pop es si di dx cx bx
	ret
endp


descriptions:
asdDescr KeyWordDescription <'page', 4, ArgType_None>
asdfDescr KeyWordDescription <'mode', 4, ArgType_HexNum>
ololonDescr KeyWordDescription <'ololon', 6, ArgType_HexNum>
descriptionsSize = ($ - descriptions) / type(KeyWordDescription)

main:
	call parseCl
	cmp ax, 1
	jne @@help
	jmp @@checkArgs
@@help:
	print "Not parsed. Your help goes here."
	ret
@@checkArgs:
	print "Ok, parsed."
	ret
end start
