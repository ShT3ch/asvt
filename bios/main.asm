.model tiny

jumps ; fixes jump out of range somehow
.code
org 100h

locals @@ ; from now on each identifier beginning from @@ will only work in the scope it was declared
start:
	jmp main

fromUserNewMode dw 07h
fromUserNewPage dw 02h


BIOS_additionalOffsetToCurrentPage equ 044Eh
BIOS_currentColumnsNumberOffset equ 044Ah
BIOS_pageNumberOffset equ 0462h
BIOS_modeOffset equ 0449h
BIOS_lastRowNumberOffset equ 0484h

currentRow dw 0

lastColor db 00h


endStringOut dw 23

rowNumberMem db 0
columnNumberMem db 0


promt db 'Gogo any key!'
promtLength equ 13

SymTable db '0123456789ABCDEF'
ModeBuf db 'MODE: __'
PageBuf db 'PAGE: __'
ModeBeg equ 6
ModeLen equ 8
PageBeg equ 6
PageLen equ 8

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


getVideoPage proc
	push es bx

	mov ax, 0
	mov es, ax
	mov bx, BIOS_pageNumberOffset
	mov al, byte ptr es:[bx]

	pop bx es
	ret
endp

getVideoMode proc
	push es bx

	mov ax, 0
	mov es, ax
	mov bx, BIOS_modeOffset
	mov al, byte ptr es:[bx]

	pop bx es
	ret
endp


getColumnsCount proc
	push es bx

	mov ax, 0
	mov es, ax
	mov bx, BIOS_currentColumnsNumberOffset
	mov ax, es:[bx]

	pop bx es
	ret
endp

getRowsCount proc
	push es bx

	mov ax, 0
	mov es, ax
	mov bx, BIOS_lastRowNumberOffset
	mov al, es:[byte ptr bx]
	inc ax

	pop bx es
	ret
endp


getAdditionalPageOffset proc
	push es bx

	mov ax, 0
	mov es, ax
	mov bx, BIOS_additionalOffsetToCurrentPage
	mov ax, es:[bx]

	pop bx es
	ret
endp

getGraphicBufferSegment proc
	call getVideoMode
	cmp ax, 7
	je @@mode7
	jmp @@otherMode
@@mode7:
	mov ax, 0B000h
	ret
@@otherMode:
	mov ax, 0B800h
	ret
endp

printChar proc
	arg x:word, y:word, attrsAndChar:word
	push bp
	mov bp,sp
	push bx cx dx es ax

	call getColumnsCount
	mov dx, y
	mul dl ; ax = y * colCount
	mov dx, x
	add ax, dx
	sal ax, 1
	mov bx, ax
	
	call getAdditionalPageOffset
	add bx, ax
	
	call getGraphicBufferSegment
	mov es, ax
	
	mov ax, attrsAndChar
	mov es:[bx], ax
	
	pop ax es dx cx bx bp
	ret 6
endp

printLine proc
	arg x:word, y:word, strRef:word, len:word, attr:word
	push bp
	mov bp,sp
	push bx cx dx di si ax
	
	
	mov si, x
	mov di, y
	
	mov ax, attr
	mov ah, al ; attr in ah
		
	mov bx, strRef
	mov cx, len
	add cx, bx ; cx = the 1st byte after string
@@loop:
	mov al, byte ptr [bx]
	push ax
	push di
	push si
	call printChar
	inc bx
	inc si
	cmp bx, cx
	je @@end
	jmp @@loop
@@end:
	pop ax si di dx cx bx bp
	ret 10
endp

printPromt proc
	mov ax, 00001001b
	push ax ; attr
	mov ax, promtLength
	push ax ; len
	mov ax, offset promt
	push ax ; strRef
	call getRowsCount
	dec ax
	push ax ; row
	mov ax, 0
	push ax ; col
	call printLine
	ret
endp

printGrid proc
	push ax bx cx dx di si
	call getColumnsCount
	sub ax, 16 * 2
	sar ax, 1
	mov di, ax  ; di = first col of grid
	call getRowsCount
	sub ax, 16
	sar ax, 1
	mov si, ax	; si = first row of grid
	
	mov cx, 0 ; cx = char code
@@loop:
	mov ax, cx
	and ax, 0Fh ; ax = col num
	mov dx, cx
	sar dx, 4 ; dx = row num
	push dx
	call getAttribute ; bl = attr
	mov bh, bl ; i need attr in bh, not bl
	mov bl, cl
	
	sal ax, 1
	
	add dx, si
	add ax, di

	
	push bx
	push dx
	push ax
	call printChar

	inc cx
	test cx, 0Fh
	je @@noSpace
	inc ax
	mov bl, ' '
	push bx
	push dx
	push ax
	call printChar
@@noSpace:	
	
	cmp cx, 100h
	je @@endLoop
	jmp @@loop
@@endLoop:
	
	pop si di dx cx bx ax
	ret
endp

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
	;462
	push savedVideoMode
	call setVideoMode
	
	push savedVideoPage
	call setVideoPage
	
	popf
	pop ax
	ret
endp restoreSavedVideoModeAndPage

; returns bl
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

proc printPageAndMode
	push bx cx dx es bp
	push cs
	pop es
	
	; prepare buffers to output
	mov bx, [currentVideoMode]
	sar bx, 4
	mov al, SymTable[bx]
	mov [ModeBuf + ModeBeg], al
	mov bx, [currentVideoMode]
	and bx, 0Fh
	mov al, SymTable[bx]
	mov [ModeBuf + ModeBeg + 1], al

	mov bx, [currentVideoPage]
	sar bx, 4
	mov al, SymTable[bx]
	mov [PageBuf + PageBeg], al
	mov bx, [currentVideoPage]
	and bx, 0Fh
	mov al, SymTable[bx]
	mov [PageBuf + PageBeg + 1], al
	
	mov cx, PageLen ; length
	mov dh, 0
	call getColumnsCount
	mov dx, ax
	sub dx, PageLen
	sar dx, 1 ; x of header
	mov bx, 0 ; y of header
	mov bp, 00001001b ; attr
	mov ax, offset ModeBuf ; strRef
	push bp cx ax bx dx
	call printLine
	inc bx
	mov ax, offset PageBuf
	push bp cx ax bx dx
	call printLine
	pop bp es dx cx bx
	ret
endp

;====================================begin of command line parser. lasciate ogni speranza voi ch'entrate

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
	pop es ; for repe cmps
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
	mov bx, [parsingDFA.currentDescription]   ;what that fuck? Bx or parsingDFA?
	mov ax, [bx.wasSet] 					; why different? why not parsingDFA?
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
	mov ax, 0 ;xor...
	mov [parsingDFA.currentKeyWordIndex], ax
	jmp @@transEnd
@@onError:
	mov [parsingDFA.currentState], State_Error
@@transEnd:

	pop bx bp
	ret 2
endp

isValuable proc near    ; 0x4h, for ex? :)
	arg char:word
	push bp
	mov bp, sp
	
	isCharInRange char,'0','9'
	cmp ax, 1
	je @@ok
	isCharInRange char,'A','F'
	cmp ax, 1
	je @@ok
	isCharInRange char,'a','f'
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
	cmp ax, 0 ;test...
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
modeDescr KeyWordDescription <'mode', 4, ArgType_HexNum>
pageDescr KeyWordDescription <'page', 4, ArgType_HexNum>
blinkDescr KeyWordDescription <'blink', 5, ArgType_None>
descriptionsSize = ($ - descriptions) / type(KeyWordDescription) ; DescriptionsNumber, bleat`

;====================================end of command line parser. that was tough, wasn't it?

proc parseHexOct ; in: dl = char; out: dl = number
	push ax
	isCharInRange dx, '0', '9'
	cmp ax, 1
	je @@numeric
@@alphabetic:
	and dl, 11011111b ; to uppercase
	sub dl, 'A' - 0Ah
	jmp @@end
@@numeric:
	sub dl, '0'
@@end:
	pop ax
	ret
endp


proc parseHexByteFromDescription
	arg descr:word
	push bp
	mov bp, sp
	push bx cx dx
	
	mov ax, 0
	mov dx, 0
	mov bx, descr
	mov cx, [bx.valueLength] 
	cmp cx, 1 ; 
	je @@oneDigit
	cmp cx, 2 ; 
	je @@twoDigits
	mov ax, 0FF00h
	jmp @@end
@@oneDigit:
	mov dl, bx.value[0]
	call parseHexOct
	mov al, dl
	mov ah, 0
	jmp @@end
	
@@twoDigits:
	mov dl, bx.value[0]
	call parseHexOct
	sal dl, 4
	mov al, dl
	mov dl, bx.value[1]
	call parseHexOct
	add al, dl
	mov ah, 0
	jmp @@end
@@end:
	pop dx cx bx bp
	ret 2
endp


proc parseClAndHandleTroubles
	call parseCl
	cmp ax, 1
	jne @@notParsed
	jmp @@checkArgs
@@notSpecified:
	print "Both -page and -mode must be specified"
	jmp @@help
@@notParsed:
	print "Can't parse some piece of command line."
@@help:
	print "TODO: Your help goes here."
	mov ax, 0
	ret
@@checkArgs:
	mov dx, [modeDescr.wasSet] ; must be true
	mov ax, [pageDescr.wasSet] ; and this one too (maybe change this to set default pages)
	add ax, dx
	cmp ax, 2 ; if both were specified
	jne @@notSpecified
	mov ax, offset modeDescr
	push ax
	call parseHexByteFromDescription
	cmp ax, 0FF00h ;error code
	je @@badMode
irp mode, <0,1,2,3,7>
	cmp ax, mode
	je @@goodMode
endm
@@badMode:
	print "Mode must be a one or two-digit hex string, one of 0, 1, 2, 3 or 7"
	jmp @@help
@@goodMode:
	mov [fromUserNewMode], ax
	mov ax, offset pageDescr
	push ax
	call parseHexByteFromDescription
	cmp ax, 0FF00h ;error code
	je @@badPage
	mov [fromUserNewPage], ax
	jmp @@ok
@@badPage:
	print "Page must be a one or two-digit hex string."
	jmp @@help
@@ok:
	print "Command line is ok"
	mov ax, 1
	ret
endp

main:
	
	call parseClAndHandleTroubles
	cmp ax, 0
	je @@end
	call saveCurrentModeAndPage

	push fromUserNewMode
	call setVideoMode

	push fromUserNewPage
	call setVideoPage

	mov ax, [blinkDescr.wasSet]
	cmp ax, 1
	je @@enableBlink
	call blinkDisable
@@enableBlink:

	call printGrid

	
	call printPageAndMode
	call printPromt
	xor ax,ax
	int 16h

	call restoreSavedVideoModeAndPage
@@end:
	mov ax,4c00h
	int 21h
end start