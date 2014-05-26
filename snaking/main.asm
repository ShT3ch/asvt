.model tiny

jumps ; fixes jump out of range somehow
.code
org 100h

locals @@ ; from now on each identifier beginning from @@ will only work in the scope it was declared
start:
	jmp main
Note_C equ 9121d
Note_Cs equ 8609d
Note_D equ 8126d
Note_Ds equ 7670d
Note_E equ 7239d
Note_F equ 6833d
Note_Fs equ 6449d
Note_G equ 6087d
Note_Gs equ 5746d
Note_A equ 5424d
Note_B equ 5120d
Note_H equ 4832d
Note_C2 equ 4560d
Note_Cs2 equ 8609d / 2
Note_D2 equ 8126d / 2
Note_Ds2 equ 7670d / 2
Note_E2 equ 7239d / 2
Note_F2 equ 6833d / 2
Note_Fs2 equ 6449d / 2
Note_G2 equ 6087d / 2
	

offSound proc
	push ax
	in      al, 61h
	and     al, 11111100b
	out     61h, al
	pop ax
	ret
endp


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


playNote proc ; bx = freq or 0 if silence, cx = length
	push ax bx cx dx es
	mov ax, 0
	mov es, ax
	add cx, [word ptr es:046Ch]
	cmp bx, 0
	je delay
	call playSoundFromBx
delay:
	cmp [word ptr es:046Ch], cx
	jne delay
	
	call offSound
	
	pop es dx cx bx ax
	ret
endp

playNote_m macro note, len
	mov bx, note
	mov cx, len
	call playNote
endm


TunePlayed db 0
playTune proc
	push ax bx cx dx
	cmp [TunePlayed], 1
	je @@end
	playNote_m Note_E2 1
	playNote_m 0 1
	playNote_m Note_E2 2
	playNote_m 0 2
	playNote_m Note_E2 2
	playNote_m 0 2
	playNote_m Note_D2 2
	playNote_m Note_E2 2
	playNote_m 0 2
	playNote_m Note_G2 2
	playNote_m 0 6
	playNote_m Note_G 2
	
@@end:
	pop dx cx bx ax
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
wallCountDescr KeyWordDescription <'wall-count', 10d, ArgType_HexNum>
wallLengthDescr KeyWordDescription <'wall-length', 11d, ArgType_HexNum>
snakeLengthDescr KeyWordDescription <'snake-length', 12d, ArgType_HexNum>
selfIntersectionsDescr KeyWordDescription <'self-intersection', 17d, ArgType_None>
passableWallsDescr KeyWordDescription <'passable-wall', 13d, ArgType_None>
descriptionsSize = ($ - descriptions) / type(KeyWordDescription)

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


proc parseHexByteFromDescription ; bx = descr; ax => parsed number
	push bx cx dx
	mov ax, 0
	mov dx, 0
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
	pop dx cx bx
	ret
endp


proc parseClAndHandleTroubles
	call parseCl
	cmp ax, 1
	jne @@help
	jmp @@checkArgs
@@help:
	print "USAGE: main -wall-count <0..9> -wall-length <0..6> -snake-length <0..FF> -self-intersection -passable-wall"
	print "wall-count			Wall count. <0..9>"
	print "wall-length			Wall count. <0..6>"
	print "snake-length			Snake length. <0..FF>"
	print "self-intersection	Allows intersection with urself"
	print "passable-wall		Creates the Passable Great Chinese wall in the middle"
	mov ax, 0
	ret
@@checkArgs:
@@checkWallCount:
	cmp [wallCountDescr.wasSet], 1
	jne @@checkWallLength
	mov bx, offset wallCountDescr
	call parseHexByteFromDescription
	cmp ax, 9
	ja @@help
	mov [WallCount], al
@@checkWallLength:
	cmp [wallLengthDescr.wasSet], 1
	jne @@checkSnakeLength
	mov bx, offset wallLengthDescr
	call parseHexByteFromDescription
	cmp ax, 6
	ja @@help
	mov [WallLength], al
@@checkSnakeLength:
	cmp [snakeLengthDescr.wasSet], 1
	jne @@checkIntersections
	mov bx, offset snakeLengthDescr
	call parseHexByteFromDescription
	cmp ax, 0FFh
	ja @@help
	mov [MaxSnakeLength], ax
@@checkIntersections:
	mov ax, [selfIntersectionsDescr.wasSet]
	mov [SelfCollisionAllowed], al
	mov ax, [passableWallsDescr.wasSet]
	mov [PassableWall], al
	mov ax, 1
	ret
endp
	
	
MapObjectType_None equ 0000h
MapObjectType_Food1 equ 0101h
MapObjectType_Food2 equ 0102h
MapObjectType_Food3 equ 0103h
MapObjectType_Obstacle1 equ 0201h
MapObjectType_Obstacle2 equ 0202h
MapObjectType_Obstacle3 equ 0203h
MapObjectType_SnakePartLeft equ 0A00h
MapObjectType_SnakePartUp equ 0A01h
MapObjectType_SnakePartRight equ 0A02h
MapObjectType_SnakePartDown equ 0A03h

Expires_Never equ 0

MapObject struc
	_Type dw MapObjectType_None
	_Expires dw Expires_Never
MapObject ends

MapWidth equ 32d
MapHeight equ 20d
TilePxSize equ 10d
ScreenPxWidth equ 320d
ScreenPxHeight equ 200d

SavedVideoMode db ?
SavedVideoPage db ?
Speed dw 1

HelpText1 db 'Arrow keys -> Move directions$'
HelpText2 db 'Enter -> Pause / Unpause$'
HelpText3 db '+/- -> Faster / Slower$'
HelpText4 db '[Press Enter to resume]$'

PauseText1 db 'PAUSED$'
PauseText2 db '[Press Enter to resume]$'
PauseText3 db '[Press Escape to leave]$'

EndText1 db 'GAME ENDED$'
EndText2 db 'Food1: ____	Food2: ____$'
EndText3 db 'Turns: ____$' 
EndText4 db 'Length: ____ / ____$'
EndText5 db '[Press Enter/Escape to leave]$'

MapSize equ MapHeight * MapWidth
Map MapObject MapSize dup(<>)
HeadCoords dw ?
HeadType dw ?
GameOver dw 0

MaxSnakeLength dw 40

Food1Eaten dw 0
Food2Eaten dw 0
Turns dw 0

Paused_Not equ 0
Paused_Pause equ 1
Paused_Help equ 2
Paused_EndGame equ 3
Paused dw Paused_Pause

SelfCollisionAllowed db 0
PassableWall db 0
PassableWallY db (MapHeight / 2)

hexAlphabet db '0123456789ABCDEF'

insertNumber proc ; dx = offset to number, cx = number
	push ax bx cx dx
	mov bh, 0
	mov bl, ch
	shr bl, 4
	mov al, hexAlphabet[bx]
	mov bx, dx
	mov [bx], al
	inc dx
	mov bh, 0
	mov bl, ch
	and bl, 0Fh
	mov al, hexAlphabet[bx]
	mov bx, dx
	mov [bx], al
	inc dx
	mov bh, 0
	mov bl, cl
	shr bl, 4
	mov al, hexAlphabet[bx]
	mov bx, dx
	mov [bx], al
	inc dx
	mov bh, 0
	mov bl, cl
	and bl, 0Fh
	mov al, hexAlphabet[bx]
	mov bx, dx
	mov [bx], al
	pop dx cx bx ax
	ret
endp

getMapObj proc ; ah = x, al = y  ===> bx = type, cx = expires
	push ax dx
	mov dh, 0
	mov dl, ah
	mov ah, MapWidth
	mul ah
	add ax, dx
	mov dx, type(MapObject)
	mul dx
	mov dx, bx
	mov bx, ax
	mov dx, Map[bx]._Type
	mov cx, Map[bx]._Expires
	mov bx, dx
	pop dx ax
	ret
endp

BoxSize equ TilePxSize
YMultiplier equ (TilePxSize * ScreenPxWidth)
drawBox proc ; ah = x, al = y, dx = color
	push ax bx cx dx di si
	push dx
	mov cx, ax
	mov ah, 0
	mov dx, YMultiplier
	mul dx
	mov di, ax
	mov al, ch
	mov ah, TilePxSize
	mul ah
	add di, ax
	pop ax ; color
	mov dx, di ; starting di for stosb
	mov bx, 0
@@whileBxLessThanBoxSize:
	cmp bx, BoxSize
	jae @@endWhile
	push ax
	cmp bx, 0
	je @@edge
	cmp bx, (BoxSize - 1)
	je @@edge
	jmp @@notEdge
@@edge:
	mov ax, 0
@@notEdge:
	mov cx, BoxSize
	mov di, dx
	cld
	rep stosb
	mov ax, 0
	dec di
	stosb
	mov di, dx
	stosb
	inc bx
	add dx, ScreenPxWidth
	pop ax
	jmp @@whileBxLessThanBoxSize
@@endWhile:

	pop si di dx cx bx ax
	ret
endp

printLine proc ; bx = word off, dh = y, dl = x
	push ax bx cx dx
	push bx
	mov bx, 0
	mov ah, 2
	int 10h
	pop dx
	mov ah, 9
	int 21h
	pop dx cx bx ax
	ret
endp

printHelp proc
	push ax bx cx dx
	mov dx, 0206h
	mov bx, offset HelpText1
	call printLine
	inc dh
	mov bx, offset HelpText2
	call printLine
	inc dh
	mov bx, offset HelpText3
	call printLine
	inc dh
	mov bx, offset HelpText4
	call printLine
	pop dx cx bx ax
	ret
endp

printPause proc
	push ax bx cx dx
	mov dx, 0206h
	mov bx, offset PauseText1
	call printLine
	inc dh
	mov bx, offset PauseText2
	call printLine
	inc dh
	mov bx, offset PauseText3
	call printLine
	pop dx cx bx ax
	ret
endp

readTimerCount proc
	pushf
	cli
	mov al, 00000000b    ; al = channel in bits 6 and 7, remaining bits clear
	out 43h, al        ; Send the latch command
	in al, 40h         ; al = low byte of count
	mov ah, al           ; ah = low byte of count
	in al, 40h         ; al = high byte of count
	rol ax, 8            ; al = low byte, ah = high byte (ax = current count)
	popf
	ret
endp

fillEndGameInfo proc
	push ax bx cx dx
	mov cx, [Food1Eaten]
	mov dx, ((offset EndText2) + 7d)
	call insertNumber
	mov cx, [Food2Eaten]
	mov dx, ((offset EndText2) + 19d)
	call insertNumber
	mov cx, [Turns]
	mov dx, ((offset EndText3) + 7d)
	call insertNumber
	mov ax, [HeadCoords]
	call getMapObj
	mov dx, ((offset EndText4) + 8d)
	call insertNumber
	mov cx, [MaxSnakeLength]
	mov dx, ((offset EndText4) + 15d)
	call insertNumber
	pop dx cx bx ax
	ret
endp

printEnd proc
	push ax bx cx dx
	call fillEndGameInfo
	mov dx, 0206h
	mov bx, offset EndText1
	call printLine
	inc dh
	mov bx, offset EndText2
	call printLine
	inc dh
	mov bx, offset EndText3
	call printLine
	inc dh
	mov bx, offset EndText4
	call printLine
	inc dh
	mov bx, offset EndText5
	call printLine
	pop dx cx bx ax
	ret
endp

generateRandomCoords proc ; ; ax => coords
	push bx cx dx
	call readTimerCount
	mov cx, ax
	mov ah, 0
	mov al, cl
	mov dl, MapWidth
	xor al, ch
	add al, 37h
	mov ah, 0
	div dl
	mov bh, ah ; remainder
	mov dl, MapHeight
	mov ah, 0
	mov al, ch
	xor al, cl
	add al, 37h
	mov ah, 0
	div dl
	mov bl, ah
	mov ax, bx
	pop dx cx bx
	ret
endp


WallCount db 5
WallLength db 4
WallOffset db ?
LastWallX equ MapWidth - 5
FirstWallX equ 4
buildRandomWalls proc ; cx = count;
	push ax bx cx dx
	mov cl, WallCount
	mov al, (LastWallX - FirstWallX)
	mov ah, 0
	div cl
	mov [WallOffset], al
@@loop:
	call getWallInfo
	push cx
	mov dl, [WallLength]
	add dl, al ; maxY of wall
@@wallLoop:
	cmp al, dl
	je @@wallDone
	mov cx, Expires_Never
	call setMapObj
	inc al
	jmp @@wallLoop
@@wallDone:
	pop cx
	dec cx
	jcxz @@done
	jmp @@loop
@@done:
	pop ax dx cx bx
	ret
endp

getWallInfo proc ; cx = wall's number; ah => x of wall, al => highest y of wall, bx => wallType
	push dx cx
	mov al, cl
	mov bl, 3
	mov ah, 0
	div bl
	inc ah
	mov bl, ah
	mov bh, 2 ; MapObj_Obstacle...
	mov al, WallOffset
	mul cl
	add al, FirstWallX
	mov ah, al
	test cl, 1
	je @@up
@@down:
	mov al, (MapHeight - 1)
	sub al, [WallLength]
	jmp @@done
@@up:
	mov al, 1
@@done:
	pop cx dx
	ret
endp

drawMapObj proc ; ah = x, al = y, bx = type, cx = expires
	push ax bx cx dx
	cmp [PassableWall], 1
	jne @@notPassableWall
	cmp al, PassableWallY
	jne @@notPassableWall
	mov dx, 20h
	call drawBox
	jmp @@end
@@notPassableWall:
	cmp bh, 0Ah
	jne @@checkIfFood1
	mov dx, 00001111b
	call drawBox
	jmp @@end
@@checkIfFood1:
	cmp bx, MapObjectType_Food1
	jne @@checkIfFood2
	mov dx, 00110000b
	call drawBox
	jmp @@end
@@checkIfFood2:
	cmp bx, MapObjectType_Food2
	jne @@checkIfFood3
	mov dx, 02Bh
	call drawBox
	jmp @@end
@@checkIfFood3:
	cmp bx, MapObjectType_Food3
	jne @@checkIfObstacle1
	mov dx, 01Bh
	call drawBox
	jmp @@end
@@checkIfObstacle1:
	cmp bx, MapObjectType_Obstacle1
	jne @@checkIfObstacle2
	mov dx, 11110000b
	call drawBox
	jmp @@end
@@checkIfObstacle2:
	cmp bx, MapObjectType_Obstacle2
	jne @@checkIfObstacle3
	mov dx, 00Bh
	call drawBox
	jmp @@end
@@checkIfObstacle3:
	cmp bx, MapObjectType_Obstacle3
	jne @@ifNone
	mov dx, 01010000b
	call drawBox

	jmp @@end
@@ifNone:
	mov dx, 0
	call drawBox
	jmp @@end
@@end:
	
	pop dx cx bx ax
	ret
endp

makeTurn proc
	pushf
	push ax bx cx dx
	cli
@@collide:
	mov ax, [HeadCoords]
	call getMapObj
	cmp bh, 0Ah
	je @@alive
	mov [Paused], Paused_EndGame
	jmp @@end
@@alive:
	cmp cx, [MaxSnakeLength]
	jb @@notWonYet 
	mov [Paused], Paused_EndGame
	jmp @@end
@@notWonYet:
	call getNextAx
	call getMapObj
	mov dx, bx
	call onCollideWith
	cmp di, 1
	je @@collide
	cmp [Paused], Paused_EndGame
	je @@end
	mov ax, [HeadCoords]
	call getMapObj
	call getNextAx
	mov [HeadCoords], ax
	mov HeadType, bx
	inc cx
	call setMapObj
	call decreaseExpirations
@@end:
	pop dx cx bx ax
	popf
	ret
endp

decreaseExpirations proc
	push ax bx cx dx
	mov ax, 0
@@whileAhLessThanWidth:
	cmp ah, MapWidth
	jae @@endAh
	mov al, 0
@@whileAlLessThanHeight:
	cmp al, MapHeight
	jae @@endAl
	
	call getMapObj
	cmp cx, Expires_Never
	je @@next
	dec cx
	cmp cx, 0
	jne @@justSet
	mov bx, MapObjectType_None
@@justSet:
	call setMapObj
@@next:
	inc al
	jmp @@whileAlLessThanHeight
@@endAl:
	inc ah
	jmp @@whileAhLessThanWidth
@@endAh:
	pop dx cx bx ax
	ret
endp

setMapObj proc ; ah = x, al = y, bx = type, cx = expires
	push ax bx cx dx
	mov dh, 0
	mov dl, ah
	mov ah, MapWidth
	mul ah
	add ax, dx
	mov dx, type(MapObject)
	mul dx
	mov dx, bx
	mov bx, ax
	mov Map[bx]._Type, dx
	mov Map[bx]._Expires, cx
	pop dx cx bx ax
	ret
endp

drawMap proc
	push ax bx cx dx
	mov ax, 0A000h
	mov es, ax
	mov ax, 0
@@whileAhLessThanWidth:
	cmp ah, MapWidth
	jae @@endAh
	mov al, 0
@@whileAlLessThanHeight:
	cmp al, MapHeight
	jae @@endAl
	
	call getMapObj
	call drawMapObj
	
	inc al
	jmp @@whileAlLessThanHeight
@@endAl:
	inc ah
	jmp @@whileAhLessThanWidth
@@endAh:
	pop dx cx bx ax
	ret
endp

MaxY equ (MapHeight - 1)
MaxX equ (MapWidth - 1)
makeSnake proc
	push ax bx cx dx
	mov ax, 0
@@whileAhLessThanWidth:
	cmp ah, MapWidth
	jae @@endAh
	mov al, 0
@@whileAlLessThanHeight:
	cmp al, MapHeight
	jae @@endAl
	mov cx, Expires_Never
	cmp ah, 0
	je @@edge
	cmp ah, MaxX
	je @@edge
	cmp al, 0
	je @@edge
	cmp al, MaxY
	je @@edge
	jmp @@notEdge
@@edge:
	mov bx, MapObjectType_Obstacle1
	jmp @@done
@@notEdge:
	mov bx, MapObjectType_None
@@done:
	call setMapObj
@@next:
	inc al
	jmp @@whileAlLessThanHeight
@@endAl:
	inc ah
	jmp @@whileAhLessThanWidth
@@endAh:
	call buildRandomWalls
	mov [HeadCoords], 0907h
	mov [HeadType], MapObjectType_SnakePartRight
	mov cx, 4
@@loop:
	mov ah, cl
	add ah, 5
	mov al, 7
	mov bx, MapObjectType_SnakePartRight
	call setMapObj
	dec cx
	jcxz @@end
	jmp @@loop
@@end:
	mov ax, 0B07h
	mov bx, MapObjectType_Food1
	mov cx, Expires_Never
	call setMapObj
	mov ax, 1007h
	mov bx, MapObjectType_Food2
	mov cx, Expires_Never
	call setMapObj
	
	pop dx cx bx ax
	ret
endp

changeSnakeDuration proc ; dx = duration change
	push ax bx cx dx
	neg dx
	mov ax, 0
@@whileAhLessThanWidth:
	cmp ah, MapWidth
	jae @@endAh
	mov al, 0
@@whileAlLessThanHeight:
	cmp al, MapHeight
	jae @@endAl
	call getMapObj
	cmp bh, 0Ah
	jne @@notSnake
	cmp cx, dx
	jle @@remove
	sub cx, dx
	call setMapObj
	jmp @@next
@@remove:
	mov bx, MapObjectType_None
	mov cx, 0
	call setMapObj
@@notSnake:
@@next:
	inc al
	jmp @@whileAlLessThanHeight
@@endAl:
	inc ah
	jmp @@whileAhLessThanWidth
@@endAh:

	pop dx cx bx ax
	ret
endp

generateMapObjWhereEmpty proc
	push ax bx cx dx
	push bx cx
@@generateRandomEmptyCoords:
	call generateRandomCoords
	call getMapObj
	cmp bx, MapObjectType_None
	jne @@generateRandomEmptyCoords
	pop cx bx
	call setMapObj
	pop dx cx bx ax
	ret
endp

getOppositeSnakeObj proc
	add bl, 2
	and bl, 3
	ret
endp

reverseSnake proc
	push ax bx cx dx
	mov ax, [HeadCoords]
	call getMapObj
	mov dx, cx
	inc dx
	mov ax, 0
@@whileAhLessThanWidth:
	cmp ah, MapWidth
	jae @@endAh
	mov al, 0
@@whileAlLessThanHeight:
	cmp al, MapHeight
	jae @@endAl
	call getMapObj
	cmp bh, 0Ah
	jne @@notSnake
	call getOppositeSnakeObj
	cmp cx, 1
	jne @@notNewHead
	mov [HeadType], bx
	mov [HeadCoords], ax
@@notNewHead:
	neg cx
	add cx, dx
	call setMapObj
	jmp @@next
@@notSnake:
@@next:
	inc al
	jmp @@whileAlLessThanHeight
@@endAl:
	inc ah
	jmp @@whileAhLessThanWidth
@@endAh:
	pop dx cx bx ax
	ret
endp

onCollideWith proc ; ax = who, dx = target type; di => isRecheckNeeded
	push ax bx cx dx
	mov di, 0
	cmp dh, 0Ah
	je @@withUrSelf
	cmp dx, MapObjectType_Food1
	je @@withFood1
	cmp dx, MapObjectType_Food2
	je @@withFood2
	cmp dx, MapObjectType_Food3
	je @@withFood3
	cmp dx, MapObjectType_Obstacle1
	je @@withObstacle1
	cmp dx, MapObjectType_Obstacle2
	je @@withObstacle2
	cmp dx, MapObjectType_Obstacle3
	je @@withObstacle3
	jmp @@end
@@withUrSelf:
	cmp [SelfCollisionAllowed], 1
	je @@end
	mov [Paused], Paused_EndGame
	jmp @@end
@@withFood1:
	inc [Food1Eaten]
	mov bx, Note_C2
	call playSoundFromBx
	mov dx, 1
	call changeSnakeDuration
	mov bx, MapObjectType_Food1
	mov cx, Expires_Never
	call generateMapObjWhereEmpty
	jmp @@end
@@withFood2:
	mov bx, Note_D2
	call playSoundFromBx
	inc [Food2Eaten]
	mov dx, -2
	call changeSnakeDuration
	mov bx, MapObjectType_Food2
	mov cx, Expires_Never
	call generateMapObjWhereEmpty
	jmp @@end
@@withFood3:
	jmp @@end
@@withObstacle1:
	mov [Paused], Paused_EndGame
	jmp @@end
@@withObstacle2:
	call reverseSnake
	mov di, 1
	jmp @@end
@@withObstacle3:
	mov ax, [HeadCoords]
	call getMapObj
	inc bl
	and bl, 3
	call canBeNewHead
	cmp ax, 1
	je @@turn
	jmp @@die
@@turn:
	mov ax, [HeadCoords]
	call setMapObj
	mov di, 1
	jmp @@end
@@die:
	mov [Paused], Paused_EndGame
	jmp @@end
@@end:
	pop dx cx bx ax
	ret
endp

canBeNewHead proc ; bx = head type to test; ax => if can
	push bx cx dx
	cmp bx, [HeadType]
	je @@ok
	add bx, [HeadType]
	test dx, 1
	jnz @@ok
	jmp @@fail
@@ok:
	mov ax, 1
	jmp @@end
@@fail:
	mov ax, 0
	jmp @@end
@@end:
	pop dx cx bx
	ret
endp

savePageAndMode proc
	push ax bx cx dx	
	mov ah,0fh
	int 10h
	mov [SavedVideoMode], al
	mov [SavedVideoPage], bh
	pop dx cx bx ax
	ret
endp

getNextAx proc ; ax = current coords, bx = snake part
	push bx cx dx
	cmp bx, MapObjectType_SnakePartLeft
	je @@left
	cmp bx, MapObjectType_SnakePartRight
	je @@right
	cmp bx, MapObjectType_SnakePartUp
	je @@up
	cmp bx, MapObjectType_SnakePartDown
	je @@down
	jmp @@end
@@left:
	dec ah
	jmp @@end
@@right:
	inc ah
	jmp @@end
@@up:
	dec al
	jmp @@end
@@down:
	inc al
	jmp @@end
@@end:
	pop dx cx bx
	ret
endp

restorePageAndMode proc
	push ax bx cx dx	
	mov ah, 0
	mov al, [SavedVideoMode]
	int 10h
	mov ah,05h
	mov al, [SavedVideoPage]
	int 10h
	pop dx cx bx ax
	ret
endp

newInt9 proc far
	push ax bx cx dx
	cli
	in al, 60h
	cmp al, 1Ch
	je @@ifEnter
	cmp al, 1
	je @@ifEscape
	cmp al, 48h
	je @@ifUp
	cmp al, 50h
	je @@ifDown
	cmp al, 4Bh
	je @@ifLeft
	cmp al, 4Dh
	je @@ifRight
	cmp al, 0Dh
	je @@ifPlus
	cmp al, 0Ch
	je @@ifMinus
	cmp al, 3Bh
	je @@ifF1
	jmp @@end
@@ifEnter:
	cmp [Paused], Paused_Not
	je @@pause
	cmp [Paused], Paused_Pause
	je @@unPause
	cmp [Paused], Paused_Help
	je @@unPause
	cmp [Paused], Paused_EndGame
	je @@exitGame
@@unPause:
	mov [Paused], Paused_Not
	jmp @@end
@@pause:
	mov [Paused], Paused_Pause
	jmp @@end
@@exitGame:
	mov [GameOver], 1
	jmp @@end
@@ifEscape:
	cmp [Paused], Paused_Pause
	je @@exitGame
	cmp [Paused], Paused_EndGame
	je @@exitGame
	jmp @@end	
@@ifUp:
	mov bx, [HeadType]
	cmp bx, MapObjectType_SnakePartDown
	je @@end
	mov ax, [HeadCoords]
	call getMapObj
	mov bx, MapObjectType_SnakePartUp
	call setMapObj
	jmp @@end
@@ifDown:
	mov bx, [HeadType]
	cmp bx, MapObjectType_SnakePartUp
	je @@end
	mov ax, [HeadCoords]
	call getMapObj
	mov bx, MapObjectType_SnakePartDown
	call setMapObj
	jmp @@end
@@ifLeft:
	mov bx, [HeadType]
	cmp bx, MapObjectType_SnakePartRight
	je @@end
	mov ax, [HeadCoords]
	call getMapObj
	mov bx, MapObjectType_SnakePartLeft
	call setMapObj
	jmp @@end
@@ifRight:
	mov bx, [HeadType]
	cmp bx, MapObjectType_SnakePartLeft
	je @@end
	mov ax, [HeadCoords]
	call getMapObj
	mov bx, MapObjectType_SnakePartRight
	call setMapObj
	jmp @@end
@@ifMinus:
	inc [Speed]
	jmp @@end
@@ifPlus:
	cmp [Speed], 1
	je @@end
	dec [Speed]
	jmp @@end
@@ifF1:
	mov [Paused], Paused_Help
	jmp @@end
@@end:
	mov al, 20h ;Send EOI (end of interrupt)
	out 20h, al ; to the 8259A PIC.
	pop dx cx bx ax
	iret
endp




oldInt9Seg dw ?
oldInt9Off dw ?

main:
	call parseClAndHandleTroubles
	cmp ax, 1
	jne @@end
	call makeSnake
	call savePageAndMode
	mov ax, 3509h
	int 21h
	mov [oldInt9Seg], es
    mov [oldInt9Off], bx
	mov ax, 2509h
    mov dx, offset newInt9
    int 21h
	mov ax, 13h
	int 10h
	call drawMap
@@loop:
	mov ax, 0
	mov es, ax
	mov cx, [Speed]
	add cx, [word ptr es:046Ch]
@@wait:
	call offSound
	cmp [GameOver], 1
	je @@endLoop
	hlt
	cmp cx, [word ptr es:046Ch]
	jne @@wait
	cmp [Paused], Paused_Not
	jne @@paused
	inc [Turns]
	call makeTurn
	call drawMap
	jmp @@loop
@@paused:
	cmp [Paused], Paused_Help
	je @@pauseHelp
	cmp [Paused], Paused_Pause
	je @@pausePause
	cmp [Paused], Paused_EndGame
	je @@pauseEnd
	jmp @@pauseLoop
@@pauseHelp:
	call printHelp
	jmp @@pauseLoop
@@pausePause:
	call printPause
	jmp @@pauseLoop
@@pauseEnd:
	call printEnd
	call playTune
	jmp @@pauseLoop
@@pauseLoop:
	call offSound
	mov di, [Paused]
@@whilePausedStay:
	cmp [GameOver], 1
	je @@endLoop
	cmp [Paused], di
	je @@whilePausedStay
	jmp @@loop
@@endLoop:
	call restorePageAndMode
    mov dx, [oldInt9Off]
	mov ax, [oldInt9Seg]
	mov ds, ax
	mov ax, 2509h
    int 21h
@@end:
	ret
end start