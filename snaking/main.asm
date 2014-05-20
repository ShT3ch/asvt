.model tiny

jumps ; fixes jump out of range somehow
.code
org 100h

locals @@ ; from now on each identifier beginning from @@ will only work in the scope it was declared
start:
	jmp main
	
MapObjectType_None equ 0000h
MapObjectType_Food1 equ 0101h
MapObjectType_Food2 equ 0102h
MapObjectType_Food3 equ 0103h
MapObjectType_Obstacle1 equ 0201h
MapObjectType_Obstacle2 equ 0202h
MapObjectType_Obstacle3 equ 0203h
MapObjectType_SnakePartLeft equ 0A01h
MapObjectType_SnakePartRight equ 0A02h
MapObjectType_SnakePartUp equ 0A03h
MapObjectType_SnakePartDown equ 0A04h

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

MapSize equ MapHeight * MapWidth
Map MapObject MapSize dup(<>)
HeadCoords dw ?
HeadType dw ?
GameOver dw 0
Paused dw 0

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

BoxSize equ (TilePxSize - 2)
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
	add di, ScreenPxWidth
	inc di
	pop ax ; color
	mov dx, di ; starting di for stosb
	mov bx, 0
@@whileBxLessThanBoxSize:
	cmp bx, BoxSize
	jae @@endWhile
	mov cx, BoxSize
	mov di, dx
	cld
	rep stosb
	inc bx
	add dx, ScreenPxWidth
	jmp @@whileBxLessThanBoxSize
@@endWhile:

	pop si di dx cx bx ax
	ret
endp

drawMapObj proc ; ah = x, al = y, bx = type, cx = expires
	push ax bx cx dx
	cmp bh, 0Ah
	jne @@checkIfFood1
	mov dx, 00Fh
	call drawBox
	jmp @@end
@@checkIfFood1:
	cmp bx, MapObjectType_Food1
	jne @@checkIfFood2
	mov dx, 00Bh
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
	mov dx, 00110000b
	call drawBox
	jmp @@end
@@checkIfObstacle2:
	cmp bx, MapObjectType_Obstacle2
	jne @@checkIfObstacle3
	mov dx, 08Fh
	call drawBox
	jmp @@end
@@checkIfObstacle3:
	cmp bx, MapObjectType_Obstacle3
	jne @@ifNone
	mov dx, 02Fh
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
	push ax bx cx dx
	mov ax, HeadCoords
	call getMapObj
	call getNextAx
	call getMapObj
	push dx
	mov dx, bx
	call onCollideWith
	pop dx
	mov ax, HeadCoords
	call getMapObj
	call getNextAx
	mov HeadCoords, ax
	mov HeadType, bx
	inc cx
	call setMapObj
	call decreaseExpirations
	pop dx cx bx ax
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
	mov cx, 9
	call setMapObj
	mov ax, 1007h
	mov bx, MapObjectType_Food2
	mov cx, 9
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

onCollideWith proc ; ax = who, dx = target type
	push ax bx cx dx
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
	mov [GameOver], 1
	jmp @@end
@@withFood1:
	mov dx, 1
	call changeSnakeDuration
	jmp @@end
@@withFood2:
	mov dx, -2
	call changeSnakeDuration
	jmp @@end
@@withFood3:
	jmp @@end
@@withObstacle1:
	mov [GameOver], 1
	jmp @@end
@@withObstacle2:
	mov [GameOver], 1
	jmp @@end
@@withObstacle3:
	mov [GameOver], 1
	jmp @@end
@@end:
	pop dx cx bx ax
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
	push ax bx
	cli
	in al, 60h
	cmp al, 1Ch
	jmp @@ifEnter
	cmp al, 1
	jmp @@ifEscape
	jmp @@end
@@ifEnter:
	mov ax, [Paused]
	not ax
	mov [Paused], ax
	jmp @@end
@@ifEscape:
	mov [GameOver], 1
	jmp @@end
@@end:
	mov al, 20h ;Send EOI (end of interrupt)
	out 20h, al ; to the 8259A PIC.
	pop bx ax
	iret
endp


oldInt9Seg dw ?
oldInt9Off dw ?

main:
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
	mov dx, 0212h
	call drawMap
@@loop:
	push es
	mov ax, 0
	mov es, ax
	mov cx, [Speed]
	add cx, [word ptr es:046Ch]
@@wait:
	cmp cx, [word ptr es:046Ch]
	jne @@wait
	pop es
	cmp [Paused], 1
	je @@paused
	call makeTurn
	call drawMap
@@paused:
	cmp [GameOver], 1
	jne @@loop
	call restorePageAndMode
    mov dx, [oldInt9Off]
	mov ax, [oldInt9Seg]
	mov ds, ax
	mov ax, 2509h
    int 21h
	ret
end start