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

isBxSnakeObjType macro
	cmp bh, 0Ah
endm

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

MapSize equ MapHeight * MapWidth
Map MapObject MapSize dup(<>)
HeadCoords dw ?

getMapObj proc ; ah = x, al = y  ===> bx = type, cx = expires
	push ax dx
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
	isBxSnakeObjType
	jne @@checkIfFood1
	mov dx, 0ABh
	call drawBox
	jmp @@end
@@checkIfFood1:
	cmp bx, MapObjectType_Food1
	jne @@checkIfFood2
	jmp @@end
@@checkIfFood2:
	cmp bx, MapObjectType_Food2
	jne @@checkIfFood3
	jmp @@end
@@checkIfFood3:
	cmp bx, MapObjectType_Food3
	jne @@checkIfObstacle1
	jmp @@end
@@checkIfObstacle1:
	cmp bx, MapObjectType_Obstacle1
	jne @@checkIfObstacle2
	jmp @@end
@@checkIfObstacle2:
	cmp bx, MapObjectType_Obstacle2
	jne @@checkIfObstacle3
	jmp @@end
@@checkIfObstacle3:
	cmp bx, MapObjectType_Obstacle3
	jne @@ifNone
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
	mov HeadCoords, ax
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
	mov bx, MapObjectType_None
	call setMapObj
@@next:
	inc al
	jmp @@whileAlLessThanHeight
@@endAl:
	inc ah
	jmp @@whileAhLessThanWidth
@@endAh:

	
	mov [HeadCoords], 0907h
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

main:
	mov ax, 0A000h
	mov es, ax
	call makeSnake
	call savePageAndMode
	mov ax, 13h
	int 10h
	call drawMap
@@loop:
	mov ax, 0
	int 16h
	call makeTurn
	call drawMap
	cmp ah, 1
	jne @@loop
	call restorePageAndMode
	ret
end start