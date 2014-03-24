title caparser
; .com arguments parser
.model tiny

.code
locals
		public parse_args
		public check_key
	
alphabet	db 27 dup(0)

skipChars macro chars
	@@testRequirements:
		cmp cx,0
		je @@exit
	@@action:
		irp char, chars
			cmp byte ptr [di], char
			je @@step
		endm
		jmp @@exit
	@@step:
		inc di
		dec cx
		jmp @@testRequirements
	@@exit:

endm

parse_args proc near
				push bp
				mov bp, sp
				push cx
				push di
				
				xor cx, cx
				mov cl, byte ptr ds:[80h]
				mov di, 81h
@@foreachword:
				mov al,' '
				repe scasb
				push di
				repne scasb
				call add_if_key
				inc cx
				loop @@foreachword
				
				pop di
				pop cx
				pop bp
				ret
parse_args endp

add_if_key proc near
				push bp
				mov bp, sp
				push bx
				push dx
				mov bx, [bp + 4]
				
				xor ax, ax
				mov al, byte ptr [bx - 1]
				cmp ax, '/'
				je @@ifkey
				cmp ax, '-'
				jne @@exit
@@ifkey:
				mov al, byte ptr [bx]
				cmp ax, 'Z'
				jle @@checkrange
				sub ax, 20h
@@checkrange:
				cmp ax, 'A'
				jl @@exit
				cmp ax, 'Z'
				jg @@exit
				sub ax, 'A'
				mov bx, offset alphabet
				add bx, ax
				mov byte ptr [bx], 1
@@exit:
				pop dx
				pop bx
				pop bp
				ret 2
add_if_key endp

check_key proc near
				push bp
				mov bp, sp
				push bx
				push cx
				
				mov cx, [bp + 4]
				mov bx, offset alphabet
				xor ax, ax
				add bx, cx
				mov al, byte ptr [bx]
				
				pop cx
				pop bx
				pop bp
				ret 2
check_key endp

end