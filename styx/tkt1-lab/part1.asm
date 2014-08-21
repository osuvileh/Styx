;;; $RCSfile: part0.asm,v $ $Revision: 1.8 $ $Date: 2005/03/09 15:31:55 $
BITS 16
	
stacksize       EQU 0200h

; Constants here
; starting address of video memory
 
videobase       EQU 0a000h
 
; some colors
 
black		EQU 0
green		EQU 00110000b
blue			EQU 00001001b
red			EQU 00000100b
white		EQU 00001111b
grey			EQU 00000111b
scrwidth    EQU 320 
; screen width in pixels, in the graphics mode 320x200

segment memscreen data
	resb 	64000
segment background data
	resb	64000

segment mystack stack
resb stacksize
stacktop:	
	
segment mydata data			; data segment

; variables here

foo	resw			1
pressesc dw		0
oldintseg resw	1
oldintoff resw		1
delay dw			300
graphicm resb	100
move dw			1
playerlocation dw 2883


segment mycode code			;code segment
; subroutines

KeybInt:
    push ds              ; put the value of ds,ax to safety
    push ax

    mov ax, mydata      ; Re-initialisation of
    mov ds, ax          ; the data segment

    cli                     ; disable other interrupts
                            ; during this one
    .getstatus:
        in al, 64h
        test al, 02h
        loopnz .getstatus      ; wait until the port is ready
        in al, 60h         ; Get the scan code of
                       ; the pressed/released key
					   
	.esc:
		cmp al, 01h
		jne .moveup
		mov word [pressesc], 1
		jmp .kbread
		
	.moveup:
		cmp al, 48h
		jne .movedown
		mov word [move], -320
		jmp .kbread
		
	.movedown:
		cmp al, 50h
		jne .moveleft
		mov word [move], 320
		jmp .kbread
		
	.moveleft:
		cmp al, 4bh
		jne .moveright
		mov word [move], -1
		jmp .kbread
		
	.moveright
		cmp al, 4dh
		jne .kbread
		mov word [move], 1


    .kbread:
        in al, 61h         ; send acknowledgment without
        or al, 10000000b   ; modifying the other bits.
        out 61h, al
        and al, 01111111b
        out 61h, al
        mov al, 20h         ; send End-of-Interrupt signal
        out 20h, al

    sti                     ; enable interrupts again

    pop ax
    pop ds      		; regain the ds,ax from stack

    iret                    ; return from interrupt

initBackground:
	mov ax, background
	mov es, ax
	mov	di, 2880
	
	mov byte [es:di], blue
	
	.topline:
		cmp	di, 3520
		je .botline
		mov byte [es:di], grey
		inc di
		jmp .topline

	.botline:
		mov	di, 62400
		jmp .botloop
		.botloop:
			cmp	di, 63040
			je	.side
			mov byte [es:di], grey
			inc di
			jmp	.botloop	
		
	.side:
		mov	di, 2880
		jmp .sideloop
		.sideloop:
			cmp di, 63040
			je .infopanel
			mov byte [es:di], blue
			inc	di
			mov	byte [es:di], blue
			inc	di
			mov byte [es:di], blue
			inc	di
			mov byte [es:di], grey
			inc	di
			mov byte [es:di], grey
			add di, 311
			mov byte [es:di], grey
			inc	di
			mov byte [es:di], grey
			inc	di
			mov byte [es:di], blue	
			inc	di
			mov	byte [es:di], blue
			inc	di
			mov byte [es:di], blue
			inc	di
			jmp .sideloop

	.infopanel:
		mov	di, 0
		jmp .infoloop
		.infoloop:
			cmp	di, 2880
			je .botpanel
			mov	byte [es:di], blue
			inc	di
			jmp .infoloop

	.botpanel:
		mov	di, 63040
		jmp .botploop
		.botploop:
			cmp	di, 64000
			je .end
			mov	byte [es:di], blue
			inc	di
			jmp .botploop

	 .end:
		ret

draw:
	call copyBackground
	call drawPlayer
	call drawStyx
	call copyMemScreen
	ret

copyBackground:
	push ds
	pusha

	;Pointers
	mov word si, 0
	mov word di, 0
	mov cx, 64000
 
	;Segment registers to correct locations
	mov ax, memscreen
	mov es, ax
	mov ax, background
	mov ds, ax
 
	;REPEAT COPY!
	rep movsb
	popa
	pop ds

    ret

copyMemScreen:
	push ds
	pusha

	;Pointers
	mov word si, 0
	mov word di, 0
	mov cx, 64000
 
	;Segment registers to correct locations
	mov ax, videobase
	mov es, ax
	mov ax, memscreen
	mov ds, ax
 
	;REPEAT COPY!
	rep movsb
	popa
	pop ds

	ret

drawPlayer:
	pusha

	mov word ax, [playerlocation]
	
	mov	di, ax
	mov	byte [es:di], red
	inc	di
	mov	byte [es:di], red

	add ax, 320
	mov di, ax
	mov	byte [es:di], red
	inc	di
	mov	byte [es:di], red

	popa
	
	ret

drawStyx:
	ret

movePlayer:
	push ax
	push di

	mov ax, background
	mov es, ax
    
    mov word ax, [move]
    add word ax, [playerlocation]
	
	mov di, ax
	cmp byte [es:di], grey
	jne .moveit
	mov word [playerlocation], ax

	.moveit:
		pop di
		pop ax
		ret

..start:

	; init segment reg
	mov ax, mydata
	mov ds, ax
    mov ax, mystack
    mov ss, ax
    mov sp, stacktop
	
	; save old address
	mov ah, 35h
	mov al, 9h
	int 21h
	mov word [oldintseg], es
	mov	word [oldintoff], bx
	
	; new interrupt
	mov	 word dx, KeybInt
	mov  word ax, mycode
	mov	ds, ax
	mov	ah, 25h
	mov al, 9h
	int 21h
	
	; re-initialisation of  the data segment
    mov ax, mydata
    mov ds, ax
	; old graphic modes
	mov	ah, 0fh
	int	10h

	mov	byte [graphicm], al
	
	; new graphic modes
	mov	ah, 0
	mov	al, 13h
	int	10h

	mov ax, videobase
	mov es, ax

	call initBackground

	
	.mainloop:
		mov word dx, [delay]
		.pause1:
			mov cx, 3000
		.pause2:
			dec cx
			jne .pause2
			dec dx
			jne .pause1
			
		call draw
		call movePlayer
		
		cmp word [pressesc], 1
		jne .mainloop
		call .dosexit


.dosexit:
	
	; reset graphic mode
	mov	ah, 0
	mov	byte al, [graphicm]
	int 10h

	; restoring old interrupt
	mov	word dx, [oldintoff]
	mov	word ax, [oldintseg]
	mov ds, ax
	mov ah, 25h
	mov al, 9h
	int 21h

	; end program int 21.4c
	mov	al, 0
	mov ah, 4ch
	int 21h

.end
