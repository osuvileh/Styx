;;; $RCSfile: part0.asm,v $ $Revision: 1.8 $ $Date: 2005/03/09 15:31:55 $
BITS 16

stacksize		EQU 0200h

; Constants here
; starting address of video memory

videobase		EQU 0a000h

; some colors

black			EQU 0
green			EQU 00110000b
blue			EQU 00001001b
red				EQU 00000100b
white			EQU 00001111b
grey			EQU 00000111b
scrwidth		EQU 320
; screen width in pixels, in the graphics mode 320x200

segment memscreen data
	resb	64000
segment background data
	resb	64000

segment mystack stack
resb stacksize
stacktop:

segment mydata data						; data segment

; variables here

pressesc		dw		0
oldintseg		resw	1
oldintoff		resw	1
graphicm		resb	100
playermove		dw		1
styxmove        dw      1
playerlocation	dw		2883
styxlocation	dw		10950
styx_in			dw		0
trailcount		dw		0
conquer			dw		0
pressshift		dw		0

left            dw      0
right           dw      0
top             dw      0
bot             dw      0

segment mycode code						;code segment
; subroutines

KeybInt:
	push ds				 ; put the value of ds,ax to safety
	push ax

	mov ax, mydata		; Re-initialisation of
	mov ds, ax			; the data segment

	cli						; disable other interrupts
							; during this one
	.getstatus:
		in al, 64h
		test al, 02h
		loopnz .getstatus	; wait until the port is ready
		in al, 60h			; Get the scan code of
							; the pressed/released key

	.shift:
		cmp al, 2ah
		jne .esc
		mov word [pressshift], 1
		jmp .kbread

	.esc:
		cmp al, 01h
		jne .moveup
		mov word [pressesc], 1
		jmp .kbread

	.moveup:				;arrow up
		cmp al, 48h
		jne .movedown
		mov word [playermove], -320
		jmp .kbread

	.movedown:				;arrow down
		cmp al, 50h
		jne .moveleft
		mov word [playermove], 320
		jmp .kbread

	.moveleft:				;arrow left
		cmp al, 4bh
		jne .moveright
		mov word [playermove], -1
		jmp .kbread

	.moveright				;arrow right
		cmp al, 4dh
		jne .kbread
		mov word [playermove], 1

	.kbread:
		in al, 61h		   ; send acknowledgment without
		or al, 10000000b   ; modifying the other bits.
		out 61h, al
		and al, 01111111b
		out 61h, al
		mov al, 20h			; send End-of-Interrupt signal
		out 20h, al

	sti						; enable interrupts again

	pop ax
	pop ds						; regain the ds,ax from stack

	iret					; return from interrupt

initBackground:
	pusha

	;load background
	mov ax, background
	mov es, ax

	;top grey line
	.topline:
		mov	di, 2880
		.toploop:
			cmp	di, 3200
			je .botline
			mov byte [es:di], grey
			inc di
			jmp .toploop

	;bottom grey line
	.botline:
		mov	di, 62720
		.botloop:
			cmp	di, 63040
			je	.side
			mov byte [es:di], grey
			inc di
			jmp	.botloop

	;blue sidepanel and horizontal grey line
	.side:
		mov	di, 2880
		.sideloop:
			cmp	di, 63040
			je .infopanel
			mov	byte [es:di], blue
			inc di
			mov	byte [es:di], blue
			inc di
			mov	byte [es:di], blue
			inc di
			mov	byte [es:di], grey
			add di, 313
			mov	byte [es:di], grey
			inc di
			mov	byte [es:di], blue
			inc di
			mov	byte [es:di], blue
			inc di
			mov	byte [es:di], blue
			inc di
			jmp .sideloop

	;blue infopanel
	.infopanel:
		mov	di, 0
		.infoloop:
			cmp	di, 2880
			je .botpanel
			mov	byte [es:di], blue
			inc	di
			jmp .infoloop

	;blue botpanel
	.botpanel:
		mov	di, 63040
		.botploop:
			cmp	di, 64000
			je .end
			mov	byte [es:di], blue
			inc di
			jmp .botploop

	.end:
		popa
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

	;pointers
	mov word si, 0
	mov word di, 0
	mov cx, 64000

	;segment registers to correct locations
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

	mov word di, [playerlocation]
	mov byte [es:di], red

	popa
	ret

drawStyx:
	push ax
	push di

	;mid pixel
	mov word ax, [styxlocation]
	mov di, ax
	mov byte [es:di], green

	;top
	sub ax, 320
	mov di, ax
	mov byte [es:di], green

	;left side
	add ax, 319
	mov di, ax
	mov byte [es:di], green

	;right side
	add ax, 2
	mov di, ax
	mov byte [es:di], green

	;bottom
	add ax, 319
	mov di, ax
	mov byte [es:di], green

	pop di
	pop ax
	ret

movePlayer:
	pusha

	;load current position
	mov ax, background
	mov es, ax
	mov word ax, [playermove]
	add word ax, [playerlocation]
	mov di, ax

	;check if player is conquering area
	cmp word [conquer], 1
	je .moveend
	cmp word [pressshift], 1
	jne .movegrey

	;area conquer
	.moveconquer:
		mov word ax, [playermove]
		add word ax, [playerlocation]
		mov di, ax
		cmp byte [es:di], blue	;check if player is in blue pixel
		je .moveit
		cmp byte [es:di], white ;check if player is in white pixel
		je .moveit
		mov word [playerlocation], ax	;if player is not in blue, make the move

		;check if the player has moved to unconquered area
		mov di, ax
		cmp byte [es:di], black
		jne .moveit
		mov word [conquer], 1
		jmp .moveit

	;check if the player has returned to the grey area
	.moveend:
		mov word di, [playerlocation]
		cmp byte [es:di], grey	;check if the player has returned to grey area
		jne .drawtrail
		mov word [conquer], 0			;set conquering flags
		mov word [pressshift], 0		;0 if player is in grey area
		jmp .moveit

	;draw trail for conquered area
	.drawtrail:
		mov word di, [playerlocation]
		mov byte [es:di], white
		jmp .moveconquer

	;normal movement in grey area
	.movegrey:
		cmp byte [es:di], grey	;playermove + current position
		jne .moveit					    ;has to be in grey area
		mov word [playerlocation], ax   ;make the grey area move
		jmp .moveit

	.moveit:
		popa
		ret

moveStyx:
    pusha

    cmp word [styxmove], 319
    je .leftbot
    cmp word [styxmove], 321
    je .rightbot
    cmp word [styxmove], -321
    je .lefttop
    cmp word [styxmove], -319
    je .righttop

    .leftbot:
        call checkLeft
        cmp word [left], 1
        je .changemove
        call checkBot
        cmp word [bot], 1
        je .changemove
        jmp .changeend
        
    .rightbot:
        call checkRight
        cmp word [right], 1
        je .changemove
        call checkBot
        cmp word [bot], 1
        je .changemove
        jmp .changeend
        
    .lefttop:
        call checkLeft
        cmp word [left], 1
        je .changemove
        call checkTop
        cmp word [top], 1
        je .changemove
        jmp .changeend
        
    .righttop:
        call checkRight
        cmp word [right], 1
        je .changemove
        call checkTop
        cmp word [top], 1
        je .changemove
        jmp .changeend

    .changemove:
        mov word [left], 0
        mov word [right], 0
        mov word [top], 0
        mov word [bot], 0
        call RANDGEN
        
    .changeend:
        mov word ax, [styxmove]
        add word ax, [styxlocation]
        mov word [styxlocation], ax

        popa
        ret

checkLeft:
    pusha
    mov ax, background
    mov es, ax
    
    mov word ax, [styxlocation]
    sub ax, 2
    mov di, ax
    cmp byte [es:di], grey
    je .setFlag
    cmp byte [es:di], white
    je .setFlag
    jmp .cont
    
    .setFlag:
        mov word [left], 1

    .cont:
	popa
	ret

checkRight:
    pusha
    mov ax, background
    mov es, ax
    
    mov word ax, [styxlocation]
    add ax, 2
    mov di, ax
    cmp byte [es:di], grey
    je .setFlag
    cmp byte [es:di], white
    je .setFlag
    jmp .cont
    
    .setFlag:
        mov word [right], 1
    .cont:
	popa
	ret
 
checkTop:
    pusha
    mov ax, background
    mov es, ax
    
    mov word ax, [styxlocation]
    sub ax, 640
    mov di, ax
    cmp byte [es:di], grey
    je .setFlag
    cmp byte [es:di], white
    je .setFlag
    jmp .cont
    
    .setFlag:
        mov word [top], 1

    .cont:
    popa
    ret
       
checkBot:
    pusha
    mov ax, background
    mov es, ax
	
    mov word ax, [styxlocation]
    add ax, 640
    mov di, ax
    cmp byte [es:di], grey
    je .setFlag
    cmp byte [es:di], white
    je .setFlag
    jmp .cont
    
    .setFlag:
        mov word [bot], 1

    .cont:
	popa
	ret

RANDGEN:        ;generate a rand no using the system time
    pusha

    
    mov ah, 00h  ;interrupts to get system time
    int 1Ah      	 ;CX:DX now hold number of clock ticks since midnight

    mov ax, dx
    xor dx, dx  ;clear dx
    mov cx, 2
    div cx      ;here dx contains the remainder of the division - from 0 to 4
    
    mov word ax, [styxlocation]

    cmp word [styxmove], 319
    je .up
    cmp word [styxmove], 321
    je .up
    cmp word [styxmove], -321
    je .down
    cmp word [styxmove], -319
    je .down
	
    .up:
        cmp dx, 0
        jne .moverup
		.movelup:
			mov word [styxmove], -321
			add word ax, [styxmove]
			mov word [styxlocation], ax
			jmp .moveend
		.moverup:
			mov word [styxmove], -319
			add word ax, [styxmove]
			mov word [styxlocation], ax
			jmp .moveend

    .down:
        cmp dx, 0
        jne .moverdown
		.moveldown:
			mov word [styxmove], 319
			add word ax, [styxmove]
			mov word [styxlocation], ax
			jmp .moveend
		.moverdown:
			mov word [styxmove], 321
			add word ax, [styxmove]
			mov word [styxlocation], ax
			jmp .moveend

    .moveend:
        mov word [left], 0
        mov word [right], 0
        mov word [top], 0
        mov word [bot], 0
        popa
        ret

..start:

	;init segment reg
	mov ax, mydata
	mov ds, ax
	mov ax, mystack
	mov ss, ax
	mov sp, stacktop

	;save old address
	mov ah, 35h
	mov al, 9h
	int 21h
	mov word [oldintseg], es
	mov	word [oldintoff], bx

	;new interrupt
	mov	word dx, KeybInt
	mov word ax, mycode
	mov	ds, ax
	mov	ah, 25h
	mov al, 9h
	int 21h

	;re-initialisation of  the data segment
	mov ax, mydata
	mov ds, ax
	;old graphic modes
	mov	ah, 0fh
	int 10h

	mov	byte [graphicm], al

	;new graphic modes
	mov	ah, 0
	mov	al, 13h
	int 10h

	mov ax, videobase
	mov es, ax

	call initBackground
    call RANDGEN

	.mainloop:
		mov dx, 10
		.pause1:
			mov cx, 10
		.pause2:
			dec cx
			jne .pause2
			dec dx
			jne .pause1

		call draw
		call movePlayer
        call moveStyx

		cmp word [pressesc], 1
		jne .mainloop
		call .dosexit

.dosexit:

	;reset graphic mode
	mov	ah, 0
	mov	byte al, [graphicm]
	int 10h

	;restoring old interrupt
	mov	word dx, [oldintoff]
	mov	word ax, [oldintseg]
	mov ds, ax
	mov ah, 25h
	mov al, 9h
	int 21h

	;end program int 21.4c
	mov al, 0
	mov ah, 4ch
	int 21h

.end