;;; $RCSfile: part0.asm,v $ $Revision: 1.8 $ $Date: 2005/03/09 15:31:55 $
BITS 16

stacksize		EQU 0200h

; Constants here
; starting address of video memory

videobase		EQU 0a000h

; some colors

black			EQU 00000000b
green			EQU 00001010b
blue				EQU 00000001b
red				EQU 00000100b
white			EQU 00001111b
grey				EQU 00000111b

yellow			EQU 00001110b
lighred			EQU 00001100b
lightcyan		EQU 00001011b
brown			EQU 00000111b
cyan				EQU 00000011b
lightblue		EQU 00001001b




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
oldintseg		resw		1
oldintoff		resw		1
graphicm		resb		100
playermove	dw		1
styxmove		dw		1
playerlocation dw		2883
styxlocation	dw		10950
styx_in			dw		0
trailcount		dw		0
conquer		dw		0
pressshift		dw		0

left				dw		0
right				dw		0
top				dw		0
bot				dw		0
conquercount dd      	0

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
			jmp short .toploop

	;bottom grey line
	.botline:
		mov	di, 62720
		.botloop:
			cmp	di, 63040
			je	.side

			mov byte [es:di], grey
			inc di
			jmp	short .botloop

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
			jmp short .infoloop

	;blue botpanel
	.botpanel:
		mov	di, 63040
		.botploop:
			cmp	di, 64000
			je .percentage

			mov	byte [es:di], blue
			inc di
			jmp short .botploop
			
	.percentage:
		mov di, 1221
		mov	byte [es:di], red
		add di, 3
		mov	byte [es:di], red
		add di, 319
		mov	byte [es:di], red
		add di, 319
		mov	byte [es:di], red
		add di, 319
		mov	byte [es:di], red
		add di, 3
		mov	byte [es:di], red

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
		je .movejump
		cmp byte [es:di], white ;check if player is in white pixel
		je .movejump
		mov word [playerlocation], ax	;if player is not in blue, make the move

		cmp word [conquer], 1
		je .movejump
		cmp byte [es:di], black
		jne .movejump
	
		mov ax, di
		sub word ax, [playermove]
		mov di, ax
		mov byte [es:di], white
		mov word [conquer], 1

		jmp far .end

	;check if the player has returned to the grey area
	.moveend:
		mov word di, [playerlocation]
		cmp byte [es:di], grey	;check if the player has returned to grey area
		jne .drawtrail

		mov word [conquer], 0			;set conquering flags
		mov word [pressshift], 0		;0 if player is in grey area
		call drawME
		jmp .end

	;draw trail for conquered area
	.drawtrail:
		mov word di, [playerlocation]
		mov byte [es:di], white
		jmp .moveconquer

	;normal movement in grey area
	.movegrey:
		cmp byte [es:di], grey	;playermove + current position
		jne .automove						;has to be in grey area
		mov word [playerlocation], ax	;make the grey area move
		jmp .end

	.movejump:
		jmp .end

	.automove:
		cmp word [playermove], -1
		je .updown
		cmp word [playermove], 1
		je .updown
		cmp word [playermove], -320
		je .leftright
		cmp word [playermove], 320
		je .leftright

		.updown:
			mov word di, [playerlocation]
			add di, 320
			cmp byte [es:di], grey
			jne .moveup

			.movedown:
				mov word [playermove], 320
				mov word ax, [playermove]
				add word ax, [playerlocation]
				mov word [playerlocation], ax
				jmp .end

			.moveup:
				mov word [playermove], -320
				mov word ax, [playermove]
				add word ax, [playerlocation]
				mov word [playerlocation], ax
				jmp .end

		.leftright:
			mov word di, [playerlocation]
			add di, 1
			cmp byte [es:di], grey
			jne .moveleft

			.moveright:
				mov word [playermove], 1
				mov word ax, [playermove]
				add word ax, [playerlocation]
				mov word [playerlocation], ax
				jmp .end

			.moveleft:
				mov word [playermove], -1
				mov word ax, [playermove]
				add word ax, [playerlocation]
				mov word [playerlocation], ax
				jmp .end

	.end:
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
		jmp .end

	.rightbot:
		call checkRight
		cmp word [right], 1
		je .changemove

		call checkBot
		cmp word [bot], 1
		je .changemove
		jmp .end

	.lefttop:
		call checkLeft
		cmp word [left], 1
		je .changemove

		call checkTop
		cmp word [top], 1
		je .changemove
		jmp .end

	.righttop:
		call checkRight
		cmp word [right], 1
		je .changemove

		call checkTop
		cmp word [top], 1
		je .changemove
		jmp short .end

	.changemove:
		call randMove

	.end:
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
	jmp short .end

	.setFlag:
		mov word [left], 1

	.end:
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
	jmp short .end

	.setFlag:
		mov word [right], 1

	.end:
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
	jmp short .end

	.setFlag:
		mov word [top], 1

	.end:
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
	jmp short .end

	.setFlag:
		mov word [bot], 1

	.end:
	popa
	ret

randMove:		 ;generate a rand no using the system time
	pusha

	mov ah, 00h	 ;interrupts to get system time
	int 1Ah			 ;CX:DX now hold number of clock ticks since midnight

	mov ax, dx
	xor dx, dx	;clear dx
	mov cx, 2
	div cx		;here dx contains the remainder of the division - from 0 to 4

	mov word ax, [styxlocation]

	cmp word [styxmove], 319
	je .checklbot
	cmp word [styxmove], 321
	je .checkrbot
	cmp word [styxmove], -321
	je .checkltop
	cmp word [styxmove], -319
	je .checkrtop

	.checklbot:
		cmp word [left], 1
		je .leftwall
		jmp .botwall

	.checkrbot:
		cmp word [right], 1
		je .rightwall
		jmp .botwall

	.checkltop:
		cmp word [left], 1
		je .leftwall
		jmp .topwall

	.checkrtop:
		cmp word [right], 1
		je .leftwall
		jmp .topwall

	.topwall:
		cmp dx, 0
		jne .movedownright
		jmp .movedownleft

	.botwall:
		cmp dx, 0
		jne .moveupright
		jmp .moveupleft

	.leftwall:
		cmp dx, 0
		jne .moveupright
		jmp .movedownright

	.rightwall:
		cmp dx, 0
		jne .moveupleft
		jmp .movedownleft

	.moveupleft:
		mov word [styxmove], -321
		add word ax, [styxmove]
		mov word [styxlocation], ax
		jmp .moveend

	.moveupright:
		mov word [styxmove], -319
		add word ax, [styxmove]
		mov word [styxlocation], ax
		jmp .moveend

	.movedownright:
		mov word [styxmove], 321
		add word ax, [styxmove]
		mov word [styxlocation], ax
		jmp .moveend

	.movedownleft:
		mov word [styxmove], 319
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

drawME:
    pusha

	mov word [conquercount], 0
    mov ax, background
    mov es, ax
    mov di, 2880
	
    .calcarea:
        cmp di, 63040
        je .calcper
        cmp byte [es:di], white
        jne .nextpixel

        mov word ax, [conquercount]
        inc ax
        mov word [conquercount], ax
            
        .nextpixel:
            inc di
            jmp .calcarea
            
    .calcper:
		mov di, 0
        mov word ax, [conquercount]
        mov cx, 580			;black area in pixels ~58032
		xor dx, dx
        div cx

		xor dx, dx
        mov cx, 10
        div cx

    .drawfirst:
		call .drawone
        cmp ax, 1
        je .drawsecond
		
		call .drawseven
        cmp ax, 7
        je .drawsecond
       
		call .drawthree
        cmp ax, 3
        je .drawsecond
       
		call .drawfive
        cmp ax, 5
        je .drawsecond

		call .drawtwo
        cmp ax, 2
        je .drawsecond
        
		call .drawsix
        cmp ax, 6
		je .drawsecond
            
		call .draweight
		cmp ax, 8
		je .drawsecond
		
		call .drawnine
		cmp ax, 9
		je .drawsecond

		call .drawfour
		cmp ax, 4
		je .drawsecond
		
		call .drawzero
		cmp ax, 0
		je .drawsecond

    .drawsecond:
		mov di, 5
		call .drawone
        cmp dx, 1
        je .end
		
		mov di, 5
		call .drawseven
        cmp dx, 7
        je .end
       
	    mov di, 5
	 	call .drawthree
        cmp dx, 3
        je .end
       
	    mov di, 5
		call .drawfive
        cmp dx, 5
        je .end

		mov di, 5
		call .drawtwo
        cmp dx, 2
        je .end
        
		mov di, 5
		call .drawsix
        cmp dx, 6
		je .end
         
		mov di, 5
		call .draweight
		cmp dx, 8
		je .end
		
		mov di, 5
		call .drawnine
		cmp dx, 9
		je .end
        
		mov di, 5
		call .drawfour
		cmp dx, 4
		je .end
		 
		mov di, 5
		call .drawzero
		cmp dx, 0
		je .end


	.end:
	popa
	ret

	.drawzero:
		add di, 570
		mov byte [es:di], red
		inc di
		mov byte [es:di], red
		add di, 959
		mov byte [es:di], blue
		inc di
		mov byte [es:di], blue
		add di, 318
		mov byte [es:di], red
		add di, 320
		mov byte [es:di], red
		add di, 321
		mov byte [es:di], red
		inc di
		mov byte [es:di], red
		mov di, 0

		ret

	.drawone:

		add di, 570
		mov byte [es:di], blue
		inc di
		mov byte [es:di], blue
		add di, 318
		mov byte [es:di], blue
		add di, 3
		mov byte [es:di], red
		add di, 317
		mov byte [es:di], blue
		add di, 3
		mov byte [es:di], red
		add di, 318
		mov byte [es:di], blue
		inc di
		mov byte [es:di], blue
		add di, 318
		mov byte [es:di], blue
		add di, 3
		mov byte [es:di], red
		add di, 317
		mov byte [es:di], blue
		add di, 3
		mov byte [es:di], red
		add di, 318
		mov byte [es:di], blue
		inc di
		mov byte [es:di], blue
		mov di, 0

		ret

	.drawtwo:
		add di, 889
		mov byte [es:di], blue
		add di, 3
		mov byte [es:di], red
		add di, 317
		mov byte [es:di], blue
		add di, 3
		mov byte [es:di], red
		add di, 637
		mov byte [es:di], red
		add di, 3
		mov byte [es:di], blue
		add di, 317
		mov byte [es:di], red
		add di, 3
		mov byte [es:di], blue
		mov di, 0

		ret

	.drawthree:
		add di, 1530
		mov byte [es:di], red
		inc di
		mov byte [es:di], red
		add di, 959
		mov byte [es:di], red
		inc di
		mov byte [es:di], red
		mov di, 0

		ret

	.drawfour:
		add di, 570
		mov byte [es:di], blue
		inc di
		mov byte [es:di], blue
		add di, 1919
		mov byte [es:di], blue
		inc di
		mov byte [es:di], blue
		mov di, 0

		ret

	.drawfive:
		add di, 889
		mov byte [es:di], red
		add di, 3
		mov byte [es:di], blue
		add di, 317
		mov byte [es:di], red
		add di, 3
		mov byte [es:di], blue
		mov di, 0

		ret

	.drawsix:
		add di, 889
		mov byte [es:di], red
		add di, 3
		mov byte [es:di], blue
		add di, 317
		mov byte [es:di], red
		add di, 3
		mov byte [es:di], blue
		add di, 640
		mov byte [es:di], red
		add di, 320
		mov byte [es:di], red
		mov di, 0

		ret

	.drawseven:
		add di, 570
		mov byte [es:di], red
		inc di
		mov byte [es:di], red
		mov di, 0

		ret
		
	.draweight:
		add di, 892
		mov byte [es:di], red
		add di, 320
		mov byte [es:di], red
		mov di, 0

		ret
		
	.drawnine:
		add di, 1849
		mov byte [es:di], blue
		add di, 320
		mov byte [es:di], blue
		mov di, 0

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
	call randMove

	.mainloop:
		mov dx, 10
		.pause1:
			mov cx, 10
		.pause2:
			dec cx
			jne .pause2
			dec dx
			jne .pause1

		call moveStyx
		call draw
		call movePlayer
		call moveStyx

		cmp word [pressesc], 1
		jne short .mainloop
		jmp short .dosexit


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