IDEAL
MODEL small
STACK 100h

DATASEG
helpCx dw (0)
helpDx dw (0)
ball_cord dw 1 dup (320,100) ;middle point (160,100)
next_ball_cord dw 1 dup (0,0)
ball_power db 1 dup (1,1) ;(Left Element)- the x-axis power, (Right Element)- the y-axis power

previous_time db 0


starting_screen_message db "||| Welcome to SUPER PONG! |||",10,10,"The game is for 2-Players. you fight",10,"against your friend in an",10,"ultimate battle of pong!",10,10,"Your goal is to score",10,"on the other player's gate",10,"For every gool you make, you get 1 point",10,"to win you need to have 3 first",10,10,"Controlls:",10,10,"Player-1: w-up, s-down",10,10,"Player-2: o-up, l-down",10,10,"if you wish to start, click on a key -$"

CODESEG
;Ball's Cord's
xBall equ [ball_cord]
yBall equ [ball_cord+2]

xDir equ [ball_power] 
yDir equ [ball_power+1]

clock equ 40:6Ch ;the clock's location in the memory
prev_time equ [previous_time]

;Colors:
black equ    00h
blue equ     01h
Dgreen equ   02h
cyan equ     03h
red equ      04h
magenta equ  05h
brown equ    06h
Lgray equ    07h
Dgray equ    08h
Lblue equ    09h
green equ    0Ah
Lcyan equ    0Bh
Lred equ     0ch
Lmagenta equ 0dh
yellow equ   0Eh
white equ    0Fh

proc start_screen
	mov dx, offset starting_screen_message
	mov ah,09h
	int 21h
	
	mov ah,0
	int 16h
endp

proc clear_ball
	push xBall
	push yBall
	push black
	call paint_pixel
	ret
endp

proc draw_ball
	push xBall
	push yBall
	push green
	call paint_pixel
	ret
endp

proc move_ball
	push ax
	push bx
	push cx
	push dx
	cmp xBall,320
	je flipPower 
	
	cmp xBall,0
	je flipPower
	
	cmp yBall,200
	je flipPower
	
	cmp yBall,0
	je flipPower
	jmp nextBall
	
	flipPower:
		mov ah,xDir
		mov al,yDir
		neg al
		mov xDir,al
		mov yDir,ah
nextBall:
	mov bx,xBall
	mov cx,yBall
	mov [next_ball_cord],bx
	mov [next_ball_cord+2],cx
	
	xor ax,ax
	xor dx,dx
	mov al,xDir
	mov dl,yDir
	add [next_ball_cord],ax
	add [next_ball_cord+2],dx
	
	push [next_ball_cord]
	push [next_ball_cord+2]
	push white
	call paint_pixel
	
	push xBall
	push yBall
	push black
	call paint_pixel
	
	mov bx,[next_ball_cord]
	mov cx,[next_ball_cord+2]
	mov xBall,bx
	mov yBall,cx
	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp

proc get_key ; returns ah=scancode al=ascii
    mov ah, 00h
    int 16h
    ret
endp

proc graphic_mode
    push ax

    mov ah, 00h
    mov al, 13h
    int 10h
    
    pop ax
    ret
endp

proc paint_pixel ; x, y, color
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

	mov ah, 0Ch
	mov bh,0
    mov al, [bp+4]
    mov dx, [bp+6]
    mov cx, [bp+8]
    int 10h

    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 6
endp

proc set_background
	;sets the whole screen to one color
	push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
	push si
	mov [helpDx],201
	setOnCX:
		mov [helpCx],321
		setScreen:
			mov ah,0Ch
			mov al,brown
			mov bh,0
			mov cx,321
			mov dx,200
			sub cx,[helpCx]
			mov dx,[helpDx]
			int 10h
			dec [helpCx]
			mov cx,[helpCx]
			loop setScreen
		dec [helpDx]
		cmp [helpDx],0
		jge setOnCX
	pop si	
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
    ret
endp
start:
    mov ax, @data
    mov ds, ax
	
	call graphic_mode
	call start_screen
	call graphic_mode
	call set_background
	
	push xBall
	push yBall
	push white
	call paint_pixel
	
	gameloop:
		mov ah, 2Ch ; Loop system: checks every iteration if time has passed
        int 21h ; ch=hour cl=minute dh=second dl=1/100 second
        cmp dl, prev_time
		jle gameloop
		mov prev_time,dl
		call get_key
		cmp al,"Q"
		je exit
		jmp gameloop_calc
		
	gameloop_calc:
		xor ax,ax
		mov al,xDir
		add [next_ball_cord],ax
		mov al,yDir
		add [next_ball_cord+2],ax
		
		push [next_ball_cord]
		push [next_ball_cord+2]
		push white
		call paint_pixel
		
		mov ax,[next_ball_cord]
		mov xBall,ax
		mov ax,[next_ball_cord+2]
		mov yBall,ax
		jmp gameloop
		
		
exit:
    mov ah, 4Ch
    int 21h
END start