IDEAL
MODEL small
STACK 100h

DATASEG
helpCx dw (0)
helpDx dw (0)

ball_cord dw 1 dup (160,50) ;middle point (160,100)
next_ball_cord dw 1 dup (160,50)
ball_power dw 1 dup (2,-2) ;(Left Element)- the x-axis power, (Right Element)- the y-axis power

racket1 dw 1 dup (50,100)
racket2 dw 1 dup (270,100)

racket_mind dw (0) ; 0-move down, 1-move up, 2-calculate next move

previous_time db 0


starting_screen_message db "||| Welcome to SUPER PONG! |||",10,10,"The game is for 2-Players. you fight",10,"against your friend in an",10,"ultimate battle of pong!",10,10,"Your goal is to score",10,"on the other player's gate",10,"For every gool you make, you get 1 point",10,"to win you need to have 3 first",10,10,"Controlls:",10,10,"Player-1: w-up, s-down",10,10,"Player-2: o-up, l-down",10,10,"if you wish to start, click on a key -$"

CODESEG
;Ball's Cord's
xBall equ [ball_cord]
yBall equ [ball_cord+2]

xDir equ [ball_power] 
yDir equ [ball_power+2]

racket_width equ 3	; controlls the rackets x distance from the center of the racket
racket_hight equ 30 ; controlls the rackets y distance from the center of the racket

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

background_color equ cyan

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
	push background_color
	call paint_pixel
	ret
endp

proc draw_ball
	push [next_ball_cord]
	push [next_ball_cord+2]
	push red
	call paint_pixel
	ret
endp

proc if_key_pressed ; If no key was pressed, skip input stage
    mov ah, 01h
    int 16h
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

proc draw_racket ;gets x,y and color of the racket
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	
	mov [helpDx], racket_hight
	add [helpDx], racket_hight
	set_by_dx:
		mov [helpCx], racket_width
		add [helpCx], racket_width
		draw_racket_loop:
			mov ah,0Ch
			mov al,[bp+4]
			mov bh,0
			
			mov cx,[bp+8]
			add cx, racket_width
			sub cx, [helpCx]
			
			mov dx, [bp+6]
			add dx, racket_hight
			sub dx, [helpDx]
			
			int 10h
			dec [helpCx]
			cmp [helpCx],-1
			jne draw_racket_loop
		dec [helpDx]
		cmp [helpDx],-1
		jne set_by_dx
		
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 6
endp

proc racket_collision ; gets through the stack the x and y of the racket and changes the ball direction based on it
	push bp
	mov bp, sp
	
	push ax
	push bx
	push cx
	push dx
	
	mov ax, [bp+6]
	mov bx, [bp+4]
	
	add ax, racket_width	; if the ball is in the racket
	add ax, 1
	cmp ax, xBall
	jl end_of_checking
	sub ax, racket_width
	sub ax, racket_width
	sub ax, 2
	cmp ax, xBall
	jg end_of_checking 
	add bx, racket_hight
	add bx, 1
	cmp bx, yBall
	jl end_of_checking
	sub bx, racket_hight
	sub bx, racket_hight
	sub bx, 2
	cmp bx, yBall
	jg end_of_checking
	
	call flip_ball_direction ; when the ball is in the racket
	
end_of_checking: ; if the ball in some case not in the range of the racket
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
endp

proc flip_ball_direction
	push ax
	push bx
	
	mov ax,yDir
	neg ax
	mov bx,xDir
	xchg ax,bx
	mov yDir,ax
	mov xDir,bx
	
	pop bx
	pop ax
	ret
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
			mov al,background_color
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
	push [racket1]
	push [racket1+2]
	push yellow
	call draw_racket
	
	push [racket1]
	push [racket1+2]
	push blue
	call paint_pixel
	
	push [racket2]
	push [racket2+2]
	push white
	call draw_racket

	push [racket2]
	push [racket2+2]
	push blue
	call paint_pixel
	
	push xBall
	push yBall
	push white
	call paint_pixel
	
	gameloop:
		mov ah, 2Ch ; Loop system: checks every iteration if time has passed
        int 21h ; ch=hour cl=minute dh=second dl=1/100 second
        cmp dl,prev_time
		je gameloop
		mov prev_time,dl
		
		call if_key_pressed
		jz way_for_game_calc1
		
		call get_key
		cmp al,"w"
		je key_w
		cmp al,"s"
		je key_s
		cmp al,"Q"
		je key_Q
		jmp gameloop_calc
		
		key_w:
			mov ax, [racket1+2]
			sub ax, racket_hight
			cmp ax, 0
			je gameloop_calc
			push [racket1]
			push [racket1+2]
			push cyan
			call draw_racket
			
			sub [racket1+2],2
			
			push [racket1]
			push [racket1+2]
			push yellow
			call draw_racket
way_for_game_calc1:
			jmp gameloop_calc
		key_s:
			mov ax, [racket1+2]
			add ax, racket_hight
			cmp ax, 200
			je gameloop_calc
			push [racket1]
			push [racket1+2]
			push cyan
			call draw_racket
			
			add [racket1+2],2
			
			push [racket1]
			push [racket1+2]
			push yellow
			call draw_racket
			jmp gameloop_calc
		key_Q:
			jmp exit
		
	gameloop_calc:
	    cmp xBall, 318
        jge right_side_boundry
        cmp xBall, 2
        jle left_side_boundry
        y_boundries:
        cmp yBall, 198
		jge top_boundry_call
        cmp yBall, 2
        jle bottom_boundry_call
		
		push [racket1]
		push [racket1+2]
		call racket_collision
		
		push [racket2]
		push [racket2+2]
		call racket_collision
        jmp gameloop_draw
		
		top_boundry_call:
			call flip_ball_direction
			jmp gameloop_draw
		
		bottom_boundry_call:
			call flip_ball_direction
			jmp gameloop_draw

        right_side_boundry:
			call flip_ball_direction
			jmp gameloop_draw

		left_side_boundry:
			call flip_ball_direction
			jmp gameloop_draw

	gameloop_draw:
		xor ax,ax
		mov ax,xDir
		add [next_ball_cord],ax
		mov ax,yDir
		add [next_ball_cord+2],ax
		
		call draw_ball
		
		call clear_ball
		
		
		mov ax,[next_ball_cord]
		mov xBall,ax
		mov ax,[next_ball_cord+2]
		mov yBall,ax
		
		push [racket1]
		push [racket1+2]
		push yellow
		call draw_racket
	
		push [racket2]
		push [racket2+2]
		push white
		call draw_racket
		jmp gameloop
		
		
exit:
    mov ax, 4C00h
    int 21h
END start
