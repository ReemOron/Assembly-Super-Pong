IDEAL
MODEL small
STACK 100h

DATASEG
helpCx dw (0)
helpDx dw (0)

ball_cord dw 1 dup (160,20) ;middle point (160,100)
next_ball_cord dw 1 dup (160,20)
ball_power dw 1 dup (4,-4) ;(Left Element)- the x-axis power, (Right Element)- the y-axis power
calc_ball_dir dw 1 dup (4,-4)

racket1 dw 1 dup (50,100)
racket2 dw 1 dup (270,100)

times_in_a_row dw (0) ; the times in a row that the ball has changed direction

racket_wanted_y dw (100)
racket_mind dw (0) ; 0-moving to wanted spot, 1-calculate next move
need_to_check dw (0) ; 0-no, 1-yes

previous_time db 0

player1_points db (0)
player2_points db (0)

starting_screen_message db "||| Welcome to SUPER PONG! |||",10,10,"The game is for 1-Player. you are going to fight against a bot in an ultimate",10,"battle of pong!",10,10,"Your goal is to score on the bot's gate",10,"To win you will only need to score once",10,"but the bot has to score 3 times to win!",10,10,"Controlls:",10,10,"Player: w-up, s-down",10,10,"Quit: shift + Q",10,10,"if you wish to start, click on a key -$"

player1_score_text db "+1 To The Player On Scroing!"
player2_score_text db "+1 To The Bot On Scroing!"

player1_win_text db 10,10,"       Player 1 Has Won The Game!       ",10,10,10,10,"well done beating the bot",10,10,10,10," $"
player2_win_text db 10,10,"       The Bot Has Won The Game! ",1,"     ",10,10,10,10,"       #skiilIssue, #getBetter$",10,10,10,10," $"

CODESEG
;Ball's Cord's
xBall equ [ball_cord]
yBall equ [ball_cord+2]
ball_radius equ 1

;Ball's Directions
xDir equ [ball_power] 
yDir equ [ball_power+2]

;Rackets
racket_width equ 6	; controlls the rackets x distance from the center of the racket
racket_hight equ 10 ; controlls the rackets y distance from the center of the racket
racket_movement_speed equ 5

;Racket 2 (the bot one)
xRacket2 equ [racket2]
yRacket2 equ [racket2+2]
ySpot equ [racket_wanted_y]

;Clock stuff
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

background_color equ black

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
	push ax
	push bx
	push cx
	push dx
	
	mov dx, ball_radius
	mov ah, 0Ch
	mov bh,0
	
	push [next_ball_cord]
	push [next_ball_cord+2]
	push white
	call paint_pixel
	
    pop dx
    pop cx
    pop bx
    pop ax
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
	
	mov ax, [bp+6] ; x of the racket
	mov bx, [bp+4] ; y of the racket
	
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
	mov ax, [bp+6]
	cmp ax, xRacket2
	jne end_of_checking
	mov [racket_mind], 1
	
end_of_checking: ; if the ball in some case not in the range of the racket
	call draw_racket1
	call draw_racket2
	
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
endp

proc draw_racket1
	push [racket1]
	push [racket1+2]
	push yellow
	call draw_racket
	ret
endp

proc clear_racket1
	push [racket1]
	push [racket1+2]
	push background_color
	call draw_racket
	ret
endp

proc draw_racket2
	push xRacket2
	push yRacket2
	push white
	call draw_racket
	ret
endp

proc clear_racket2
	push xRacket2
	push yRacket2
	push background_color
	call draw_racket
	ret
endp

proc racket_calc
	push ax
	push bx
	push cx
	push dx
	
	mov ax, xBall
	add ax, 5
	cmp ax, 180
	jl normal_check
	sub ax, 10
	cmp ax, 180
	jg normal_check
	
	jmp calc_ball_hit
	
normal_check:
	cmp [racket_mind], 0
	je got_to_wanted
	
	cmp [racket_mind], 1
	je calc_ball_hit

	got_to_wanted:
		mov bx, ySpot
		sub bx, 5 ; half of the racket's hight
		cmp bx, yRacket2
		jg lower_y
		add bx, 10 ; the racket's hight
		cmp bx, yRacket2
		jl upper_y
		jmp way_for_end_calc
		
		upper_y:
			mov ax, yRacket2
			sub ax, racket_hight
			sub ax, racket_movement_speed
			cmp ax, 0
			jl way_for_end_calc
			
			call clear_racket2
			sub yRacket2, racket_movement_speed
			call draw_racket2
			jmp way_for_end_calc
			
		lower_y:
			mov ax, yRacket2
			add ax, racket_movement_speed
			add ax, racket_hight
			cmp ax, 200
			jg way_for_end_calc
			
			call clear_racket2
			add yRacket2, racket_movement_speed
			call draw_racket2
way_for_end_calc:
			jmp end_calc
			
	calc_ball_hit:
		
		mov ax, xBall
		mov bx, yBall
		
		mov cx, xDir
		mov [calc_ball_dir], cx
		mov cx, yDir
		mov [calc_ball_dir+2], cx
		
		check_x_of_try_ball:
			cmp bx, 198
			jge on_walls
			cmp bx, 2
			jle on_walls
			jmp not_on_walls
			
			on_walls:
				call flip_calc_ball_direction

			not_on_walls:
				;mov cx, [calc_ball_dir]
				add ax, [calc_ball_dir]
				
				;mov cx, [calc_ball_dir+2]
				add bx, [calc_ball_dir+2]
				
				cmp ax, 260
				jl check_x_of_try_ball
				
				mov ySpot, bx
				mov [racket_mind], 0
end_calc:
	pop dx
	pop cx
	pop bx
	pop ax
	ret
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

proc flip_calc_ball_direction
	push ax
	push bx
	
	mov ax, [calc_ball_dir+2]
	neg ax
	mov bx, [calc_ball_dir]
	xchg ax, bx
	mov [calc_ball_dir+2], ax
	mov [calc_ball_dir], bx
	
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

proc three_seconds
	push ax
	push cx
	push dx
	
	mov cx, 2Dh
	mov dx, 0C6C0h
	mov ah, 86h
	int 15h
	
	pop dx
	pop cx
	pop ax
	ret
endp

proc ball_in
	push ax
	push bx
	push cx
	push dx

	cmp xBall, 2
	jle score_to_player2
	
	cmp xBall, 318
	jge score_to_player1
	
	jmp ball_not_in
	
	score_to_player1:
		inc [player1_points]
		jmp reset_ball

	score_to_player2:
		inc [player2_points]
		
	reset_ball:
		call clear_ball
		mov xBall, 160
		mov yBall, 100
		mov [next_ball_cord], 160
		mov [next_ball_cord+2], 100
		call draw_ball
; wait for theree seconds
		call three_seconds ; need to change!
		
ball_not_in:
	pop dx
	pop cx
	pop bx
	pop ax
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
	
	push xRacket2
	push yRacket2
	push white
	call draw_racket
	
	gameloop:
		mov ah, 2Ch ; Loop system: checks every iteration if time has passed
        int 21h ; ch=hour cl=minute dh=second dl=1/100 second
        cmp dl,prev_time
		je gameloop
		mov prev_time,dl
		
		cmp [times_in_a_row], 10
		jl regular_start
		
		mov xBall, 160
		mov yBall, 100
		
		regular_start:
		call if_key_pressed
		jz way_for_game_calc1
		
		call get_key
		cmp al, "w"
		je key_w_check
		cmp al, "s"
		je key_s_check
		cmp al, "o"
		je key_o_check
		cmp al, "l"
		je key_l_check
		cmp al, "Q"
		je key_Q
		jmp gameloop_calc
		
		key_w_check:
			mov ax, [racket1+2]
			sub ax, racket_hight
			cmp ax, 0
			je gameloop_calc
			call clear_racket1
			
			mov ax, racket_movement_speed
			sub [racket1+2], ax
			
			call draw_racket1

way_for_game_calc1:
			jmp gameloop_calc
		key_s_check:
			mov ax, [racket1+2]
			add ax, racket_hight
			cmp ax, 200
			je gameloop_calc
			call clear_racket1
			
			mov ax, racket_movement_speed
			add [racket1+2], ax
			
			call draw_racket1			
			jmp gameloop_calc
			
		key_o_check:
			mov ax, [racket2+2]
			sub ax, racket_hight
			cmp ax, 0
			je gameloop_calc
			call clear_racket2
			
			mov ax, racket_movement_speed
			sub [racket2+2], ax
			
			call draw_racket2
			jmp gameloop_calc
		key_l_check:
			mov ax, [racket2+2]
			add ax, racket_hight
			cmp ax, 200
			je gameloop_calc
			call clear_racket2
			
			mov ax, racket_movement_speed
			add [racket2+2], ax
			
			call draw_racket2			
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
		
		push xRacket2
		push yRacket2
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
		
		call racket_calc
		
		cmp [player1_points], 3
		jge player1_win
		
		cmp [player2_points], 3
		jge player2_win
		
		call ball_in
		
		jmp gameloop
	
	player1_win:
		call set_background
		
		mov dx, offset player1_win_text
		mov ah, 9h
		int 21h

		jmp exit
		
	player2_win:
		call set_background
		
		mov dx, offset player2_win_text
		mov ah, 9h
		int 21h

exit:
    mov ax, 4C00h
    int 21h
END start
