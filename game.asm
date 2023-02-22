IDEAL
MODEL small
STACK 100h

DATASEG
filename db 'startScreen.bmp',0
filehandle dw ?
Header db 54 dup (0)
Palette db 256*4 dup (0)
ScrLine db 320 dup (0)

helpCx dw (0)
helpDx dw (0)

ball_cord dw 1 dup (160,100) ;middle point (160,100)
next_ball_cord dw 1 dup (160,100)

ball_power dw 1 dup (3,-3) ;(Left Element)- the x-axis power, (Right Element)- the y-axis power
calc_ball_dir dw 1 dup (3,-3)

racket1 dw 1 dup (50,100)
racket2 dw 1 dup (270,100)
racket_movement_speed_val dw (6)
hit db (0)

times_in_a_row dw (0) ; the times in a row that the ball has changed direction
hits_needed dw (0)
hit_counter dw (0)

racket_wanted_y dw (100)
racket_mind dw (0) ; 0-moving to wanted spot, 1-calculate next move

previous_time db 0

player1_points dw (2)
player2_points dw (0)

starting_screen_message db "||| Welcome to SUPER PONG! |||",10,10,"The game is for 1-Player. you are going to fight against a bot in an ultimate",10,"battle of pong!",10,10,"Your goal is to score on the bot's gate",10,"To win you will only need to score once,",10,"but the bot has to score 3 times to win!","Try to win as fasr as you can",10,"because every time the bot scores,",10,"the ball gets faster!",10,10,10,"Controlls:",10,10,"Player: w-up, s-down",10,10,"Quit: shift + Q",10,10,"if you wish to start, press on any key",10,10,"when you lose you can press",10,"on any key to restart$"

player1_score_text db 10,10,10,"        +1 To The Player On Scroing!$"
player2_score_text db 10,10,10,"        +1 To The Bot On Scroing!$"

player1_win_text db 10,10,"       Player 1 Has Won The Game!       ",10,10,10,10,"well done beating the bot", 10,10,10 , 10,10,10 , 10,10,10 ," $"
player2_win_text db 10,10,"       The Bot Has Won The Game! ",1,"     ",10,10,10,10,"       #skiilIssue, #getBetter", 10,10,10 , 10,10,10 , 10,10,10 ," $"

CODESEG
;Ball's Cord's
xBall equ [ball_cord]
yBall equ [ball_cord+2]
ball_radius equ 3

min_y equ 20
max_y equ 180

;Ball's Directions
xDir equ [ball_power] 
yDir equ [ball_power+2]

;Rackets
racket_width equ 6	; controlls the rackets x distance from the center of the racket
racket_hight equ 20 ; controlls the rackets y distance from the center of the racket
racket_movement_speed equ [racket_movement_speed_val]

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

proc OpenFile
	; Open file
	mov ah, 3Dh
	xor al, al
	mov dx, offset filename
	int 21h

	mov [filehandle], ax
	ret
endp OpenFile

proc ReadHeader
	; Read BMP file header, 54 bytes
	mov ah,3fh
	mov bx, [filehandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	ret
endp ReadHeader

proc ReadPalette
	; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	ret
endp ReadPalette

proc CopyPal
	; Copy the colors palette to the video memory
	; The number of the first color should be sent to port 3C8h
	; The palette is sent to port 3C9h
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx

PalLoop:
	; Note: Colors in a BMP file are saved as BGR values rather than RGB .
	mov al,[si+2] ; Get red value .
	shr al,2 ; Max. is 255, but video palette maximal
	; ; value is 63. Therefore dividing by 4.
	out dx,al ; Send it .
	mov al,[si+1] ; Get green value .
	shr al,2
	out dx,al ; Send it .
	mov al,[si] ; Get blue value .
	shr al,2
	out dx,al ; Send it .
	add si,4 ; Point to next color .
	; (There is a null chr. after every color)
	loop PalLoop
	ret
endp CopyPal

proc CopyBitmap
	; BMP graphics are saved upside-down.
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx,200
	
PrintBMPLoop:
	push cx
	; di = cx*320, point to the correct screen line
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	; Read one line
	mov ah,3fh
	mov cx,320
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb;
	mov cx,320
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	;rep movsb is same as the following coderep movsb is same as the following code :
	;mov es:di, ds:si
	;inc si
	;inc di
	;dec cx
	; ... ;loop until cx=0
	pop cx
	loop PrintBMPLoop
	ret
endp CopyBitmap

proc start_screen

	mov dx, offset starting_screen_message
	mov ah,09h
	int 21h
	
	mov ah,0
	int 16h
endp

proc clear_ball

	mov [helpDx], ball_radius
	add [helpDx], ball_radius
	set_by_dx2:
		mov [helpCx], ball_radius
		add [helpCx], ball_radius
		draw_ball_loop2:
			mov ah,0Ch
			mov al,background_color
			mov bh,0
			
			mov cx, xBall
			add cx, ball_radius
			sub cx, [helpCx]
			
			mov dx, yBall
			add dx, ball_radius
			sub dx, [helpDx]
			
			int 10h
			dec [helpCx]
			cmp [helpCx],-1
			jne draw_ball_loop2
		dec [helpDx]
		cmp [helpDx],-1
		jne set_by_dx2
	ret
endp

proc draw_ball
	push ax
	push bx
	push cx
	push dx
	
	mov [helpDx], ball_radius
	add [helpDx], ball_radius
	set_by_dx1:
		mov [helpCx], ball_radius
		add [helpCx], ball_radius
		draw_ball_loop:
			mov ah,0Ch
			mov al,white
			mov bh,0
			
			mov cx, [next_ball_cord]
			add cx, ball_radius
			sub cx, [helpCx]
			
			mov dx, [next_ball_cord+2]
			add dx, ball_radius
			sub dx, [helpDx]
			
			int 10h
			dec [helpCx]
			cmp [helpCx],-1
			jne draw_ball_loop
		dec [helpDx]
		cmp [helpDx],-1
		jne set_by_dx1
	
    pop dx
    pop cx
    pop bx
    pop ax
	ret
endp

proc play_sound ;gets the sound
    push bp
    mov bp, sp
    push ax

    in al, 61h ; open the speaker
    or al, 00000011b
    out 61h, al
    
    mov al, 0B6h ; make al send the notes
    out 43h, al

    mov ax, [bp+4] ; play the sound
    out 42h, al ; sending lower bytes
    mov al, ah
    out 42h, al ; sending upper byte

    pop bp
    pop ax
    ret 2
endp

proc wait_sound ; waiting 1/10 of a second
    push ax
    push cx
    push dx
    
    mov ah, 86h
    mov cx, 0001h
    mov dx, 86A0h
    int 15h

    pop dx
    pop cx
    pop ax
    ret
endp

proc stop_sound
    push ax

    in al, 61h
    and al, 11111100b
    out 61h, al

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

proc paint_pixel ; x, y, color (dont really need this function but just to check things)
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
	add ax, ball_radius
	add ax, 1 ; one pixel before the ball is interacting
	cmp ax, xBall
	jl jmp_to_end_of_checking
	sub ax, racket_width
	sub ax, racket_width
	sub ax, ball_radius
	sub ax, ball_radius
	sub ax, 2 ; reversing the 1 pixel
	cmp ax, xBall
	jg jmp_to_end_of_checking 
	add bx, racket_hight
	add bx, ball_radius
	add bx, 1 ; same
	cmp bx, yBall
	jl jmp_to_end_of_checking
	sub bx, racket_hight
	sub bx, racket_hight
	sub bx, ball_radius
	sub bx, ball_radius
	sub bx, 2 ; same
	cmp bx, yBall
	jg jmp_to_end_of_checking

	cmp [times_in_a_row], 4
	jl continue

	mov xBall, 160
	mov yBall, 100
	mov [next_ball_cord], 160
	mov [next_ball_cord+2], 100
	call draw_ball
	mov [times_in_a_row], 0
	jmp end_of_checking

jmp_to_end_of_checking:
	mov [times_in_a_row], 0
	
	jmp end_of_checking
	
continue:
	call flip_ball_direction ; when the ball is in the racket
	call clear_ball
	mov ax, [bp+6]
	cmp ax, [racket1]
	je racket1_check
	; if it is racket2
	mov ax, [hits_needed]
	inc [hit_counter]
	cmp [hit_counter], ax
	jl keep_on_trying
	
	mov [racket_mind], 0
	jmp end_of_checking
	
keep_on_trying:	
	mov [racket_mind], 1
	push 6833
	call play_sound
	jmp end_of_checking
	
racket1_check:
	inc [hit_counter]
	push 6833
	call play_sound
	
end_of_checking: ; if the ball in some case not in the range of the racket
	inc [times_in_a_row]
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
	push green
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
	
	jmp calc_ball_hit

	got_to_wanted:
		mov ax, [hits_needed]
		cmp [hit_counter], ax
		jl go_normally
		
		jmp way_for_end_calc
		
	go_normally:
		mov bx, ySpot
		sub bx, 10 ; half of the racket's hight
		cmp bx, yRacket2
		jg lower_y
		add bx, 20 ; the racket's hight
		cmp bx, yRacket2
		jl upper_y
		jmp way_for_end_calc
		
		upper_y:

			mov ax, yRacket2
			sub ax, racket_hight
			sub ax, racket_movement_speed
			cmp ax, 0
			jl set0
			
			call clear_racket2
			mov ax, racket_movement_speed
			sub yRacket2, ax
			call draw_racket2

			jmp way_for_end_calc
				set0:
					call clear_racket2
					mov ax, racket_hight
					mov yRacket2, ax
					call draw_racket2
					jmp way_for_end_calc
		lower_y:
			mov ax, yRacket2
			add ax, racket_movement_speed
			add ax, racket_hight
			cmp ax, 200
			jg set200
			
			call clear_racket2
			mov ax, racket_movement_speed
			add yRacket2, ax
			call draw_racket2
way_for_end_calc:
			jmp end_calc
				set200:
					call clear_racket2
					mov ax, 200
					sub ax, racket_hight
					mov yRacket2, ax
					call draw_racket2
					jmp way_for_end_calc
			
	calc_ball_hit:
		
		mov ax, xBall
		mov bx, yBall
		
		mov cx, xDir
		mov [calc_ball_dir], cx
		mov cx, yDir
		mov [calc_ball_dir+2], cx
		
		check_x_of_try_ball:
			cmp bx, 194
			jge on_walls
			cmp bx, 6
			jle on_walls
			jmp not_on_walls
			
			on_walls:
				call flip_calc_ball_direction

			not_on_walls:
				add ax, [calc_ball_dir]
				
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
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
    ret
endp

proc wait_second
	push ax
	push bx
	push cx
	push dx
	
	mov cx, 000Fh
	mov dx, 4240h
	mov ah, 86h
	int 15h
	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp

proc three_seconds
	push ax
	push bx
	push cx
	push dx
	
	mov cx, 0002Dh
	mov dx, 0C6C0h
	mov ah, 86h
	int 15h
	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp

proc reset_ball
	call set_background
	call clear_ball
		
	mov xBall, 160
	mov yBall, 100
	mov [next_ball_cord], 160
	mov [next_ball_cord+2], 100
	call draw_ball
	mov [racket1], 50
	mov [racket1+2], 100
	mov xRacket2, 270
	mov yRacket2, 100
	call draw_racket1
	call draw_racket2
		
	mov xDir, 3
	mov yDir, -3
	ret
endp

proc ball_in
	push ax
	push bx
	push cx
	push dx

	cmp xBall, 10
	jle score_to_player2
	
	cmp xBall, 310
	jge score_to_player1
	
	jmp ball_not_in
	
	score_to_player1:
		inc [player1_points]
		call set_background
		mov dx, offset player1_score_text
		mov ah, 09h
		int 21h
		
		push 18242
		call play_sound
		call wait_sound
		call stop_sound

		call three_seconds

		jmp reset_ball_jmp

	score_to_player2:
		inc [player2_points]
		call set_background
		mov dx, offset player2_score_text
		mov ah, 09h
		int 21h
		
		push 18242
		call play_sound
		call wait_sound
		call stop_sound

reset_ball_jmp:
		call wait_second
		call wait_second
		call reset_ball
		call wait_second
		mov ax, [player1_points]
		add xDir, ax
		add racket_movement_speed, ax
		
		sub yDir, ax

		mov ax, xDir
		mov [calc_ball_dir], ax
		mov ax, yDir
		mov [calc_ball_dir+2], ax
; wait for theree seconds
		
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
	
	mov ah, 2Ch
	int 21h
	xor dh,dh
	and dl, 0111111b
	mov [hits_needed], dx
	
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
	push green
	call draw_racket
	gameloop:
		mov ah, 2Ch ; Loop system: checks every iteration if time has passed
        int 21h ; ch=hour cl=minute dh=second dl=1/100 second
        cmp dl,prev_time
		je gameloop
		mov prev_time,dl
		
		call stop_sound
		
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
			jle gameloop_calc
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
			jge gameloop_calc
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
        cmp yBall, 195
		jge top_boundry_call
        cmp yBall, 5
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
			push 4560
			call play_sound
			jmp gameloop_draw
		
		bottom_boundry_call:
			call flip_ball_direction
			push 4560
			call play_sound
			jmp gameloop_draw
			
reset_the_ball:
		call clear_ball
		mov xBall, 160
		mov yBall, 100
		mov [next_ball_cord], 160
		mov [next_ball_cord+2], 100
		call draw_ball
		call wait_second

	gameloop_draw:
		xor ax,ax
		mov ax,xDir
		add [next_ball_cord],ax
		mov ax,yDir
		add [next_ball_cord+2],ax
		
		call clear_ball
		
		call draw_ball
		
		
		mov ax,[next_ball_cord]
		mov xBall,ax
		mov ax,[next_ball_cord+2]
		mov yBall,ax
		
		
		call draw_racket1
		call draw_racket2
		
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

		jmp restart
		
	player2_win:
		call set_background
		
		mov dx, offset player2_win_text
		mov ah, 9h
		int 21h
		
	restart:

		
exit:
    mov ax, 4C00h
    int 21h
END start
