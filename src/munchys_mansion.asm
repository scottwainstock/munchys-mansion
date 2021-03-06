  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
game_state .rs 1  ; .rs 1 means reserve one byte of space

ball_x       .rs 1  ; ball horizontal position
ball_y       .rs 1  ; ball vertical position
ball_up      .rs 1  ; 1 = ball moving up
ball_down    .rs 1  ; 1 = ball moving down
ball_left    .rs 1  ; 1 = ball moving left
ball_right   .rs 1  ; 1 = ball moving right
ball_speed_x .rs 1  ; ball horizontal speed per frame
ball_speed_y .rs 1  ; ball vertical speed per frame

munchy_top_left_x     .rs 1
munchy_top_right_x    .rs 1
munchy_bottom_left_x  .rs 1
munchy_bottom_right_x .rs 1
munchy_top_left_y     .rs 1
munchy_top_right_y    .rs 1
munchy_bottom_left_y  .rs 1
munchy_bottom_right_y .rs 1
munchy_animation_frame_num .rs 1

projectile_thrown .rs 1
projectile_x      .rs 1
projectile_y      .rs 1
projectile_up     .rs 1
projectile_down   .rs 1
projectile_left   .rs 1
projectile_right  .rs 1
projectile_speed  .rs 1
projectile_animation_idx .rs 1

buttons1      .rs 1
last_buttons1 .rs 1

score1_high .rs 1  ; player 1 score, 0-15
score1_low  .rs 1  ; player 1 score, 0-15
score1_ones .rs 1  ; player 1 score, 0-15
score1_tens .rs 1  ; player 1 score, 0-15

pointer_low  .rs 1   ; pointer variables are declared in RAM
pointer_high .rs 1   ; low byte first, high byte immediately after

current_background_index .rs 1

frame_count .rs 1

;; CONSTANTS
STATE_TITLE     = $0
STATE_PLAYING   = $1
STATE_GAME_OVER = $2
STATE_BESTIARY  = $3

TITLE_BACKGROUND_IDX     = $0
PLAYING_BACKGROUND_IDX   = $2
GAME_OVER_BACKGROUND_IDX = $4
BESTIARY_BACKGROUND_IDX  = $6
  
RIGHT_WALL      = $F4  ; when ball reaches one of these, do something
TOP_WALL        = $20
BOTTOM_WALL     = $E0
LEFT_WALL       = $04
  
A_PRESSED       = $80
B_PRESSED       = $40
SEL_PRESSED     = $20
START_PRESSED   = $10
UP_PRESSED      = $08
DOWN_PRESSED    = $04
LEFT_PRESSED    = $02
RIGHT_PRESSED   = $01

BALL_ADDR = $0200
BALL_STRT_SPRITE = $75

MUNCHY_TL_ADDR = $0204
MUNCHY_TL_STRT_SPRITE = $32
MUNCHY_TR_ADDR = $0208
MUNCHY_TR_STRT_SPRITE = $33
MUNCHY_BL_ADDR = $020C
MUNCHY_BL_STRT_SPRITE = $42
MUNCHY_BR_ADDR = $0210
MUNCHY_BR_STRT_SPRITE = $43

PROJECTILE_ADDR = $0214
PROJECTILE_STRT_SPRITE = $00
NUM_PROJECTILE_ANIMATIONS = $5

;;;;;;;;;;;;;;;;;;

  .bank 0
  .org $C000 
RESET:
  sei          ; disable IRQs
  cld          ; disable decimal mode
  ldx #$40
  stx $4017    ; disable APU frame IRQ
  ldx #$FF
  txs          ; Set up stack
  inx          ; now X = 0
  stx $2000    ; disable NMI
  stx $2001    ; disable rendering
  stx $4010    ; disable DMC IRQs

  jsr VBlankWait

clrmem:
  lda #$00
  sta $0000, x
  sta $0100, x
  sta $0300, x
  sta $0400, x
  sta $0500, x
  sta $0600, x
  sta $0700, x
  lda #$FE
  sta $0200, x
  inx
  bne clrmem

  jsr VBlankWait

  jsr LoadPalettes
  jsr LoadSprites
   
	;;:Set starting game state
  lda #STATE_TITLE
  sta game_state
  ldx #TITLE_BACKGROUND_IDX
  sta current_background_index

  jsr LoadBackground

  lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  sta $2000
  lda #%00011110   ; enable sprites, enable background, no clipping on left side
  sta $2001

Forever:
  jmp Forever     ;jump back to Forever, infinite loop, waiting for NMI

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NMI:
  lda #$00
  sta $2003       ; set the low byte (00) of the RAM address
  lda #$02
  sta $4014       ; set the high byte (02) of the RAM address, start the transfer

  ldx frame_count
  inx
  stx frame_count

  jsr DrawScore

  ;; This is the PPU clean up section, so rendering the next frame starts properly.
  lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  sta $2000
  lda #%00011110   ; enable sprites, enable background, no clipping on left side
  sta $2001
  lda #$00        ;;tell the ppu there is no background scrolling
  sta $2005
  sta $2005

  jsr ReadController1  ;;get the current button data for player 1
  
  ;; all graphics updates done by here, run game engine

  GameEngine:  
    lda game_state
    cmp #STATE_TITLE
    beq EngineTitle
      
    lda game_state
    cmp #STATE_GAME_OVER
    beq EngineGameOver
    
    lda game_state
    cmp #STATE_PLAYING
    beq EnginePlaying

    lda game_state
    cmp #STATE_BESTIARY
    beq EngineBestiary
  
  EngineTitle:
    .CheckStart:
		  lda buttons1
		  and #START_PRESSED
      beq .CheckStartDone

      jsr InitPlayingState

    .CheckStartDone:
      jmp GameEngineDone

  EngineBestiary:
    .CheckStart:
		  lda buttons1
		  and #SEL_PRESSED
      beq .CheckStartDone

			; setup background
  		ldx #PLAYING_BACKGROUND_IDX
  		sta current_background_index
  		jsr LoadBackground
  		
  		lda #STATE_PLAYING
  		sta game_state

			jsr UnhideSprites

    .CheckStartDone:
      jmp GameEngineDone

  EngineGameOver:
		lda buttons1
		and #START_PRESSED
    beq .CheckStartDone

    jsr InitPlayingState

    .CheckStartDone:
      jmp GameEngineDone
 
  EnginePlaying:
 		jsr UpdateSprites

    .CheckSelect:
		  lda buttons1
		  and #SEL_PRESSED
      beq .CheckSelectDone

      jsr InitBestiaryState

      .CheckSelectDone:
        ;noop

    .CheckP1Up:
		  lda buttons1
		  and #UP_PRESSED
      beq .CheckP1UpDone

		  lda buttons1
      sta last_buttons1

      lda munchy_top_left_y
      cmp #TOP_WALL
      beq .CheckP1UpDone

      lda munchy_top_left_y
      sec
      sbc #$02
      sta munchy_top_left_y

      lda munchy_top_right_y
      sec
      sbc #$02
      sta munchy_top_right_y

      lda munchy_bottom_left_y
      sec
      sbc #$02
      sta munchy_bottom_left_y

      lda munchy_bottom_right_y
      sec
      sbc #$02
      sta munchy_bottom_right_y
      .CheckP1UpDone:
        ;noop

    .CheckP1Down:
		  lda buttons1
		  and #DOWN_PRESSED
      beq .CheckP1DownDone

		  lda buttons1
      sta last_buttons1

      lda munchy_top_left_y
      cmp #BOTTOM_WALL
      beq .CheckP1DownDone

      lda munchy_top_left_y
      clc
      adc #$02
      sta munchy_top_left_y
      lda munchy_top_right_y
      clc
      adc #$02
      sta munchy_top_right_y
      lda munchy_bottom_left_y
      clc
      adc #$02
      sta munchy_bottom_left_y
      lda munchy_bottom_right_y
      clc
      adc #$02
      sta munchy_bottom_right_y
      .CheckP1DownDone:
        ;noop

    .CheckP1Right:
		  lda buttons1
		  and #RIGHT_PRESSED
      beq .CheckP1RightDone

		  lda buttons1
      sta last_buttons1

      jsr AnimateMunchyRight

      lda munchy_top_right_x
      cmp #RIGHT_WALL
      beq .CheckP1RightDone

      lda munchy_top_left_x
      clc
      adc #$02
      sta munchy_top_left_x
      lda munchy_top_right_x
      clc
      adc #$02
      sta munchy_top_right_x
      lda munchy_bottom_left_x
      clc
      adc #$02
      sta munchy_bottom_left_x
      lda munchy_bottom_right_x
      clc
      adc #$02
      sta munchy_bottom_right_x
      .CheckP1RightDone:
        ;noop

    .CheckP1Left:
		  lda buttons1
		  and #LEFT_PRESSED
      beq .CheckP1LeftDone

		  lda buttons1
      sta last_buttons1

      jsr AnimateMunchyLeft

      lda munchy_top_left_x
      cmp #LEFT_WALL
      beq .CheckP1LeftDone

      lda munchy_top_left_x
      sec
      sbc #$02
      sta munchy_top_left_x
      lda munchy_top_right_x
      sec
      sbc #$02
      sta munchy_top_right_x
      lda munchy_bottom_left_x
      sec
      sbc #$02
      sta munchy_bottom_left_x
      lda munchy_bottom_right_x
      sec
      sbc #$02
      sta munchy_bottom_right_x
      .CheckP1LeftDone:
        ;noop

    .CheckP1B:
		  lda buttons1
		  and #B_PRESSED
      beq .CheckP1BDone
      
      lda projectile_thrown
      cmp #$1
      beq .CheckP1BDone

      ;; figure out which direction to fire
      ; left or right? default to right if neither
      lda last_buttons1
		  and #LEFT_PRESSED
      bne .SetProjectileLeft
      lda last_buttons1
		  and #RIGHT_PRESSED
      bne .SetProjectileRight

      jmp .LeftOrRightDone

      .SetProjectileLeft:
        lda #$1
        sta projectile_left
        jmp .LeftOrRightDone
      .SetProjectileRight:
        lda #$1
        sta projectile_right
        jmp .LeftOrRightDone
      .LeftOrRightDone:

      ; up or down?
      lda last_buttons1
		  and #UP_PRESSED
      bne .SetProjectileUp
      lda last_buttons1
		  and #DOWN_PRESSED
      bne .SetProjectileDown

      jmp .FireProjectile

      .SetProjectileUp:
        lda #$1
        sta projectile_up
        jmp .FireProjectile
      .SetProjectileDown:
        lda #$1
        sta projectile_down
        jmp .FireProjectile

      ; time to fire
      .FireProjectile:
				; reset sprint
				lda PROJECTILE_STRT_SPRITE
				sta PROJECTILE_ADDR + 1

        lda #$1
        sta projectile_thrown

        lda #0
        sta projectile_animation_idx
        
        lda munchy_top_right_x
        sta projectile_x
        lda munchy_top_right_y
        sta projectile_y
  
        jsr ProjectileSound

      .CheckP1BDone:
        ;noop

    .MoveProjectileUp:
      lda projectile_up
      beq .MoveProjectileUpDone

      lda projectile_y
      sec
      sbc projectile_speed
      sta projectile_y
    
      lda projectile_y
      cmp #TOP_WALL
      bcs .MoveProjectileUpDone
      
      jsr ResetProjectile

      .MoveProjectileUpDone:

    .MoveProjectileDown:
      lda projectile_down
      beq .MoveProjectileDownDone

      lda projectile_y
      clc
      adc projectile_speed
      sta projectile_y
    
      lda projectile_y
      cmp #BOTTOM_WALL
      bcc .MoveProjectileDownDone

      jsr ResetProjectile

      .MoveProjectileDownDone:

    .MoveProjectileRight:
      lda projectile_right
      beq .MoveProjectileRightDone

      lda projectile_x
      clc
      adc projectile_speed
      sta projectile_x
    
      lda projectile_x
      cmp #RIGHT_WALL
      bcc .MoveProjectileRightDone
      
      jsr ResetProjectile

      .MoveProjectileRightDone:

    .MoveProjectileLeft:
      lda projectile_left
      beq .MoveProjectileLeftDone

      lda projectile_x
      sec
      sbc projectile_speed
      sta projectile_x
    
      lda projectile_x
      cmp #LEFT_WALL
      bcs .MoveProjectileLeftDone
      
      jsr ResetProjectile

      .MoveProjectileLeftDone:

		.AnimateProjectile:
			lda projectile_thrown
			cmp #1
			bne .AnimateProjectileDone

			; do it every 50th frame
			ldx frame_count
			cpx #0
			beq .AnimateProjectileChangeTile
			cpx #50
			beq .AnimateProjectileChangeTile
			cpx #100
			beq .AnimateProjectileChangeTile
			cpx #150
			beq .AnimateProjectileChangeTile
			cpx #200
			beq .AnimateProjectileChangeTile
			cpx #250
			beq .AnimateProjectileChangeTile

			jmp .AnimateProjectileIncTile

			.AnimateProjectileChangeTile:
				; grab from the animation index
				ldx projectile_animation_idx
				lda projectile_tiles, X
				sta PROJECTILE_ADDR + 1

				ldx projectile_animation_idx
				inx
				stx projectile_animation_idx

			.AnimateProjectileIncTile:
				; reset animation index as necessary
				ldx projectile_animation_idx
				cpx #NUM_PROJECTILE_ANIMATIONS
				bne .AnimateProjectileDone

				ldx #0
 			  stx projectile_animation_idx

			.AnimateProjectileDone:
  
    .MoveBallRight:
      lda ball_right
      beq .MoveBallRightDone   ;;if ball_right=0, skip this section
    
      lda ball_x
      clc
      adc ball_speed_x        ;;ball_x position = ball_x + ball_speed_x
      sta ball_x
    
      lda ball_x
      cmp #RIGHT_WALL
      bcc .MoveBallRightDone      ;;if ball x < right wall, still on screen, skip next section
      lda #$00
      sta ball_right
      lda #$01
      sta ball_left         ;;bounce, ball now moving left

      ;;in real game, give point to player 1, reset ball
      jsr IncrementScoreOne
			jsr CheckIfGameIsOver

      .MoveBallRightDone:
  
    .MoveBallLeft:
      lda ball_left
      beq .MoveBallLeftDone   ;;if ball_left=0, skip this section
    
      lda ball_x
      sec
      sbc ball_speed_x        ;;ball_x position = ball_x - ball_speed_x
      sta ball_x
    
      lda ball_x
      cmp #LEFT_WALL
      bcs .MoveBallLeftDone      ;;if ball x > left wall, still on screen, skip next section
      lda #$01
      sta ball_right
      lda #$00
      sta ball_left         ;;bounce, ball now moving right

			jsr CheckIfGameIsOver

      .MoveBallLeftDone:
  
    .MoveBallUp:
      lda ball_up
      beq .MoveBallUpDone   ;;if ball_up=0, skip this section
    
      lda ball_y
      sec
      sbc ball_speed_y        ;;ball_y position = ball_y - ball_speed_y
      sta ball_y
    
      lda ball_y
      cmp #TOP_WALL
      bcs .MoveBallUpDone      ;;if ball y > top wall, still on screen, skip next section
      lda #$01
      sta ball_down
      lda #$00
      sta ball_up         ;;bounce, ball now moving down

      .MoveBallUpDone:
  
    .MoveBallDown:
      lda ball_down
      beq .MoveBallDownDone   ;;if ball_up=0, skip this section
    
      lda ball_y
      clc
      adc ball_speed_y        ;;ball_y position = ball_y + ball_speed_y
      sta ball_y
    
      lda ball_y
      cmp #BOTTOM_WALL
      bcc .MoveBallDownDone      ;;if ball y < bottom wall, still on screen, skip next section
      lda #$00
      sta ball_down
      lda #$01
      sta ball_up         ;;bounce, ball now moving down

      .MoveBallDownDone:
  
    .CheckPaddleCollision:
      ;;if ball x < paddle1x
      ;;  if ball y > paddle y top
      ;;    if ball y < paddle y bottom
      ;;      bounce, ball now moving left

    .Done:
    	jmp GameEngineDone

  GameEngineDone:  
    rti

;---------------------------;
;     SUBROUTINES           ;
;---------------------------;
ProjectileSound:
  lda #%00000001
  sta $4015 ;enable square 1

  lda #%10111111 ;Duty 10, Volume F
  sta $4000

  lda #$E9
  sta $4002
  lda #$00
  sta $4003

  lda #0
  sta $4000

	rts

ScoreSound:
  lda #%00000001
  sta $4015 ;enable square 1

  lda #%10111111 ;Duty 10, Volume F
  sta $4000

  lda #$C9    ;0C9 is a C# in NTSC mode
  sta $4002
  lda #$00
  sta $4003

  lda #0
  sta $4000

	rts

DebugSound:
  lda #%00000001
  sta $4015 ;enable square 1

  lda #%10111111 ;Duty 10, Volume F
  sta $4000

  lda #$A9    ;0C9 is a C# in NTSC mode
  sta $4002
  lda #$00
  sta $4003

  lda #0
  sta $4000

	rts

CheckIfGameIsOver:
  lda score1_tens
	cmp #$01
  bne .Check1OnesDone

  lda score1_ones
	cmp #$05
  beq .GameIsOver
  .Check1OnesDone:

  jmp .Done

  .GameIsOver:
		; setup background
		ldx #GAME_OVER_BACKGROUND_IDX
  	sta current_background_index
	  jsr LoadBackground

		lda #STATE_GAME_OVER
		sta game_state

    jmp .Done

  .Done:
	  rts

VBlankWait:
  bit $2002
  bpl VBlankWait
  rts

LoadPalettes:
  lda $2002             ; read PPU status to reset the high/low latch
  lda #$3F
  sta $2006             ; write the high byte of $3F00 address
  lda #$00
  sta $2006             ; write the low byte of $3F00 address
  ldx #$00              ; start out at 0
  .Loop:
    lda palette, x        ; load data from address (palette + the value in x)
	                          ; 1st time through loop it will load palette+0
	                          ; 2nd time through loop it will load palette+1
	                          ; 3rd time through loop it will load palette+2
	                          ; etc
	  sta $2007             ; write to PPU
	  inx                   ; X = X + 1
	  cpx #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
    bne .Loop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
	             ; if compare was equal to 32, keep going down
  rts

LoadSprites:
	ldx #$00              ; start at 0
	LoadSpritesLoop:
	  lda sprites, x      ; load data from address (sprites +  x)
	  sta $0200, x ; store into RAM address ($0200 + x)
	  inx
	  cpx #$28
	  bne LoadSpritesLoop
  rts

LoadBackground:
	jsr TurnOffScreenAndNMI

  stx current_background_index

  lda $2002             ; read PPU status to reset the high/low latch
  lda #$20
  sta $2006             ; write the high byte of $2000 address
  lda #$00
  sta $2006             ; write the low byte of $2000 address

  lda backgrounds, X ;load A with the low byte of the room address
  sta pointer_low  ;store A in the zero-page RAM
  lda backgrounds+1, X
  sta pointer_high       ; put the high byte of the address into pointer
  
  ldx #$00            ; start at pointer + 0
  ldy #$00

  .OutsideLoop:
    .InsideLoop:
  		lda [pointer_low], y  ; copy one background byte from address in pointer plus Y
  		sta $2007           ; this runs 256 * 4 times
  		
  		iny                 ; inside loop counter
  		cpy #$00
      bne .InsideLoop      ; run the inside loop 256 times before continuing down
  		
  		inc pointer_high       ; low byte went 0 to 256, so high byte needs to be changed now
  		
  		inx
  		cpx #$04
      bne .OutsideLoop     ; run the outside loop 256 times before continuing down

	jsr TurnOnScreenAndNMI

	rts

IncrementScoreOne:
  .Inc1Ones:
    lda score1_ones      ; load the lowest digit of the number
    clc 
    adc #$01           ; add one
    sta score1_ones
    cmp #$0A           ; check if it overflowed, now equals 10
    bne .Done        ; if there was no overflow, all done
  .Inc1Tens:
    lda #$00
    sta score1_ones      ; wrap digit to 0
    lda score1_tens      ; load the next digit
    clc 
    adc #$01           ; add one, the carry from previous digit
    sta score1_tens
    cmp #$0A           ; check if it overflowed, now equals 10
    bne .Done        ; if there was no overflow, all done
  .Done:
		jsr ScoreSound
		rts

TurnOffScreenAndNMI:
	; turn off screen and NMI
	lda #$00
	sta $2000
	sta $2001

	rts

TurnOnScreenAndNMI:
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	lda $2002 ;avoid getting a partial vblank
	lda #%10000000
	sta $2000

	rts

InitBestiaryState:
  ldx #BESTIARY_BACKGROUND_IDX
  sta current_background_index
  jsr LoadBackground

	jsr HideSprites
  
  lda #STATE_BESTIARY
  sta game_state

  rts

UnhideSprites:
	lda ball_y
	sta BALL_ADDR
	lda munchy_top_left_y
	lda MUNCHY_TL_ADDR
	lda munchy_top_right_y
	lda MUNCHY_TR_ADDR
	lda munchy_bottom_left_y
	lda MUNCHY_BL_ADDR
	lda munchy_bottom_right_y
	lda MUNCHY_BR_ADDR

	rts

HideSprites:
	lda $FF
	sta BALL_ADDR
	sta MUNCHY_TL_ADDR
	sta MUNCHY_TR_ADDR
	sta MUNCHY_BL_ADDR
	sta MUNCHY_BR_ADDR

	rts

InitPlayingState:
  ;;;Set some initial ball stats
  lda #$01
  sta ball_down
  sta ball_right
  lda #$00
  sta ball_up
  sta ball_left

  lda #$50
  sta ball_y
  lda #$80
  sta ball_x
  
  lda #$01
  sta ball_speed_x
  sta ball_speed_y
  
  ; set initial munchy location
	lda #80
	sta munchy_top_left_y
	sta munchy_top_left_x
	sta munchy_top_right_y
	sta munchy_bottom_left_x
	
	lda #88
	sta munchy_top_right_x
	sta munchy_bottom_left_y
	sta munchy_bottom_right_x
	sta munchy_bottom_right_y

  lda #$00
  sta munchy_animation_frame_num

  ; setup scores
  lda #0
  sta score1_ones
  sta score1_tens
  
  lda #$20
  sta score1_high
  lda #$4C
  sta score1_low

  ; projectile state
  lda #0
  sta projectile_thrown
  sta projectile_x
  sta projectile_y

  lda #1
  sta projectile_speed

	; setup background
  ldx #PLAYING_BACKGROUND_IDX
  sta current_background_index
  jsr LoadBackground
  
  lda #STATE_PLAYING
  sta game_state

  rts

AnimateMunchyRight:
  ; check if munchy is facing right
  lda MUNCHY_TL_ADDR + 1
  cmp #MUNCHY_TL_STRT_SPRITE
  beq .Animate

  jsr FlipMunchy

  ; Swap tiles to be right facing
  lda #MUNCHY_TL_STRT_SPRITE
  sta MUNCHY_TL_ADDR + 1
  lda #MUNCHY_TR_STRT_SPRITE
  sta MUNCHY_TR_ADDR + 1
  lda #MUNCHY_BL_STRT_SPRITE
  sta MUNCHY_BL_ADDR + 1
  lda #MUNCHY_BR_STRT_SPRITE
  sta MUNCHY_BR_ADDR + 1

  .Animate:
     ;lda MUNCHY_BL_STRT_SPRITE + 1
     ;sta MUNCHY_BL_ADDR + 1
     ;lda MUNCHY_BR_STRT_SPRITE + 1
     ;sta MUNCHY_BR_ADDR + 1

  rts

AnimateMunchyLeft:
  ; check if munchy is facing left
  lda MUNCHY_TL_ADDR + 1
  cmp #MUNCHY_TL_STRT_SPRITE
  bne .Animate

  jsr FlipMunchy

  ; Swap tiles to be left facing
  lda #MUNCHY_TR_STRT_SPRITE
  sta MUNCHY_TL_ADDR + 1
  lda #MUNCHY_TL_STRT_SPRITE
  sta MUNCHY_TR_ADDR + 1
  lda #MUNCHY_BR_STRT_SPRITE
  sta MUNCHY_BL_ADDR + 1
  lda #MUNCHY_BL_STRT_SPRITE
  sta MUNCHY_BR_ADDR + 1

  .Animate:
     ;lda munchy_animation_frame_num
     ;cmp #$03
     ;bne .ShowNextFrame

     ;; reset animation count
     ;lda #$0
     ;sta munchy_animation_frame_num

     ;.ShowNextFrame:
     ;  ; update leg frames
     ;  ldx frame_counter
		 ; cmp 170
     ;  bcs .FrameThree

     ;  ldx frame_counter
		 ; cmp 85
     ;  bcs .FrameTwo

     ;  lda #MUNCHY_BL_STRT_SPRITE
     ;  clc 
     ;  adc munchy_animation_frame_num
     ;  sta MUNCHY_BL_ADDR + 1

     ;  lda #MUNCHY_BR_STRT_SPRITE
     ;  clc 
     ;  adc munchy_animation_frame_num
     ;  sta MUNCHY_BR_ADDR + 1
 
     ;  .Done:
     ;   ; increment frame count
     ;   inx
     ;   txa
     ;   sta munchy_animation_frame_num
  rts

FlipMunchy:
  lda MUNCHY_TL_ADDR + 2
  eor #%01000000
  sta MUNCHY_TL_ADDR + 2
  sta MUNCHY_TR_ADDR + 2
  sta MUNCHY_BL_ADDR + 2
  sta MUNCHY_BR_ADDR + 2

  rts

ResetProjectile:
  lda #$0
  sta projectile_thrown
  sta projectile_x
  sta projectile_y
  sta projectile_left
  sta projectile_right
  sta projectile_up
  sta projectile_down

  rts

UpdateSprites:
  ; update projectile location
  lda projectile_y
  sta PROJECTILE_ADDR
  lda projectile_x
  sta PROJECTILE_ADDR + 3

  ; update ball location
  lda ball_y
  sta BALL_ADDR
  lda ball_x
  sta BALL_ADDR + 3
  
	;;; update Munchy location
	; top left
  lda munchy_top_left_y
  sta MUNCHY_TL_ADDR
  lda munchy_top_left_x
  sta MUNCHY_TL_ADDR + 3

	; top right
  lda munchy_top_right_y
  sta MUNCHY_TR_ADDR
  lda munchy_top_right_x
  sta MUNCHY_TR_ADDR + 3

	; bottom left
  lda munchy_bottom_left_y
  sta MUNCHY_BL_ADDR
  lda munchy_bottom_left_x
  sta MUNCHY_BL_ADDR + 3

	; bottom right
  lda munchy_bottom_right_y
  sta MUNCHY_BR_ADDR
  lda munchy_bottom_right_x
  sta MUNCHY_BR_ADDR + 3

  rts

DrawScore:
  lda $2002
  lda score1_high
  sta $2006
  lda score1_low
  sta $2006
  
  lda score1_tens      ; next digit
  sta $2007
  lda score1_ones      ; last digit
  sta $2007

  rts
 
ReadController1:
  lda #$01
  sta $4016
  lda #$00
  sta $4016
  ldx #$08
  .Loop:
    lda $4016
    lsr A            ; bit0 -> Carry
    rol buttons1     ; bit0 <- Carry
    dex
    bne .Loop
    rts
  
;;;;;;;;;;;;;;  
  
  .bank 1
  .org $E000

title_background:
  .include "backgrounds/title_background.asm"

playing_background:
  .include "backgrounds/playing_background.asm"

gameover_background:
  .include "backgrounds/gameover_background.asm"

bestiary_background:
  .include "backgrounds/bestiary_background.asm"

backgrounds:
  .word title_background, playing_background, gameover_background, bestiary_background

palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

projectile_tiles:
	.db PROJECTILE_STRT_SPRITE, PROJECTILE_STRT_SPRITE + 1, PROJECTILE_STRT_SPRITE + 2, PROJECTILE_STRT_SPRITE + 3, PROJECTILE_STRT_SPRITE + 4

sprites:
  ;vert tile attr horiz
  .db 0, BALL_STRT_SPRITE, %00000000, 0 ;ball
  .db 0, MUNCHY_TL_STRT_SPRITE, %00000000, 0 ;munchy top left
  .db 0, MUNCHY_TR_STRT_SPRITE, %00000000, 0 ;munchy top right
  .db 0, MUNCHY_BL_STRT_SPRITE, %00000000, 0 ;munchy bottom left
  .db 0, MUNCHY_BR_STRT_SPRITE, %00000000, 0 ;munchy bottom right
  .db 0, PROJECTILE_STRT_SPRITE, %00000000, 0 ;projectile
  .db 0, PROJECTILE_STRT_SPRITE + 1, %00000000, 0 ;projectile
  .db 0, PROJECTILE_STRT_SPRITE + 2, %00000000, 0 ;projectile
  .db 0, PROJECTILE_STRT_SPRITE + 3, %00000000, 0 ;projectile
  .db 0, PROJECTILE_STRT_SPRITE + 4, %00000000, 0 ;projectile

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
;;;;;;;;;;;;;;  
  
  .bank 2
  .org $0000
  .incbin "../graphics/munchys_mansion.chr"   ;includes 8KB graphics file from SMB1
