  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
game_state  .rs 1  ; .rs 1 means reserve one byte of space

ball_x      .rs 1  ; ball horizontal position
ball_y      .rs 1  ; ball vertical position
ball_up     .rs 1  ; 1 = ball moving up
ball_down   .rs 1  ; 1 = ball moving down
ball_left   .rs 1  ; 1 = ball moving left
ball_right  .rs 1  ; 1 = ball moving right
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

buttons1   .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; player 2 gamepad buttons, one bit per button

score1_high .rs 1  ; player 1 score, 0-15
score1_low  .rs 1  ; player 1 score, 0-15
score1_ones .rs 1  ; player 1 score, 0-15
score1_tens .rs 1  ; player 1 score, 0-15
score2_high .rs 1  ; player 1 score, 0-15
score2_low  .rs 1  ; player 1 score, 0-15
score2_ones .rs 1  ; player 2 score, 0-15
score2_tens .rs 1  ; player 2 score, 0-15

pointer_low  .rs 1   ; pointer variables are declared in RAM
pointer_high  .rs 1   ; low byte first, high byte immediately after
current_background_index .rs 1

;; DECLARE SOME CONSTANTS HERE
STATE_TITLE     = $00  ; displaying title screen
STATE_PLAYING   = $01  ; move paddles/ball, check for collisions
STATE_GAME_OVER  = $02  ; displaying game over screen
  
RIGHT_WALL      = $F4  ; when ball reaches one of these, do something
TOP_WALL        = $20
BOTTON_WALL     = $E0
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
MUNCHY_BL_STRT_SPRITE = $34
MUNCHY_BR_ADDR = $0210
MUNCHY_BR_STRT_SPRITE = $35

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
	ldx #0
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

  jsr DrawScore

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  lda #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  sta $2000
  lda #%00011110   ; enable sprites, enable background, no clipping on left side
  sta $2001
  lda #$00        ;;tell the ppu there is no background scrolling
  sta $2005
  sta $2005
    
  ;;;all graphics updates done by here, run game engine

  jsr ReadController1  ;;get the current button data for player 1
  jsr ReadController2  ;;get the current button data for player 2
  
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
  
  GameEngineDone:  
    jsr UpdateSprites
    rti
 
;;;;;;;;
 
  EngineTitle:
    .CheckStart:
		  LDA buttons1 ; player 1 - A
		  AND #START_PRESSED ; erase everything but bit 0
      BEQ .CheckStartDone   ; branch to ReadADone if button is NOT pressed (0)
      jsr InitPlayingState

    .CheckStartDone:
      JMP GameEngineDone

;;;;;;;;; 
 
  EngineGameOver:
		LDA buttons1 ; player 1 - A
		AND #START_PRESSED ; erase everything but bit 0
    BEQ .CheckStartDone   ; branch to ReadADone if button is NOT pressed (0)
    jsr InitPlayingState

    .CheckStartDone:
      JMP GameEngineDone
 
;;;;;;;;;;;
 
  EnginePlaying:
    .CheckP1Up:
		  lda buttons1
		  and #UP_PRESSED
      beq .CheckP1UpDone

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

      lda munchy_top_left_y
      cmp #BOTTON_WALL
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

    .MoveBallRight:
      LDA ball_right
      BEQ .MoveBallRightDone   ;;if ball_right=0, skip this section
    
      LDA ball_x
      CLC
      ADC ball_speed_x        ;;ball_x position = ball_x + ball_speed_x
      STA ball_x
    
      LDA ball_x
      CMP #RIGHT_WALL
      BCC .MoveBallRightDone      ;;if ball x < right wall, still on screen, skip next section
      LDA #$00
      STA ball_right
      LDA #$01
      STA ball_left         ;;bounce, ball now moving left

      ;;in real game, give point to player 1, reset ball
      jsr IncrementScoreOne
			jsr CheckIfGameIsOver

      .MoveBallRightDone:
  
    .MoveBallLeft:
      LDA ball_left
      BEQ .MoveBallLeftDone   ;;if ball_left=0, skip this section
    
      LDA ball_x
      SEC
      SBC ball_speed_x        ;;ball_x position = ball_x - ball_speed_x
      STA ball_x
    
      LDA ball_x
      CMP #LEFT_WALL
      BCS .MoveBallLeftDone      ;;if ball x > left wall, still on screen, skip next section
      LDA #$01
      STA ball_right
      LDA #$00
      STA ball_left         ;;bounce, ball now moving right

      ;;in real game, give point to player 2, reset ball
      jsr IncrementScoreTwo
			jsr CheckIfGameIsOver

      .MoveBallLeftDone:
  
    .MoveBallUp:
      LDA ball_up
      BEQ .MoveBallUpDone   ;;if ball_up=0, skip this section
    
      LDA ball_y
      SEC
      SBC ball_speed_y        ;;ball_y position = ball_y - ball_speed_y
      STA ball_y
    
      LDA ball_y
      CMP #TOP_WALL
      BCS .MoveBallUpDone      ;;if ball y > top wall, still on screen, skip next section
      LDA #$01
      STA ball_down
      LDA #$00
      STA ball_up         ;;bounce, ball now moving down

      .MoveBallUpDone:
  
    .MoveBallDown:
      LDA ball_down
      BEQ .MoveBallDownDone   ;;if ball_up=0, skip this section
    
      LDA ball_y
      CLC
      ADC ball_speed_y        ;;ball_y position = ball_y + ball_speed_y
      STA ball_y
    
      LDA ball_y
      CMP #BOTTON_WALL
      BCC .MoveBallDownDone      ;;if ball y < bottom wall, still on screen, skip next section
      LDA #$00
      STA ball_down
      LDA #$01
      STA ball_up         ;;bounce, ball now moving down

      .MoveBallDownDone:
  
    .CheckPaddleCollision:
      ;;if ball x < paddle1x
      ;;  if ball y > paddle y top
      ;;    if ball y < paddle y bottom
      ;;      bounce, ball now moving left

      JMP GameEngineDone

;;;;;;;;;;;;;;;;;;;;;;;;;;;
  AnimateMunchyRight:
    ; check if munchy is facing right
    lda MUNCHY_TL_ADDR + 1
    cmp #MUNCHY_TL_STRT_SPRITE
    beq .Done

    jsr FlipMunchy

    lda #MUNCHY_TL_STRT_SPRITE
    sta MUNCHY_TL_ADDR + 1
    lda #MUNCHY_TR_STRT_SPRITE
    sta MUNCHY_TR_ADDR + 1
    lda #MUNCHY_BL_STRT_SPRITE
    sta MUNCHY_BL_ADDR + 1
    lda #MUNCHY_BR_STRT_SPRITE
    sta MUNCHY_BR_ADDR + 1

    ; walking animation here

    .Done:

    rts

  AnimateMunchyLeft:
    ; check if munchy is facing left
    lda MUNCHY_TL_ADDR + 1
    cmp #MUNCHY_TL_STRT_SPRITE
    bne .Done

    jsr FlipMunchy

    lda #MUNCHY_TR_STRT_SPRITE
    sta MUNCHY_TL_ADDR + 1
    lda #MUNCHY_TL_STRT_SPRITE
    sta MUNCHY_TR_ADDR + 1
    lda #MUNCHY_BR_STRT_SPRITE
    sta MUNCHY_BL_ADDR + 1
    lda #MUNCHY_BL_STRT_SPRITE
    sta MUNCHY_BR_ADDR + 1

    ; walking animation here

    .Done:

    rts

  FlipMunchy:
    lda MUNCHY_TL_ADDR + 2
    eor #%01000000
    sta MUNCHY_TL_ADDR + 2
    sta MUNCHY_TR_ADDR + 2
    sta MUNCHY_BL_ADDR + 2
    sta MUNCHY_BR_ADDR + 2

    rts
 
  UpdateSprites:
    ;update ball location
    lda ball_y
    sta BALL_ADDR
    lda ball_x
    sta BALL_ADDR + 3
    
		;update Munchy location
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

    lda $2002
    lda score2_high
    sta $2006
    lda score2_low
    sta $2006

    lda score2_tens      ; next digit
    sta $2007
    lda score2_ones      ; last digit
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
    
  ReadController2:
    lda #$01
    sta $4016
    lda #$00
    sta $4016
    ldx #$08
    .Loop:
      lda $4017
      lsr A            ; bit0 -> Carry
      rol buttons2     ; bit0 <- Carry
      dex
      bne .Loop
      rts  

;---------------------------;
;     SUBROUTINES           ;
;---------------------------;
BounceSound:
  lda #%00000001
  sta $4015 ;enable square 1

  lda #%10111111 ;Duty 10, Volume F
  sta $4000

  lda #$D9    ;0C9 is a C# in NTSC mode
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

CheckIfGameIsOver:
  lda score1_tens
	cmp #$01
  bne .Check1OnesDone

  lda score1_ones
	cmp #$05
  beq .GameIsOver
  .Check1OnesDone:

  lda score2_tens
	cmp #$01
  bne .Check2OnesDone

  lda score2_ones
	cmp #$05
  beq .GameIsOver
  .Check2OnesDone:

  jmp .Done

  .GameIsOver:
		; setup background
		ldx #4
  	sta current_background_index

	  jsr TurnOffScreenAndNMI
	  jsr LoadBackground
	  jsr TurnOnScreenAndNMI

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
	  cpx #$14
	  bne LoadSpritesLoop
  rts

LoadBackground:
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

IncrementScoreTwo:
  .Inc2Ones:
    lda score2_ones      ; load the lowest digit of the number
    clc 
    adc #$01           ; add one
    sta score2_ones
    cmp #$0A           ; check if it overflowed, now equals 10
    bne .Done        ; if there was no overflow, all done
  .Inc2Tens:
    lda #$00
    sta score2_ones      ; wrap digit to 0
    lda score2_tens      ; load the next digit
    clc 
    adc #$01           ; add one, the carry from previous digit
    sta score2_tens
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
  
  lda #$02
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

  ; setup scores
  lda #0
  sta score1_ones
  sta score1_tens
  sta score2_ones
  sta score2_tens
  
  lda #$20
  sta score1_high
  sta score2_high
  lda #$4C
  sta score1_low
  lda #$51
  sta score2_low

	; setup background
	ldx #2
  sta current_background_index
  
  jsr TurnOffScreenAndNMI
  jsr LoadBackground
  jsr TurnOnScreenAndNMI
  
  lda #STATE_PLAYING
  sta game_state
  rts
        
;;;;;;;;;;;;;;  
  
  .bank 1
  .org $E000

title_background:
  .include "title_background.asm"

playing_background:
  .include "playing_background.asm"

gameover_background:
  .include "gameover_background.asm"

backgrounds:
  .word title_background, playing_background, gameover_background

palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

sprites:
  ;vert tile attr horiz
  .db 0, BALL_STRT_SPRITE, %00000000, 0 ;ball
  .db 0, MUNCHY_TL_STRT_SPRITE, %00000000, 0 ;munchy top left
  .db 0, MUNCHY_TR_STRT_SPRITE, %00000000, 0 ;munchy top right
  .db 0, MUNCHY_BL_STRT_SPRITE, %00000000, 0 ;munchy bottom left
  .db 0, MUNCHY_BR_STRT_SPRITE, %00000000, 0 ;munchy bottom right

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the 
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial
  
  
;;;;;;;;;;;;;;  
  
  .bank 2
  .org $0000
  .incbin "mario.chr"   ;includes 8KB graphics file from SMB1
