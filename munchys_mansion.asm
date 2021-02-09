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

MUNCHY_TOP_LEFT_ADDR = $0204
MUNCHY_TOP_RIGHT_ADDR = $0208
MUNCHY_BOTTOM_LEFT_ADDR = $020C
MUNCHY_BOTTOM_RIGHT_ADDR = $0210

;;;;;;;;;;;;;;;;;;

  .bank 0
  .org $C000 
RESET:
  SEI          ; disable IRQs
  CLD          ; disable decimal mode
  LDX #$40
  STX $4017    ; disable APU frame IRQ
  LDX #$FF
  TXS          ; Set up stack
  INX          ; now X = 0
  STX $2000    ; disable NMI
  STX $2001    ; disable rendering
  STX $4010    ; disable DMC IRQs

  jsr VBlankWait

clrmem:
  LDA #$00
  STA $0000, x
  STA $0100, x
  STA $0300, x
  STA $0400, x
  STA $0500, x
  STA $0600, x
  STA $0700, x
  LDA #$FE
  STA $0200, x
  INX
  BNE clrmem

  jsr VBlankWait

  jsr LoadPalettes
  jsr LoadSprites
   
	;;:Set starting game state
  LDA #STATE_TITLE
  STA game_state
	LDX #0
  STA current_background_index

  jsr LoadBackground

  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop, waiting for NMI

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

NMI:
  LDA #$00
  STA $2003       ; set the low byte (00) of the RAM address
  LDA #$02
  STA $4014       ; set the high byte (02) of the RAM address, start the transfer

  JSR DrawScore

  ;;This is the PPU clean up section, so rendering the next frame starts properly.
  LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
  STA $2000
  LDA #%00011110   ; enable sprites, enable background, no clipping on left side
  STA $2001
  LDA #$00        ;;tell the ppu there is no background scrolling
  STA $2005
  STA $2005
    
  ;;;all graphics updates done by here, run game engine

  JSR ReadController1  ;;get the current button data for player 1
  JSR ReadController2  ;;get the current button data for player 2
  
  GameEngine:  
    LDA game_state
    CMP #STATE_TITLE
    BEQ EngineTitle    ;;game is displaying title screen
      
    LDA game_state
    CMP #STATE_GAME_OVER
    BEQ EngineGameOver  ;;game is displaying ending screen
    
    LDA game_state
    CMP #STATE_PLAYING
    BEQ EnginePlaying   ;;game is playing
  
  GameEngineDone:  
    JSR UpdateSprites  ;;set ball/paddle sprites from positions
    RTI             ; return from interrupt
 
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
		  LDA buttons1
		  AND #UP_PRESSED
      BEQ .CheckP1UpDone

      lda munchy_top_left_y
      cmp #TOP_WALL
      beq .CheckP1UpDone

      LDA munchy_top_left_y
      SEC
      SBC #$02
      STA munchy_top_left_y

      LDA munchy_top_right_y
      SEC
      SBC #$02
      STA munchy_top_right_y

      LDA munchy_bottom_left_y
      SEC
      SBC #$02
      STA munchy_bottom_left_y

      LDA munchy_bottom_right_y
      SEC
      SBC #$02
      STA munchy_bottom_right_y
      .CheckP1UpDone:
        ;noop

    .CheckP1Down:
		  LDA buttons1
		  AND #DOWN_PRESSED
      BEQ .CheckP1DownDone

      lda munchy_top_left_y
      CMP #BOTTON_WALL
      beq .CheckP1DownDone

      LDA munchy_top_left_y
      CLC
      ADC #$02
      STA munchy_top_left_y
      LDA munchy_top_right_y
      CLC
      ADC #$02
      STA munchy_top_right_y
      LDA munchy_bottom_left_y
      CLC
      ADC #$02
      STA munchy_bottom_left_y
      LDA munchy_bottom_right_y
      CLC
      ADC #$02
      STA munchy_bottom_right_y
      .CheckP1DownDone:
        ;noop

    .CheckP1Right:
		  LDA buttons1
		  AND #RIGHT_PRESSED
      BEQ .CheckP1RightDone

      lda munchy_top_right_x
      CMP #RIGHT_WALL
      beq .CheckP1RightDone

      LDA munchy_top_left_x
      CLC
      ADC #$02
      STA munchy_top_left_x
      LDA munchy_top_right_x
      CLC
      ADC #$02
      STA munchy_top_right_x
      LDA munchy_bottom_left_x
      CLC
      ADC #$02
      STA munchy_bottom_left_x
      LDA munchy_bottom_right_x
      CLC
      ADC #$02
      STA munchy_bottom_right_x
      .CheckP1RightDone:
        ;noop

    .CheckP1Left:
		  LDA buttons1
		  AND #LEFT_PRESSED
      BEQ .CheckP1LeftDone

      lda munchy_top_left_x
      CMP #LEFT_WALL
      beq .CheckP1LeftDone

      LDA munchy_top_left_x
      SEC
      SBC #$02
      STA munchy_top_left_x
      LDA munchy_top_right_x
      SEC
      SBC #$02
      STA munchy_top_right_x
      LDA munchy_bottom_left_x
      SEC
      SBC #$02
      STA munchy_bottom_left_x
      LDA munchy_bottom_right_x
      SEC
      SBC #$02
      STA munchy_bottom_right_x
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
 
  UpdateSprites:
    ;update ball location
    lda ball_y
    sta BALL_ADDR
    lda ball_x
    sta BALL_ADDR + 3
    
		;update Munchy location
		; top left
    lda munchy_top_left_y
    sta MUNCHY_TOP_LEFT_ADDR
    lda munchy_top_left_x
    sta MUNCHY_TOP_LEFT_ADDR + 3

		; top right
    lda munchy_top_right_y
    sta MUNCHY_TOP_RIGHT_ADDR
    lda munchy_top_right_x
    sta MUNCHY_TOP_RIGHT_ADDR + 3

		; bottom left
    lda munchy_bottom_left_y
    sta MUNCHY_BOTTOM_LEFT_ADDR
    lda munchy_bottom_left_x
    sta MUNCHY_BOTTOM_LEFT_ADDR + 3

		; bottom right
    lda munchy_bottom_right_y
    sta MUNCHY_BOTTOM_RIGHT_ADDR
    lda munchy_bottom_right_x
    sta MUNCHY_BOTTOM_RIGHT_ADDR + 3

    RTS
 
  DrawScore:
    LDA $2002
    LDA score1_high
    STA $2006
    LDA score1_low
    STA $2006
    
    LDA score1_tens      ; next digit
    STA $2007
    LDA score1_ones      ; last digit
    STA $2007

    LDA $2002
    LDA score2_high
    STA $2006
    LDA score2_low
    STA $2006

    LDA score2_tens      ; next digit
    STA $2007
    LDA score2_ones      ; last digit
    STA $2007

    RTS
   
  ReadController1:
    LDA #$01
    STA $4016
    LDA #$00
    STA $4016
    LDX #$08
    .Loop:
      LDA $4016
      LSR A            ; bit0 -> Carry
      ROL buttons1     ; bit0 <- Carry
      DEX
      BNE .Loop
      RTS
    
  ReadController2:
    LDA #$01
    STA $4016
    LDA #$00
    STA $4016
    LDX #$08
    .Loop:
      LDA $4017
      LSR A            ; bit0 -> Carry
      ROL buttons2     ; bit0 <- Carry
      DEX
      BNE .Loop
      RTS  

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
		LDX #4
  	STA current_background_index

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
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$3F
  STA $2006             ; write the high byte of $3F00 address
  LDA #$00
  STA $2006             ; write the low byte of $3F00 address
  LDX #$00              ; start out at 0
  .Loop:
	  LDA palette, x        ; load data from address (palette + the value in x)
	                          ; 1st time through loop it will load palette+0
	                          ; 2nd time through loop it will load palette+1
	                          ; 3rd time through loop it will load palette+2
	                          ; etc
	  STA $2007             ; write to PPU
	  INX                   ; X = X + 1
	  CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
    BNE .Loop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
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
  STX current_background_index

  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA backgrounds, X ;load A with the low byte of the room address
  STA pointer_low  ;store A in the zero-page RAM
  LDA backgrounds+1, X
  STA pointer_high       ; put the high byte of the address into pointer
  
  LDX #$00            ; start at pointer + 0
  LDY #$00

  .OutsideLoop:
    .InsideLoop:
  		LDA [pointer_low], y  ; copy one background byte from address in pointer plus Y
  		STA $2007           ; this runs 256 * 4 times
  		
  		INY                 ; inside loop counter
  		CPY #$00
      BNE .InsideLoop      ; run the inside loop 256 times before continuing down
  		
  		INC pointer_high       ; low byte went 0 to 256, so high byte needs to be changed now
  		
  		INX
  		CPX #$04
      BNE .OutsideLoop     ; run the outside loop 256 times before continuing down
	rts

IncrementScoreOne:
  .Inc1Ones:
    LDA score1_ones      ; load the lowest digit of the number
    CLC 
    ADC #$01           ; add one
    STA score1_ones
    CMP #$0A           ; check if it overflowed, now equals 10
    BNE .Done        ; if there was no overflow, all done
  .Inc1Tens:
    LDA #$00
    STA score1_ones      ; wrap digit to 0
    LDA score1_tens      ; load the next digit
    CLC 
    ADC #$01           ; add one, the carry from previous digit
    STA score1_tens
    CMP #$0A           ; check if it overflowed, now equals 10
    BNE .Done        ; if there was no overflow, all done
  .Done:
		jsr ScoreSound
		rts

IncrementScoreTwo:
  .Inc2Ones:
    LDA score2_ones      ; load the lowest digit of the number
    CLC 
    ADC #$01           ; add one
    STA score2_ones
    CMP #$0A           ; check if it overflowed, now equals 10
    BNE .Done        ; if there was no overflow, all done
  .Inc2Tens:
    LDA #$00
    STA score2_ones      ; wrap digit to 0
    LDA score2_tens      ; load the next digit
    CLC 
    ADC #$01           ; add one, the carry from previous digit
    STA score2_tens
    CMP #$0A           ; check if it overflowed, now equals 10
    BNE .Done        ; if there was no overflow, all done
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
  LDA #$01
  STA ball_down
  STA ball_right
  LDA #$00
  STA ball_up
  STA ball_left

  LDA #$50
  STA ball_y
  LDA #$80
  STA ball_x
  
  LDA #$02
  STA ball_speed_x
  STA ball_speed_y
  
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
	LDX #2
  STA current_background_index
  
  jsr TurnOffScreenAndNMI
  jsr LoadBackground
  jsr TurnOnScreenAndNMI
  
  LDA #STATE_PLAYING
  STA game_state
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
  .db 0, $75, $00, 0 ;ball
  .db 0, $32, $00, 0 ;munchy top left
  .db 0, $33, $00, 0 ;munchy top right
  .db 0, $34, $00, 0 ;munchy bottom left
  .db 0, $35, $00, 0 ;munchy bottom right

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
