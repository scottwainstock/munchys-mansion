  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, no bank swapping
  .inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

;; DECLARE SOME VARIABLES HERE
  .rsset $0000  ;;start variables at ram location 0
  
gamestate  .rs 1  ; .rs 1 means reserve one byte of space
ballx      .rs 1  ; ball horizontal position
bally      .rs 1  ; ball vertical position
ballup     .rs 1  ; 1 = ball moving up
balldown   .rs 1  ; 1 = ball moving down
ballleft   .rs 1  ; 1 = ball moving left
ballright  .rs 1  ; 1 = ball moving right
ballspeedx .rs 1  ; ball horizontal speed per frame
ballspeedy .rs 1  ; ball vertical speed per frame
paddle1ytop   .rs 1  ; player 1 paddle top vertical position
paddle1ybot   .rs 1  ; player 1 paddle top vertical position
paddle1yend   .rs 1  ; player 1 paddle top vertical position
paddle2ytop   .rs 1  ; player 2 paddle bottom vertical position
paddle2ybot   .rs 1  ; player 2 paddle bottom vertical position
paddle2yend   .rs 1  ; player 2 paddle bottom vertical position
buttons1   .rs 1  ; player 1 gamepad buttons, one bit per button
buttons2   .rs 1  ; player 2 gamepad buttons, one bit per button
score1High     .rs 1  ; player 1 score, 0-15
score1Low     .rs 1  ; player 1 score, 0-15
score1Ones     .rs 1  ; player 1 score, 0-15
score1Tens     .rs 1  ; player 1 score, 0-15
score2High     .rs 1  ; player 1 score, 0-15
score2Low     .rs 1  ; player 1 score, 0-15
score2Ones     .rs 1  ; player 2 score, 0-15
score2Tens     .rs 1  ; player 2 score, 0-15
pointerLo  .rs 1   ; pointer variables are declared in RAM
pointerHi  .rs 1   ; low byte first, high byte immediately after
current_background_index .rs 1

;; DECLARE SOME CONSTANTS HERE
STATETITLE     = $00  ; displaying title screen
STATEPLAYING   = $01  ; move paddles/ball, check for collisions
STATEGAMEOVER  = $02  ; displaying game over screen
  
RIGHTWALL      = $F4  ; when ball reaches one of these, do something
TOPWALL        = $20
BOTTOMWALL     = $E0
LEFTWALL       = $04
  
PADDLE1X       = $09  ; horizontal position for paddles, doesnt move
PADDLE2X       = $F0

A_PRESSED       = $80
B_PRESSED       = $40
SEL_PRESSED     = $20
START_PRESSED   = $10
UP_PRESSED      = $08
DOWN_PRESSED    = $04
LEFT_PRESSED    = $02
RIGHT_PRESSED   = $01

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
   
	;;:Set starting game state
  LDA #STATETITLE
  STA gamestate
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
    LDA gamestate
    CMP #STATETITLE
    BEQ EngineTitle    ;;game is displaying title screen
      
    LDA gamestate
    CMP #STATEGAMEOVER
    BEQ EngineGameOver  ;;game is displaying ending screen
    
    LDA gamestate
    CMP #STATEPLAYING
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

      lda paddle1ytop
      cmp #TOPWALL
      beq .CheckP1UpDone

      LDA paddle1ytop
      SEC             ; make sure carry flag is set
      SBC #$02        ; A = A - 1
      STA paddle1ytop ; save new paddley position
      LDA paddle1ybot
      SEC             ; make sure carry flag is set
      SBC #$02        ; A = A - 1
      STA paddle1ybot ; save new paddley position
			SEC
			SBC #$08
      STA paddle1yend ; save new paddley end position
      .CheckP1UpDone:
        ;noop

    .CheckP1Down:
		  LDA buttons1
		  AND #DOWN_PRESSED
      BEQ .CheckP1DownDone

      lda paddle1yend
      CMP #BOTTOMWALL
      beq .CheckP1DownDone

      LDA paddle1ytop
      CLC             ; make sure the carry flag is clear
      ADC #$02        ; A = A + 1
      STA paddle1ytop ; save new paddley position
      LDA paddle1ybot
      CLC             ; make sure the carry flag is clear
      ADC #$02        ; A = A + 1
      STA paddle1ybot ; save new paddley position
			CLC
 		  ADC #$08
      STA paddle1yend ; save new paddley end position
      .CheckP1DownDone:
        ;noop

    .CheckP2Up:
		  LDA buttons2
		  AND #UP_PRESSED
      BEQ .CheckP2UpDone
      
      lda paddle2ytop
      cmp #TOPWALL
      beq .CheckP2UpDone

      LDA paddle2ytop
      SEC             ; make sure carry flag is set
      SBC #$02        ; A = A - 1
      STA paddle2ytop ; save new paddley position
      LDA paddle2ybot
      SEC             ; make sure carry flag is set
      SBC #$02        ; A = A - 1
      STA paddle2ybot ; save new paddley position
			SEC
      SBC #$08        ; A = A - 1
			STA paddle2yend
      .CheckP2UpDone:
        ;noop

    .CheckP2Down:
		  LDA buttons2
		  AND #DOWN_PRESSED
      BEQ .CheckP2DownDone

      lda paddle2yend
      CMP #BOTTOMWALL
      beq .CheckP2DownDone

      LDA paddle2ytop
      CLC             ; make sure the carry flag is clear
      ADC #$02        ; A = A + 1
      STA paddle2ytop ; save new paddley position
      LDA paddle2ybot
      CLC             ; make sure the carry flag is clear
      ADC #$02        ; A = A + 1
      STA paddle2ybot ; save new paddley position
			CLC
			ADC #08
      STA paddle2yend
      .CheckP2DownDone:
        ;noop

    .MoveBallRight:
      LDA ballright
      BEQ .MoveBallRightDone   ;;if ballright=0, skip this section
    
      LDA ballx
      CLC
      ADC ballspeedx        ;;ballx position = ballx + ballspeedx
      STA ballx
    
      LDA ballx
      CMP #RIGHTWALL
      BCC .MoveBallRightDone      ;;if ball x < right wall, still on screen, skip next section
      LDA #$00
      STA ballright
      LDA #$01
      STA ballleft         ;;bounce, ball now moving left

      ;;in real game, give point to player 1, reset ball
      jsr IncrementScoreOne
			jsr CheckIfGameIsOver

      .MoveBallRightDone:
  
    .MoveBallLeft:
      LDA ballleft
      BEQ .MoveBallLeftDone   ;;if ballleft=0, skip this section
    
      LDA ballx
      SEC
      SBC ballspeedx        ;;ballx position = ballx - ballspeedx
      STA ballx
    
      LDA ballx
      CMP #LEFTWALL
      BCS .MoveBallLeftDone      ;;if ball x > left wall, still on screen, skip next section
      LDA #$01
      STA ballright
      LDA #$00
      STA ballleft         ;;bounce, ball now moving right

      ;;in real game, give point to player 2, reset ball
      jsr IncrementScoreTwo
			jsr CheckIfGameIsOver

      .MoveBallLeftDone:
  
    .MoveBallUp:
      LDA ballup
      BEQ .MoveBallUpDone   ;;if ballup=0, skip this section
    
      LDA bally
      SEC
      SBC ballspeedy        ;;bally position = bally - ballspeedy
      STA bally
    
      LDA bally
      CMP #TOPWALL
      BCS .MoveBallUpDone      ;;if ball y > top wall, still on screen, skip next section
      LDA #$01
      STA balldown
      LDA #$00
      STA ballup         ;;bounce, ball now moving down

      .MoveBallUpDone:
  
    .MoveBallDown:
      LDA balldown
      BEQ .MoveBallDownDone   ;;if ballup=0, skip this section
    
      LDA bally
      CLC
      ADC ballspeedy        ;;bally position = bally + ballspeedy
      STA bally
    
      LDA bally
      CMP #BOTTOMWALL
      BCC .MoveBallDownDone      ;;if ball y < bottom wall, still on screen, skip next section
      LDA #$00
      STA balldown
      LDA #$01
      STA ballup         ;;bounce, ball now moving down

      .MoveBallDownDone:
  
    .CheckPaddleCollision:
      ;;if ball x < paddle1x
      ;;  if ball y > paddle y top
      ;;    if ball y < paddle y bottom
      ;;      bounce, ball now moving left
  		lda ballx
  		cmp #PADDLE1X
      bcs .CheckPaddle1CollisionDone

  		lda bally
      cmp paddle1ytop
      bcc .CheckPaddle1CollisionDone

  		lda bally
      cmp paddle1yend
      bcs .CheckPaddle1CollisionDone

  		lda #$00
      sta ballleft
      lda #$01
      sta ballright                      ;; bounce, ball now moving up
			jsr BounceSound

      .CheckPaddle1CollisionDone:

  		lda ballx
  		cmp #PADDLE2X
      bcs .CheckPaddle2CollisionDone

  		lda bally
      cmp paddle2ytop
      bcc .CheckPaddle2CollisionDone

  		lda bally
      cmp paddle2yend
      bcs .CheckPaddle2CollisionDone

  		LDA #$01
  		STA ballleft
  		LDA #$00
  		STA ballright                      ;; bounce, ball now moving up
			jsr BounceSound

      .CheckPaddle2CollisionDone:

      JMP GameEngineDone

;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  UpdateSprites:
    LDA bally  ;;update all ball sprite info
    STA $0200
    LDA #$75
    STA $0201
    LDA #$00
    STA $0202
    LDA ballx
    STA $0203
    
    ; update paddle 1
    LDA paddle1ytop
    STA $0204
    LDA #$85
    STA $0205
    LDA #$00
    STA $0206
    LDA #PADDLE1X
    STA $0207

    LDA paddle1ybot
    STA $0208
    LDA #$85
    STA $0209
    LDA #$00
    STA $020A
    LDA #PADDLE1X
    STA $020B

    ; update paddle 2
    LDA paddle2ytop
    STA $020C
    LDA #$85
    STA $020D
    LDA #$00
    STA $020E
    LDA #PADDLE2X
    STA $020F

    LDA paddle2ybot
    STA $0210
    LDA #$85
    STA $0211
    LDA #$00
    STA $0212
    LDA #PADDLE2X
    STA $0213

    RTS
 
  DrawScore:
    LDA $2002
    LDA score1High
    STA $2006
    LDA score1Low
    STA $2006
    
    LDA score1Tens      ; next digit
    STA $2007
    LDA score1Ones      ; last digit
    STA $2007

    LDA $2002
    LDA score2High
    STA $2006
    LDA score2Low
    STA $2006

    LDA score2Tens      ; next digit
    STA $2007
    LDA score2Ones      ; last digit
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
  lda score1Tens
	cmp #$01
  bne .Check1OnesDone

  lda score1Ones
	cmp #$05
  beq .GameIsOver
  .Check1OnesDone:

  lda score2Tens
	cmp #$01
  bne .Check2OnesDone

  lda score2Ones
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

		lda #STATEGAMEOVER
		sta gamestate

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

LoadBackground:
  STX current_background_index

  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte of $2000 address
  LDA #$00
  STA $2006             ; write the low byte of $2000 address

  LDA backgrounds, X ;load A with the low byte of the room address
  STA pointerLo  ;store A in the zero-page RAM
  LDA backgrounds+1, X
  STA pointerHi       ; put the high byte of the address into pointer
  
  LDX #$00            ; start at pointer + 0
  LDY #$00

  .OutsideLoop:
    .InsideLoop:
  		LDA [pointerLo], y  ; copy one background byte from address in pointer plus Y
  		STA $2007           ; this runs 256 * 4 times
  		
  		INY                 ; inside loop counter
  		CPY #$00
      BNE .InsideLoop      ; run the inside loop 256 times before continuing down
  		
  		INC pointerHi       ; low byte went 0 to 256, so high byte needs to be changed now
  		
  		INX
  		CPX #$04
      BNE .OutsideLoop     ; run the outside loop 256 times before continuing down
	rts

IncrementScoreOne:
  .Inc1Ones:
    LDA score1Ones      ; load the lowest digit of the number
    CLC 
    ADC #$01           ; add one
    STA score1Ones
    CMP #$0A           ; check if it overflowed, now equals 10
    BNE .Done        ; if there was no overflow, all done
  .Inc1Tens:
    LDA #$00
    STA score1Ones      ; wrap digit to 0
    LDA score1Tens      ; load the next digit
    CLC 
    ADC #$01           ; add one, the carry from previous digit
    STA score1Tens
    CMP #$0A           ; check if it overflowed, now equals 10
    BNE .Done        ; if there was no overflow, all done
  .Done:
		jsr ScoreSound
		rts

IncrementScoreTwo:
  .Inc2Ones:
    LDA score2Ones      ; load the lowest digit of the number
    CLC 
    ADC #$01           ; add one
    STA score2Ones
    CMP #$0A           ; check if it overflowed, now equals 10
    BNE .Done        ; if there was no overflow, all done
  .Inc2Tens:
    LDA #$00
    STA score2Ones      ; wrap digit to 0
    LDA score2Tens      ; load the next digit
    CLC 
    ADC #$01           ; add one, the carry from previous digit
    STA score2Tens
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
  STA balldown
  STA ballright
  LDA #$00
  STA ballup
  STA ballleft
  
  LDA #$50
  STA bally
  LDA #$80
  STA ballx
  
  LDA #$02
  STA ballspeedx
  STA ballspeedy
  
  ; set initial paddle y stats
  LDA #$50
  STA paddle1ytop
  LDA #$58
  STA paddle1ybot
  LDA #$80
  STA paddle2ytop
  LDA #$88
  STA paddle2ybot
  
  ; setup scores
  lda #0
  sta score1Ones
  sta score1Tens
  sta score2Ones
  sta score2Tens
  
  lda #$20
  sta score1High
  sta score2High
  
  lda #$4C
  sta score1Low
  
  lda #$51
  sta score2Low

	; setup background
	LDX #2
  STA current_background_index
  
  jsr TurnOffScreenAndNMI
  jsr LoadBackground
  jsr TurnOnScreenAndNMI
  
  LDA #STATEPLAYING
  STA gamestate
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
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .db $88, $35, $00, $88   ;sprite 3

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
