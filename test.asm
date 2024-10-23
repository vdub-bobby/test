/*************************************************************************
-------------------------------------------------------------------------
---------Testing Binary for Trying Out Stuff-----------------------------
-------------------------------------------------------------------------
	Bob Montgomery
	(c) 2012-2024



*/




 
	processor 6502
	include vcs.h
	include macro.h

;-----------------------MY MACROS-----------------------------------------


;--this macro aligns at page boundary (only if needed) and echoes the bytes used
    MAC PAGEALIGN
        if <* != 0
        	echo "---", ((* & $FF00) + $100) - *, "bytes left at PAGEALIGN", [{1}]d, "at location", *
    	    align 256
    	else
    	    echo "--- $0 bytes left at PAGEALIGN", [{1}]d, "at location", *
        endif
    ENDM
	
    MAC ALIGNGFXDATA
.marker set *
        IF DATASPACEBEFOREGFX > (* & $FF)
            ds DATASPACEBEFOREGFX - (* & $FF)
        ENDIF
        ECHO "---", (*-.marker), "bytes left at ALIGNGFXDATA", [{1}]d, "at location", .marker
    ENDM
    
    
    
    ;--pass in 1- to 4-digit decimal number and will spit out it as hex BCD number
    MACRO DecimalToBCD
.Ones SET {1}%10
.Tens Set ({1}/10)%10
.Hundreds Set ({1}/100)%10
.Thousands Set ({1}/1000)%10
        .word [.Ones]+[.Tens<<4]+[.Hundreds<<8]+[.Thousands<<12]
    ENDM
     
    
        
;-------------------------Constants Below---------------------------------

; TV System
PAL         = 1
NTSC        = 0
PAL60       = 2
SECAM       = 3

SYSTEM      = NTSC

    IF SYSTEM = NTSC || SYSTEM = PAL60
SYSTEMSPEED =   60
    ELSE
SYSTEMSPEED =   50
    ENDIF

    IF SYSTEMSPEED = 50
;--scanline count constants
VBLANK_TIMER = 76
OVERSCAN_TIMER = 52
    ELSE
    ;for NTSC or PAL60
VBLANK_TIMER = 33       ;--I think this is fine
OVERSCAN_TIMER = 36     ;--I think this is fine also, with the skipping the sound subroutine if we don't have time (see below)
    ENDIF

SOUNDTIMEBUFFER     =   (8*76)/64
;--scanline count constants end


    IF SYSTEM = NTSC
;-------------------------COLOR CONSTANTS (NTSC)--------------------------
GRAY		=	$00
GOLD		=	$10
ORANGE		=	$20
BURNTORANGE	=	$30
RED		    =	$40
PURPLE		=	$50
PURPLEBLUE	=	$60
BLUE		=	$70
BLUE2		=	$80
LIGHTBLUE	=	$90
TURQUOISE	=	$A0
GREEN		=	$B0
BROWNGREEN	=	$C0
TANGREEN	=	$D0
TAN		    =	$E0
BROWN		=	$F0
    ELSE
        IF SYSTEM = PAL || SYSTEM = PAL60


;-------------------------COLOR CONSTANTS (PAL)--------------------------
GRAY		=	$00
GOLD		=	$20
ORANGE		=	$20
BURNTORANGE	=	$40
RED		    =	$60
PURPLE		=	$80
PURPLEBLUE	=	$A0
BLUE		=	$C0
BLUE2		=	$D0
LIGHTBLUE	=	$B0
TURQUOISE	=	$90
GREEN		=	$70
BROWNGREEN	=	$50
TANGREEN	=	$30
TAN		    =	$20
BROWN		=	$20
        ELSE    ;secam
;-------------------------COLOR CONSTANTS (SECAM)--------------------------
BLACK   =   $00
BLUE    =   $02
RED     =   $04
MAGENTA =   $06
GREEN   =   $08
CYAN    =   $0A
YELLOW  =   $0C
WHITE   =   $0F
        ENDIF

    ENDIF
;--------------------------TIA CONSTANTS----------------------------------

	;--NUSIZx CONSTANTS
	;	player:
ONECOPYNORMAL		=	$00
TWOCOPIESCLOSE		=	$01
TWOCOPIESMED		=	$02
THREECOPIESCLOSE	=	$03
TWOCOPIESWIDE		=	$04
ONECOPYDOUBLE		=	$05
THREECOPIESMED		=	$06
ONECOPYQUAD		    =	$07
	;	missile:
SINGLEWIDTHMISSILE	=	$00
DOUBLEWIDTHMISSILE	=	$10
QUADWIDTHMISSILE	=	$20
OCTWIDTHMISSILE		=	$30

	;---CTRLPF CONSTANTS
	;	playfield:
REFLECTEDPF		=	%00000001
SCOREPF			=	%00000010
PRIORITYPF		=	%00000100
	;	ball:
SINGLEWIDTHBALL		=	SINGLEWIDTHMISSILE
DOUBLEWIDTHBALL		=	DOUBLEWIDTHMISSILE
QUADWIDTHBALL		=	QUADWIDTHMISSILE
OCTWIDTHBALL		=	OCTWIDTHMISSILE

	;---HMxx CONSTANTS
LEFTSEVEN		=	$70
LEFTSIX			=	$60
LEFTFIVE		=	$50
LEFTFOUR		=	$40
LEFTTHREE		=	$30
LEFTTWO			=	$20
LEFTONE			=	$10
NOMOVEMENT		=	$00
RIGHTONE		=	$F0
RIGHTTWO		=	$E0
RIGHTTHREE		=	$D0
RIGHTFOUR		=	$C0
RIGHTFIVE		=	$B0
RIGHTSIX		=	$A0
RIGHTSEVEN		=	$90
RIGHTEIGHT		=	$80

	;---AUDCx CONSTANTS (P Slocum's naming convention)
SAWSOUND		=	1
ENGINESOUND		=	3
SQUARESOUND		=	4
BASSSOUND		=	6
PITFALLSOUND		=	7
NOISESOUND		=	8
LEADSOUND		=	12
BUZZSOUND		=	15

	;---SWCHA CONSTANTS (JOYSTICK)
J0RIGHT		=	%10000000
J0LEFT		=	%01000000
J0DOWN		=	%00100000
J0UP		=	%00010000
J1RIGHT		=	%00001000
J1LEFT		=	%00000100
J1DOWN		=	%00000010
J1UP		=	%00000001

	;---SWCHB CONSTANTS (CONSOLE SWITCHES)
P1DIFF		=	%10000000
P0DIFF		=	%01000000
BWCOLOR		=	%00001000
SELECT		=	%00000010
RESET		=	%00000001




;-------------------------End Constants-----------------------------------

;---Macros

	MAC FILLER
		REPEAT {1}
		.byte {2}
		REPEND
	ENDM
	
	

;--------Variables To Follow-------------

	SEG.U Variables
   	org $80

FrameCounter ds 1

RandomNumber ds 1
Temp ds 3  


   ; Display Remaining RAM
   echo "----",($100 - *) , "bytes left (ZP RAM)"
;---End Variables

	seg Bank0

	org $1000
	rorg $1000

Start
;	sta $1FF8
    nop
    nop
    nop
    CLEAN_START

;--Some Initial Setup


; 	jsr InitialSetupSubroutine  ;--inline to save 4 bytes
; InitialSetupSubroutine




;----------------------------------------------------------------------------
;--------------------GAME MAIN LOOP------------------------------------------
;----------------------------------------------------------------------------
MainGameLoop
; 
	jsr VBLANKRoutine
	jsr KernelRoutineGame
	jsr OverscanRoutine
	jmp MainGameLoop

;----------------------------------------------------------------------------
;----------------------Kernel Routine----------------------------------------
;----------------------------------------------------------------------------
	
KernelRoutineGame

	
	lda #0
	sta WSYNC
	sta VBLANK

	
	ldy #198
KernelLoop
	sta WSYNC
	sty COLUBK
	dey
	bne KernelLoop

	rts

;----------------------------------------------------------------------------
;-------------------VBLANK Routine-------------------------------------------
;----------------------------------------------------------------------------
    SUBROUTINE
VBLANKRoutine
    lda #%00001111
VSYNCWaitLoop2
    sta WSYNC
    sta VSYNC
    lsr
    bne VSYNCWaitLoop2      ;different version of this loop
    
    

	lda #VBLANK_TIMER
	sta TIM64T
	
    dec FrameCounter
    
    
WaitForVBLANKEnd
	lda INTIM
 	bpl WaitForVBLANKEnd
    
	rts
	
;----------------------------------------------------------------------------
;------------------------Overscan Routine------------------------------------
;----------------------------------------------------------------------------
OverscanRoutine

	ldy #2
	sty WSYNC
	sty VBLANK
	lda #OVERSCAN_TIMER
	sta TIM64T
	
	          ;--for testing
WaitForOverscanEnd
	lda INTIM
 	bpl WaitForOverscanEnd

 	rts
	
;----------------------------------------------------------------------------
;----------------------------End Main Routines-------------------------------
;----------------------------------------------------------------------------

   



;****************************************************************************

;----------------------------------------------------------------------------
;----------------------Begin Functions---------------------------------------
;----------------------------------------------------------------------------

    ;--inlining this (in two places!)
UpdateRandomNumber
    lda RandomNumber
    lsr
    bcc SkipEOR
    eor #$B2
SkipEOR
    sta RandomNumber
    rts
	
;----------------------------------------------------------------------------
;     ;--this is to make sure the "DivideLoop" doesn't cross a page boundary.
;     if ((* + 5) & $FF00) != ((* + 9) & $FF00)
;         echo "---Aligned PositionASpriteSubroutineBank2 -", $FF - ((* + 4) & $FF), "bytes left at location", *
;         ds $FF - ((* + 4) & $FF)
;     else
;         echo "---Aligned PositionASpriteSubroutineBank2 not necessary"
;     endif
;     SUBROUTINE
; PositionASpriteSubroutineBank2
;     sec
; 	sta HMCLR
; PositionASpriteSubroutineBank2NoHMCLR	
; 	sta WSYNC
; .DivideLoop			;				this loop can't cross a page boundary!!!
; 	sbc #15
; 	bcs .DivideLoop	;+4		 4
; 	eor #7
; 	asl
; 	asl
; 	asl
; 	asl				;+10	14      sets carry
; 	sta.wx HMP0,X	;+5		19
; 	sta RESP0,X		;+4		23
; 	sta WSYNC
; 	sta HMOVE
; 	rts                 ;+9      9
;----------------------------------------------------------------------------
	
	;--reserve space for hot spots

    org $1FF8
    rorg $1FF8
    
    ds 2


	org $1FFC
    rorg $1FFC
    
	.word Start
	.word Start

