;This is a demonstration of drawing the bar onto a layer 3 stripe.

;Make sure the layer 3 settings in LM are:
;-Blank layer 3
;-[check] Force Layer 3 tiles with priority above other layers and sprites
;-[check] Enable advanced bypass settings for Layer 3
;
;-[uncheck] CGADSUB for Layer 3
;-[uncheck] Move layer 3 to subscreen
;
;-Vertical scroll: None
;-Horizontal scroll: None
;-Initial Y position/offset: 0
;-Initial X position/offset: 0

;Also, I highly recommend having the define "!StatusBar_UsingCustomProperties" set to 1 from "GraphicalBarDefines/StatusBarSettings.asm"
;as it could write garbage tiles or crash your game.

;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;all files in the routine folder in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

;This code measures the player's X position in a horizontal level.
;it measures from the very left side of the area the screen can possibly go
;to the right edge of the stage (last screen). Can be described as "progress"


incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;Don't touch these
	!XFlipForHorizontalBar = and(equal(!Default_StripeImage_Direction, 0), notequal(!Default_LeftwardsBar, 0))
	!YFlipForVerticalBar = and(notequal(!Default_StripeImage_Direction, 0), notequal(!Default_StripeVerticalDownwardsBar, 0))

;This is a simple test ASM using the graphical bar.
;best tested using uberasm tool.


main:
;Get x position percentage in horizontal level.
;This is basically the progress meter.
	;LDA $13D9|!addr						;\Prevent "COURSE CLEAR!" text from messing up.
	LDA $1493|!addr
	BEQ +							;/
	RTL
	+
	REP #$20
	LDA $94							;\Player's X position "progress"
	SEC							;|
	SBC #$0008						;>because the minimum player's X pos is #$0008
	BPL +							;|\Prevent underflow.
	LDA #$0000						;||
	+							;|/
	STA !Scratchram_GraphicalBar_FillByteTbl		;/
	SEP #$20
	LDA.b #!Default_MiddleLength				;\Input length (middle)
	STA !Scratchram_GraphicalBar_TempLength			;/
	LDA #$E0						;\The maximum X position the player can be (right edge of the level)
	STA !Scratchram_GraphicalBar_FillByteTbl+2		;|
	LDA $5E							;|
	DEC							;|
	STA !Scratchram_GraphicalBar_FillByteTbl+3		;/
	LDA.b #!Default_LeftPieces				;\Input amount of pieces in each of the 3 types of sections.
	STA !Scratchram_GraphicalBar_LeftEndPiece		;|
	LDA.b #!Default_MiddlePieces				;|
	STA !Scratchram_GraphicalBar_MiddlePiece		;|
	LDA.b #!Default_RightPieces				;|
	STA !Scratchram_GraphicalBar_RightEndPiece		;/
	JSL GraphicalBarELITE_CalculateGraphicalBarPercentage		;>Get percentage
	if notequal(and(notequal(!PaletteChanging, 0), notequal(!StatusBar_UsingCustomProperties, 0)), 0)
		REP #$20
		LDA $00						;>We are going to need this to determine its palette
		PHA
		SEP #$20
	endif
	JSL GraphicalBarELITE_DrawGraphicalBar				;>get bar values.
	STZ $00								;>Use Level-layer3 tileset
	JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
	;Write to layer 3
		;Position
			if !ExtendLeftOrUpwards != 0
				;Here this adjust to make the bar extend leftwards/upwards, it is calculated like this:
				;[ExtendUpOrLeftPos = InputXY - (length-1)]
				;Which is translated to [-(length-1) + InputXY] for code optimizations.
				;Note that the length is treated as 8-bit signed, but since the max is +127
				;and the screen is only 32 tiles wide, you shouldn't need such a length.
				JSL GraphicalBarWriteToStatusBar_CountNumberOfTiles	;\-(length-1)...
				TXA							;|
				EOR #$FF						;|
				INC							;/
				if !Default_StripeImage_Direction == 0			;\...plus InputXY
					;Horizontal					;|
					CLC						;|
					ADC #!Default_StripeImage_XPos			;|
					STA $00						;|
					LDA #!Default_StripeImage_YPos			;|
					STA $01						;|
				else							;|
					;Vertical					;|
					CLC						;|
					ADC #!Default_StripeImage_YPos			;|
					STA $01						;|
					LDA #!Default_StripeImage_XPos			;|
					STA $00						;|
				endif							;/
			else
				LDA #!Default_StripeImage_XPos
				STA $00
				LDA #!Default_StripeImage_YPos
				STA $01
			endif
		LDA #$05							;\Layer
		STA $02								;/
		LDA.b #(!Default_StripeImage_Direction<<7)			;\Direction (no RLE)
		STA $03								;/
		JSL GraphicalBarWriteToStatusBar_CountNumberOfTiles		;\Number of tiles
		STX $04								;|\Number of tiles for stripe
		STZ $05								;|/
		JSL GraphicalBarWriteToStatusBar_SetupStripe			;/>X (16-bit) = length of stripe
		;Tile numbers
			LDA #$7F						;\Bank bytes
			STA $02							;|
			STA $05							;/
			REP #$21						;
			TXA							;
			ADC.w #$7F837D+4					;
			STA $00							;>$00-$02 is now the address to write our bar tiles
			SEP #$20						;
		;Properties (YXPCCCTT)
			LDA #$7F						;
			STA $05							;
			REP #$21						;
			TXA							;
			ADC.w #$7F837D+4+1					;
			STA $03							;>$03-$05 is the address to write our bar properties
			SEP #$20						;
			STX $08							;>$08-$09 = stripe length index, last use on stack was potentially the palette
			SEP #$30
			if !PaletteChanging == 0
				if !Default_LeftwardsBar == 0
					LDA.b #!Default_StatusBar_TilePropertiesSetting			;\Properties
				else
					LDA.b #(!Default_StatusBar_TilePropertiesSetting|(!XFlipForHorizontalBar<<6)|(!YFlipForHorizontalBar<<7))
				endif
			else
				.PaletteThresholds
					LDX #$00		;>Default the index to 0
					REP #$20
					PLA			;>Pull out fill amount
					CMP.w #19		;>Threshold 1
					BCC ..GetValueFromTable
					INX
					CMP.w #37		;>Threshold 2
					BCC ..GetValueFromTable
					
					;At or above threshold 2
						INX
					
					..GetValueFromTable
						SEP #$20
						LDA PaletteTable,x
			endif
			STA $06								;/
		;Write bar tiles
			if or(and(equal(!Default_StripeImage_Direction, 0), equal(!Default_LeftwardsBar, 0)), and(notequal(!Default_StripeImage_Direction, 0), notequal(!Default_StripeVerticalDownwardsBar, 0))) ;If [horizontal, rightwards], or [vertical, downwards], it's a positive direction
				JSL GraphicalBarWriteToStatusBar_WriteBarToHUDFormat2		;>rightwards or downwards
			else
				JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwardsFormat2	;>Leftwards or upwards.
			endif
			SEP #$30
	RTL
	
	PaletteTable:
		;These contain the colors to use based on how much fill,
		;ordered in increasing thresholds, in YXPCCCTT form.
		;Formula for making YXPCCCTT data format workable with defines: [(!YFlip<<7)+(!XFlipForHorizontalBar<<6)+(!Priority<<5)+(!palette<<2)+(!PageNumber)]
		; -!YFlip: Not mentioned in the defines, but for formula only (valid range 0-1).
		; -!XFlipForHorizontalBar: Mentioned in the defines, X flip (valid range 0-1).
		; -!Priority: Goes in front or behind other things (valid range 0-1)
		; -!palette: Not mentioned in the defines, but for formula only (valid range 0-7).
		; -!PageNumber: Not mentioned in the defines, but for formula only (valid range 0-3).
		;
		;Because this code never assumes of you having a horizontal bar of 2 8x8 tiles tall, the Y flip is never needed
		;((0<<7) results 0, which means no addition occurs).
			db (!YFlipForVerticalBar<<7)+(!XFlipForHorizontalBar<<6)+(1<<5)+(3<<2)+(0)	;>If below Threshold 1
			db (!YFlipForVerticalBar<<7)+(!XFlipForHorizontalBar<<6)+(1<<5)+(7<<2)+(0)	;>If below Threshold 2
			db (!YFlipForVerticalBar<<7)+(!XFlipForHorizontalBar<<6)+(1<<5)+(6<<2)+(0)	;>If at or above Threshold 2