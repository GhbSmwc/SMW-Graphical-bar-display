;Same as "Level_Simple.asm", but this is for vertical bars, not horizontal.
;Also note that this isn't a stripe image, so this alone is for multi-line
;status bar or overworld border+.

;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;all files in the routine folder in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

;This code measures the player's X position in a horizontal level.
;it measures from the very left side of the area the screen can possibly go
;to the right edge of the stage (last screen). Can be described as "progress"


incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;This is a simple test ASM using the graphical bar.
;best tested using uberasm tool.

main:
if !sa1 != 0
	%invoke_sa1(mainSA1)
	RTL
	mainSA1:
endif
;Get x position percentage in horizontal level.
;This is basically the progress meter.
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
	JSL GraphicalBarELITE_DrawGraphicalBarSubtractionLoopEdition	;>get bar values.
	STZ $00								;>Use Level-layer3 tileset
	JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
	if !Default_VerticalBarDirection == $00
		LDA.b #!Default_GraphicalBar_Pos_Tile_VerticalUpwards
		STA $00
		LDA.b #!Default_GraphicalBar_Pos_Tile_VerticalUpwards>>8
		STA $01
		LDA.b #!Default_GraphicalBar_Pos_Tile_VerticalUpwards>>16
		STA $02
	else
		LDA.b #!Default_GraphicalBar_Pos_Tile_VerticalDownwards
		STA $00
		LDA.b #!Default_GraphicalBar_Pos_Tile_VerticalDownwards>>8
		STA $01
		LDA.b #!Default_GraphicalBar_Pos_Tile_VerticalDownwards>>16
		STA $02
	endif
	if !StatusBar_UsingCustomProperties != 0
		if !Default_VerticalBarDirection == $00
			LDA.b #!Default_GraphicalBar_Pos_Properties_VerticalUpwards
			STA $03
			LDA.b #!Default_GraphicalBar_Pos_Properties_VerticalUpwards>>8
			STA $04
			LDA.b #!Default_GraphicalBar_Pos_Properties_VerticalUpwards>>16
			STA $05
		else
			LDA.b #!Default_GraphicalBar_Pos_Properties_VerticalDownwards
			STA $03
			LDA.b #!Default_GraphicalBar_Pos_Properties_VerticalDownwards>>8
			STA $04
			LDA.b #!Default_GraphicalBar_Pos_Properties_VerticalDownwards>>16
			STA $05
		endif
		if !PaletteChanging == 0
			LDA.b #!Default_StatusBar_TilePropertiesSetting			;\Properties
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
		if !Default_VerticalBarDirection == $02
			ORA.b #%10000000
		endif
		STA $06								;/
	endif
	LDA #!Default_VerticalBarDirection				;\Direction
	STA $07								;/
	if !StatusBarFormat == $01
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUDVertically	;>Write to status bar
	else
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUDVerticallyFormat2	;>Write to status bar
	endif
	RTL
	
	PaletteTable:
		;These contain the colors to use based on how much fill,
		;ordered in increasing thresholds, in YXPCCCTT form.
		;Formula for making YXPCCCTT data format workable with defines: [(!YFlip<<7)+(!Default_LeftwardsBar<<6)+(!Priority<<5)+(!palette<<2)+(!PageNumber)]
		; -!YFlip: Not mentioned in the defines, but for formula only (valid range 0-1).
		; -!Default_LeftwardsBar: Mentioned in the defines, X flip (valid range 0-1).
		; -!Priority: Goes in front or behind other things (valid range 0-1)
		; -!palette: Not mentioned in the defines, but for formula only (valid range 0-7).
		; -!PageNumber: Not mentioned in the defines, but for formula only (valid range 0-3).
		;
		;Because this code never assumes of you having a horizontal bar of 2 8x8 tiles tall, the Y flip is never needed
		;((0<<7) results 0, which means no addition occurs).
			db (!Default_LeftwardsBar<<6)+(1<<5)+(3<<2)+(0)	;>If below Threshold 1
			db (!Default_LeftwardsBar<<6)+(1<<5)+(7<<2)+(0)	;>If below Threshold 2
			db (!Default_LeftwardsBar<<6)+(1<<5)+(6<<2)+(0)	;>If at or above Threshold 2