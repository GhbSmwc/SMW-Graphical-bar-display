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
;Get x position percentage in horizontal level.
;This is basically the progress meter.
if !CPUMode != 0
	%invoke_sa1(mainSA1)
	RTL
	mainSA1:
endif
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
	LDA.b #!Default_GraphicalBar_Pos_Tile
	STA $00
	LDA.b #!Default_GraphicalBar_Pos_Tile>>8
	STA $01
	LDA.b #!Default_GraphicalBar_Pos_Tile>>16
	STA $02
	if !StatusBar_UsingCustomProperties != 0
		LDA.b #!Default_GraphicalBar_Pos_Properties
		STA $03
		LDA.b #!Default_GraphicalBar_Pos_Properties>>8
		STA $04
		LDA.b #!Default_GraphicalBar_Pos_Properties>>16
		STA $05
		if !PaletteChanging == 0
			if !Default_LeftwardsBar == 0
				LDA.b #!Default_StatusBar_TilePropertiesSetting			;\Properties
			else
				LDA.b #(!Default_StatusBar_TilePropertiesSetting|(!Default_LeftwardsBar<<6))
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
	endif
	if !Default_LeftwardsBar == 0
		if !StatusBarFormat = $01
			JSL GraphicalBarWriteToStatusBar_WriteBarToHUD			;>Write to status bar
		else
			JSL GraphicalBarWriteToStatusBar_WriteBarToHUDFormat2		;>Write to status bar
		endif
	else
		if !StatusBarFormat = $01
			JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwards
		else
			JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwardsFormat2
		endif
	endif
	RTL
	
	PaletteTable:
		;These contain the colors to use based on how much fill,
		;ordered in increasing thresholds, in YXPCCCTT form.
		;The color to use is a palette in the 4th argument of the function called (must be an integer 0-7)
			db GetLayer3YXPCCCTT(!Default_StatusBar_TileProperties_YFlip,!Default_LeftwardsBar,!Default_StatusBar_TileProperties_Priority,3,!Default_StatusBar_TileProperties_Page) ;>If below Threshold 1
			db GetLayer3YXPCCCTT(!Default_StatusBar_TileProperties_YFlip,!Default_LeftwardsBar,!Default_StatusBar_TileProperties_Priority,7,!Default_StatusBar_TileProperties_Page) ;>If below Threshold 2
			db GetLayer3YXPCCCTT(!Default_StatusBar_TileProperties_YFlip,!Default_LeftwardsBar,!Default_StatusBar_TileProperties_Priority,6,!Default_StatusBar_TileProperties_Page) ;>If at or above Threshold 2