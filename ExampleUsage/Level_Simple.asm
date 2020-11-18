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

!PaletteChanging = 1
 ;^0 = stays the same palette regardless the amount of fill in bar
 ; 1 = change color based on how much fill. See code below marked
 ; [PaletteChangingCode]

main:
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
	JSL GraphicalBarELITE_DrawGraphicalBar				;>get bar values.
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
				LDX #$00
				REP #$20
				PLA
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
		;must be ordered in increasing thresholds, in YXPCCCTT form.
			db %00101100	;>If below Threshold 1
			db %00111100	;>If below Threshold 2
			db %00111000	;>If at or above Threshold 2