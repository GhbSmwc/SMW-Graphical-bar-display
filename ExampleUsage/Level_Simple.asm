;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;"GraphicalBarELITE.asm" in the library folder for this to work (unless you edit the
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
	REP #$20
	LDA $94							;\Player's X position "progress"
	SEC							;|
	SBC #$0008						;>because the minimum player's X pos is #$0008
	STA !Scratchram_GraphicalBar_FillByteTbl		;/
	SEP #$20
	LDA.b #!Default_MiddleLength				;\Input length (middle)
	STA !Scratchram_GraphicalBar_TempLength			;/
	LDA #$E0						;\The maximum X positon the player can be (right edge of the level)
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
	JSL GraphicalBarELITE_DrawGraphicalBar				;>get bar values.
	JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
	LDA.b #!Default_GraphicalBarPosition
	STA $00
	LDA.b #!Default_GraphicalBarPosition>>8
	STA $01
	LDA.b #!Default_GraphicalBarPosition>>16
	STA $02
	if !StatusBar_UsingCustomProperties != 0
		LDA.b #!Default_GraphicalBarProperties
		STA $03
		LDA.b #!Default_GraphicalBarProperties>>8
		STA $04
		LDA.b #!Default_GraphicalBarProperties>>16
		STA $05
		if !Default_LeftwardsBar == 0
			LDA.b #!Default_StatusBar_TilePropertiesSetting			;\Properties
		else
			LDA.b #(!Default_StatusBar_TilePropertiesSetting|(!Default_LeftwardsBar<<6))
		endif
		STA $06								;/
	endif
	if !Default_LeftwardsBar == 0
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUD			;>Write to status bar
	else
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwards
	endif
	RTL