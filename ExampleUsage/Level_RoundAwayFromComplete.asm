;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;"GraphicalBarELITE.asm" in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

;This is a coin meter to be used as a test for rounding towards empty or full
;("complete") detection.

;To see the rounding detection in action with 0-99 coins, have the number of pieces
;in the whole bar be 49 pieces or less. An easy way to do this with the least editing
;is to have !Default_MiddleLength set to 5.

incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;This is a simple test ASM using the graphical bar.
;best tested using uberasm tool.

;These (below) are tables to convert whats in !Scratchram_GraphicalBar_FillByteTbl to tile numbers.
main:

.InputRatio
	LDA $0DBF						;\Player's coins as the quantity.
	STA !Scratchram_GraphicalBar_FillByteTbl		;/
	LDA #$00						;>Clear...
	STA !Scratchram_GraphicalBar_FillByteTbl+1		;/the high byte (no STZ because STZ <3 byte address> does not exist).
	STA !Scratchram_GraphicalBar_FillByteTbl+3		;>Also clear the high byte of max quantity
	LDA.b #99						;\Okay, 99 coins is the maximum coins you can have before it rolls over to 0.
	STA !Scratchram_GraphicalBar_FillByteTbl+2		;/
.InputGraphicalBarAttributes
	LDA.b #!Default_LeftPieces				;\Left and right pieces
	STA !Scratchram_GraphicalBar_LeftEndPiece		;|
	LDA.b #!Default_RightPieces				;|
	STA !Scratchram_GraphicalBar_RightEndPiece		;/
	LDA.b #!Default_MiddlePieces				;\Number of pieces in each middle byte/8x8 tile
	STA !Scratchram_GraphicalBar_MiddlePiece		;/
	LDA.b #!Default_MiddleLength				;\length (number of middle tiles)
	STA !Scratchram_GraphicalBar_TempLength			;/
.ConvertToBar
	JSL GraphicalBarELITE_CalculateGraphicalBarPercentage	;>Get ratio/percentage [Here, I assume you are using uberasmTool]
	..RoundingDetect
	CPY #$00						;\check rounding flags (Y is only #$00 to #$02)
	BEQ ..BarWrite						;|
	CPY #$01						;|
	BEQ ..RoundedEmpty					;|
	BRA ..RoundedFull					;/>Of course, if Y cannot be 0 and 1, it has to be 2, so no extra checks.
	
	..RoundedEmpty
	REP #$20
	INC $00							;>if fill amount is a nonzero less than 0.5, make it display fillvalue = 1 to not display "empty".
	SEP #$20
	BRA ..BarWrite						;>and done

	..RoundedFull
	REP #$20
	DEC $00							;>if fill amount is at least Max-0.5 and less than Max, make it display fillvalue = max-1 to not display "full".
	SEP #$20
	
	..BarWrite
	JSL GraphicalBarELITE_DrawGraphicalBar
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