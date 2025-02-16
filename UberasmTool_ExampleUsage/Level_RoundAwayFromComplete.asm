;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;all files in the routine folder in the library folder for this to work (unless you edit the
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
if !CPUMode != 0
	%invoke_sa1(mainSA1)
	RTL
	mainSA1:
endif
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
	JSL GraphicalBarELITE_RoundAwayEmptyFull		;>Avoid rounding towards 0 or MaxPieces when they are not those numbers.
	JSL GraphicalBarELITE_DrawGraphicalBarSubtractionLoopEdition
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