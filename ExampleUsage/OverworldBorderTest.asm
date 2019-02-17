;Unlike the other ASM files here, this is meant to run in gamemode $0E,
;which is the overworld mode.
;
;You need Overworld Border plus (OWB+ I abbreviated it) patch in order to write tiles to the overworld
;border.
;
;Also, because the OWB+ and SSB (Super status bar) patches are the same tile format (each tile have 2
;bytes adjacent to one another), I recommend you to use both those patches and not use other status bar
;patches to test both level and Overworld versions at once.

incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;This measures Mario's bonus stars (not luigi).

main:
incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"
;^These are needed so the defines relating to the graphical bars work.

.InputRatio
	LDA $0F48|!addr						;\Quantity low byte (example: current HP). Use RAM here.
	STA !Scratchram_GraphicalBar_FillByteTbl		;/
	LDA #$00						;\High byte of above. Should your value here is 8-bit or only 1 byte long,
	STA !Scratchram_GraphicalBar_FillByteTbl+1		;/use [LDA #$00 : STA !Scratchram_GraphicalBar_FillByteTbl+1].
	LDA #$63						;\Max quantity low byte (example: max HP). Can be a fixed value (#$) or adjustable RAM in-game.
	STA !Scratchram_GraphicalBar_FillByteTbl+2		;/
	LDA #$00						;\High byte of above, same format as <Value_high_byte>, so do the same
	STA !Scratchram_GraphicalBar_FillByteTbl+3		;/as that if your value is 8-bit.
.InputGraphicalBarAttributes
	LDA.b #!Default_LeftPieces				;\Left end normally have 3 pieces.
	STA !Scratchram_GraphicalBar_LeftEndPiece		;/
	LDA.b #!Default_MiddlePieces				;\Number of pieces in each middle byte/8x8 tile
	STA !Scratchram_GraphicalBar_MiddlePiece		;/
	LDA.b #!Default_RightPieces				;\Right end
	STA !Scratchram_GraphicalBar_RightEndPiece		;/
	LDA.b #!Default_MiddleLength				;\length (number of middle tiles)
	STA !Scratchram_GraphicalBar_TempLength			;/
.ConvertToBar
	JSL GraphicalBarELITE_CalculateGraphicalBarPercentage		;>Get percentage
	
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
	
	JSL GraphicalBarELITE_DrawGraphicalBar				;>get bar values.
	JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
	LDA.b #!Default_GraphicalBarPositionOverworldMap		;\Setup address to where to draw the bar.
	STA $00								;|
	LDA.b #!Default_GraphicalBarPositionOverworldMap>>8		;|
	STA $01								;|
	LDA.b #!Default_GraphicalBarPositionOverworldMap>>16		;|
	STA $02								;/
	if !StatusBar_UsingCustomProperties != 0
		LDA.b #!Default_GraphicalBarPropertiesOverworldMap		;\Same as above but properties
		STA $03								;|
		LDA.b #!Default_GraphicalBarPropertiesOverworldMap>>8		;|
		STA $04								;|
		LDA.b #!Default_GraphicalBarPropertiesOverworldMap>>16		;|
		STA $05								;/
		LDA.b #!Default_Overworld_TilePropertiesSetting			;\Properties
		STA $06								;/
	endif
	if !Default_LeftwardsBar == 0						;\Write to status bar
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUD			;|
	else									;|
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwards		;|
	endif									;/
	RTL