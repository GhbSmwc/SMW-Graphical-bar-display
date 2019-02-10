;This is a demonstration on how to have a bar that extends LEFTWARDS as you increase
;length of the bar (not to be confused with "leftwards-filling").
;Normally, without the [JSL GraphicalBarWriteToStatusBar_BarExtendLeft], the bar would
;would extend rightwards, even when set to fill leftwards. This is useful for bars at proportional
;lengths towards the maximum values (e.g. higher maximum HP = longer bar) that would be placed on
;the right side of the screen.
;
;Sample inputs:
;
; Player's high byte X position (RAM $95): the length of the bar
; Coin counter: Amount filled.

incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;Don't touch these. This calculates the total length of the bar for end tiles.
	!LeftEndTile	= 0
	!RightEndTile	= 0
	
	if !Default_LeftPieces != 0
		!LeftEndTile = 1
	endif
	if !Scratchram_GraphicalBar_RightEndPiece != 0
		!RightEndTile = 1
	endif
	
	!TotalEnds = !LeftEndTile+!RightEndTile
	!TotalLength = !Default_GraphicalBarPositionExtendLeftwards_MaxMiddleLength+!TotalEnds

;Again, this is an uberasm tool test file.
main:
.ClearBarAreaWhenShorten
	print "bug here-----------------------------------$",pc
	LDX.b #(!TotalLength-1)*!StatusBarFormat
	LDA #$FC							;>Blank tile
	
	..Loop
	STA !Default_GraphicalBarPositionExtendLeftwards-((!TotalLength-1)*!StatusBarFormat),x
	DEX #!StatusBarFormat
	BPL ..Loop
.InputRatio
	LDA $0DBF|!addr						;\Quantity: coins
	STA !Scratchram_GraphicalBar_FillByteTbl		;|
	LDA #$00						;|
	STA !Scratchram_GraphicalBar_FillByteTbl+1		;/
	LDA.b #99						;\Max quantity: 99 coins (100 would wrap back at 0).
	STA !Scratchram_GraphicalBar_FillByteTbl+2		;|
	LDA #$00						;|
	STA !Scratchram_GraphicalBar_FillByteTbl+3		;/
.InputGraphicalBarAttributes
	LDA.b #!Default_LeftPieces						;\Left end normally have 3 pieces.
	STA !Scratchram_GraphicalBar_LeftEndPiece				;/
	LDA.b #!Default_MiddlePieces						;\Number of pieces in each middle byte/8x8 tile
	STA !Scratchram_GraphicalBar_MiddlePiece				;/
	LDA.b #!Default_RightPieces						;\Right end
	STA !Scratchram_GraphicalBar_RightEndPiece				;/
	LDA $95									;\length (number of middle tiles)
	CMP.b #!Default_GraphicalBarPositionExtendLeftwards_MaxMiddleLength	;|\Prevent overwriting data that is located before the status bar area.
	BCC ..LowerThanMax							;||
	LDA.b #!Default_GraphicalBarPositionExtendLeftwards_MaxMiddleLength	;|/

	..LowerThanMax
	STA !Scratchram_GraphicalBar_TempLength					;
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
	
	LDA.b #!Default_GraphicalBarPositionExtendLeftwards		;\Input rightmost tile position
	STA $00								;|
	LDA.b #!Default_GraphicalBarPositionExtendLeftwards>>8		;|
	STA $01								;|
	LDA.b #!Default_GraphicalBarPositionExtendLeftwards>>16		;|
	STA $02								;/
	
	if !StatusBar_UsingCustomProperties != 0
		LDA.b #!Default_GraphicalBarPropertiesExtendLeftwards		;\Same as above but tile properties
		STA $03								;|
		LDA.b #!Default_GraphicalBarPropertiesExtendLeftwards>>8	;|
		STA $04								;|
		LDA.b #!Default_GraphicalBarPropertiesExtendLeftwards>>16	;|
		STA $05								;/
	endif
	JSL GraphicalBarWriteToStatusBar_BarExtendLeft				;>Extend leftwards bar (modifies the starting tile to move in accordance to the length of the bar, in tiles).
	if !Default_LeftwardsBar == 0						;\Write to status bar
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUD			;|
	else									;|
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwards		;|
	endif									;/
	RTL
