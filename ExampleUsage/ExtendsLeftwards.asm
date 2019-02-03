;This is a demonstration on how to have a leftwards-filling bar that extends LEFTWARDS as you increase
;length of the bar.
;
;Sample inputs:
;
; Player's high byte X position (RAM $95): the length of the bar
; Coin counter: Amount filled.

incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;Again, this is an uberasm tool test file.
main:
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
	LDA.b #!Default_LeftPieces				;\Left end normally have 3 pieces.
	STA !Scratchram_GraphicalBar_LeftEndPiece		;/
	LDA.b #!Default_MiddlePieces				;\Number of pieces in each middle byte/8x8 tile
	STA !Scratchram_GraphicalBar_MiddlePiece		;/
	LDA.b #!Default_RightPieces				;\Right end
	STA !Scratchram_GraphicalBar_RightEndPiece		;/
	LDA $95							;\length (number of middle tiles)
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
	
	;Here, the "origin" or "starting tile" is always the left side, even if the bar is x-flipped.
	;As the bar extends leftwards, this starting tile must move left. This can be done by taking
	;the desired position of the last tile (the rightmost tile; right side), subtract it by the
	;number of tiles -1, and the difference is the starting tile:
	;
	;BeginningTilePos = DesiredLastTilePos - (NumberOfTiles - 1)
	;
	;However, if you are using a super status bar format or overworld border plus (2 bytes adjacent
	;for every tile), the formula is this, since you have to move over 2 bytes for every tile:
	;
	;BeginningTilePos = DesiredLastTilePos - ((NumberOfTiles - 1)*2)
	print "bug              ",pc
	JSL GraphicalBarWriteToStatusBar_CountNumberOfTiles		;>(NumberOfTiles - 1)
	TXA
	if !StatusBarFormat == $02
		ASL							;>*2
	endif
	STA $00								;\24-bit subtrahend
	STZ $01								;|
	STZ $02								;/
	LDA.b #!Default_GraphicalBarPositionExtendLeftwards		;\Do 24-bit subtraction
	SEC								;|
	SBC $00								;|
	STA $00								;|
	LDA.b #!Default_GraphicalBarPositionExtendLeftwards>>8		;|
	SBC $01								;|
	STA $01								;|
	LDA.b #!Default_GraphicalBarPositionExtendLeftwards>>16		;|
	SBC $02								;|
	STA $02								;/
	
	if !StatusBar_UsingCustomProperties != 0
		;Same as above, but for tile properties
		JSL GraphicalBarWriteToStatusBar_CountNumberOfTiles		;>(NumberOfTiles - 1)
		TXA
		if !StatusBarFormat == $02
			ASL							;>*2
		endif
		STA $03								;\24-bit subtrahend
		STZ $04								;|
		STZ $05								;/
		LDA.b #!Default_GraphicalBarPropertiesExtendLeftwards		;\Do 24-bit subtraction
		SEC								;|
		SBC $03								;|
		STA $03								;|
		LDA.b #!Default_GraphicalBarPropertiesExtendLeftwards>>8	;|
		SBC $04								;|
		STA $04								;|
		LDA.b #!Default_GraphicalBarPropertiesExtendLeftwards>>16	;|
		SBC $05								;|
		STA $05								;/
	endif
	if !Default_LeftwardsBar == 0						;\Write to status bar
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUD			;|
	else									;|
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwards		;|
	endif									;/
	RTL
