;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;all files in the routine folder in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

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
	!TotalLength = !Default_GraphicalBar_MaxMiddleLength+!TotalEnds

;Again, this is an uberasm tool test file.
main:
if !sa1 != 0
	LDA.b #mainSA1				; \ Put the address
	STA $3180				;  | to jump in
	LDA.b #mainSA1>>8			;  | $3180 - $3182.
	STA $3181				;  |
	LDA.b #mainSA1>>16			;  |
	STA $3182				; /
	JSR $1E80				; Invoke SA-1 and wait to finish.
	RTL
	mainSA1:
endif
.ClearBarAreaWhenShorten
	;This code removes leftover "ghost tiles" artifact when the bar shortens after being extended.
	LDX.b #(!TotalLength-1)*!StatusBarFormat
	LDA #$FC							;>Blank tile
	
	..Loop
	STA !Default_GraphicalBar_Pos_Tile_ExtendLeftwards-((!TotalLength-1)*!StatusBarFormat),x
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
	LDA $95									;\length (number of middle tiles), based on what screen boundary on the X-axis the player is in (Length = Floor(MarioXPos/256))
	CMP.b #!Default_GraphicalBar_MaxMiddleLength				;|\Prevent overwriting data that is located before the status bar area.
	BCC ..LowerThanMax							;||
	LDA.b #!Default_GraphicalBar_MaxMiddleLength				;|/

	..LowerThanMax
	STA !Scratchram_GraphicalBar_TempLength					;
.ConvertToBar
	JSL GraphicalBarELITE_CalculateGraphicalBarPercentage		;>Get percentage
	JSL GraphicalBarELITE_RoundAwayEmptyFull
	JSL GraphicalBarELITE_DrawGraphicalBarSubtractionLoopEdition	;>get bar values.
	STZ $00								;>Use Level-layer3 tileset
	JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
	
	LDA.b #!Default_GraphicalBar_Pos_Tile_ExtendLeftwards		;\Input rightmost tile position
	STA $00								;|
	LDA.b #!Default_GraphicalBar_Pos_Tile_ExtendLeftwards>>8		;|
	STA $01								;|
	LDA.b #!Default_GraphicalBar_Pos_Tile_ExtendLeftwards>>16		;|
	STA $02								;/
	
	if !StatusBar_UsingCustomProperties != 0
		LDA.b #!Default_GraphicalBar_Pos_Properties_ExtendLeftwards	;\Same as above but tile properties
		STA $03								;|
		LDA.b #!Default_GraphicalBar_Pos_Properties_ExtendLeftwards>>8	;|
		STA $04								;|
		LDA.b #!Default_GraphicalBar_Pos_Properties_ExtendLeftwards>>16	;|
		STA $05								;/
		if !Default_LeftwardsBar == 0
			LDA.b #!Default_StatusBar_TilePropertiesSetting			;\Properties
		else
			LDA.b #(!Default_StatusBar_TilePropertiesSetting|(!Default_LeftwardsBar<<6))
		endif
		STA $06								;/
	endif
	if !StatusBarFormat = $01
		JSL GraphicalBarWriteToStatusBar_BarExtendLeft				;>Extend leftwards bar (modifies the starting tile to move in accordance to the length of the bar, in tiles).
	else
		JSL GraphicalBarWriteToStatusBar_BarExtendLeftFormat2			;>Extend leftwards bar (modifies the starting tile to move in accordance to the length of the bar, in tiles).
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
