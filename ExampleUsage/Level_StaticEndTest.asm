;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;all files in the routine folder in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

;This code tests the static end tile that moves based on the length of the bar.
;TODO
;-Write blank tiles prior this routine so if the bar extends and contract, don't leave behind duplicate tiles.
;-Write a static end tile on the side of the bar that doesn't move

!GraphicalBarExampleTest_StaticEnd_ExtendLeft = 1


incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;This is a simple test ASM using the graphical bar.
;best tested using uberasm tool.

main:
;^These are needed so the defines relating to the graphical bars work.

.InputRatio
	LDA $14							;\Frame counter as quantity
	STA !Scratchram_GraphicalBar_FillByteTbl		;/
	LDA #$00						;\High byte of above. Should your value here is 8-bit or only 1 byte long,
	STA !Scratchram_GraphicalBar_FillByteTbl+1		;/use [LDA #$00 : STA !Scratchram_GraphicalBar_FillByteTbl+1].
	LDA #$FF						;\Max quantity low byte (example: max HP). Can be a fixed value (#$) or adjustable RAM in-game.
	STA !Scratchram_GraphicalBar_FillByteTbl+2		;/
	LDA #$00						;\High byte of above, same format as <Value_high_byte>, so do the same
	STA !Scratchram_GraphicalBar_FillByteTbl+3		;/as that if your value is 8-bit.
.InputGraphicalBarAttributes
	LDA.b #$00						;\No end tiles.
	STA !Scratchram_GraphicalBar_LeftEndPiece		;|
	STA !Scratchram_GraphicalBar_RightEndPiece		;/
	LDA.b #!Default_MiddlePieces				;\Number of pieces in each middle byte/8x8 tile
	STA !Scratchram_GraphicalBar_MiddlePiece		;/
	LDA.b $95						;\Screen number = number of middle tiles.
	STA !Scratchram_GraphicalBar_TempLength			;/
.ConvertToBar
	JSL GraphicalBarELITE_CalculateGraphicalBarPercentage		;>Get percentage
	JSL GraphicalBarELITE_RoundAwayEmptyFull
	JSL GraphicalBarELITE_DrawGraphicalBar				;>get bar values.
	JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
	if !GraphicalBarExampleTest_StaticEnd_ExtendLeft == 0
		LDA.b #!Default_GraphicalBarPosition				;\Setup address to where to draw the bar.
		STA $00								;|
		LDA.b #!Default_GraphicalBarPosition>>8				;|
		STA $01								;|
		LDA.b #!Default_GraphicalBarPosition>>16			;|
		STA $02								;/
		if !StatusBar_UsingCustomProperties != 0
			LDA.b #!Default_GraphicalBarProperties				;\Same as above but properties
			STA $03								;|
			LDA.b #!Default_GraphicalBarProperties>>8			;|
			STA $04								;|
			LDA.b #!Default_GraphicalBarProperties>>16			;|
			STA $05								;/
			if !Default_LeftwardsBar == 0
				LDA.b #!Default_StatusBar_TilePropertiesSetting			;\Properties
			else
				LDA.b #(!Default_StatusBar_TilePropertiesSetting|(!Default_LeftwardsBar<<6))
			endif
			STA $06								;/
		endif
	else
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
			if !Default_LeftwardsBar == 0
				LDA.b #!Default_StatusBar_TilePropertiesSetting			;\Properties
			else
				LDA.b #(!Default_StatusBar_TilePropertiesSetting|(!Default_LeftwardsBar<<6))
			endif
			STA $06								;/
		endif
	endif
	;Handle leftwards
	if !GraphicalBarExampleTest_StaticEnd_ExtendLeft != 0
		if !StatusBarFormat = $01
			JSL GraphicalBarWriteToStatusBar_BarExtendLeft				;>Extend leftwards bar (modifies the starting tile to move in accordance to the length of the bar, in tiles).
		else
			JSL GraphicalBarWriteToStatusBar_BarExtendLeftFormat2			;>Extend leftwards bar (modifies the starting tile to move in accordance to the length of the bar, in tiles).
		endif
	endif
	;Write static end tile that moves based on the length of the bar:
	if !GraphicalBarExampleTest_StaticEnd_ExtendLeft == 0
		JSL GraphicalBarWriteToStatusBar_CountNumberOfTiles
		INX							;After last middle tile.
		if !StatusBarFormat != $01
			TXA
			ASL
			TAY
		else
			TXY
		endif
		LDA #$00 : STA [$00],y						;>Tile number for rightside
			LDA.b #!Default_StatusBar_TilePropertiesSetting : STA [$03],y	;>Tile properties 
	else
		REP #$20
		DEC $00			;\Go to a location 1 tile to the left from the leftside tile.
		if !StatusBarFormat != 0
			DEC $00			;>DEC twice due to that you have to move 2 bytes over to move over by 1 tile.
		endif
		if !StatusBar_UsingCustomProperties != 0
			DEC $03			;/
			if !StatusBarFormat
				DEC $03
			endif
		endif
		SEP #$20
		LDA #$00 : STA [$00]		;>Tile number for leftside
		if !StatusBar_UsingCustomProperties != 0
			LDA #$00 : STA [$03]		;>Tile properties
		endif
		REP #$20
		INC $00			;\Restore
		if !StatusBarFormat != 0
			INC $00
		endif
		if !StatusBar_UsingCustomProperties != 0
			INC $03			;/
			if !StatusBarFormat != 0
				INC $03
			endif
		endif
		SEP #$20
	endif
	;Write to status bar
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