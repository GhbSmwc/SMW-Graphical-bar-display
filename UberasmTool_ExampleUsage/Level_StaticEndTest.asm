;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;all files in the routine folder in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

;This code tests the static end tile that moves based on the length of the bar.
;The length of the bar is based on what screen on the X axis the player is on.

incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"
;^These are needed so the defines relating to the graphical bars work.

;This is a simple test ASM using the graphical bar.
;best tested using uberasm tool.

main:
if !CPUMode != 0
	%invoke_sa1(mainSA1)
	RTL
	mainSA1:
endif
.ClearTiles
	;Clear out tiles. This removes leftover ghost duplicate tiles on the status bar when the bar shortens.
	LDX.b #(((!Default_GraphicalBar_MaxMiddleLength+!GraphicalBarExampleTest_StaticLeft+!GraphicalBarExampleTest_StaticRight)-1)*!StatusBarFormat) ;> clear ALL tiles
	..Loop
	
	LDA #$FC
	if !GraphicalBarExampleTest_StaticEnd_ExtendLeft == 0
		STA.l !GraphicalBarExampleTest_StaticEndsExtendRightBarPos-(!GraphicalBarExampleTest_StaticLeft*!StatusBarFormat),x ;>[-!GraphicalBarExampleTest_StaticLeft*!StatusBarFormat] to also remove the lefside
		if !StatusBar_UsingCustomProperties != 0
			LDA.b #%00111000
			STA !GraphicalBarExampleTest_StaticEndsExtendRightBarPropsPos-(!GraphicalBarExampleTest_StaticLeft*!StatusBarFormat),x
		endif
		print "Tile range (tile numbers only): $", hex(!GraphicalBarExampleTest_StaticEndsExtendRightBarPos-(!GraphicalBarExampleTest_StaticLeft*!StatusBarFormat)), " to $", hex((!GraphicalBarExampleTest_StaticEndsExtendRightBarPos-(!GraphicalBarExampleTest_StaticLeft*!StatusBarFormat))+(((!Default_GraphicalBar_MaxMiddleLength+!GraphicalBarExampleTest_StaticLeft+!GraphicalBarExampleTest_StaticRight)-1)*!StatusBarFormat))
	else
		STA.l !GraphicalBarExampleTest_StaticEndsExtendLeftBarPos-(((!Default_GraphicalBar_MaxMiddleLength+!GraphicalBarExampleTest_StaticLeft)-1)*!StatusBarFormat),x
		if !StatusBar_UsingCustomProperties != 0
			LDA.b #%00111000
			STA !GraphicalBarExampleTest_StaticEndsExtendLeftBarPropsPos-(((!Default_GraphicalBar_MaxMiddleLength+!GraphicalBarExampleTest_StaticLeft)-1)*!StatusBarFormat),x
		endif
		print "Tile range (tile numbers only): $", hex(!GraphicalBarExampleTest_StaticEndsExtendLeftBarPos-(((!Default_GraphicalBar_MaxMiddleLength+!GraphicalBarExampleTest_StaticLeft)-1))), " to $", hex(!GraphicalBarExampleTest_StaticEndsExtendLeftBarPos-(((!Default_GraphicalBar_MaxMiddleLength+!GraphicalBarExampleTest_StaticLeft)-1))+(((!GraphicalBarExampleTest_StaticLeft+!GraphicalBarExampleTest_StaticRight)-1)*!StatusBarFormat))
	endif
	DEX #!StatusBarFormat
	BPL ..Loop

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
	CMP.b #!Default_GraphicalBar_MaxMiddleLength		;\Length (capped).
	BCC +							;|
	LDA.b #!Default_GraphicalBar_MaxMiddleLength		;|
	STA !Scratchram_GraphicalBar_TempLength			;/
	+
	BEQ .Done						;>Check if the bar is 0 length (middle), to see if the tile clearing routine clears only all the tiles that it needs to clear.
.ConvertToBar
	JSL GraphicalBarELITE_CalculateGraphicalBarPercentage		;>Get percentage
	JSL GraphicalBarELITE_RoundAwayEmptyFull
	JSL GraphicalBarELITE_DrawGraphicalBarSubtractionLoopEdition	;>get bar values.
	STZ $00								;>Use Level-layer3 tileset
	JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
	if !GraphicalBarExampleTest_StaticEnd_ExtendLeft == 0
		LDA.b #!GraphicalBarExampleTest_StaticEndsExtendRightBarPos		;\Setup address to where to draw the bar.
		STA $00								;|
		LDA.b #!GraphicalBarExampleTest_StaticEndsExtendRightBarPos>>8		;|
		STA $01								;|
		LDA.b #!GraphicalBarExampleTest_StaticEndsExtendRightBarPos>>16		;|
		STA $02								;/
		if !StatusBar_UsingCustomProperties != 0
			LDA.b #!GraphicalBarExampleTest_StaticEndsExtendRightBarPropsPos		;\Same as above but properties
			STA $03								;|
			LDA.b #!GraphicalBarExampleTest_StaticEndsExtendRightBarPropsPos>>8	;|
			STA $04								;|
			LDA.b #!GraphicalBarExampleTest_StaticEndsExtendRightBarPropsPos>>16	;|
			STA $05								;/
			if !Default_LeftwardsBar == 0
				LDA.b #!Default_StatusBar_TilePropertiesSetting			;\Properties
			else
				LDA.b #(!Default_StatusBar_TilePropertiesSetting|(!Default_LeftwardsBar<<6))
			endif
			STA $06								;/
		endif
	else
		LDA.b #!GraphicalBarExampleTest_StaticEndsExtendLeftBarPos		;\Input rightmost tile position
		STA $00								;|
		LDA.b #!GraphicalBarExampleTest_StaticEndsExtendLeftBarPos>>8		;|
		STA $01								;|
		LDA.b #!GraphicalBarExampleTest_StaticEndsExtendLeftBarPos>>16		;|
		STA $02								;/
		
		if !StatusBar_UsingCustomProperties != 0
			LDA.b #!GraphicalBarExampleTest_StaticEndsExtendLeftBarPropsPos		;\Same as above but tile properties
			STA $03								;|
			LDA.b #!GraphicalBarExampleTest_StaticEndsExtendLeftBarPropsPos>>8	;|
			STA $04								;|
			LDA.b #!GraphicalBarExampleTest_StaticEndsExtendLeftBarPropsPos>>16	;|
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
	;Write static end tile that moves based on the length of the bar:
	if !GraphicalBarExampleTest_StaticLeft != 0
		LDA !Scratchram_GraphicalBar_TempLength
		BEQ .Done
		LDA.b #!GraphicalBarExampleTest_LeftSideTileNum
		STA $07
		LDA.b #!GraphicalBarExampleTest_LeftSideTileProps
		STA $06
		if !StatusBarFormat == 1
			JSL GraphicalBarWriteToStatusBar_WriteBarStaticTileToHUDLeftside
		else
			JSL GraphicalBarWriteToStatusBar_WriteBarStaticTileToHUDLeftsideFormat2
		endif
	endif
	if !GraphicalBarExampleTest_StaticRight
		LDA.b #!GraphicalBarExampleTest_RightSideTileNum
		STA $07
		LDA.b #!GraphicalBarExampleTest_RightSideTileProps
		STA $06
		if !StatusBarFormat == 1
			JSL GraphicalBarWriteToStatusBar_WriteBarStaticTileToHUDRightside
		else
			JSL GraphicalBarWriteToStatusBar_WriteBarStaticTileToHUDRightsideFormat2
		endif
	endif
	.Done
		RTL