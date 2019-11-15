;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;all files in the routine folder in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

;This code tests the static end tile that moves based on the length of the bar.
;TODO
;-Write blank tiles prior this routine so if the bar extends and contract, don't leave behind duplicate tiles.
;-Write a static end tile on the side of the bar that doesn't move

;0 to not write static end tiles, otherwise set to 1 (don't set to any other number.).
 !GraphicalBarExampleTest_StaticLeft = 1
 !GraphicalBarExampleTest_StaticRight = 1
;other settings:
!GraphicalBarExampleTest_StaticEnd_ExtendLeft = 0
!GraphicalBarExampleTest_Length = 30
 ;^Maximum middle length (this alone excludes the static end tiles.).
;Some tile number, properties, and positioning settings:
!GraphicalBarExampleTest_LeftSideTileNum = $00
!GraphicalBarExampleTest_LeftSideTileProps = %00111000
!GraphicalBarExampleTest_RightSideTileNum = $01
!GraphicalBarExampleTest_RightSideTileProps = %00111000
if !sa1 == 0
 !GraphicalBarExampleTest_NoExtendLeftBarPos = $7FA002
 !GraphicalBarExampleTest_NoExtendLeftBarPropsPos = $7FA003
 !GraphicalBarExampleTest_ExtendLeftBarPos = $7fA03C
 !GraphicalBarExampleTest_ExtendLeftBarPropsPos = $7fA0BD
else
 !GraphicalBarExampleTest_NoExtendLeftBarPos = $404002
 !GraphicalBarExampleTest_NoExtendLeftBarPropsPos = $404003
 !GraphicalBarExampleTest_ExtendLeftBarPos = $40403C
 !GraphicalBarExampleTest_ExtendLeftBarPropsPos = $40403C
endif

incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;This is a simple test ASM using the graphical bar.
;best tested using uberasm tool.

main:
;^These are needed so the defines relating to the graphical bars work.
.ClearTiles
	;NOTE: when length of bar is 0, the tile clearer may not
	;clear out the end tiles that are beyond the 
	LDX.b #(((!GraphicalBarExampleTest_Length+!GraphicalBarExampleTest_StaticLeft+!GraphicalBarExampleTest_StaticRight)-1)*!StatusBarFormat) ;> clear ALL tiles
	LDA #$FC
	..Loop
	if !GraphicalBarExampleTest_StaticEnd_ExtendLeft == 0
		STA.l !GraphicalBarExampleTest_NoExtendLeftBarPos-(!GraphicalBarExampleTest_StaticLeft*!StatusBarFormat),x ;>[-!GraphicalBarExampleTest_StaticLeft*!StatusBarFormat] to also remove the lefside
	else
		STA.l !GraphicalBarExampleTest_ExtendLeftBarPos-(((!GraphicalBarExampleTest_Length+!GraphicalBarExampleTest_StaticLeft)-1)*!StatusBarFormat),x
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
	BEQ .Done						;>Check if the bar is 0 length (middle), to see if the tile clearing routine clears only all the tiles that it needs to clear.
.ConvertToBar
	JSL GraphicalBarELITE_CalculateGraphicalBarPercentage		;>Get percentage
	JSL GraphicalBarELITE_RoundAwayEmptyFull
	JSL GraphicalBarELITE_DrawGraphicalBar				;>get bar values.
	JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
	if !GraphicalBarExampleTest_StaticEnd_ExtendLeft == 0
		LDA.b #!GraphicalBarExampleTest_NoExtendLeftBarPos		;\Setup address to where to draw the bar.
		STA $00								;|
		LDA.b #!GraphicalBarExampleTest_NoExtendLeftBarPos>>8		;|
		STA $01								;|
		LDA.b #!GraphicalBarExampleTest_NoExtendLeftBarPos>>16		;|
		STA $02								;/
		if !StatusBar_UsingCustomProperties != 0
			LDA.b #!GraphicalBarExampleTest_NoExtendLeftBarPropsPos		;\Same as above but properties
			STA $03								;|
			LDA.b #!GraphicalBarExampleTest_NoExtendLeftBarPropsPos>>8	;|
			STA $04								;|
			LDA.b #!GraphicalBarExampleTest_NoExtendLeftBarPropsPos>>16	;|
			STA $05								;/
			if !Default_LeftwardsBar == 0
				LDA.b #!Default_StatusBar_TilePropertiesSetting			;\Properties
			else
				LDA.b #(!Default_StatusBar_TilePropertiesSetting|(!Default_LeftwardsBar<<6))
			endif
			STA $06								;/
		endif
	else
		LDA.b #!GraphicalBarExampleTest_ExtendLeftBarPos		;\Input rightmost tile position
		STA $00								;|
		LDA.b #!GraphicalBarExampleTest_ExtendLeftBarPos>>8		;|
		STA $01								;|
		LDA.b #!GraphicalBarExampleTest_ExtendLeftBarPos>>16		;|
		STA $02								;/
		
		if !StatusBar_UsingCustomProperties != 0
			LDA.b #!GraphicalBarExampleTest_ExtendLeftBarPos		;\Same as above but tile properties
			STA $03								;|
			LDA.b #!GraphicalBarExampleTest_ExtendLeftBarPos>>8		;|
			STA $04								;|
			LDA.b #!GraphicalBarExampleTest_ExtendLeftBarPos>>16		;|
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
	LDA #!GraphicalBarExampleTest_LeftSideTileNum
	STA $07
	LDA.b #!GraphicalBarExampleTest_LeftSideTileProps
	STA $06
	if !StatusBarFormat == 1
		JSL GraphicalBarWriteToStatusBar_WriteBarStaticTileToHUDLeftside
	else
		JSL GraphicalBarWriteToStatusBar_WriteBarStaticTileToHUDLeftsideFormat2
	endif
	LDA #!GraphicalBarExampleTest_RightSideTileNum
	STA $07
	LDA.b #!GraphicalBarExampleTest_RightSideTileProps
	STA $06
	if !StatusBarFormat == 1
		JSL GraphicalBarWriteToStatusBar_WriteBarStaticTileToHUDRightside
	else
		JSL GraphicalBarWriteToStatusBar_WriteBarStaticTileToHUDRightsideFormat2
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
	.Done
	RTL