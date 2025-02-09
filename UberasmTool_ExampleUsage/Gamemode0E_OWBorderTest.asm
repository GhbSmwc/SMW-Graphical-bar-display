;Unlike the other ASM files here, this is meant to run in gamemode $0E,
;which is the overworld mode.
;
;You need Overworld Border plus (OWB+ I abbreviated it) patch in order to write tiles to the overworld
;border. Insert this file in the tool's gamemode folder and in list.txt, assign this to number $0E:
;gamemode:
;; Insert files here
;;0E		Gamemode0E_OWBorderTest.asm

incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;This measures Mario's bonus stars (not luigi).

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

.InputRatio
	LDA $0F48|!addr						;\Quantity low byte (example: Mario's bonus stars). Use RAM here.
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
	JSL GraphicalBarELITE_RoundAwayEmptyFull			;>Avoid rounding towards 0 or MaxPieces when they are not those numbers.
	
	JSL GraphicalBarELITE_DrawGraphicalBarSubtractionLoopEdition	;>get bar values.
	LDA #$02							;\Use Level-layer3 tileset
	STA $00								;/
	JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
	LDA.b #!Default_GraphicalBar_Pos_Tile_OverworldMap		;\Setup address to where to draw the bar.
	STA $00								;|
	LDA.b #!Default_GraphicalBar_Pos_Tile_OverworldMap>>8		;|
	STA $01								;|
	LDA.b #!Default_GraphicalBar_Pos_Tile_OverworldMap>>16		;|
	STA $02								;/
	LDA.b #!Default_GraphicalBar_Pos_Properties_OverworldMap	;\Same as above but properties
	STA $03								;|
	LDA.b #!Default_GraphicalBar_Pos_Properties_OverworldMap>>8	;|
	STA $04								;|
	LDA.b #!Default_GraphicalBar_Pos_Properties_OverworldMap>>16	;|
	STA $05								;/
	if !Default_LeftwardsBar == 0
		LDA.b #!Default_Overworld_TilePropertiesSetting			;\Properties change based on how much fill
	else
		LDA.b #(!Default_Overworld_TilePropertiesSetting|(!Default_LeftwardsBar<<6))
	endif
	STA $06								;/
	;Write to Overworld border (thankfully, there's only one type of data format for tile customizations
	;on the overworld border, since there is only one patch that uses the SSB-styled tile handling).
	if !Default_LeftwardsBar == 0
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUDFormat2		;>Write to status bar
	else
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwardsFormat2
	endif
	RTL