;Don't touch
	incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
	incsrc "../GraphicalBarDefines/SpriteOAMSettings.asm"
	incsrc "../GraphicalBarDefines/StatusBarSettings.asm"
	;Handle bar direction
		!GraphicalBar_OAMXFlip = 0
		!GraphicalBar_OAMYFlip = 0
		if !PatchSprite_Direction == 1
			!GraphicalBar_OAMXFlip = 1
		elseif !PatchSprite_Direction == 3
			!GraphicalBar_OAMYFlip = 1
		endif
end:
	.SpriteGraphicalBar
		REP #$20
		LDA $94							;\Player's X position "progress"
		SEC							;|
		SBC #$0008						;>because the minimum player's X pos is #$0008
		BPL +							;|\Prevent underflow.
		LDA #$0000						;||
		+							;|/
		STA !Scratchram_GraphicalBar_FillByteTbl		;/
		SEP #$20
		LDA.b #!Default_MiddleLength				;\Input length (middle)
		STA !Scratchram_GraphicalBar_TempLength			;/
		LDA #$E0						;\The maximum X position the player can be (right edge of the level)
		STA !Scratchram_GraphicalBar_FillByteTbl+2		;|
		LDA $5E							;|
		DEC							;|
		STA !Scratchram_GraphicalBar_FillByteTbl+3		;/
		LDA.b #!Default_LeftPieces				;\Input amount of pieces in each of the 3 types of sections.
		STA !Scratchram_GraphicalBar_LeftEndPiece		;|
		LDA.b #!Default_MiddlePieces				;|
		STA !Scratchram_GraphicalBar_MiddlePiece		;|
		LDA.b #!Default_RightPieces				;|
		STA !Scratchram_GraphicalBar_RightEndPiece		;/
		JSL GraphicalBarELITE_CalculateGraphicalBarPercentage		;>Get percentage
		JSL GraphicalBarELITE_DrawGraphicalBarSubtractionLoopEdition	;>get bar values.
	
		REP #$20
		LDA.w #..LeftEnd
		STA $00
		LDA.w #..Middle
		STA $03
		LDA.w #..RightEnd
		STA $06
		SEP #$20
		LDA.b #..LeftEnd>>16
		STA $02
		STA $05
		STA $08
		JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTilesIndirectAddressTable
		REP #$20
		LDA #$00F8
		STA $00
		LDA #$0000
		STA $02
		SEP #$20
		
		JSL GraphicalBarWriteToStatusBar_CountNumberOfTiles
		INX
		STX $04				;\Store number of tiles in $04.
		STZ $05				;/
		if !PatchSprite_Direction < 2
			LDA.b #!GraphicalBar_OAMXFlip
		else
			LDA.b #!GraphicalBar_OAMYFlip
		endif
		STA $06
		LDA.b #(!GraphicalBar_OAMYFlip<<7)+(!GraphicalBar_OAMXFlip<<6)+(3<<4)+(!PatchSprite_Palette<<1)+(!PageNum<<0)
		STA $07
		if !PatchSprite_Direction < 2
			JSL GraphicalBarWriteToOAM_DrawOamGraphicalBarHoriz
		else
			JSL GraphicalBarWriteToOAM_DrawOamGraphicalBarVert
		endif
		RTL
		
		
		..LeftEnd
			db $85,$86,$87,$95
		..Middle
			db $96,$97,$8A,$8B,$9A,$9B,$C0,$C1,$D0
		..RightEnd
			db $D1,$E0,$E1,$F0