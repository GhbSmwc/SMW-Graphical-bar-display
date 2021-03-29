incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"
incsrc "../SharedSub_Defines/SubroutineDefs.asm"

print "MAIN ",pc
	PHB : PHK : PLB
	JSR SpriteCode
	PLB
	RTL

SpriteCode:
	JSR Graphics
	RTS
	
	
Graphics:
.InputRatio
	print "custom sprite graphical bar test" $, pc
	LDA $14							;\Quantity
	STA !Scratchram_GraphicalBar_FillByteTbl		;/
	LDA #$00						;\High byte quantity
	STA !Scratchram_GraphicalBar_FillByteTbl+1		;/
	LDA #$FF						;\Max quantity
	STA !Scratchram_GraphicalBar_FillByteTbl+2		;/
	LDA #$00						;\High byte of max quantity
	STA !Scratchram_GraphicalBar_FillByteTbl+3		;/
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
	PHX
	JSL !CalculateGraphicalBarPercentage				;>Get percentage
	JSL !RoundAwayEmptyFull
	JSL !DrawGraphicalBar						;>get bar values.
	LDA #$01							;\Use Level-sprite tileset
	STA $00								;/
	JSL !ConvertBarFillAmountToTiles				;>Convert tiles.
	PLX
	RTS