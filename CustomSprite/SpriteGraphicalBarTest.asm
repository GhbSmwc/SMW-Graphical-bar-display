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
	
	%GetDrawInfo()	;Y = OAM index, $00 = sprite scrn X pos, $01 = sprite scrn Y pos
	;Note to self:
	;Y index SHOULD be a multiple of 4
	;$0200 = Xpos (bits 0-7), bit 8 is in $0420, but the index for that is each 1 rather than 4.
	;$0201 = Ypos (8-bit).
	;$0202 = Tile number
	;$0203 = Tile Properties (YXPPCCCT)
	;$0204 to $03FF = repeat of above every 4 bytes.
	;
	;We start at $0300 because I have a feeling that $0200-$02FF is used by something else.
	LDA $00
	STA $0300|!addr,y
	LDA $01
	STA $0301|!addr,y
	LDA !Scratchram_GraphicalBar_FillByteTbl
	STA $0302|!addr,y
	LDA.b #%00110001
	STA $0303|!addr,y
	STA $030F|!addr,y
	LDY #$00
	LDA #$00
	JSL $01B7B3|!BankB
	
	PLX
	RTS