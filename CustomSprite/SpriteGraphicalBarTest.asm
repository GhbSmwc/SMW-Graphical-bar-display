incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"
incsrc "../SharedSub_Defines/SubroutineDefs.asm"

;Extra bytes note:
;EXB1:
; $00 = Horizontal graphical bar - fill rightwards.
; $01 = Horizontal graphical bar - fill leftwards (YXPPCCCT's X bit set to 1).
;
;

print "MAIN ",pc
	PHB : PHK : PLB
	JSR SpriteCode
	PLB
	RTL

SpriteCode:
	PHB : PHK : PLB
	JSR DrawSpriteGraphicalBar
	PLB
	RTS
	
DrawSpriteGraphicalBar:
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
	PLX
	
	%GetDrawInfo()	;Y = OAM index, $00 = sprite scrn X pos, $01 = sprite scrn Y pos
	;Note to self:
	;%GetDrawInfo() uses a destroy-return-address to "double-out" the subroutine upon returning, by "pulling" before the RTL.
	;This results in using the 2nd-last return address (pull, then RTL uses the previous return address, not the current one)
	;Pushing and not pulling afterwards before calling %GetDrawInfo() can cause the game to crash due to now other data is in the stack
	;to destroy the return address. Give thanks to JamesD28.
	;
	;Y index SHOULD be a multiple of 4
	;$0200 = Xpos (bits 0-7), bit 8 is in $0420, but the index for that is each 1 rather than 4.
	;$0201 = Ypos (8-bit).
	;$0202 = Tile number
	;$0203 = Tile Properties (YXPPCCCT)
	;$0204 to $03FF = repeat of above every 4 bytes.
	;
	;We start at $0300 because I have a feeling that $0200-$02FF is used by something else.

	JSL !CountNumberOfTiles		;>Have this OUTSIDE the loop and have the information of how many tiles in $02...
	INX
	STX $02				;>Store number of tiles in $02.
	LDA $00
	STA $03				;>Store the initial tile X pos in $03 (this makes writing each tile in each 8 pixels to the right)
	LDX #$00
	..OAMLoop
		LDA $03						;\X pos
		STA $0300|!addr,y				;/
		
		LDA $01						;\Y pos
		STA $0301|!addr,y				;/
		
		LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Tile number
		STA $0302|!addr,y				;/
		
		...HandleXFlip
			PHX
			LDX $15E9|!addr
			LDA !extra_byte_1,x
			PLX
			BEQ ....NoFlip
			....Flip
				LDA.b #%01110001
				BRA ....Write
			....NoFlip
				LDA.b #%00110001
			....Write
				STA $0303|!addr,y		;>YXPPCCCT
	..Next
		PHX
		LDX $15E9|!addr
		LDA !extra_byte_1,x
		PLX
		BEQ ...NoFlip
		...Flip
			LDA $03			;\Move tile X position by 8 pixels
			SEC			;|
			SBC #$08		;/
			BRA ...Write
		...NoFlip
			LDA $03			;\Move tile X position by 8 pixels
			CLC			;|
			ADC #$08		;/
		...Write
			STA $03			;
		INY			;\Next OAM slot
		INY			;|
		INY			;|
		INY			;/
		INX			;>Next graphical bar slot
		CPX $02			;>...so it doesn't need to execute the subroutine repeatedly.
		BCC ..OAMLoop
	..Done
	
	LDX $15E9|!addr			;>Restore sprite slot
	LDY #$00			;\Finish OAM
	LDA $02				;|>Number of tiles to write, minus 1.
	DEC				;|
	JSL $01B7B3|!BankB		;/
	RTS