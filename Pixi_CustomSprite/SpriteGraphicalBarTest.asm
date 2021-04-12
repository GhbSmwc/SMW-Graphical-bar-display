incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/SpriteOAMSettings.asm"
incsrc "../SharedSub_Defines/SubroutineDefs.asm"

;Extra bytes note:
;EXB1: X position
;EXB2: Y position
;EXB3: Length (number of middle tiles)
;EXB4: Direction:
; For horizontal:
;  $00 = Left to right
;  $01 = Right to left (YXPPCCCT's X bit set)
; For Vertical:
;  $00 = Bottom to top
;  $01 = Top to bottom (YXPPCCCT's Y bit set)

!Sprite_Graphical_Bar_Prop			= %00110001	;>YXPPCCCT
!Sprite_Graphical_Bar_HorizontalOrVertical	= 0		;>0 = horizontal, 1 = vertical

;Note: A maximum of 16 OAM slots can be used (so up to 16 tiles can be drawn),
;any higher results the 17th and beyond not being drawn. Also, there is a maximum
;of 127 ($7F) pixel displacement from the sprite origin, which also prevent writing
;such tiles (normally to prevent tiles wrapping the screen, OAM technically have a
;9-bit X position).

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
		LDA.b #!Default_PixiSprite_LeftEndPieces		;\Left end normally have 3 pieces.
		STA !Scratchram_GraphicalBar_LeftEndPiece		;/
		LDA.b #!Default_PixiSprite_MiddlePieces			;\Number of pieces in each middle byte/8x8 tile
		STA !Scratchram_GraphicalBar_MiddlePiece		;/
		LDA.b #!Default_PixiSprite_RightEndPieces		;\Right end
		STA !Scratchram_GraphicalBar_RightEndPiece		;/
		LDA !extra_byte_3,x					;\length (number of middle tiles)
		STA !Scratchram_GraphicalBar_TempLength			;/
	.ConvertToBar
		PHX								;>Preserve sprite slot index
		JSL !CalculateGraphicalBarPercentage				;>Get percentage
		JSL !RoundAwayEmptyFull
		JSL !DrawGraphicalBar						;>get bar values.
		LDA #$01							;\Use Level-sprite tileset
		STA $00								;/
		JSL !ConvertBarFillAmountToTiles				;>Convert tiles.
		PLX								;>Restore sprite slot index
		
		;Beyond this point is whats different unlike the layer 3 version.
		
		%GetDrawInfo()	;Y = OAM index, $00 = sprite scrn X pos, $01 = sprite scrn Y pos
		;Note to self:
		;%GetDrawInfo() uses a destroy-return-address to "double-out" the subroutine upon returning, by "pulling" (PLA 3 times) before the RTL.
		;This results in using the 2nd-last return address instead of the normal behavior or JSL->RTL
		;Pushing and then not pulling afterwards before calling %GetDrawInfo() can cause the game to crash due to now other data is in the stack
		;to destroy the return address. Give thanks to JamesD28.
		;
		;We start at $0300 because I have a feeling that $0200-$02FF is used by something else.

		JSL !CountNumberOfTiles		;\Get number of tiles of the graphical bar
		INX				;|
		STX $04				;|
		STZ $05				;/
		
		LDX $15E9|!addr			;>Sprite index
		
		LDA $00				;\X position
		CLC				;|
		ADC !extra_byte_1,x		;|
		STA $02				;/
		LDA $01				;\Y position
		CLC				;|
		ADC !extra_byte_2,x		;|
		STA $03				;/
		
		LDA !extra_byte_4,x		;\Set direction
		STA $06				;/
		
		LDA #!Sprite_Graphical_Bar_Prop	;\Properties
		STA $07				;/
		
		if !Sprite_Graphical_Bar_HorizontalOrVertical == 0
			JSL !DrawSpriteGraphicalBarHoriz
		else
			JSL !DrawSpriteGraphicalBarVert
		endif

		..Done
			LDX $15E9|!addr			;>Restore sprite slot
			LDY #$00			;\Finish OAM
			LDA $04				;|>Total number of tiles to write, minus 1.
			DEC				;|
			JSL $01B7B3|!BankB		;/
			RTS