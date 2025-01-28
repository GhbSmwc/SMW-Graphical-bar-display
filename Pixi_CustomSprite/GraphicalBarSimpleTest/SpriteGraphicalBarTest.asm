incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/SpriteOAMSettings.asm"

;I strongly recommend using the "No more sprite tile limits" patch:
;https://www.smwcentral.net/?p=section&a=details&id=24816 as you might
;encounter tile issues (tile priority and disappearing tiles).

;Extra bytes note:
;EXB1: X position [signed], Relative to "body of sprite" (the egg sprite).
;EXB2: Y position [signed], same as above.
;EXB3: Length (number of middle tiles).
;EXB4: Direction (depends on define !Sprite_Graphical_Bar_HorizontalOrVertical ):
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
	JSR DrawSprite
	PLB
	RTS
	
DrawSprite:
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
		%GraphicalBarCalculateGraphicalBarPercentage()				;>Get percentage
		%GraphicalBarRoundAwayEmptyFull()
		%GraphicalBarDrawGraphicalBarSubtractionLoopEdition()			;>get bar values.
		LDA #$01							;\Use Level-sprite tileset
		STA $00								;/
		%GraphicalBarConvertBarFillAmountToTiles()				;>Convert tiles.
		PLX								;>Restore sprite slot index
		
		;Beyond this point is whats different unlike the layer 3 version.
	.DrawSprite
		%GetDrawInfo()	;Y = OAM index, $00 = sprite scrn X pos, $01 = sprite scrn Y pos
		;Note to self:
		;%GetDrawInfo() uses a destroy-return-address to "double-out" the subroutine upon returning, by "pulling" (PLA 3 times) before the RTL.
		;This results in using the 2nd-last return address instead of the normal behavior or JSL->RTL
		;Pushing and then not pulling afterwards before calling %GetDrawInfo() can cause the game to crash due to now other data is in the stack
		;to destroy the return address. Give thanks to JamesD28.
		;
		;We start at $0300/$0460 (slots 64-127, latter half of the array) because I have a feeling that $0200-$02FF and $0420-$045F is used by something else.
		;
		;Because we are writing at increasing OAM indexes (INY #4 instead of DEY #4), the first tile written will be written “on top” (in front) of the next
		;tile when they overlap and every subsequent tile will be placed underneath (behind). So if you have the code draw the bar first (such as this example
		;shown here), and then the body of the sprite afterwards, the bar will be written on top of the body of the sprite. However, this assumes you are using
		;the "No more sprite tile limits", as sprite tiles slot are inconsistent in vanilla SMW.
	..GraphicalBar
		%GraphicalBarCountNumberOfTiles()	;\Get number of tiles of the graphical bar ($04-$05)
		INX					;|
		STX $04					;|
		STZ $05					;/
		
		LDX $15E9|!addr			;>Sprite index
		
		LDA $00				;\X position ($02)
		CLC				;|
		ADC !extra_byte_1,x		;|
		STA $02				;/
		LDA $01				;\Y position ($03)
		CLC				;|
		ADC !extra_byte_2,x		;|
		STA $03				;/
		
		LDA !extra_byte_4,x		;\Set direction ($06)
		STA $06				;/
		
		LDA #!Sprite_Graphical_Bar_Prop	;\Properties ($07)
		STA $07				;/
		
		if !Sprite_Graphical_Bar_HorizontalOrVertical == 0
			%GraphicalBarDrawSpriteGraphicalBarHoriz()
		else
			%GraphicalBarDrawSpriteGraphicalBarVert()
		endif
		;^After DrawSpriteGraphicalBarHoriz or DrawSpriteGraphicalBarVert,
		; Y is already the correct next OAM index after the last bar tile written.
	..DrawSpriteBody
		;This draws the main body of the sprite, so this is not part of the bar.
		LDA $00				;\X pos
		STA $0300|!addr,y		;/
		LDA $01				;\Y pos
		STA $0301|!addr,y		;/
		LDA #$00			;\Tile number
		STA $0302|!addr,y		;/
		LDA.b #%00010001		;\Properties
		STA $0303|!addr,y		;/
		TYA				;\Convert OAM index to slot number (SXBit_Indexing = SlotNumber/4).
		LSR #2				;|(we only need to preserve (PHY) and restore (PLY) the Y index if we need it back afterwards)
		TAY				;/
		LDA $0460|!addr,y		;\Manually set the size bit to 16x16
		ORA.b #%00000010		;|
		STA $0460|!addr,y		;/

	..Done
		LDX $15E9|!addr			;>Restore sprite slot
		LDY #$FF			;\Finish OAM
		LDA $04				;|>Total number of tiles (main body plus bar) to write, minus 1.
		;DEC				;|We could do INC and then DEC, but that can be simplified to just removing both as they both cancel each other out.
		JSL $01B7B3|!BankB		;/
		RTS