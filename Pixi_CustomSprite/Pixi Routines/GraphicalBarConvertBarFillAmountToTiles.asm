;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Convert fill amount in bar to tile numbers. NOTE: does not work with double-bar.
;Scroll down for the double-bar version.
;
;Also note: Because the first byte to be inserted is always the jump destination
;when you call this subroutine, the tables have to be at the bottom else your game
;will glitch/crash and the fact you cannot jump to a specific area in a routines file on pixi
;(always points to the first byte inserted)
;
;Note to self about the gamemode values:
; $0D-$0E covers overworld load and overworld.
; $13-$14 covers level load and level.
;
;Input:
; - !Scratchram_GraphicalBar_FillByteTbl to (!Scratchram_GraphicalBar_FillByteTbl+NumbOfTiles)-1:
;   fill amount array to convert to tile numbers.
; - $00: What set of graphics to use. Under default setting and code:
; -- #$00 = Level, layer 3
; -- #$01 = Level, sprite
; -- #$02 = Overworld, layer 3
;   You can add more sets of bar tiles by adding a new table as well as adding code
;   to use the new table.
; - !Scratchram_GraphicalBar_LeftEndPiece: Number of pieces in left byte (0-255), also
;   the maximum amount of fill for this byte itself. If 0, it's not included in table.
; - !Scratchram_GraphicalBar_MiddlePiece: Same as above but each middle byte.
; - !Scratchram_GraphicalBar_RightEndPiece: Same as above but for right end.
; - !Scratchram_GraphicalBar_TempLength: The length of the bar (only counts
;   middle bytes)
;Output:
; - !Scratchram_GraphicalBar_FillByteTbl to !Scratchram_GraphicalBar_FillByteTbl+x:
;   converted to tile numbers.
;Overwritten/Destroyed:
; - $01: Needed to tell if all the middle tiles are done
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Convert tile code following:
		?ConvertBarFillAmountToTiles:
			PHB						;>Preserve bank (so that table indexing work properly)
			PHK						;>push current bank
			PLB						;>pull out as regular bank
			if !Setting_GraphicalBar_IndexSize == 0
				LDX #$00
			else
				REP #$10								;>16-bit XY
				LDX #$0000								;>The index for what byte tile position to write.
			endif
		;Left end
			?.LeftEndTranslate
				LDA !Scratchram_GraphicalBar_LeftEndPiece	;\can only be either 0 or the correct number of pieces listed in the table.
				BEQ ?.MiddleTranslate				;/
				if !Setting_GraphicalBar_IndexSize == 0
					LDA !Scratchram_GraphicalBar_FillByteTbl	;\Y = amount filled byte
					TAY						;/
				else
					REP #$20
					LDA !Scratchram_GraphicalBar_FillByteTbl
					AND #$00FF
					TAY
					SEP #$20
				endif
				LDA $00
				BEQ ?..LevelLayer3
				CMP #$01
				BEQ ?..LevelSprite
			
			?..OverworldLayer3
				LDA ?GraphicalBar_LeftEnd8x8s_Ow_L3,y
				BRA ?..WriteTable
			?..LevelLayer3
				LDA ?GraphicalBar_LeftEnd8x8s_Lvl_L3,y				;\Convert byte to tile number byte
				BRA ?..WriteTable
			?..LevelSprite
				LDA ?GraphicalBar_LeftEnd8x8s_Lvl_Spr,y
			?..WriteTable
				STA !Scratchram_GraphicalBar_FillByteTbl		;/
				INX							;>next tile byte
		;Middle
			?.MiddleTranslate
				LDA !Scratchram_GraphicalBar_MiddlePiece	;\check if middle exist.
				BEQ ?.RightEndTranslate				;|
				LDA !Scratchram_GraphicalBar_TempLength		;|
				BEQ ?.RightEndTranslate				;/
	
				if !Setting_GraphicalBar_IndexSize == 0
					LDA !Scratchram_GraphicalBar_TempLength		;\Number of middle tiles to convert
					STA $01						;/
				else
					REP #$20
					LDA !Scratchram_GraphicalBar_TempLength
					AND #$00FF
					STA $01
				endif
				?..Loop
					if !Setting_GraphicalBar_IndexSize == 0
						LDA !Scratchram_GraphicalBar_FillByteTbl,x	;>Y = the fill amount
						TAY
					else
						LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\amount of filled, indexed
						AND #$00FF					;|
						TAY						;/
						SEP #$20
					endif
					LDA $00
					BEQ ?...LevelLayer3
					CMP #$01
					BEQ ?...LevelSprite
					
					?...OverworldLayer3
						LDA ?GraphicalBar_Middle8x8s_Ow_L3,y
						BRA ?...WriteTable
					?...LevelLayer3
						LDA ?GraphicalBar_Middle8x8s_Lvl_L3,y			;\amount filled as tile graphics
						BRA ?...WriteTable
					?...LevelSprite
						LDA ?GraphicalBar_Middle8x8s_Lvl_Spr,y
					?...WriteTable
						STA !Scratchram_GraphicalBar_FillByteTbl,x	;/
			
					?...Next
						INX
						if !Setting_GraphicalBar_IndexSize != 0
							REP #$20
						endif
						DEC $01
						BNE ?..Loop
				SEP #$20
		;Right end
			?.RightEndTranslate
				LDA !Scratchram_GraphicalBar_RightEndPiece
				BEQ ?.Done
				if !Setting_GraphicalBar_IndexSize == 0
					LDA !Scratchram_GraphicalBar_FillByteTbl,x
					TAY
				else
					REP #$20
					LDA !Scratchram_GraphicalBar_FillByteTbl,x
					AND #$00FF
					TAY
					SEP #$20
				endif
				LDA $00
				BEQ ?..LevelLayer3
				CMP #$01
				BEQ ?..LevelSprite
			
				?..Overworld
					LDA ?GraphicalBar_RightEnd8x8s_Ow_L3,y
					BRA ?..WriteTable
				?..LevelLayer3
					LDA ?GraphicalBar_RightEnd8x8s_Lvl_L3,y
					BRA ?..WriteTable
				?..LevelSprite
					LDA ?GraphicalBar_RightEnd8x8s_Lvl_Spr,y
				?..WriteTable
					STA !Scratchram_GraphicalBar_FillByteTbl,x
		;Done
			?.Done
				SEP #$30					;>Just in case
				PLB						;>Pull bank
				RTL
	;These are tile numbers. Each number, starting from the
	;left represent each tile of pieces ordered from empty
	;(0) to full (in this default number of pieces, it is 3
	;for both ends and 8 for middles).
	
	;Tiles will glitch out if the number of pieces in the
	;corresponding type of bar part (left middle and right)
	;does not equal to the number of tile numbers +1 here,
	;when they use invalid indexing that would points to
	;bytes beyond the table.
	;This is for level:
		;Layer 3
			?GraphicalBar_LeftEnd8x8s_Lvl_L3:
			;Left end fill amount tile numbers:
			db $36		;>Fill amount/index: $00
			db $37		;>Fill amount/index: $01
			db $38		;>Fill amount/index: $02
			db $39		;>Fill amount/index: $03
			?GraphicalBar_Middle8x8s_Lvl_L3:
			;Middle fill amount tile numbers
			db $55		;>Fill amount/index: $00
			db $56		;>Fill amount/index: $01
			db $57		;>Fill amount/index: $02
			db $58		;>Fill amount/index: $03
			db $59		;>Fill amount/index: $04
			db $65		;>Fill amount/index: $05
			db $66		;>Fill amount/index: $06
			db $67		;>Fill amount/index: $07
			db $68		;>Fill amount/index: $08
			?GraphicalBar_RightEnd8x8s_Lvl_L3:
			;Right end fill amount tile numbers:
			db $50		;>Fill amount/index: $00
			db $51		;>Fill amount/index: $01
			db $52		;>Fill amount/index: $02
			db $53		;>Fill amount/index: $03
		;Sprite
			?GraphicalBar_LeftEnd8x8s_Lvl_Spr:
			;Left end fill amount tile numbers:
			db $85		;>Fill amount/index: $00
			db $86		;>Fill amount/index: $01
			db $87		;>Fill amount/index: $02
			db $95		;>Fill amount/index: $03
			?GraphicalBar_Middle8x8s_Lvl_Spr:
			;Middle fill amount tile numbers
			db $96		;>Fill amount/index: $00
			db $97		;>Fill amount/index: $01
			db $8A		;>Fill amount/index: $02
			db $8B		;>Fill amount/index: $03
			db $9A		;>Fill amount/index: $04
			db $9B		;>Fill amount/index: $05
			db $C0		;>Fill amount/index: $06
			db $C1		;>Fill amount/index: $07
			db $D0		;>Fill amount/index: $08
			?GraphicalBar_RightEnd8x8s_Lvl_Spr:
			;Right end fill amount tile numbers:
			db $D1		;>Fill amount/index: $00
			db $E0		;>Fill amount/index: $01
			db $E1		;>Fill amount/index: $02
			db $F0		;>Fill amount/index: $03
	;These here are the same as above but intended for overworld border.
		?GraphicalBar_LeftEnd8x8s_Ow_L3:
		db $80		;>Fill amount/index: $00
		db $81		;>Fill amount/index: $01
		db $82		;>Fill amount/index: $02
		db $83		;>Fill amount/index: $03
		?GraphicalBar_Middle8x8s_Ow_L3:
		db $84		;>Fill amount/index: $00
		db $85		;>Fill amount/index: $01
		db $86		;>Fill amount/index: $02
		db $87		;>Fill amount/index: $03
		db $88		;>Fill amount/index: $04
		db $89		;>Fill amount/index: $05
		db $8A		;>Fill amount/index: $06
		db $8B		;>Fill amount/index: $07
		db $8C		;>Fill amount/index: $08
		?GraphicalBar_RightEnd8x8s_Ow_L3:
		db $8D		;>Fill amount/index: $00
		db $8E		;>Fill amount/index: $01
		db $8F		;>Fill amount/index: $02
		db $90		;>Fill amount/index: $03