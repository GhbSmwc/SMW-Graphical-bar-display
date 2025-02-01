;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Convert bar fill amounts to tiles, indirect addressing table edition,
;Unlike ConvertBarFillAmountToTiles, which $00 represent which presets
;of tile number tables to use, this lets you manually set the tile number
;table to use outside this subroutine.
;Input:
; -!Scratchram_GraphicalBar_FillByteTbl to (!Scratchram_GraphicalBar_FillByteTbl+NumbOfTiles)-1:
;  fill amount array to convert to tile numbers.
; -$00 to $02: 24-bit address representing the (starting) location of the table containing the left end
; -$03 to $05: 24-bit address representing the (starting) location of the table containing the middles
; -$06 to $08: 24-bit address representing the (starting) location of the table containing the right end
;Output:
; -!Scratchram_GraphicalBar_FillByteTbl to (!Scratchram_GraphicalBar_FillByteTbl+NumbOfTiles)-1
;  the converted-to-tile-numbers array
;Destroyed
;-$09: Used to track the middle tiles if all of them are done.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Convert tile code following:
		?ConvertBarFillAmountToTilesIndirectAddressTable:
			;PHB						;>Preserve bank (so that table indexing work properly)
			;PHK						;>push current bank
			;PLB						;>pull out as regular bank
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
				LDA [$00],y
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
					STA $09						;/
				else
					REP #$20
					LDA !Scratchram_GraphicalBar_TempLength
					AND #$00FF
					STA $09
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
					LDA [$03],y
					STA !Scratchram_GraphicalBar_FillByteTbl,x	;/
			
					?...Next
						INX
						if !Setting_GraphicalBar_IndexSize != 0
							REP #$20
						endif
						DEC $09
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
				LDA [$06],y
				STA !Scratchram_GraphicalBar_FillByteTbl,x
		;Done
			?.Done
				SEP #$30					;>Just in case
				;PLB						;>Pull bank
				RTL