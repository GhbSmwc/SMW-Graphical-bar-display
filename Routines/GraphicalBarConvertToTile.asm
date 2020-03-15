incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;Main routines to call are:
;-ConvertBarFillAmountToTiles
;-ConvertBarFillAmountToTilesDoubleBar
;-ConvertBarFillAmountToTilesEdgeOverMultipleTiles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Convert fill amount in bar to tile numbers. NOTE: does not work with double-bar.
;Scroll down for the double-bar version. This routine checks the gamemode address
;$0100 to determine which tile table to use for adoption for overworld border plus
;patch.
;
;Note to self about the gamemode values:
; $0D-$0E covers overworld load and overworld.
; $13-$14 covers level load and level.
;
;Input:
; -!Scratchram_GraphicalBar_LeftEndPiece: Number of pieces in left byte (0-255), also
;  the maximum amount of fill for this byte itself. If 0, it's not included in table.
; -!Scratchram_GraphicalBar_MiddlePiece: Same as above but each middle byte.
; -!Scratchram_GraphicalBar_RightEndPiece: Same as above but for right end.
; -!Scratchram_GraphicalBar_TempLength: The length of the bar (only counts
;   middle bytes)
;Output:
; -!Scratchram_GraphicalBar_FillByteTbl to !Scratchram_GraphicalBar_FillByteTbl+x:
;  converted to tile numbers.
;Overwritten/Destroyed:
; -$00 Needed for fast checking if overworld or not (better than
;  checking $0100 every time using BCC/BCS):
; --#$00 for level
; --#$01 for overworld.
;  I deliberately make it use scratch RAM in the case you have
;  even more tables (3+ states of graphics) for each graphic.
;  Feel free to edit the code though.
; -$01 Needed to convert the middle tiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
		GraphicalBar_LeftEnd8x8s:
		;Left end fill amount tile numbers:
		db $36		;>Fill amount/index: $00
		db $37		;>Fill amount/index: $01
		db $38		;>Fill amount/index: $02
		db $39		;>Fill amount/index: $03
		GraphicalBar_Middle8x8s:
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
		GraphicalBar_RightEnd8x8s:
		;Right end fill amount tile numbers:
		db $50		;>Fill amount/index: $00
		db $51		;>Fill amount/index: $01
		db $52		;>Fill amount/index: $02
		db $53		;>Fill amount/index: $03
	;These here are the same as above but intended for overworld border.
		GraphicalBar_LeftEnd8x8s_Ow:
		db $80		;>Fill amount/index: $00
		db $81		;>Fill amount/index: $01
		db $82		;>Fill amount/index: $02
		db $83		;>Fill amount/index: $03
		GraphicalBar_Middle8x8s_Ow:
		db $84		;>Fill amount/index: $00
		db $85		;>Fill amount/index: $01
		db $86		;>Fill amount/index: $02
		db $87		;>Fill amount/index: $03
		db $88		;>Fill amount/index: $04
		db $89		;>Fill amount/index: $05
		db $8A		;>Fill amount/index: $06
		db $8B		;>Fill amount/index: $07
		db $8C		;>Fill amount/index: $08
		GraphicalBar_RightEnd8x8s_Ow:
		db $8D		;>Fill amount/index: $00
		db $8E		;>Fill amount/index: $01
		db $8F		;>Fill amount/index: $02
		db $90		;>Fill amount/index: $03
	;Convert tile code following:
		ConvertBarFillAmountToTiles:
			PHB						;>Preserve bank (so that table indexing work properly)
			PHK						;>push current bank
			PLB						;>pull out as regular bank
		;Level or overworld?
			.WhatTableToUse
				STZ $00						;>Default to "Level"
				LDA $0100|!addr					;\If gamemode value is #$0F or higher, that is level
				CMP #$0F					;|
				BCS ..Level					;/
			
			..Overworld
				INC $00						;>Otherwise assume its overworld.
			
			..Level
				if !Setting_GraphicalBar_IndexSize == 0
					LDX #$00
				else
					REP #$10								;>16-bit XY
					LDX #$0000								;>The index for what byte tile position to write.
				endif
		;Left end
			.LeftEndTranslate
				LDA !Scratchram_GraphicalBar_LeftEndPiece	;\can only be either 0 or the correct number of pieces listed in the table.
				BEQ .MiddleTranslate				;/
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
				BEQ ..Level
			
			..Overworld
				LDA GraphicalBar_LeftEnd8x8s_Ow,y
				BRA ..WriteTable
			
			..Level
				LDA GraphicalBar_LeftEnd8x8s,y				;\Convert byte to tile number byte
			
			..WriteTable
				STA !Scratchram_GraphicalBar_FillByteTbl		;/
				INX							;>next tile byte
		;Middle
			.MiddleTranslate
				LDA !Scratchram_GraphicalBar_MiddlePiece	;\check if middle exist.
				BEQ .RightEndTranslate				;|
				LDA !Scratchram_GraphicalBar_TempLength		;|
				BEQ .RightEndTranslate				;/
	
				if !Setting_GraphicalBar_IndexSize == 0
					LDA !Scratchram_GraphicalBar_TempLength		;\Number of middle tiles to convert
					STA $01						;/
				else
					REP #$20
					LDA !Scratchram_GraphicalBar_TempLength
					AND #$00FF
					STA $01
				endif
				..Loop
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
					BEQ ...Level
			
					...Overworld
						LDA GraphicalBar_Middle8x8s_Ow,y
						BRA ...WriteTable
					
					...Level
						LDA GraphicalBar_Middle8x8s,y			;\amount filled as tile graphics
					
					...WriteTable
						STA !Scratchram_GraphicalBar_FillByteTbl,x	;/
			
					...Next
						INX
						if !Setting_GraphicalBar_IndexSize != 0
							REP #$20
						endif
						DEC $01
						BNE ..Loop
				SEP #$20
		;Right end
			.RightEndTranslate
				LDA !Scratchram_GraphicalBar_RightEndPiece
				BEQ .Done
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
				BEQ ..Level
			
				..Overworld
					LDA GraphicalBar_RightEnd8x8s_Ow,y
					BRA ..WriteTable
				
				..Level
					LDA GraphicalBar_RightEnd8x8s,y
				
				..WriteTable
					STA !Scratchram_GraphicalBar_FillByteTbl,x
		;Done
			.Done
			SEP #$30					;>Just in case
			PLB						;>Pull bank
			RTL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Convert fill amount in bar to tile numbers, double-bar edition
;Inputs and outputs the same as above.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;How this works in relation of using the table (regarding each 8x8 byte section):
;1) Amount of FirstFill multiply by the number of pieces +1 (example: LeftEndFirstFill*4).
;   The value you have now is currently the index number for each fill value for FirstFill
;   while having SecondFill's index as "empty". Basically, this is the " leftmost column"
;   of the table.
;
;2) Load up the corresponding byte/tile (so if it's left end on the first bar, it must be
;   left end as well for the other bar.) of the second bar. You should have the value for
;   SecondFill. Then add the value by the value you have in step 1 to get the "row number"
;   for getting SecondFill's index number. Remember that SecondBar is located before FirstBar
;   in memory address.
;
;So basically the index formula is this:
;
; IndexNumber = (FirstFill * (Pieces + 1)) + SecondFill
;
;Note that IndexNumber is 8-bit (only used for what tile number for the table, not the byte
;table containing the amount of fill), because having that much fill will eat up large
;amounts of 8x8 graphics exponentially (this also means the fill in the bar will also be 8-bit).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;These (below) are tables to convert whats in !Scratchram_GraphicalBar_FillByteTbl to tile numbers.
;The format here is basically a combination represented as "(FirstFill;SecondFill)" where
;"FirstFill" the amount of fill that overlaps "SecondFill". The amount of fill in SecondFill
;ALWAYS measures from the leftmost of the bar, NOT from where FirstFill ends at. So example:
;an 8x8 byte is "(2;5)", the 2 represent 2 pieces filled with FirstFill, while the 5 is the
;amount of fill for SecondFill. It should look like in-game as 3 pieces (5-2=3) of SecondFill.
;Should SecondFill be less than FirstFill (like (2;1)), it should reuse the same tile as if
;they're equal, this means that the table will have duplicated tile numbers, and increases by 1
;each time FirstFill increases.

;The table here are sorted with increasing SecondFill, and once it gets full, it will increase
;FirstFill by 1 and resets SecondFill, and repeats starting back at increasing SecondFill.
;
;The number of bytes in these tables are (NumbPieces+1)^2. Make sure you have the exact size
;to avoid glitchy tiles.

;Be very careful not to have very large number of pieces, as the number of combinations will
;increase exponentially, potentially won't even be able to fit in the VRAM storage area with
;that many 8x8 tiles.

;Be extra careful with the bank borders, 2 bar fill data tables are stored with
;!Scratchram_GraphicalBar_FillByteTbl being SecondFill and
;!Scratchram_GraphicalBar_FillByteTbl+!Setting_GraphicalBar_SecondFillByteTableOffset being FirstFill. Bank border is basically
;the line between $**FFFF and $XXFFFF, since the index addressing only handle 16-bit processing.

;If you are editing the number of pieces in any bar parts here, the number of pieces must equal
;to (NumberOfPieces+1)^2 number of values in the table here.
GraphicalBar_LeftEnd8x8sDoubleBar:
db $29,$2A,$2B,$2C    ;>(0;0), (0;1), (0;2), (0;3). When FirstFill = 0, and SecondFill is any value
db $2D,$2D,$2F,$35    ;>(1;0), (1;1), (1;2), (1;3). When FirstFill = 1, and SecondFill is any value
db $36,$36,$36,$37    ;>(2;0), (2;1), (2;2), (2;3). When FirstFill = 2, and SecondFill is any value
db $38,$38,$38,$38    ;>(3;0), (3;1), (3;2), (3;3). When FirstFill = 3, and SecondFill is any value
GraphicalBar_Middle8x8sDoubleBar:
db $39,$45,$46,$47,$48,$49,$4B,$4C,$4D ;>(0;0), (0;1), (0;2), (0;3), (0;4), (0;5), (0;6), (0;7), (0;8)
db $4E,$4E,$4F,$50,$51,$52,$53,$54,$55 ;>(1;0), (1;1), (1;2), (1;3), (1;4), (1;5), (1;6), (1;7), (1;8)
db $56,$56,$56,$57,$58,$59,$5A,$5B,$5C ;>(2;0), (2;1), (2;2), (2;3), (2;4), (2;5), (2;6), (2;7), (2;8)
db $5D,$5D,$5D,$5D,$5E,$5F,$60,$61,$62 ;>(3;0), (3;1), (3;2), (3;3), (3;4), (3;5), (3;6), (3;7), (3;8)
db $63,$63,$63,$63,$63,$65,$66,$67,$68 ;>(4;0), (4;1), (4;2), (4;3), (4;4), (4;5), (4;6), (4;7), (4;8)
db $69,$69,$69,$69,$69,$69,$6A,$6B,$6C ;>(5;0), (5;1), (5;2), (5;3), (5;4), (5;5), (5;6), (5;7), (5;8)
db $6D,$6D,$6D,$6D,$6D,$6D,$6D,$6E,$6F ;>(6;0), (6;1), (6;2), (6;3), (6;4), (6;5), (6;6), (6;7), (6;8)
db $71,$71,$71,$71,$71,$71,$71,$71,$72 ;>(7;0), (7;1), (7;2), (7;3), (7;4), (7;5), (7;6), (7;7), (7;8)
db $73,$73,$73,$73,$73,$73,$73,$73,$73 ;>(8;0), (8;1), (8;2), (8;3), (8;4), (8;5), (8;6), (8;7), (8;8)
GraphicalBar_RightEnd8x8sDoubleBar:
db $74,$75,$79,$7A    ;>(0;0), (0;1), (0;2), (0;3)
db $7B,$7B,$7C,$7D    ;>(1;0), (1;1), (1;2), (1;3)
db $7E,$7E,$7E,$7F    ;>(2;0), (2;1), (2;2), (2;3)
db $80,$80,$80,$80    ;>(3;0), (3;1), (3;2), (3;3)

;^Just realized, this is called a "two-dimensional array".

	ConvertBarFillAmountToTilesDoubleBar:
	PHB									;>Save bank
	PHK									;\Switch bank
	PLB									;/
	if !Setting_GraphicalBar_IndexSize == 0
		LDX #$00
	else
		REP #$10								;>16-bit XY
		LDX #$0000								;>The index for what byte tile position to write.
	endif
	.LeftEndTranslate
	LDA !Scratchram_GraphicalBar_LeftEndPiece				;\Check if left end exist
	BEQ .MiddleTranslate							;/
	if !sa1 == 0
		INC												;>Pieces + 1
		STA $4202											;>(Pieces + 1) times...
		LDA !Scratchram_GraphicalBar_FillByteTbl+!Setting_GraphicalBar_SecondFillByteTableOffset	;\...FirstFill (remember Commutative property means same result regardless of order)
		STA $4203											;/
		JSR WaitCalculation										;>Wait 12 cycles in total (8 is minimum needed)
		LDA $4216											;>Load product (low byte only)
	else
		LDA #$00											;\Multiply mode
		STA $2250											;/
		LDA !Scratchram_GraphicalBar_LeftEndPiece							;\Pieces + 1
		INC												;/
		STA $2251											;\(Pieces + 1) times...
		STZ $2252											;/
		LDA !Scratchram_GraphicalBar_FillByteTbl+!Setting_GraphicalBar_SecondFillByteTableOffset	;\...FirstFill (remember Commutative property means same result regardless of order)
		STA $2253											;|
		STZ $2254											;/
		NOP												;\Wait 5 cycles until calculation is done
		BRA $00												;/
		LDA $2306											;>Load product (low byte only)
	endif
	CLC									;\Add by SecondFill
	ADC !Scratchram_GraphicalBar_FillByteTbl				;/
	if !Setting_GraphicalBar_IndexSize != 0
		REP #$20								;\Rid the high byte
		AND #$00FF								;|
		SEP #$20								;/
	endif
	TAY									;>Transfer #$00XX fill value to y
	LDA GraphicalBar_LeftEnd8x8sDoubleBar,y					;\Convert left end
	STA !Scratchram_GraphicalBar_FillByteTbl				;/
	INX									;>next tile

	.MiddleTranslate

	LDA !Scratchram_GraphicalBar_MiddlePiece	;\check if middle exist.
	BEQ .RightEndTranslate				;|
	LDA !Scratchram_GraphicalBar_TempLength		;|
	BEQ .RightEndTranslate				;/
	
	LDA !Scratchram_GraphicalBar_TempLength
	STA $00
	
	..Loop
	if !sa1 == 0
		LDA !Scratchram_GraphicalBar_MiddlePiece							;\Pieces + 1
		INC												;/
		STA $4202											;>(Pieces + 1) times...
		LDA !Scratchram_GraphicalBar_FillByteTbl+!Setting_GraphicalBar_SecondFillByteTableOffset,x	;\...FirstFill (remember Commutative property means same result reguardless of order)
		STA $4203											;/(+1 because it starts on the first middle tile, after the left end)
		JSR WaitCalculation										;>Wait 12 cycles in total (8 is minimum needed)
		LDA $4216											;>Load product (low byte only)
	else
		LDA #$00											;\Multiply mode
		STA $2250											;/
		LDA !Scratchram_GraphicalBar_MiddlePiece							;\Pieces + 1
		INC												;/
		STA $2251											;\(Pieces + 1) times...
		STZ $2252											;/
		LDA !Scratchram_GraphicalBar_FillByteTbl+!Setting_GraphicalBar_SecondFillByteTableOffset,x	;\...FirstFill (remember Commutative property means same result reguardless of order)
		STA $2253											;|
		STZ $2254											;/
		NOP												;\Wait 5 cycles until calculation is done
		BRA $00												;/
		LDA $2306											;>Load product (low byte only)
	endif
	CLC													;\Add by SecondFill
	ADC !Scratchram_GraphicalBar_FillByteTbl,x								;/
	if !Setting_GraphicalBar_IndexSize != 0
		REP #$20								;\Rid the high byte
		AND #$00FF								;|
		SEP #$20								;/
	endif
	TAY									;>Transfer #$00XX fill value to y
	LDA GraphicalBar_Middle8x8sDoubleBar,y					;\Convert middle tile
	STA !Scratchram_GraphicalBar_FillByteTbl,x				;/
	
	...Next
	INX						;>Next byte or 8x8 tile
	DEC $00						;\loop until all middles done
	BNE ..Loop					;/
	
	
	.RightEndTranslate
	LDA !Scratchram_GraphicalBar_RightEndPiece
	BEQ .Done
	if !sa1 == 0
		LDA !Scratchram_GraphicalBar_RightEndPiece
		INC												;>Pieces + 1
		STA $4202											;>(Pieces + 1) times...
		LDA !Scratchram_GraphicalBar_FillByteTbl+!Setting_GraphicalBar_SecondFillByteTableOffset,x	;\...FirstFill (remember Commutative property means same result reguardless of order)
		STA $4203											;/
		JSR WaitCalculation										;>Wait 12 cycles in total (8 is minimum needed)
		LDA $4216											;>Load product (low byte only)
	else
		LDA #$00											;\Multiply mode
		STA $2250											;/
		LDA !Scratchram_GraphicalBar_RightEndPiece							;\Pieces + 1
		INC												;/
		STA $2251											;\(Pieces + 1) times...
		STZ $2252											;/
		LDA !Scratchram_GraphicalBar_FillByteTbl+!Setting_GraphicalBar_SecondFillByteTableOffset,x	;\...FirstFill (remember Commutative property means same result reguardless of order)
		STA $2253											;|
		STZ $2254											;/
		NOP												;\Wait 5 cycles until calculation is done
		BRA $00												;/
		LDA $2306											;>Load product (low byte only)
	endif
	CLC									;\Add by SecondFill
	ADC !Scratchram_GraphicalBar_FillByteTbl,x				;/
	if !Setting_GraphicalBar_IndexSize != 0
		REP #$20								;\Rid the high byte
		AND #$00FF								;|
		SEP #$20								;/
	endif
	TAY									;>Transfer #$00XX fill value to y
	LDA GraphicalBar_RightEnd8x8sDoubleBar,y						;\Convert middle tile
	STA !Scratchram_GraphicalBar_FillByteTbl,x				;/
	
	.Done
	SEP #$10								;>8-bit XY
	PLB									;>Restore bank
	RTL
	
	if !sa1 == 0
		WaitCalculation:	;>The register to perform multiplication and division takes 8/16 cycles to complete.
		RTS
	endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Handle fill amount when they cross borders.
;
;This code is designed to handle display multiple tiles having "partial-filled"
;amounts by taking one tile, and determine what tile to use based on the next tile.
;ideal for outlined edges or slated edge fills.
;
;Works like this: Consider the fill amount: [$03, $01]
;Now the "head" of the fill is on the second byte, to accomodate this, the first byte
;will take that amount ($03) and add by the next tile ($01, which results $03 + $01 = $04).
;
;This by default, uses the slanted fill edge and not the outlined edge.
;
;Input:
; -!Scratchram_GraphicalBar_LeftEndPiece: Number of pieces in left byte (0-255), also
;  the maximum amount of fill for this byte itself. If 0, it's not included in table.
; -!Scratchram_GraphicalBar_MiddlePiece: Same as above but each middle byte.
; -!Scratchram_GraphicalBar_RightEndPiece: Same as above but for right end.
; -!Scratchram_GraphicalBar_TempLength: The length of the bar (only counts
;   middle bytes)
;Output:
; -!Scratchram_GraphicalBar_FillByteTbl to !Scratchram_GraphicalBar_FillByteTbl+x:
;  converted to tile numbers.
;Overwritten/Destroyed:
; -$00 Needed for fast checking if overworld or not (better than
;  checking $0100 every time using BCC/BCS):
; --#$00 for level
; --#$01 for overworld.
;  I deliberately make it use scratch RAM in the case you have
;  even more tables (3+ states of graphics) for each graphic.
;  Feel free to edit the code though.
; -$01 Needed to convert the middle tiles
; -$02 Needed to avoid checking a tile that is after the last tile.
;
;Note: There isn't a double bar seperate graphics for this one
;because IT TAKES A HUGE AMOUNT OF SPACE on the graphics.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;This is for level:
		GraphicalBar_LeftEnd8x8sFillEdgeCross:
		db $36		;>Index: $00 (Filled amount: $00 out of $03)
		db $37		;>Index: $01 (Filled amount: $01 out of $03)
		db $38		;>Index: $02 (Filled amount: $02 out of $03)
		db $39		;>Index: $03 (Filled amount: $03 out of $03)
		db $45		;>Index: $04 ($03 + $01 = Index: $04)
		db $46		;>Index: $05 ($03 + $02 = Index: $05)
		db $47		;>Index: $06 ($03 + $03 = Index: $06)
		db $47		;>Index: $07 ($03 + $04 = Index: $07)
		db $47		;>Index: $08 ($03 + $05 = Index: $08)
		db $47		;>Index: $09 ($03 + $06 = Index: $09)
		db $47		;>Index: $0A ($03 + $07 = Index: $0A)
		db $47		;>Index: $0B ($03 + $08 = Index: $0B)
		GraphicalBar_Middle8x8sFillEdgeCross:
		db $4B		;>Index: $00 (Filled amount: $00 out of $08)
		db $4C		;>Index: $01 (Filled amount: $01 out of $08)
		db $4D		;>Index: $02 (Filled amount: $02 out of $08)
		db $4E		;>Index: $03 (Filled amount: $03 out of $08)
		db $4F		;>Index: $04 (Filled amount: $04 out of $08)
		db $50		;>Index: $05 (Filled amount: $05 out of $08)
		db $51		;>Index: $06 (Filled amount: $06 out of $08)
		db $52		;>Index: $07 (Filled amount: $07 out of $08)
		db $53		;>Index: $08 (Filled amount: $08 out of $08)
		db $54		;>Index: $09 ($08 + $01 = Index: $09)
		db $55		;>Index: $0A ($08 + $02 = Index: $0A)
		db $56		;>Index: $0B ($08 + $03 = Index: $0B)
		db $56		;>Index: $0C ($08 + $04 = Index: $0C)
		db $56		;>Index: $0D ($08 + $05 = Index: $0D)
		db $56		;>Index: $0E ($08 + $06 = Index: $0E)
		db $56		;>Index: $0F ($08 + $07 = Index: $0F)
		db $56		;>Index: $10 ($08 + $08 = Index: $10) Maximum value of next tile ($08 of next middle tile vs $03 of right end)
		GraphicalBar_RightEnd8x8sFillEdgeCross:
		db $57		;>Index: $00 (Filled amount: $00 out of $06)
		db $58		;>Index: $01 (Filled amount: $01 out of $06)
		db $59		;>Index: $02 (Filled amount: $02 out of $06)
		db $5A		;>Index: $03 (Filled amount: $03 out of $06)
		db $5B		;>Index: $04 (Filled amount: $04 out of $06)
		db $5C		;>Index: $05 (Filled amount: $05 out of $06)
		db $5D		;>Index: $06 (Filled amount: $06 out of $06)
	;These here are the same as above but intended for overworld border.
		GraphicalBar_LeftEnd8x8sFillEdgeCross_Ow:
		db $36		;>Index: $00 (Filled amount: $00 out of $03)
		db $37		;>Index: $01 (Filled amount: $01 out of $03)
		db $38		;>Index: $02 (Filled amount: $02 out of $03)
		db $39		;>Index: $03 (Filled amount: $03 out of $03)
		db $45		;>Index: $04 ($03 + $01 = Index: $04)
		db $46		;>Index: $05 ($03 + $02 = Index: $05)
		db $47		;>Index: $06 ($03 + $03 = Index: $06)
		db $47		;>Index: $07 ($03 + $04 = Index: $07)
		db $47		;>Index: $08 ($03 + $05 = Index: $08)
		db $47		;>Index: $09 ($03 + $06 = Index: $09)
		db $47		;>Index: $0A ($03 + $07 = Index: $0A)
		db $47		;>Index: $0B ($03 + $08 = Index: $0B)
		GraphicalBar_Middle8x8sFillEdgeCross_Ow:
		db $4B		;>Index: $00 (Filled amount: $00 out of $08)
		db $4C		;>Index: $01 (Filled amount: $01 out of $08)
		db $4D		;>Index: $02 (Filled amount: $02 out of $08)
		db $4E		;>Index: $03 (Filled amount: $03 out of $08)
		db $4F		;>Index: $04 (Filled amount: $04 out of $08)
		db $50		;>Index: $05 (Filled amount: $05 out of $08)
		db $51		;>Index: $06 (Filled amount: $06 out of $08)
		db $52		;>Index: $07 (Filled amount: $07 out of $08)
		db $53		;>Index: $08 (Filled amount: $08 out of $08)
		db $54		;>Index: $09 ($08 + $01 = Index: $09)
		db $55		;>Index: $0A ($08 + $02 = Index: $0A)
		db $56		;>Index: $0B ($08 + $03 = Index: $0B)
		db $56		;>Index: $0C ($08 + $04 = Index: $0C)
		db $56		;>Index: $0D ($08 + $05 = Index: $0D)
		db $56		;>Index: $0E ($08 + $06 = Index: $0E)
		db $56		;>Index: $0F ($08 + $07 = Index: $0F)
		db $56		;>Index: $10 ($08 + $08 = Index: $10) Maximum value of next tile ($08 of next middle tile vs $03 of right end)
		GraphicalBar_RightEnd8x8sFillEdgeCross_Ow:
		db $57		;>Index: $00 (Filled amount: $00 out of $06)
		db $58		;>Index: $01 (Filled amount: $01 out of $06)
		db $59		;>Index: $02 (Filled amount: $02 out of $06)
		db $5A		;>Index: $03 (Filled amount: $03 out of $06)
		db $5B		;>Index: $04 (Filled amount: $04 out of $06)
		db $5C		;>Index: $05 (Filled amount: $05 out of $06)
		db $5D		;>Index: $06 (Filled amount: $06 out of $06)
	;Convert tile code following:
		ConvertBarFillAmountToTilesEdgeOverMultipleTiles:
			PHB						;>Preserve bank (so that table indexing work properly)
			PHK						;>push current bank
			PLB						;>pull out as regular bank
			;Obtain the maximum index so that when being on the last tile, and checks the "next" tile won't use
			;an invalid value (done if the bar ends with left end or middle tile):
				JSL CountNumberOfTiles
				TXA
				STA $02
		;Level or overworld?
			.WhatTableToUse
				STZ $00						;>Default to "Level"
				LDA $0100|!addr					;\If gamemode value is #$0F or higher, that is level
				CMP #$0F					;|
				BCS ..Level					;/
			
				..Overworld
					INC $00						;>Otherwise assume its overworld.
			
				..Level
					if !Setting_GraphicalBar_IndexSize == 0
						LDX #$00
					else
						REP #$10								;>16-bit XY
						LDX #$0000								;>The index for what byte tile position to write.
					endif
		;Left end
			.LeftEndTranslate
				LDA !Scratchram_GraphicalBar_LeftEndPiece	;\can only be either 0 or the correct number of pieces listed in the table.
				BEQ .MiddleTranslate				;/
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
				BEQ ..Level
				
				..Overworld
					...CheckNextTileForOutline
						INX							;>Next tile
						CPX $02							;\If next tile is nonexistent (past the last tile),
						BEQ ....Valid						;|treat as if you shouldn't add with it.
						BCS ....Invalid						;/
						
						....Valid
							LDA !Scratchram_GraphicalBar_FillByteTbl,x		;>Next tile's fill amount...
							DEX							;>Back to current tile
							CLC							;\...Add with current tile's amount
							ADC !Scratchram_GraphicalBar_FillByteTbl,x		;/
							TAY							;>The combined index into Y.
							BRA ....LoadTileNumber
						....Invalid
							DEX						;>Back to current tile
							LDA !Scratchram_GraphicalBar_FillByteTbl,x	;>Load current tile without adding by an invalid value
							TAY
						....LoadTileNumber
							LDA GraphicalBar_LeftEnd8x8sFillEdgeCross_Ow,y
							BRA ..WriteTable
			
				..Level
					...CheckNextTileForOutline
						INX							;>Next tile
						CPX $02							;\If next tile is nonexistent (past the last tile),
						BEQ ....Valid						;|treat as if you shouldn't add with it.
						BCS ....Invalid						;/
						
						....Valid
							LDA !Scratchram_GraphicalBar_FillByteTbl,x		;>Next tile's fill amount...
							DEX							;>Back to current tile
							CLC							;\...Add with current tile's amount
							ADC !Scratchram_GraphicalBar_FillByteTbl,x		;/
							TAY							;>The combined index into Y.
							BRA ....LoadTileNumber
						....Invalid
							DEX						;>Back to current tile
							LDA !Scratchram_GraphicalBar_FillByteTbl,x	;>Load current tile without adding by an invalid value
							TAY
						....LoadTileNumber
							LDA GraphicalBar_LeftEnd8x8sFillEdgeCross,y
							BRA ..WriteTable
				..WriteTable
				STA !Scratchram_GraphicalBar_FillByteTbl		;/
				INX							;>next tile byte
		;Middle
			.MiddleTranslate
				LDA !Scratchram_GraphicalBar_MiddlePiece	;\check if middle exist.
				BEQ .RightEndTranslate				;|
				LDA !Scratchram_GraphicalBar_TempLength		;|
				BEQ .RightEndTranslate				;/
	
				if !Setting_GraphicalBar_IndexSize == 0
					LDA !Scratchram_GraphicalBar_TempLength		;\Number of middle tiles to convert
					STA $01						;/
				else
					REP #$20
					LDA !Scratchram_GraphicalBar_TempLength
					AND #$00FF
					STA $01
				endif
				..Loop
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
					BEQ ...Level
				
					...Overworld
						....CheckNextTileForOutline
						INX							;>Next tile
						CPX $02							;\If next tile is nonexistent (past the last tile),
						BEQ .....Valid						;|treat as if you shouldn't add with it.
						BCS .....Invalid						;/
						
						.....Valid
							LDA !Scratchram_GraphicalBar_FillByteTbl,x		;>Next tile's fill amount...
							DEX							;>Back to current tile
							CLC							;\...Add with current tile's amount
							ADC !Scratchram_GraphicalBar_FillByteTbl,x		;/
							TAY							;>The combined index into Y.
							BRA .....LoadTileNumber
						.....Invalid
							DEX						;>Back to current tile
							LDA !Scratchram_GraphicalBar_FillByteTbl,x	;>Load current tile without adding by an invalid value
							TAY
						.....LoadTileNumber
							LDA GraphicalBar_Middle8x8sFillEdgeCross_Ow,y
							BRA ...WriteTable
					
					...Level
						....CheckNextTileForOutline
						INX							;>Next tile
						CPX $02							;\If next tile is nonexistent (past the last tile),
						BEQ .....Valid						;|treat as if you shouldn't add with it.
						BCS .....Invalid						;/
						
						.....Valid
							LDA !Scratchram_GraphicalBar_FillByteTbl,x		;>Next tile's fill amount...
							DEX							;>Back to current tile
							CLC							;\...Add with current tile's amount
							ADC !Scratchram_GraphicalBar_FillByteTbl,x		;/
							TAY							;>The combined index into Y.
							BRA .....LoadTileNumber
						.....Invalid
							DEX						;>Back to current tile
							LDA !Scratchram_GraphicalBar_FillByteTbl,x	;>Load current tile without adding by an invalid value
							TAY
						.....LoadTileNumber
							LDA GraphicalBar_Middle8x8sFillEdgeCross,y
					...WriteTable
						STA !Scratchram_GraphicalBar_FillByteTbl,x	;/
					
					...Next
						INX
						if !Setting_GraphicalBar_IndexSize != 0
							REP #$20
						endif
						DEC $01
						BNE ..Loop
					SEP #$20
		;Right end (there is no next tile after this)
			.RightEndTranslate
				LDA !Scratchram_GraphicalBar_RightEndPiece
				BEQ .Done
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
				BEQ ..Level
			
				..Overworld
					LDA GraphicalBar_RightEnd8x8sFillEdgeCross_Ow,y
					BRA ..WriteTable
			
				..Level
					LDA GraphicalBar_RightEnd8x8sFillEdgeCross,y
				
				..WriteTable
					STA !Scratchram_GraphicalBar_FillByteTbl,x
		;Done
			.Done
				SEP #$30					;>Just in case
				PLB						;>Pull bank
				RTL
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Count tiles. Stupid that you cannot call a separate subroutine
	;file from a subroutine file.
	;Input:
	;-!Scratchram_GraphicalBar_LeftEndPiece,
	; !Scratchram_GraphicalBar_MiddlePiece,
	; !Scratchram_GraphicalBar_TempLength, and
	; !Scratchram_GraphicalBar_RightEndPiece: used to find how many
	; tiles.
	;Output:
	; X = Number of bytes or 8x8 tiles the bar takes up of minus 1
	;     For example: 9 total bytes, this routine would output X=$08.
	;     Returns X=$FF should not a single tile exist.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	CountNumberOfTiles:
		LDX #$00
		LDA !Scratchram_GraphicalBar_LeftEndPiece
		BEQ +
		INX
		+
		LDA !Scratchram_GraphicalBar_MiddlePiece
		BEQ +
		TXA
		CLC
		ADC !Scratchram_GraphicalBar_TempLength
		TAX
		+
		LDA !Scratchram_GraphicalBar_RightEndPiece
		BEQ +
		INX
		+
		DEX					;>Subtract by 1 because index 0 exists.
		RTL