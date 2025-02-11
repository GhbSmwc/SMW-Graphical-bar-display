?CalculateGraphicalBarPercentageRoundDown:
	;This is the main calculation for all 3 variations of CalculateGraphicalBarPercentage, prior to modifying the quantity amount.
	;Integer division always rounds down, by default. Any rounding besides down require checking the remainder.
	;
	;Output:
	;	$00-$03: Fill amount (rounded down)
	;	$04-$05: Remainder
	?.FindTotalPieces
		?..FindTotalMiddle
			if !CPUMode != 0
				LDA !Scratchram_GraphicalBar_MiddlePiece	;\TotalMiddlePieces = MiddlePieces*MiddleLength
				STA $00						;|Note: Multiply two 8-bit numbers.
				STZ $01						;|
				LDA !Scratchram_GraphicalBar_TempLength		;|
				STA $02						;|
				STZ $03						;/
				%MathMul16_16()				;MiddlePieceper8x8 * NumberOfMiddle8x8. Stored into $04-$07 (will read $04-$05 since number of pieces are 16bit, not 32)
			else
				LDA !Scratchram_GraphicalBar_MiddlePiece	;\TotalMiddlePieces = MiddlePieces*MiddleLength
				STA $4202					;|
				LDA !Scratchram_GraphicalBar_TempLength		;|
				STA $4203					;/
				XBA						;\Wait 8 cycles (XBA takes 3, NOP takes 2) for calculation
				XBA						;|
				NOP						;/
				LDA $4216					;\Store product.
				STA $04						;|
				LDA $4217					;|
				STA $05						;/
			endif
		?..FindTotalEnds ;>2 8-bit pieces added together, should result a 16-bit number not exceeding $01FE (if $200 or higher, can cause overflow since carry is only 0 or 1, highest highbyte increase is 1).
			STZ $01						;>Clear highbyte
			LDA !Scratchram_GraphicalBar_LeftEndPiece	;\Lowbyte total
			CLC						;|
			ADC !Scratchram_GraphicalBar_RightEndPiece	;|
			STA $00						;/
			LDA $01						;\Handle high byte (if an 8-bit low byte number exceeds #$FF, the high byte will be #$01.
			ADC #$00					;|$00-$01 should now hold the total fill pieces in the end bytes/8x8 tiles.
			STA $01						;/
		?..FindGrandTotal
			REP #$20
			LDA $04						;>Total middle pieces
			CLC
			ADC $00						;>Plus total end
	?.TotalPiecesTimesQuantity
		;STA $00						;>Store grand total in input A of 32x32bit multiplication
		;STZ $02						;>Rid the highword (#$0000XXXX)
		;LDA !Scratchram_GraphicalBar_FillByteTbl	;\Store quantity
		;STA $04						;/
		;STZ $06						;>Rid the highword (#$0000XXXX)
		;SEP #$20
		;%MathMul32_32()				;>Multiply together. Results in $08-$0F (8 bytes; 64 bit).
		
		STA $00						;>Store 16-bit total pieces into multiplicand
		LDA !Scratchram_GraphicalBar_FillByteTbl	;\Store 16-bit quantity into multiplier
		STA $02						;/
		SEP #$20
		%MathMul16_16()				;>Multiply together ($04-$07 (32-bit) is product)

		;Okay, the reason why I use the 32x32 bit multiplication is because
		;it is very easy to exceed the value of #$FFFF (65535) should you
		;have a large number of pieces in the bar (long bar, or large number
		;per byte).
		
		;Also, you may see "duplicate" routines with the only difference is
		;that they are different number of bytes for the size of values to
		;handle, they are included and used because some of my code preserves
		;them and are not to be overwritten by those routines, so a smaller
		;version is needed, and plus, its faster to avoid using unnecessarily
		;large values when they normally can't reach that far.
		
		;And finally, I don't directly use SA-1's multiplication and division
		;registers outside of routines here, because they are signed. The
		;amount of fill are unsigned.

	?.DivideByMaxQuantity
		;REP #$20
		;LDA $08						;\Store result into dividend (32 bit only, its never to exceed #$FFFFFFFF), highest it can go is #$FFFE0001
		;STA $00						;|
		;LDA $0A						;|
		;STA $02						;/
		;LDA !Scratchram_GraphicalBar_FillByteTbl+2	;\Store MaxQuantity into divisor.
		;STA $04						;/
		;SEP #$20
		;%MathDiv32_16()				;>;[$00-$03 : Quotient, $04-$05 : Remainder], After this division, its impossible to be over #$FFFF.

		REP #$20					;\Store result into dividend (32 bit only, its never to exceed #$FFFFFFFF), highest it can go is #$FFFE0001
		LDA $04						;|
		STA $00						;|
		LDA $06						;|
		STA $02						;/
		LDA !Scratchram_GraphicalBar_FillByteTbl+2	;\Store MaxQuantity into divisor.
		STA $04						;/
		SEP #$20
		%MathDiv32_16()				;>;[$00-$03 : Quotient (rounded down), $04-$05 : Remainder], After this division, its impossible to be over #$FFFF.
		?..CheckRoundToZero
			LDY #$00
			REP #$20
			LDA $00
			ORA $02
			BNE ?...No				;>If quotient is nonzero, then no.
			LDA $04
			BEQ ?...No				;>If quotient is zero AND remainder is zero, then it's exactly zero
			LDY #$01				;>Otherwise if Q = 0 and R != 0, then the fill amount is between 0 and 1, which rounded to zero.
			?...No
			SEP #$20
	RTL