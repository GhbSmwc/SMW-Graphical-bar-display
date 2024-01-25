;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Calculate ratio of Quantity/MaxQuantity to FilledPieces/TotalMaxPieces.
;
;Basically, this routine calculates the "percentage" amount of pieces
;filled. It does this formula in order for this to work (solve for
;"FilledPieces"):
;
; Cross multiply:
;
;   Quantity          FilledPieces
;   -----------   =   ------------
;   MaxQuantity       TotalMaxPieces
;
; Turns into:
;
; (Quantity * TotalMaxPieces)
; ---------------------------  = FilledPieces
;        MaxQuantity
;
; You may be wondering, why multiply first, and then divide, unlike most modern
; programmers to calculate a percentage? Well, it is because we are dealing
; with integers and division (not floating points). After division is performed,
; the quotient is rounded (division routine alone rounds downwards, but this one
; here rounds half-up). Performing any rounding before the last step tends to
; increase (accumulate) the error (how far off from the exact value). It is
; better to perform rounding ONLY on the final result than anytime before the
; last operation.
;
; Example: 1 out of 3 HP on a 62-pieced bar results 20.[6] pieces filled
; (bracketed digits means repeating digits).
;  Division rounding first:
;   (1 HP / 3 Max HP) * 62 = 0 filled out of 62 pieces in bar. It is off by a huge 20.[6] units.
;   This is going to result the bar only displaying full or empty, if quantity is less than 50%, will show 0%, otherwise
;   it will show 100%.
;  Division rounding last:
;   (1 HP * 62 pieces) / 3 HP = 21 filled out of 62 pieces in bar. It is off by a tiny 0.[3] units.
;   Multiplying with 2 integers always results the correct value, unless an overflow occurs, but this
;   subroutine utilizes "16bit*16bit = 32bit" and "32bit/16bit = R:16bit Q:16bit" subroutines, so 16-bit
;   overflows during multiplication is impossible.
;The variables are:
;*Quantity = the amount of something, say current HP.
;*MaxQuantity = the maximum amount of something, say max HP.
;*FilledPieces = the number of pieces filled in the whole bar (rounded 1/2 up).
; *Note that this value isn't capped (mainly Quantity > MaxQuantity), the
;  "DrawGraphicalBar" subroutine will detect and will not display over max,
;  just in case if you somehow want to use the over-the-max-value on advance
;  use (such as filling 2 separate bars, filling up the 2nd one after the 1st
;  is full).
;*TotalMaxPieces = the number of pieces of the whole bar when full.
;
;Input:
; -!Scratchram_GraphicalBar_FillByteTbl to !Scratchram_GraphicalBar_FillByteTbl+1:
;  the quantity.
; -!Scratchram_GraphicalBar_FillByteTbl+2 to !Scratchram_GraphicalBar_FillByteTbl+3:
;  the max quantity.
; -!Scratchram_GraphicalBar_LeftEndPiece: number of pieces in left end
; -!Scratchram_GraphicalBar_MiddlePiece: same as above but for each middle
; -!Scratchram_GraphicalBar_RightEndPiece: same as above, but right end
; -!Scratchram_GraphicalBar_TempLength: number of middle bytes excluding both ends.
;
;Output:
; -$00 to $01: the "percentage" amount of fill in the bar (rounded 1/2 up, done by
;  checking if the remainder after division, is being >= half of the divisor
;  (MaxQuantity)).
; -Y register: if rounded towards empty (fill amount = 0) or full:
; --Y = #$00 if:
; ---Exactly full (or more, so it treats as if the bar is full if more than enough)
;    or exactly empty.
; ---Anywhere between full or empty
; --Y = #$01 if rounded to empty (so a nonzero value less than 0.5 pieces filled).
; --Y = #$02 if rounded to full (so if full amount is 62, values from 61.5 to 61.9).
;  This is useful in case you don't want the bar to display completely full or empty
;  when it is not.
;Overwritten/Destroyed:
; -$02 to $09: because math routines need that much bytes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
?CalculateGraphicalBarPercentage:
	?.FindTotalPieces
		?..FindTotalMiddle
			if !Setting_GraphicalBar_SNESMathOnly == 0
				LDA !Scratchram_GraphicalBar_MiddlePiece	;\TotalMiddlePieces = MiddlePieces*MiddleLength
				STA $00						;|Note: Multiply two 8-bit numbers.
				STZ $01						;|
				LDA !Scratchram_GraphicalBar_TempLength		;|
				STA $02						;|
				STZ $03						;/
				JSR ?MathMul16_16				;MiddlePieceper8x8 * NumberOfMiddle8x8. Stored into $04-$07 (will read $04-$05 since number of pieces are 16bit, not 32)
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
		;JSR MathMul32_32				;>Multiply together. Results in $08-$0F (8 bytes; 64 bit).
		
		STA $00						;>Store 16-bit total pieces into multiplicand
		LDA !Scratchram_GraphicalBar_FillByteTbl	;\Store 16-bit quantity into multiplier
		STA $02						;/
		SEP #$20
		JSR ?MathMul16_16				;>Multiply together ($04-$07 (32-bit) is product)

		;Okay, the reason why I use the 32x32 bit multiplication is because
		;it is very easy to exceed the value of #$FFFF (65535) should you
		;have a number of pieces in the bar (long bar, or large number per
		;byte).
		
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
		;JSR MathDiv32_16				;>;[$00-$03 : Quotient, $04-$05 : Remainder], After this division, its impossible to be over #$FFFF.

		REP #$20					;\Store result into dividend (32 bit only, its never to exceed #$FFFFFFFF), highest it can go is #$FFFE0001
		LDA $04						;|
		STA $00						;|
		LDA $06						;|
		STA $02						;/
		LDA !Scratchram_GraphicalBar_FillByteTbl+2	;\Store MaxQuantity into divisor.
		STA $04						;/
		SEP #$20
		JSR ?MathDiv32_16				;>;[$00-$03 : Quotient, $04-$05 : Remainder], After this division, its impossible to be over #$FFFF.
		?..Rounding
			REP #$20
			LDA !Scratchram_GraphicalBar_FillByteTbl+2	;>Max Quantity
			LSR						;>Divide by 2 (halfway point of max)..
			BCC ?...ExactHalfPoint				;>Should a remainder in the carry is 0 (no remainder), don't round the 1/2 point
			INC						;>Round the 1/2 point

			?...ExactHalfPoint
				CMP $04						;>Half of max compares with remainder
				BEQ ?...RoundDivQuotient				;>If HalfPoint = Remainder, round upwards
				BCS ?...NoRoundDivQuotient			;>If HalfPoint > remainder (or remainder is smaller), round down (if exactly full, this branch is taken).

			?...RoundDivQuotient
				;^this also gets branched to if the value is already an exact integer number of pieces (so if the
				;quantity is 50 out of 100, and a bar of 62, it would be perfectly at 31 [(50*62)/100 = 31]
				LDA $00						;\Round up an integer
				INC						;/
				STA $08						;>move towards $08 because 16bit*16bit multiplication uses $00 to $07

		;check should this rounded value made a full bar when it is actually not:
				?....RoundingUpTowardsFullCheck
					;Just as a side note, should the bar be EXACTLY full (so 62/62 and NOT 61.9/62, it guarantees
					;that the remainder is 0, so thus, no rounding is needed.) This is due to the fact that
					;[Quantity * FullAmount / MaxQuantity] when Quantity and MaxQuantity are the same number,
					;thus, canceling each other out (so 62 divide by 62 = 1) and left with FullAmount (the
					;number of pieces in the bar)
					
					;Get the full number of pieces
					if !Setting_GraphicalBar_SNESMathOnly == 0
						LDA !Scratchram_GraphicalBar_MiddlePiece	;\Get amount of pieces in middle
						AND #$00FF					;|
						STA $00						;|
						LDA !Scratchram_GraphicalBar_TempLength		;|
						AND #$00FF					;|
						STA $02						;/
						SEP #$20
						JSR ?MathMul16_16				;>[$04-$07: Product]
					else
						SEP #$20
						LDA !Scratchram_GraphicalBar_MiddlePiece
						STA $4202
						LDA !Scratchram_GraphicalBar_TempLength
						STA $4203
						XBA						;\Wait 8 cycles (XBA takes 3, NOP takes 2) for calculation
						XBA						;|
						NOP						;/
						LDA $4216					;\[$04-$07: Product]
						STA $04						;|
						LDA $4217					;|
						STA $05						;/
					endif
					LDY #$00					;>Default that the meter didn't round towards empty/full (cannot be before the above subroutine since it overwrites Y).

					;add the 2 ends tiles amount (both are 8-bit, but results 16-bit)
					
					;NOTE: should the fill amount be exactly full OR greater, Y will be #$00.
					;This is so that greater than full is 100% treated as exactly full.
					LDA #$00					;\A = $YYXX, (initially YY is $00)
					XBA						;/
					LDA !Scratchram_GraphicalBar_LeftEndPiece	;\get total pieces
					CLC						;|\carry is set should overflow happens (#$FF -> #$00)
					ADC !Scratchram_GraphicalBar_RightEndPiece	;//
					XBA						;>A = $XXYY
					ADC #$00					;>should that overflow happen, increase the A's upper byte (the YY) by 1 ($01XX)
					XBA						;>A = $YYXX, addition maximum shouldn't go higher than $01FE. A = 16-bit total ends pieces
					REP #$20
					CLC						;\plus middle pieces = full amount
					ADC $04						;/
					CMP $08						;>compare with rounded fill amount
					BNE ?.....TransferFillAmtBack			;\should the rounded up fill matches with the full value, flag that
					LDY #$02					;/it had rounded to full.

					?.....TransferFillAmtBack
						LDA $08						;\move the fill amount back to $00.
						STA $00						;/
						BRA ?.Done
		
			?...NoRoundDivQuotient
				?....RoundingDownTowardsEmptyCheck
					LDY #$00					;>Default that the meter didn't round towards empty/full.
					LDA $00						;\if the rounded down (result from fraction part is less than .5) quotient value ISN't zero,
					BNE ?.Done					;/(exactly 1 piece filled or more) don't even consider setting Y to #$01.
					LDA $04						;\if BOTH rounded down quotient and the remainder are zero, the bar is TRUELY completely empty
					BEQ ?.Done					;/and don't set Y to #$01.
					
					LDY #$01					;>indicate that the value was rounded down towards empty
					
					?.Done
					SEP #$20
	RTL
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Unsigned 32bit / 16bit Division
	; By Akaginite (ID:8691), fixed the overflow
	; bitshift by GreenHammerBro (ID:18802)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Arguments
	; $00-$03 : Dividend
	; $04-$05 : Divisor
	; Return values
	; $00-$03 : Quotient
	; $04-$05 : Remainder
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	?MathDiv32_16:	REP #$20
			ASL $00
			ROL $02
			LDY #$1F
			LDA.w #$0000
	?-		ROL A
			BCS ?+
			CMP $04
			BCC ?++
	?+		SBC $04
			SEC
	?++		ROL $00
			ROL $02
			DEY
			BPL ?-
			STA $04
			SEP #$20
			RTS
	if !sa1 == 0
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; 16bit * 16bit unsigned Multiplication
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Argusment
		; $00-$01 : Multiplicand
		; $02-$03 : Multiplier
		; Return values
		; $04-$07 : Product
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		?MathMul16_16:	REP #$20
				LDY $00
				STY $4202
				LDY $02
				STY $4203
				STZ $06
				LDY $03
				LDA $4216
				STY $4203
				STA $04
				LDA $05
				REP #$11
				ADC $4216
				LDY $01
				STY $4202
				SEP #$10
				CLC
				LDY $03
				ADC $4216
				STY $4203
				STA $05
				LDA $06
				CLC
				ADC $4216
				STA $06
				SEP #$20
				RTS
	else
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; 16bit * 16bit unsigned Multiplication SA-1 version
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Argusment
		; $00-$01 : Multiplicand
		; $02-$03 : Multiplier
		; Return values
		; $04-$07 : Product
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
		?MathMul16_16:	STZ $2250
				REP #$20
				LDA $00
				STA $2251
				ASL A
				LDA $02
				STA $2253
				BCS ?+
				LDA.w #$0000
		?+		BIT $02
				BPL ?+
				CLC
				ADC $00
		?+		CLC
				ADC $2308
				STA $06
				LDA $2306
				STA $04
				SEP #$20
				RTS
	endif
	

	
	