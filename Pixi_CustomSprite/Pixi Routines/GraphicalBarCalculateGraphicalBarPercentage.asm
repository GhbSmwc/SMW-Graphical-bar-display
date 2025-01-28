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
	%GraphicalBarCalculateGraphicalBarPercentageRoundDown()
	?.RoundHalfUp
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
				
				%GraphicalBarGetMaxBarInAForRoundToMaxCheck()
				
				LDY #$00					;>Default that the meter didn't round towards empty/full (cannot be before the above subroutine since it overwrites Y).
				
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