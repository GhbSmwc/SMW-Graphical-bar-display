;Subroutines this file included (in case if your game already have those routines
;to avoid duplicate subroutines):
;-MathMul16_16
;-MathMul32_32 (not actually used, as of version 3.15)
;-MathDiv32_16
;-MathDiv
;
;Main routines to use:
;-CalculateGraphicalBarPercentage
;-CalculateGraphicalBarPercentageRoundUp
;-CalculateGraphicalBarPercentageRoundDown
;-DrawGraphicalBar (depreciated & commented out, please use the one below instead)
;-DrawGraphicalBarSubtractionLoopEdition
;-RoundAwayEmpty
;-RoundAwayFull
;-RoundAwayEmptyFull
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SA-1 handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Only include this if there is no SA-1 detection, such as including this
;in a (seperate) patch.
	if defined("sa1") == 0
		!dp = $0000
		!addr = $0000
		!sa1 = 0
		!gsu = 0

		if read1($00FFD6) == $15
			sfxrom
			!dp = $6000
			!addr = !dp
			!gsu = 1
		elseif read1($00FFD5) == $23
			sa1rom
			!dp = $3000
			!addr = $6000
			!sa1 = 1
		endif
	endif
incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"


;Don't touch
	;Determine should registers be SNES or SA-1
	!CPUMode = 0
	if (and(equal(!sa1, 1),equal(!Setting_GraphicalBar_SNESMathOnly, 0)))
		!CPUMode = 1
	endif

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
;  "DrawGraphicalBar" (and "DrawGraphicalBarSubtractionLoopEdition") subroutine will
;  detect and will not display over max, just in case if you somehow want to use the
;  over-the-max-value on advance use (such as filling 2 separate bars, filling up
;  the 2nd one after the 1st is full).
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
; -$00 to $01: the "percentage" amount of fill in the bar, Rounded:
; --CalculateGraphicalBarPercentage: 1/2 up, done by checking if the remainder
;   after division, is being >= half of the divisor (MaxQuantity)).
; --CalculateGraphicalBarPercentageRoundDown: Rounds down an integer.
; --CalculateGraphicalBarPercentageRoundUp: Rounds up an integer, if remainder
;   is nonzero.
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
CalculateGraphicalBarPercentage:
	JSL CalculateGraphicalBarPercentageRoundDown
	.RoundHalfUp
	..Rounding
		REP #$20
		LDA !Scratchram_GraphicalBar_FillByteTbl+2	;>Max Quantity
		LSR						;>Divide by 2 (halfway point of max).. (LSR would shift bit 0 into carry, thus if number is odd, carry is set)
		BCC ...ExactHalfPoint				;>Should a remainder in the carry is 0 (no remainder), don't round the 1/2 point
		INC						;>Round the 1/2 point...
			;^Reason 1/2 point must be rounded up, is so to truly check if remainder is greater or equal to half of MaxQuantity, to round the pieces filled up,
			; when MaxQuantity is odd. e.g. 1 Quantity * 62 pieces / 5 MaxQuantity = Q:12 R:2, which is 12 and 2/5, or 12.4. The 1/2 point of 5 is EXACTLY 2.5,
			; not 2 (LSR divides A by 2 and round down, thus resulting 2). This means that the lowest remainder integer to trigger a round-up of the amount of
			; pieces filled would be 3.

		...ExactHalfPoint
			CMP $04						;>Half of max compares with remainder
			BEQ ...RoundDivQuotient				;>If HalfPoint = Remainder, round upwards
			BCS ...NoRoundDivQuotient			;>If HalfPoint > remainder (or remainder < HalfPoint), round down (if exactly full, this branch is taken).

		...RoundDivQuotient
			;^this also gets branched to if the value is already an exact integer number of pieces (so if the
			;quantity is 50 out of 100, and a bar of 62, it would be perfectly at 31 [(50*62)/100 = 31]
			LDA $00						;\Round up an integer
			INC						;/
			STA $08						;>move towards $08 because 16bit*16bit multiplication uses $00 to $07

	;check should this rounded value made a full bar when it is actually not:
			....RoundingUpTowardsFullCheck
				;Just as a side note, should the bar be EXACTLY full (so 62/62 and NOT 61.9/62, it guarantees
				;that the remainder is 0, so thus, no rounding is needed.) This is due to the fact that
				;[Quantity * FullAmount / MaxQuantity] when Quantity and MaxQuantity are the same number,
				;thus, canceling each other out (so 62 divide by 62 = 1) and left with FullAmount (the
				;number of pieces in the bar)
				
				JSL GetMaxBarInAForRoundToMaxCheck
				
				LDY #$00					;>Default that the meter didn't round towards empty/full (cannot be before the above subroutine since it overwrites Y).
				
				CMP $08						;>compare with rounded fill amount
				BNE .....TransferFillAmtBack			;\should the rounded up fill matches with the full value, flag that
				LDY #$02					;/it had rounded to full.

				.....TransferFillAmtBack
					LDA $08						;\move the fill amount back to $00.
					STA $00						;/
					BRA .Done
	
		...NoRoundDivQuotient
			....RoundingDownTowardsEmptyCheck
				LDY #$00					;>Default that the meter didn't round towards empty/full.
				LDA $00						;\if the rounded down (result from fraction part is less than .5) quotient value ISN't zero,
				BNE .Done					;/(exactly 1 piece filled or more) don't even consider setting Y to #$01.
				LDA $04						;\if BOTH rounded down quotient and the remainder are zero, the bar is TRUELY completely empty
				BEQ .Done					;/and don't set Y to #$01.
				
				LDY #$01					;>indicate that the value was rounded down towards empty
				
				.Done
				SEP #$20
	RTL
CalculateGraphicalBarPercentageRoundUp:
	JSL CalculateGraphicalBarPercentageRoundDown
	REP #$20
	LDA $04				;\If remainder is zero, (meaning exactly an integer), don't increment
	BEQ .NoRoundUp			;/
	.RoundUp
		INC $00				;>Otherwise if there is a remainder (between Quotient and Quotient+1), use Quotient+1
		if !Setting_GraphicalBar_SNESMathOnly == 0
			LDA $00				;\Preserve rounded quotient
			PHA				;/
		endif
		JSL GetMaxBarInAForRoundToMaxCheck
		if !Setting_GraphicalBar_SNESMathOnly == 0
			REP #$30
			TAY
			PLA				;\Restore quotient
			STA $00				;/
			TYA
			SEP #$30
		endif
		LDY #$00
		REP #$20
		CMP $00
		SEP #$20
		BNE .NoRoundToMax
		LDY #$02
	.NoRoundToMax
	.NoRoundUp
	SEP #$20
	RTL
CalculateGraphicalBarPercentageRoundDown:
	;This is the main calculation for all 3 variations of CalculateGraphicalBarPercentage, prior to modifying the quantity amount.
	;Integer division always rounds down, by default. Any rounding besides down require checking the remainder.
	;
	;Output:
	;	$00-$03: Fill amount (rounded down)
	;	$04-$05: Remainder
	.FindTotalPieces
		..FindTotalMiddle
			if !Setting_GraphicalBar_SNESMathOnly == 0
				LDA !Scratchram_GraphicalBar_MiddlePiece	;\TotalMiddlePieces = MiddlePieces*MiddleLength
				STA $00						;|Note: Multiply two 8-bit numbers.
				STZ $01						;|
				LDA !Scratchram_GraphicalBar_TempLength		;|
				STA $02						;|
				STZ $03						;/
				JSL MathMul16_16				;MiddlePieceper8x8 * NumberOfMiddle8x8. Stored into $04-$07 (will read $04-$05 since number of pieces are 16bit, not 32)
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
		..FindTotalEnds ;>2 8-bit pieces added together, should result a 16-bit number not exceeding $01FE (if $200 or higher, can cause overflow since carry is only 0 or 1, highest highbyte increase is 1).
			STZ $01						;>Clear highbyte
			LDA !Scratchram_GraphicalBar_LeftEndPiece	;\Lowbyte total
			CLC						;|
			ADC !Scratchram_GraphicalBar_RightEndPiece	;|
			STA $00						;/
			LDA $01						;\Handle high byte (if an 8-bit low byte number exceeds #$FF, the high byte will be #$01.
			ADC #$00					;|$00-$01 should now hold the total fill pieces in the end bytes/8x8 tiles.
			STA $01						;/
		..FindGrandTotal
			REP #$20
			LDA $04						;>Total middle pieces
			CLC
			ADC $00						;>Plus total end
	.TotalPiecesTimesQuantity
		;STA $00						;>Store grand total in input A of 32x32bit multiplication
		;STZ $02						;>Rid the highword (#$0000XXXX)
		;LDA !Scratchram_GraphicalBar_FillByteTbl	;\Store quantity
		;STA $04						;/
		;STZ $06						;>Rid the highword (#$0000XXXX)
		;SEP #$20
		;JSL MathMul32_32				;>Multiply together. Results in $08-$0F (8 bytes; 64 bit).
		
		STA $00						;>Store 16-bit total pieces into multiplicand
		LDA !Scratchram_GraphicalBar_FillByteTbl	;\Store 16-bit quantity into multiplier
		STA $02						;/
		SEP #$20
		JSL MathMul16_16				;>Multiply together ($04-$07 (32-bit) is product)

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

	.DivideByMaxQuantity
		;REP #$20
		;LDA $08						;\Store result into dividend (32 bit only, its never to exceed #$FFFFFFFF), highest it can go is #$FFFE0001
		;STA $00						;|
		;LDA $0A						;|
		;STA $02						;/
		;LDA !Scratchram_GraphicalBar_FillByteTbl+2	;\Store MaxQuantity into divisor.
		;STA $04						;/
		;SEP #$20
		;JSL MathDiv32_16				;>;[$00-$03 : Quotient, $04-$05 : Remainder], After this division, its impossible to be over #$FFFF.

		REP #$20					;\Store result into dividend (32 bit only, its never to exceed #$FFFFFFFF), highest it can go is #$FFFE0001
		LDA $04						;|
		STA $00						;|
		LDA $06						;|
		STA $02						;/
		LDA !Scratchram_GraphicalBar_FillByteTbl+2	;\Store MaxQuantity into divisor.
		STA $04						;/
		SEP #$20
		JSL MathDiv32_16				;>;[$00-$03 : Quotient (rounded down), $04-$05 : Remainder], After this division, its impossible to be over #$FFFF.
		..CheckRoundToZero
			LDY #$00
			REP #$20
			LDA $00
			ORA $02
			BNE ...No				;>If quotient is nonzero, then no.
			LDA $04
			BEQ ...No				;>If quotient is zero AND remainder is zero, then it's exactly zero
			LDY #$01				;>Otherwise if Q = 0 and R != 0, then the fill amount is between 0 and 1, which rounded to zero.
			...No
			SEP #$20
	RTL
GetMaxBarInAForRoundToMaxCheck:
	;Must be called with 16-bit A.
	;Get the full number of pieces (for checking if rounding a number between Max-1 and Max to Max.)
	;Output: A (16-bit): Maximum fill amount (processor flag for A is 8-bit though)
	;Destroys:
	; -$00-$07 in LoROM
	; -$04-$05 in SA-1
	if !Setting_GraphicalBar_SNESMathOnly == 0
		LDA !Scratchram_GraphicalBar_MiddlePiece	;\Get amount of pieces in middle
		AND #$00FF					;|
		STA $00						;|
		LDA !Scratchram_GraphicalBar_TempLength		;|
		AND #$00FF					;|
		STA $02						;/
		SEP #$20
		JSL MathMul16_16				;>[$04-$07: Product]
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
	RTL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Convert amount of fill to each fill per byte.
;
;Note: I recommend using DrawGraphicalBarSubtractionLoopEdition instead,
;realizing this routine is much less optimized than the aforementioned.
;
;This basically divides the amount of fill in the whole bar into each
;addends of the amount of fill stored in each byte in the table, in
;this order: N bytes (including zero) being maxed out, 0 or 1 fraction byte,
;and then N bytes (including zero) empty, in that order, keeping the order
;of the maximums of each byte unchanged. It works similar to euclidean
;division, but the end tiles may have different max amounts.
;
; Note that this data is always stored in this order, even on a leftwards
; bar (its up to the write tile routine to  invert the order).
;
; Example:
; -Left and right end tiles have 3 pieces (therefore ends are 3-max)
; -Middle each tile have 8 pieces (8-max)
; -Middle tile length set to 7.
; -Fill amount = $0017 (23)
;
; Stored:
;  M,  M,  M,  F,  E,  E,  E,  E,  E
; $03,$08,$08,$04,$00,$00,$00,$00,$00 ; <------- (3/3, 8/8, 8/8, 4/8, 0/8, 0/8, 0/8, 0/8, and then 0/3)
;
; M = full
; F = fraction
; E = empty
;
; An analogy is you fill a cup of water until it's full, then the next
; cup until it's full, until you got all the cups full or have run out
; of water (repeated subtraction until no more is left):
;
; (1) FillAmount compares with the capacity of the cup.
;
; (2.a) If FillAmount is greater or equal: Then [CupAmount[n] = CupCapicity[n]] (full cup) and
; then [FillAmount = FillAmount - CupCapicity[n]] (amount taken out of FillAmount and then
; added into the cup).
;
; (2.b) Otherwise [CupAmount[n] = FillAmount], and then FillAmount is exhausted (FillAmount = 0).
;
; Then move on to the next cup (n increments by 1) and repeat back to (1) until n equals to NumberOfCups.
;
;Notes:
; -This routine output only have 1 partially filled (non-full and non-empty)
;  byte, due to only 1 "fraction" is supported. To have custom edge, after
;  this routine is done, you simply read the amount of the fraction to
;  determine the edge is crossing the next 8x8 byte.
; -The fraction byte/8x8 tile includes the value 0 (it's actually 0 to max-1,
;  not 1 to max-1), thus if there are only full bytes tile and empty bytes after,
;  the first empty byte after the last full byte is considered the fraction tile.
;
;Input:
; -$00 to $01: The amount of fill for the WHOLE bar.
; -!Scratchram_GraphicalBar_LeftEndPiece: Number of pieces in left byte (0-255), also
;  the maximum amount of fill for this byte itself. If 0, it's not included in table.
; -!Scratchram_GraphicalBar_MiddlePiece: Same as above but each middle byte.
; -!Scratchram_GraphicalBar_RightEndPiece: Same as above but for right end.
; -!Scratchram_GraphicalBar_TempLength: The length of the bar (only counts
;   middle bytes)
;Output:
; -!Scratchram_GraphicalBar_FillByteTbl to !Scratchram_GraphicalBar_FillByteTbl+EndAddress:
;  A table array containing the amount of fill for each byte, explained previously.
;
;  The numbers of each byte should total equal to the value stored in ram address
;  $00 prior. Should the bar be more than full, the table will act as if the bar
;  full and will not store higher values nor write additional bytes beyond table.
; -$08 to $09 are used for handling fill for each of the 3 groups of bytes
;  (left, middle, and right). Once the routine is done, it's the amount of
;  fill you have input for $00 to $01 (not capped to the value to be full
;  if greater than).
;
;  The end of the address going to be used is this:
;
;  EndAddress = (L + MLength + R) - 1
;
;  -L (left end) and/or R (right end) are 0 if there are no pieces for each of them, otherwise 1.
;  -MLength (Middle length) is basically !Scratchram_GraphicalBar_TempLength. If that or
;   if MiddlePiece = zero (either 16 or 8-bit, this will be zero and will not be
;   included).
;  This can be read as each byte means each 8x8 tile.
;Overwritten/Destroyed:
; -$00 to $07: garbage:
; --$00 to $01: will be when this routine is finished:
; ---The amount right end contains if right end exist and no regards to left
;    end and middle.
; ---#$00 if no right end exist but middle exist.
; ---The amount left end contains when middle and right end doesn't exist.
; --$02 to $07: needed to move values to another address due to subroutines,
;   as well as outputs of the subroutines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;DrawGraphicalBar:
;	if !Setting_GraphicalBar_IndexSize == 0
;		LDX #$00					;>Index to write all our bytes/8x8s after the first tile.
;		REP #$20					;>16-bit A
;		LDA $00						;\make a backup on the amount of fill because $00 is used by math routines,
;		STA $08						;/and in case if any of the 3 parts gets disabled.
;	else
;		REP #$30					;>16-bit AXY
;		LDX #$0000					;>Index to write all our bytes/8x8s after the first tile.
;		LDA $00						;\make a backup on the amount of fill because $00 is used by math routines,
;		STA $08						;/and in case if any of the 3 parts gets disabled.
;	endif
;	.LeftEnd
;		;LeftendFill = Clamp(InputFill, 0, LeftMax)
;		LDA !Scratchram_GraphicalBar_LeftEndPiece	;\check if the left end was present
;		AND #$00FF					;|
;		BEQ .Middle					;/
;		
;		CMP $00						;>Number of pieces on left end (max pieces) compares with number of pieces filled
;		BCC ..Full					;>If max pieces is < pieces filled (pieces filled > max), cap it to full
;		
;		..NotFull
;		LDA $00						;>Load the valid non-full value
;		
;		..Full
;		SEP #$20					;\Write only on the first byte of the table.
;		STA !Scratchram_GraphicalBar_FillByteTbl	;/
;		INX
;	.Middle
;		;MiddleFill = InputFill - LeftMax
;		;NumberOfFullMiddles = clamp(floor(MiddleFill/InputMiddlePiecesEachMax), 0, InputMiddleLength)
;		;MiddleFractionLocation = NumberOfFullMiddles + 1 (indexes the fraction tile after the last full middle), if all middles are full,
;		; it is not written.
;		;MiddleFractionAmount = MiddleFill % InputMiddlePiecesEachMax (% is the modulo operator).
;		;NumberOfMiddleEmpties = clamp(InputMiddleLength - (NumberOfFullMiddles + 1), 0, InputMiddleLength)
;		; ^Note: This writes $00 to all the remaining middles when there are 2+ less full tiles than InputMiddleLength
;		;  (A fraction tile can also be $00, thus the first number that is not maxed in the table is always a fraction,
;		;  i.e: [$03,$08,$08,$00,$00] <- the first $00 after the $08 is reguarded as a fraction.)
;		LDA !Scratchram_GraphicalBar_MiddlePiece	;\Both of these have to be nonzero to include middle.
;		BNE +						;|
;		JMP .RightEnd					;|
;		+						;|
;		LDA !Scratchram_GraphicalBar_TempLength		;|
;		BNE +						;|
;		JMP .RightEnd					;/
;		+
;		REP #$20
;		LDA !Scratchram_GraphicalBar_LeftEndPiece	;>Left end maximum
;		AND #$00FF					;
;		CMP $00						;>compares with amount filled
;		SEP #$20					;
;		BCC ..ReachesMiddle				;>If maximum < filled (filled >= maximum)
;		;The gimmick here is that the Y acts as a counter of how many middle tiles to process.
;		;Each time you write a tile (full, fraction, or empty), it subtracts Y by 1 after, and once
;		;that is zero, tells it to stop writing any additional tiles. So no worries that it
;		;would write additional bytes beyond the expected right end if the fill amount is
;		;bigger than the bar's maximum value (or total pieces).
;		..EmptyMiddle
;			if !Setting_GraphicalBar_IndexSize == 0
;				LDA !Scratchram_GraphicalBar_TempLength
;				TAY
;			else
;				REP #$20
;				LDA !Scratchram_GraphicalBar_TempLength
;				AND #$00FF
;				TAY
;				SEP #$20
;			endif
;			...Loop
;				LDA #$00					;\Write empty for the middle section
;				STA !Scratchram_GraphicalBar_FillByteTbl,x	;/
;		
;				....Next
;				INX						;>next byte/8x8
;				DEY
;				if !Setting_GraphicalBar_IndexSize == 0
;					CPY #$00
;				else
;					CPY #$0000
;				endif
;				BNE ...Loop
;				JMP .RightEnd
;
;		..ReachesMiddle
;			if !Setting_GraphicalBar_IndexSize == 0
;				LDA !Scratchram_GraphicalBar_TempLength		;\number of middles to write in Y (used as how many middles, either full, partially or empty left to write)
;				TAY						;/
;			else
;				REP #$20					;\number of middles to write in Y
;				LDA !Scratchram_GraphicalBar_TempLength		;|
;				AND #$00FF					;|
;				TAY						;|
;				SEP #$20					;/
;			endif
;			LDA !Scratchram_GraphicalBar_LeftEndPiece	;\Akaginite's (ID:8691) 16-bit subtract by 8-bit [MiddleFillOnly = TotalFilled - LeftEnd]
;			REP #$21					;|>A = 16bit and carry set
;			AND #$00FF					;|>Remove high byte
;			EOR #$FFFF					;|\Invert the now 16-bit number.
;			INC A						;|/>INC does not affect the carry.
;			ADC $00						;/>And negative LeftEnd plus filled to get MiddleFillOnly [MiddleFillOnly = (-LeftEnd) + TotalFilled]
;
;		..NumberOfFullMiddles
;			STA $08						;>middle fill (amount of fill in middle only)
;			STA $00						;>store the middlefill in $00 for dividend
;			LDA !Scratchram_GraphicalBar_MiddlePiece	;\middlepiece as divisor [NumberOfFull8x8s = MiddleFill/PiecesPer8x8, with NumberOfFull8x8s rounded down.]
;			AND #$00FF					;|
;			STA $02						;/
;			PHY						;>protect number of middle tiles left
;			SEP #$30					;>8-bit AXY
;			JSL MathDiv					;>$00: number of full bytes/8x8s, $02: fraction byte/8x8 [FractionAmount = MiddleFill MOD PiecesPer8x8]
;			LDA $01						;\check if the number of full bytes/8x8s is bigger than 255
;			BEQ ...ValidNumbFullMiddles			;/
;			
;			...InvalidNumbFullMiddles
;				LDA #$FF					;\cap the number of full middle 8x8s to max 8-bit number
;				STA $00						;/(if for some reason if you want such a length, but shouldn't hurt if you put less)
;			
;			...ValidNumbFullMiddles
;				if !Setting_GraphicalBar_IndexSize == 0
;					REP #$20					;>16-bit A
;				else
;					REP #$30					;>16-bit AXY
;				endif
;				PLY						;>restore number of middle tiles left
;				LDA $00						;>number of full tiles to write
;				SEP #$20					;>8-bit A
;				BEQ ..FractionAfterFullMiddles			;>skip to fraction because there is no full middle byte/8x8
;
;
;			...Loop
;				LDA !Scratchram_GraphicalBar_MiddlePiece	;\write full tiles
;				STA !Scratchram_GraphicalBar_FillByteTbl,x	;/
;		
;				....Next
;					INX						;>next byte/8x8
;					DEY						;>subtract number of middles left by 1
;					if !Setting_GraphicalBar_IndexSize == 0
;						CPY #$00
;					else
;						CPY #$0000					;\end the loop should the entire middle section be full or higher
;					endif
;					BEQ ..MiddleDone				;/(avoids adding an extra middle tile, which should be avoided at all cost)
;					DEC $00						;\end the loop should all full middles are written.
;					BNE ...Loop					;/
;		
;		..FractionAfterFullMiddles
;			LDA $02						;\(remainder) Fraction tiles after all the full middles
;			STA !Scratchram_GraphicalBar_FillByteTbl,x	;/
;		
;		..EmptyAfterFraction
;			INX						;>After fraction
;			DEY						;>number of bytes/8x8s before the last middle
;			if !Setting_GraphicalBar_IndexSize == 0
;				CPY #$00					;>countdown before the final middle
;			else
;				CPY #$0000					;>countdown before the final middle
;			endif
;			BEQ ..MiddleDone				;>avoid writing the very first empty past the last middle
;		
;			...Loop
;				LDA #$00					;\write empty
;				STA !Scratchram_GraphicalBar_FillByteTbl,x	;/
;			
;				....Next
;					INX						;\loop until all middle tiles done.
;					DEY
;					if !Setting_GraphicalBar_IndexSize == 0
;						CPY #$00
;					else
;						CPY #$0000
;					endif
;					BNE ...Loop					;/won't add another empty tile.
;		
;		..MiddleDone
;			REP #$20
;			LDA !Scratchram_GraphicalBar_LeftEndPiece	;\8-bit left end
;			AND #$00FF					;/
;			CLC						;\re-include left end, now back to having total amount of filled
;			ADC $08						;|pieces
;			STA $08						;/
;			SEP #$20
;	.RightEnd
;		;RightEndFill = clamp((InputFill - (LeftMax + (InputMiddlePiecesEachMax * InputMiddleLength))), 0, RightMax)
;		LDA !Scratchram_GraphicalBar_RightEndPiece	;\check if right end exist
;		BEQ .Done					;/
;		
;		if !Setting_GraphicalBar_SNESMathOnly == 0
;			LDA !Scratchram_GraphicalBar_MiddlePiece	;\MiddlePieceTotal = MiddlePiecePer8x8 * Length
;			STA $00						;|
;			STZ $01						;|
;			LDA !Scratchram_GraphicalBar_TempLength		;|
;			STA $02						;|
;			STZ $03						;/
;			if !Setting_GraphicalBar_IndexSize == 0
;				JSL MathMul16_16
;				REP #$20					;>16-bit A
;			else
;				PHX						;>Preserve X due to destroyed high byte from the following SEP.
;				SEP #$30					;>8-bit AXY
;				JSL MathMul16_16				;>$04 to $07: 32 bit product (the total amount in middle)
;				REP #$30					;>16-bit AXY
;				PLX						;>restore X
;			endif
;		else
;			LDA !Scratchram_GraphicalBar_MiddlePiece	;\MiddlePieceTotal = MiddlePiecePer8x8 * Length
;			STA $4202					;|
;			LDA !Scratchram_GraphicalBar_TempLength		;|
;			STA $4203					;/
;			XBA						;\Wait 8 cycles (XBA takes 3, NOP takes 2) for calculation
;			XBA						;|
;			NOP						;/
;			LDA $4216					;\Product in $04-$05
;			STA $04						;|
;			LDA $4217					;|
;			STA $05						;/
;			REP #$20					;>16-bit A
;		endif
;
;		LDA !Scratchram_GraphicalBar_LeftEndPiece	;\Add by left end piece [TotalLeftEndAndMiddle = MiddlePieceTotal + LeftEnd]
;		AND #$00FF					;|
;		CLC						;|
;		ADC $04						;|This should mark the boundary between middle and right end
;		STA $04						;/
;		LDA $08						;\RightEndFillOnly = TotalFilled - (MiddlePieceTotal+LeftEnd)
;		SEC						;|this result should be less than or equal to 255
;		SBC $04						;/>SBC clears the carry should an unsigned underflow occurs ($00 -> $FF) from borrowing (small - bigger). Value should be < 255
;		BCC ..EmptyRightEnd				;>carry clear means that the total filled is less than the amount needed to reach the right end.
;		STA $00						;>Store right end fill to $00-$01 (still 16-bit to prevent right end from randomly overflowing)
;		LDA !Scratchram_GraphicalBar_RightEndPiece	;\RightEnd's maximum (8-bit)
;		AND #$00FF					;/
;		CMP $00						;>compare with amount of fill only right end (that potentially be over 255)
;		BCC ..FullRightEnd				;>If maximum < fill pieces (or right end's filled pieces >= maximum), cap the fill value
;		SEP #$20
;		LDA $00						;>amount of fill, assuming it's 0 to max.
;		BRA ..SetRightEndFill
;		
;		..EmptyRightEnd
;			SEP #$20
;			LDA #$00
;		
;		..FullRightEnd
;			SEP #$20
;		
;		..SetRightEndFill
;			STA !Scratchram_GraphicalBar_FillByteTbl,x
;		
;	.Done
;		SEP #$30					;>8-bit AXY
;		RTL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Convert amount of fill to each fill per byte, repeated subtraction edition.
;
;Same as the other version, "DrawGraphicalBar" however does not use
;multiplication and division routines. In fact, this alone does not use any
;other subroutines AT ALL.
;
;It works by:
;
;(1) Taking the given/remaining fill amount, compares or subtracts by the
;    maximum amount given for each byte in table: [Difference = RemainingFill - Maximum]
;(1.1) If “Difference” becomes negative (RemainingFill < Maximum) “RemainingFill”
;      (prior this subtraction into the negative) is copied and stored into
;      the byte in the table array and then “RemainingFill” is set to 0.
;      In simple terms, use all the rest if remaining fill is small.
;(1.2) If zero or positive number occurs (RemainingFill >= Maximum),
;      a byte in the table array is set to “Maximum”, “RemainingFill” is set to
;      “Difference” (as in, RemainingFill := RemainingFill - Maximum).
;      In simple terms, remaining amount deducted to “completely fill” a byte
;      in table array.
; (2) Index for tile array increases, and repeat back to step (1),
;     basically "go to the next tile"
;
;^Essentially, you are transferring a given amount and “distributing” a given value
; to each consecutive byte in the table. This is division in the form of repeated
; subtraction. Much lighter than the other version.
;
;Input:
; -$00 to $01: The amount of fill for the WHOLE bar.
; -!Scratchram_GraphicalBar_LeftEndPiece: Number of pieces in left byte (0-255), also
;  the maximum amount of fill for this byte itself. If 0, it's not included in table.
; -!Scratchram_GraphicalBar_MiddlePiece: Same as above but each middle byte.
; -!Scratchram_GraphicalBar_RightEndPiece: Same as above but for right end.
; -!Scratchram_GraphicalBar_TempLength: The length of the bar (only counts
;   middle bytes)
;Output:
; -!Scratchram_GraphicalBar_FillByteTbl to !Scratchram_GraphicalBar_FillByteTbl+EndAddress:
;  A table array containing the amount of fill for each byte (N bytes (including zero) full,
;  0 or 1 bytes a fraction, and then N bytes (including zero) empty), the address it ends at is:
;
;  EndAddress = (L + MLength + R) - 1
;
;  -L and R are 0 if set to 0 number of pieces, 1 otherwise on any nonzero values.
;  -MLength is how many middle tiles.
;
; -$00 to $01: The leftover fill amount. If bar isn't full, it will be #$0000, otherwise its
;  [RemainingFill = OriginalFill - EntireBarCapicity]. (overall calculation: RemainingFill = max((InputFillAmount - BarMaximumFull), 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawGraphicalBarSubtractionLoopEdition:
		LDX #$00
	.Leftend
		LDA !Scratchram_GraphicalBar_LeftEndPiece       ;\If left end does not exist, skip
		BEQ .Middle                                     ;/
		LDA $00                                         ;\Fillamount = Fillamount - MaxAmount (without writing to $00)
		SEC                                             ;|(SBC clears carry if an unsigned underflow occurs (x < 0))
		SBC !Scratchram_GraphicalBar_LeftEndPiece       ;|
		LDA $01                                         ;|
		SBC #$00                                        ;/
		BCC ..NotFull                                   ;>If Fillamount < MaxAmount, use all remaining fill amount of $00.
		
		..Full ;>Otherwise set the byte to max, and deduct the remaining fill amount by maximum.
			LDA !Scratchram_GraphicalBar_LeftEndPiece       ;\Full left end.
			STA !Scratchram_GraphicalBar_FillByteTbl        ;/
			LDA $00                                         ;\Fill amount deducted.
			SEC                                             ;|
			SBC !Scratchram_GraphicalBar_LeftEndPiece       ;|
			STA $00                                         ;|
			LDA $01                                         ;|
			SBC #$00                                        ;|
			STA $01                                         ;/
			BRA ..NextByte
		
		..NotFull
			LDA $00                                         ;\Take all the rest of $00.
			STA !Scratchram_GraphicalBar_FillByteTbl        ;|
			STZ $00                                         ;|
			STZ $01                                         ;/
		
		..NextByte
			INX                                             ;>Next tile byte
	.Middle
		LDA !Scratchram_GraphicalBar_MiddlePiece        ;\If middle does not exist, skip
		BEQ .RightEnd                                   ;|
		LDA !Scratchram_GraphicalBar_TempLength         ;|
		BEQ .RightEnd                                   ;/
		
		LDA !Scratchram_GraphicalBar_TempLength         ;\Loop counter for number of middle tiles.
		TAY                                             ;/
		
		..LoopMiddleTiles
			LDA $00                                         ;\Fillamount = Fillamount - MaxAmount (without writing to $00)
			SEC                                             ;|(SBC clears carry if an unsigned underflow occurs (x < 0))
			SBC !Scratchram_GraphicalBar_MiddlePiece        ;|
			LDA $01                                         ;|
			SBC #$00                                        ;/
			BCC ...NotFull                                   ;>If Fillamount < MaxAmount, use all remaining fill amount of $00.
		
			...Full ;>Otherwise set the byte to max, and deduct the remaining fill amount by maximum.
				LDA !Scratchram_GraphicalBar_MiddlePiece        ;\Full middle tile.
				STA !Scratchram_GraphicalBar_FillByteTbl,x      ;/
				LDA $00                                         ;\Fill amount deducted.
				SEC                                             ;|
				SBC !Scratchram_GraphicalBar_MiddlePiece        ;|
				STA $00                                         ;|
				LDA $01                                         ;|
				SBC #$00                                        ;|
				STA $01                                         ;/
				BRA ...NextByte
			
			...NotFull
				LDA $00                                         ;\Take all the rest of $00.
				STA !Scratchram_GraphicalBar_FillByteTbl,x      ;|
				STZ $00                                         ;|
				STZ $01                                         ;/
			
			...NextByte
				INX                                             ;>Next middle tile or to the right end.
				DEY                                             ;\Loop till all middle tiles done.
				BNE ..LoopMiddleTiles                           ;/
	.RightEnd
		LDA !Scratchram_GraphicalBar_RightEndPiece      ;\If right end does not exist, skip
		BEQ .Done                                       ;/

		LDA $00                                         ;\Fillamount = Fillamount - MaxAmount (without writing to $00)
		SEC                                             ;|(SBC clears carry if an unsigned underflow occurs (x < 0))
		SBC !Scratchram_GraphicalBar_RightEndPiece      ;|
		LDA $01                                         ;|
		SBC #$00                                        ;/
		BCC ..NotFull                                   ;>If Fillamount < MaxAmount, use all remaining fill amount of $00.
		
		..Full ;>Otherwise set the byte to max, and deduct the remaining fill amount by maximum.
			LDA !Scratchram_GraphicalBar_RightEndPiece      ;\Full right end.
			STA !Scratchram_GraphicalBar_FillByteTbl,x      ;/
			LDA $00                                         ;\Fill amount deducted.
			SEC                                             ;|
			SBC !Scratchram_GraphicalBar_RightEndPiece      ;|
			STA $00                                         ;|
			LDA $01                                         ;|
			SBC #$00                                        ;|
			STA $01                                         ;/
			BRA .Done
		
		..NotFull
			LDA $00                                         ;\Take all the rest of $00.
			STA !Scratchram_GraphicalBar_FillByteTbl,x      ;|
			STZ $00                                         ;|
			STZ $01                                         ;/
	.Done
		RTL
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Round away code
	;Input:
	; -Y: rounding status, obtained from CalculateGraphicalBarPercentage:
	; --$00 = not rounded to full or empty
	; --$01 = rounded to empty
	; --$02 = rounded to full
	;Output:
	; -$00-$01: Percentage, rounded away from 0 and max.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	RoundAwayEmpty:
		CPY #$01
		BEQ RoundAwayEmptyFull_RoundedEmpty
		RTL
	RoundAwayFull:
		CPY #$02
		BEQ RoundAwayEmptyFull_RoundedFull
		RTL
	RoundAwayEmptyFull:
		CPY #$00						;\check rounding flags (Y is only #$00 to #$02)
		BEQ .NotRounded						;|
		CPY #$01						;|
		BEQ .RoundedEmpty					;|
		BRA .RoundedFull					;/>Of course, if Y cannot be 0 and 1, it has to be 2, so no extra checks.
		
		.RoundedEmpty ;>Asar treats this sublabel as [RoundAwayEmptyFull_RoundedEmpty]
		REP #$20						;\Turn a number rounded to 0 to 1 as the amount filled
		INC $00							;|
		SEP #$20						;/
		BRA .NotRounded						;>and done

		.RoundedFull ;>Asar treats this sublabel as [RoundAwayEmptyFull_RoundedFull]
		REP #$20						;\Turn a number rounded to full to [FullAmount-1] (so if rounded to 62/62, display 61/62).
		DEC $00							;|
		SEP #$20						;/
		
		.NotRounded
		RTL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Other subroutines use by the outer subroutines (mainly math).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if !Setting_Beta32bitMultiplication != 0
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Unsigned 32bit * 32bit Multiplication
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Argument
	; $00-$03 : Multiplicand
	; $04-$07 : Multiplier
	; Return values
	; $08-$0F : Product
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;GHB's note to self:
	;$4202 = 1st Multiplicand
	;$4203 = 2nd Multiplicand
	;$4216 = Product
	;During SA-1:
	;$2251 = 1st Multiplicand
	;$2253 = 2nd Multiplicand
	;$2306 = Product

	if !CPUMode != 0
		!Reg4202 = $2251
		!Reg4203 = $2253
		!Reg4216 = $2306
	else
		!Reg4202 = $4202
		!Reg4203 = $4203
		!Reg4216 = $4216
	endif

	MathMul32_32:
			if !CPUMode != 0
				STZ $2250
				STZ $2252
			endif
			REP #$21
			LDY $00
			BNE +
			STZ $08
			STZ $0A
			STY $0C
			BRA ++
	+		STY !Reg4202
			LDY $04
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			STZ $0A
			STZ $0C
			LDY $05
			LDA !Reg4216		;>This is always spitting out as 0.
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $08
			LDA $09
			ADC !Reg4216
			LDY $06
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $09
			LDA $0A
			ADC !Reg4216
			LDY $07
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $0A
			LDA $0B
			ADC !Reg4216
			STA $0B
			
	++		LDY $01
			BNE +
			STY $0D
			BRA ++
	+		STY !Reg4202
			LDY $04
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			LDY #$00
			STY $0D
			LDA $09
			ADC !Reg4216
			LDY $05
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $09
			LDA $0A
			ADC !Reg4216
			LDY $06
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $0A
			LDA $0B
			ADC !Reg4216
			LDY $07
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $0B
			LDA $0C
			ADC !Reg4216
			STA $0C
			
	++		LDY $02
			BNE +
			STY $0E
			BRA ++
	+		STY !Reg4202
			LDY $04
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			LDY #$00
			STY $0E
			LDA $0A
			ADC !Reg4216
			LDY $05
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $0A
			LDA $0B
			ADC !Reg4216
			LDY $06
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $0B
			LDA $0C
			ADC !Reg4216
			LDY $07
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $0C
			LDA $0D
			ADC !Reg4216
			STA $0D
			
	++		LDY $03
			BNE +
			STY $0F
			BRA ++
	+		STY !Reg4202
			LDY $04
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			LDY #$00
			STY $0F
			LDA $0B
			ADC !Reg4216
			LDY $05
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $0B
			LDA $0C
			ADC !Reg4216
			LDY $06
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $0C
			LDA $0D
			ADC !Reg4216
			LDY $07
			STY !Reg4203
			if !CPUMode != 0
				STZ $2254	;>Multiplication actually happens when $2254 is written.
				NOP		;\Wait till multiplication is done
				BRA $00		;/
			endif
			
			STA $0D
			LDA $0E
			ADC !Reg4216
			STA $0E
	++		SEP #$20
			RTL
	endif
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; unsigned 16bit / 16bit Division
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; Arguments
	; $00-$01 : Dividend
	; $02-$03 : Divisor
	; Return values
	; $00-$01 : Quotient
	; $02-$03 : Remainder
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	MathDiv:	REP #$20
			ASL $00
			LDY #$0F
			LDA.w #$0000
	-		ROL A
			CMP $02
			BCC +
			SBC $02
	+		ROL $00
			DEY
			BPL -
			STA $02
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

	MathDiv32_16:	REP #$20
			ASL $00
			ROL $02
			LDY #$1F
			LDA.w #$0000
	-		ROL A
			BCS +
			CMP $04
			BCC ++
	+		SBC $04
			SEC
	++		ROL $00
			ROL $02
			DEY
			BPL -
			STA $04
			SEP #$20
			RTL
	if !CPUMode == 0
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; 16bit * 16bit unsigned Multiplication
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		; Argusment
		; $00-$01 : Multiplicand
		; $02-$03 : Multiplier
		; Return values
		; $04-$07 : Product
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

		MathMul16_16:	REP #$20
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
				RTL
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
	
		MathMul16_16:	STZ $2250
				REP #$20
				LDA $00
				STA $2251
				ASL A
				LDA $02
				STA $2253
				BCS +
				LDA.w #$0000
		+		BIT $02
				BPL +
				CLC
				ADC $00
		+		CLC
				ADC $2308
				STA $06
				LDA $2306
				STA $04
				SEP #$20
				RTL
	endif