;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;all files in the routine folder in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

;This code tests out a range-based bar, where the bar measures a value within a specified range,
;therefore acting like HTML's "meter" tag where MIN is mapped to display 0 and MAX is displayed
;as (MAX-MIN), this uses my MapRangeToStartAt0 routine. This example test functions like a:
;-boss's phase-based HP bar, when the bar gets depleted, it refills
; and decrements a number next to it, therefore the boss switches phase.
;-EXP progress bar: as you gain EXP, the bar fills up, once full, the number next to it
; increments and the bar will wrap back to 0 and fills up the remaining increase since
; the previous bar
;
;Unlike storing only the quantity as 0 to Max per range individually (the next bar is 0-Max
;instead of continuing from Max), the quantity is total-based and handles all the ranges.
;Addition and subtraction of the quantity can carry over to the next range instead of
;stopping at the end of each ranges, for example:
;
;The boss have 200 HP at the start:
;-Phase 1: 101 to 200 HP
;-Phase 2: 1 to 100 HP
;When the boss have 105 HP and takes 10 damage, HP will now be 95 HP instead of 100,
;thus the damage have carried over to phase 2. Unlike having this:
;HP starts at 100 on phase 1, and a counter stored indicating the phase number, later
;in the boss fight, with 5 HP left and takes 10 damage, phase 2 triggers (update the
;phase number) and HP is guaranteed to fully refill back to 100, regardless of how much
;HP that is less than damage, which means 5 remaining damage have been nullified.
;
;Same goes with EXP, but filling upwards at it progress. After leveling up, you may still
;have remaining EXP, so the EXP increase will continue on that next level instead of always
;stopping the increase once the next level is reached.
;
;Cave story, for example, do not carry EXP over to the next level, thus the next bar to
;fill up is always empty: https://cavestory.fandom.com/wiki/Game_objects#Experience

;Anyway, feel free to use this code as a template for your hack.
;The range table is at the bottom.

incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

!GraphicalBar_RangeBased_EvenDistribution = 0
 ;^0 = Each range can have different sizes, based on a table
 ;     defining the sizes.
 ; 1 = Each range have the same sizes, and the bar advances
 ;     every N amount.

!GraphicalBar_RangeBased_RangeSize = 5
 ;^How big each range is. Only used when 
 ; !GraphicalBar_RangeBased_EvenDistribution = 1.
!GraphicalBar_RangeBased_HighestSegment = 5
 ;^The maximum segment when !GraphicalBar_RangeBased_EvenDistribution = 1.
 ; Once reached, displays a full bar.

!ControlerToChangeQuantity = $15
;^$15 = press and hold
; $16 = 1-frame presses (tap repeatedly)


;This is a simple test ASM using the graphical bar.
;best tested using uberasm tool.

main:
if !CPUMode != 0
	%invoke_sa1(mainSA1)
	RTL
	mainSA1:
endif
;Increment or decrement quantity based on up/down controls:
.Control
	LDA !ControlerToChangeQuantity				;\Control quantity based on tapping up and down.
	BIT.b #%00000100					;|
	BNE ..Down						;|
	BIT.b #%00001000					;|
	BNE ..Up						;|
	REP #$30						;|
	BRA +							;|

	..Down
		REP #$30						;|
		LDA !Freeram_RangeBasedValue				;|
		BEQ +							;|
		DEC A							;|
		STA !Freeram_RangeBasedValue				;|
		BRA +							;|

	;Here is what limits the quantity from exceeding a certain value
	..Up
		REP #$30						;|
		LDA !Freeram_RangeBasedValue				;|
		if !GraphicalBar_RangeBased_EvenDistribution == 0
			CMP.w RangeTableEnd-2				;|>Get last number in the table (#Label would pull out an address instead the number in the address)
		else
			CMP.w #(!GraphicalBar_RangeBased_RangeSize*!GraphicalBar_RangeBased_HighestSegment)
		endif
		BCS +							;|
		INC A							;|
		STA !Freeram_RangeBasedValue				;/
	
	+
	if !GraphicalBar_RangeBased_EvenDistribution == 0
		;This here is where it calculate what interval and how much fill
		;in between two numbers starting from the lower number of the two.
		.IntervalRange
			LDX.w #(RangeTableEnd-RangeTable)-2			;\Search what range quantity is on.
			..Loop							;|
				LDA !Freeram_RangeBasedValue			;|Starts at the last index, compares 
				CMP RangeTable,x				;|if quantity >= to the number in the table
				BCS ..IntervalFound				;|and found if true, will have that index.
				DEX #2						;|
				BPL ..Loop					;|
			INX #2							;|>Don't use index-2! 0 is the lowest that is valid.

			..IntervalFound
				;Prevent index from exceeding beyond table.
				TXA						;|\Store index*2 here
				STA !Scratchram_WhatRange			;|/
				CPX.w #((RangeTableEnd-RangeTable)-4)		;|
				BCC ..ValidRange				;|
				LDX.w #((RangeTableEnd-RangeTable)-4)		;|
		
			..ValidRange
				LDA RangeTable,x				;|\Minimum
				STA $02						;|/
				+
			..WriteIntervalNumber
				;Writes what bar number onto HUD
				SEP #$20					;|\Take 16-bit interval number, take only the low byte, and write to status bar (up to 9 is displayed)
				LDA !Scratchram_WhatRange			;||(SEP #$20 instead of #$30 to keep X and Y register's high bytes intact)
				LSR						;||To display more than 9, consider using hexdec routine.
				STA !Interval_Write_Pos_Tile			;||
				if !StatusBar_UsingCustomProperties != 0
					LDA.b #!Default_StatusBar_TilePropertiesSetting
					STA !Interval_Write_Pos_Properties
				endif
				REP #$20					;|/
		
			..InsertMinMax
				INX #2							;|>Next number in table (minimum X=$0000 after this).
				LDA RangeTable,x					;|\Maximum
				STA $04							;|/
				LDA !Freeram_RangeBasedValue				;|\Quantity
				STA $00							;//
				SEP #$30
			
				JSL GraphicalBarOtherRoutines_MapRangeToStartAt0	;>Convert range to where MIN is 0.
				REP #$20						;\Insert mapped range and value.
				LDA $00							;|
				STA !Scratchram_GraphicalBar_FillByteTbl		;|
				LDA $04							;|
				STA !Scratchram_GraphicalBar_FillByteTbl+2		;|
				SEP #$20						;/
	else
		.EachSegmentSameSize
			LDA !Freeram_RangeBasedValue				;\Quantity/RangeSize
			STA $00							;|$00 Quotent = what range
			LDA.w #!GraphicalBar_RangeBased_RangeSize		;|$02 Remainder = amount in bar.
			STA $02							;|
			SEP #$30						;|
			JSL GraphicalBarELITE_MathDiv				;/
			REP #$20
		
			;Prevents range "index" from exceeding max.
			LDA $00
			CMP.w #!GraphicalBar_RangeBased_HighestSegment
			BCC ..NotHighestSegment

			..HighestSegment
				LDA.w #(!GraphicalBar_RangeBased_HighestSegment*!GraphicalBar_RangeBased_RangeSize)
				STA !Scratchram_GraphicalBar_FillByteTbl
				BRA ..WriteMaxRange					;>Tell it to display QuantityAtHighest*5 out of 5 (beyond full bar but displays 100% anyways).
		
			;process handling the remainder as fill in bar and quotient
			;to display as a single-digit number.
			..NotHighestSegment
				LDA $02							;\Remainder as fill in bar
				STA !Scratchram_GraphicalBar_FillByteTbl		;/
		
			..WriteMaxRange
				LDA.w #!GraphicalBar_RangeBased_RangeSize
				STA !Scratchram_GraphicalBar_FillByteTbl+2
		
			..WriteQuotient
				;Writes what bar number onto HUD
				SEP #$20
				LDA $00
				STA !Interval_Write_Pos_Tile
				if !StatusBar_UsingCustomProperties != 0
					LDA.b #!Default_StatusBar_TilePropertiesSetting
					STA !Interval_Write_Pos_Properties
				endif
	endif

	;Process graphical bar.
	.ProcessGraphicalBar
		LDA.b #!Default_LeftPieces					;\Input amount of pieces in each of the 3 types of sections.
		STA !Scratchram_GraphicalBar_LeftEndPiece			;|
		LDA.b #!Default_MiddlePieces					;|
		STA !Scratchram_GraphicalBar_MiddlePiece			;|
		LDA.b #!Default_RightPieces					;|
		STA !Scratchram_GraphicalBar_RightEndPiece			;/
		LDA.b #!Default_MiddleLength					;\length (number of middle tiles)
		STA !Scratchram_GraphicalBar_TempLength				;/
		JSL GraphicalBarELITE_CalculateGraphicalBarPercentage		;>Get percentage
		JSL GraphicalBarELITE_DrawGraphicalBarSubtractionLoopEdition	;>get bar values.
		STZ $00								;>Use Level-layer3 tileset
		JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
		LDA.b #!Default_GraphicalBar_Pos_Tile
		STA $00
		LDA.b #!Default_GraphicalBar_Pos_Tile>>8
		STA $01
		LDA.b #!Default_GraphicalBar_Pos_Tile>>16
		STA $02
		if !StatusBar_UsingCustomProperties != 0
			LDA.b #!Default_GraphicalBar_Pos_Properties
			STA $03
			LDA.b #!Default_GraphicalBar_Pos_Properties>>8
			STA $04
			LDA.b #!Default_GraphicalBar_Pos_Properties>>16
			STA $05
			if !Default_LeftwardsBar == 0
				LDA.b #!Default_StatusBar_TilePropertiesSetting			;\Properties
			else
				LDA.b #(!Default_StatusBar_TilePropertiesSetting|(!Default_LeftwardsBar<<6))
			endif
			STA $06								;/
		endif
		if !Default_LeftwardsBar == 0
			if !StatusBarFormat = $01
				JSL GraphicalBarWriteToStatusBar_WriteBarToHUD			;>Write to status bar
			else
				JSL GraphicalBarWriteToStatusBar_WriteBarToHUDFormat2		;>Write to status bar
			endif
		else
			if !StatusBarFormat = $01
				JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwards
			else
				JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwardsFormat2
			endif
		endif
		RTL
	
	;These are the range values. Each number in table is the minimum to be in a
	;particular range (and be less than the next number, exclusively).
	;
	;NOTE:
	;-Make sure the numbers are in increasing order and no duplicates.
	;-Make sure all your numbers are in between "RangeTable" and
	; "RangeTableEnd".
	;-The first number must be zero.
	;-When used as a boss health bar, you must have the last number
	; be its maximum HP.
	;
	;-If you are using this as a phase-based boss health bar, be aware that as the boss
	; starts with full HP, this value will equal to the last number in the table.
	; Since that last number is "separate" and not part of the previous index number
	; (because all numbers are minimums to that index, not max),  you'll have an additional
	; index that last for 1 HP.
	;
	; This means you have a first phase that is very short, lasting until the boss
	; takes its first hit.
	;
	; To prevent this, simply check if the index number is beyond the second-last item,
	; in this case, if index_x is >= $04, then cap it to $02. This will cause the
	; full HP amount to be included in the intended first phase at index_x = $02.
	; See the comments to the right of the table.
	;
	; This NumberOfSegmentIndexes+1 range also applies to fixed range size as well
	; (GraphicalBar_RangeBased_EvenDistribution = 1), due to the fact that the MODULO
	; operations with integers results an output in the range of 0 to Divisor-1 (example:
	; Y = X MOD 5 results Y to only be 0, 1, 2, 3 and 4 but not 5). Again limit the index
	; should the quantity be equal (or greater than as a failsafe; BCS) to the last number.
	;
	;--Although you could simply have it so the last number in table is +1 from
	;  the maximum HP (boss starts at 200 HP, however the table have 201 as the last
	;  number), however it would cause the bar to never be 100% full (display 1-less from max).
	;
	;-When used as a level progression bar, thanks to the above information,
	; it can be treated as the maximum exp and the last level up in which you can
	; no longer gain any additional EXP on that new level (this final level have
	; only have 1 number, not only a minimum to be that level but also the maximum).
	;
	;-Because every number is the minimum, to be on a particular index, the bar will
	; never display 100%, rather up to 99%-ish, since the highest number without advancing
	; is [NextMinimum - 1], which is 1-less from a full bar. The only time the bar is full
	; is once the last number in table is reached.
	;
	; Also, the index number is 16-bit, because:
	; -The values in the table occupy 2 bytes, so the index is [Index = ItemNumber*2]
	; -The loop uses BPL to know if all the items in the table have been checked.
	;  Causing the index number to be treated as "signed".
	; -Therefore if each item in the table were 1 byte (8-bit), up to 128 items will work properly
	; -But the table have 16-bit numbers, so it is cut in half again, up to 64 items are allowed.
	; -Therefore, you can have 16,384 ($4000) items in table on a 16-bit index and 16-bit table numbers.
	
	;Table comments here assumes you're using it as a phase-based boss HP bar.
	if !GraphicalBar_RangeBased_EvenDistribution == 0
		RangeTable:
		dw 0			;>item number 0 (index_x = $00) (quantity: 0-100)
		dw 101			;>item number 1 (index_x = $02) (quantity: 101-199 (supposed to be 101-200, but all numbers, even the last, are minimums), the intended first phase)
		dw 200			;>item number 2 (index_x = $04) (quantity: 200 (max health), the unintended "full health phase")
		RangeTableEnd:
	endif