?CalculateGraphicalBarPercentageRoundUp:
	%GraphicalBarCalculateGraphicalBarPercentageRoundDown()
	REP #$20
	LDA $04				;\If remainder is zero, (meaning exactly an integer), don't increment
	BEQ ?.NoRoundUp			;/
	?.RoundUp
		INC $00				;>Otherwise if there is a remainder (between Quotient and Quotient+1), use Quotient+1
		if !CPUMode != 0
			LDA $00				;\Preserve rounded quotient
			PHA				;/
		endif
		%GraphicalBarGetMaxBarInAForRoundToMaxCheck()
		if !CPUMode != 0
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
		BNE ?.NoRoundToMax
		LDY #$02
	?.NoRoundToMax
	?.NoRoundUp
	SEP #$20
	RTL