;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Round away code (away from full and empty)
;
;Reason for splitting the ASM files and/or having duplicate
;routines is because of pixi's limitations of handling routines
;
;Input:
; -Y: rounding status, obtained from CalculateGraphicalBarPercentage:
; --$00 = not rounded to full or empty
; --$01 = rounded to empty
; --$02 and up = rounded to full
;Output:
; -$00-$01: Percentage, rounded away from 0 and max.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
?RoundAwayEmptyFull:
	CPY #$02		;>Needed so it compares Y instead of A depending on the codes prior calling this subroutine
	BNE ?.No
	
	?.RoundFull
		REP #$20
		DEC $00
		SEP #$20
	?.No
	?.Done
		RTL