;This contains misc routines that least likely going to be used
;in most cases.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Convert range of two numbers as a percentage
;Basically this is the same as HTML's "meter" tag, allowing you to map
;MIN number to 0 on the bar and MAX to a full bar. If the number is out
;of range, would either display empty on the bar or full. This is useful
;for things like an EXP progress bar or "continuous" stacked health bars
;(damage that carry over to the next bar should the current bar is
;subtracted more than the value itself contains, instead of always a
;full bar on the next regardless of the damage). Here is an example:
;
;Boss have 200 HP as max HP, and you want each bar to represent a phase:
; 1-100 final phase
; 101-200 first phase
;
; Later in the fight, the boss have 105 HP, after taking 10 damage, the boss
; now have 95 HP. This results the bar to display:
;
; 105 HP -> 5% as bar number 2
; 95 HP -> 95% as bar number 1
;
;Calculated by taking the quantity and max quantity and subtracting both
;by min quantity, for example:
;
; 105 out of 200 HP -> (105-100) out of (200-100) HP -> 5 out of 100
; 95 out of 200 HP -> 95 out of (200-100) HP -> 95 out of 100
;
;Note that this routine handles ONE range at a time, you have to
;make tables and check if the boss's HP is between two numbers
;in a table.
;
;Another note is that if you have equal range sizes (2100, 200, 300)
;you can use division and the remainder is the fill amount while the
;quotient is what bar the HP number is on.
;
;Input:
; -$00-$01 (16-bit): The quantity
; -$02-$03 (16-bit): The minimal quantity
; -$04-$05 (16-bit): The maximal quantity
;Output:
; -$00-$01 (16-bit): The quantity, after subtracted by minimal quantity
;                    (If quantity is < MIN, then return 0).
; -$02-$03 (16-bit): The minimal quantity (same number as entered before)
; -$04-$05 (16-bit): The maximal quantity (subtracted by minimal quantity).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MapRangeToStartAt0:
	REP #$20
	LDA $02			;\If min is 0, no subtraction needed
	BEQ .Done		;/
	LDA $00			;\Map quantity to be zero-based
	SEC			;|
	SBC $02			;|
	BCS +			;|>SBC clears carry if unsigned underflow, check if Quantity - MIN results in a negative, then
	LDA #$0000		;| bottom out at 0 instead.
	+
	STA $00			;/
	LDA $04			;\Same goes with max.
	SEC			;|
	SBC $02			;|
	STA $04			;/
	
	.Done
	SEP #$20
	RTL