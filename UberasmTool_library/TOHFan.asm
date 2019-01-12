incsrc "../TOHFanDefines/Defines.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Insert this in uberasm tool's library and call it in "level" as "main".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TowerOfHeavenFan:
	LDA !Freeram_TowerOfHeavenFanned	;\If player not touching the air current of the fan (which sets the I bit), reset
	BIT.b #%00000001			;|the sound bit and return.
	BEQ .OffFan				;/
.OnFan
	LDA !Freeram_TowerOfHeavenFanned	;\Clear I bit if set so that it stops performing the airblow when player leaves block.
	AND.b #%11111110			;|
	STA !Freeram_TowerOfHeavenFanned	;/
	LDA !Freeram_TowerOfHeavenFanned	;\If S bit is set, don't play the sound again.
	AND.b #%00000010			;|
	BNE ..AlreadySfx			;/
	LDA #$17				;\Play sound effect
	STA $1DFC|!addr				;/
	LDA !Freeram_TowerOfHeavenFanned	;\Set S bit so it knows its already played
	ORA.b #%00000010			;|
	STA !Freeram_TowerOfHeavenFanned	;/
..AlreadySfx
.AccelUp
;	LDA $xx					;\If you have a menu interface or other things
;	ORA $yy					;|that uses the B button, remove the semicolons
;	BNE ..NoEffectController		;/to prevent the fans from messing up your menu controls...
	LDA #$80
	if !Setting_HoldJump == 1
		TRB $15				;>Clear the hold jump button
	elseif !Setting_HoldJump == 2
		TSB $15
	endif
;..NoEffectController				;>...And this one too.
	LDA $7D					;>Load Y speed.
	BPL ..NoCompare				;>Due to an overflow, BCC applies lower than #$80 as well.
	CMP #$A8				;\>Fastest upwards speed
	BCC .Return				;/>If faster than that, don't continue going upwards
..NoCompare
	SEC					;\Accelerate upwards
	SBC #!FanAccel				;|
	STA $7D					;/
	BRA .Return
.OffFan
	LDA !Freeram_TowerOfHeavenFanned	;\Allow sfx when the player gets back on
	AND.b #%11111101			;|fan.
	STA !Freeram_TowerOfHeavenFanned	;/
.Return
	RTL