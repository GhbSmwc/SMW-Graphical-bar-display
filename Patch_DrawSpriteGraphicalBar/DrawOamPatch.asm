!PatchMode	= 0
 ;^0 = patch
 ; 1 = uninstall

!OAMSlot = $B0
 ;^Oam to use, NOTE: Must be divisible by 4 due to bytes arranged in: XXXXXXXX YYYYYYYY NNNNNNNN YXPPCCCT (and repeats 127 more times).
 ; $B0 is what the megaman X HP bar uses (starting at)

;This is the patch version that draws a OAM-based sprite graphical bar, without taking up any sprite slots.

;This patch is based on the mega man X HP bar by anonimzwx (https://www.smwcentral.net/?p=profile&id=20832 ), and also the suggestion
;by lx5: https://discord.com/channels/161245277179609089/161247652946771969/827647409429151816

;And yes, this may conflict with some sprite HUD patches because $00A2E6 is somewhat a common address to use.

;You'd think this can be converted to just an uberasm tool code, but this is wrong. Uberasm tool code runs in between after transferring OAM
;RAM ($0200-$041F and $0420-$049F) to SNES register (the code at $008449 does this) and before calling $7F8000 (clears OAM slots), so
;therefore, writing OAM on uberasm tool will get cleared before drawn.
;Don't touch these unless you know what you're doing
	;Get defines
		incsrc "SharedSub_Defines/SubroutineDefs.asm"
	;Check if the user defined oam slot to use is valid
		assert !OAMSlot%4 == 0, "Invalid OAM slot number."
;Macros
	macro findSlot()
		?loop:
			;Output: X = Slot to use
			CPX #$0200			;\If slot index reaches the end, break (no slots available)
			BEQ ?break			;/
			LDA $0201|!base,x		;\If slot unused, use that slot
			CMP #$F0			;|
			BEQ ?break			;/
			INX #4				;\Next slot
			BRA ?loop			;/
		?break:
	endmacro

;Patch stuff
if !PatchMode == 0
	org $00A2E6				;>$00A2E6 is the code that runs at the end of the frame, after ALL sprite tiles are written.
	autoclean JML DrawGraphicalBar
else
	if read4($00A2E6) != $028AB122			;22 B1 8A 02 -> JSL.L CODE_028AB1
		autoclean read3($00A2E6+1)
	endif
	org $00A2E6
	JSL $028AB1
endif

if !PatchMode == 0
	freecode
	;;;;;;;;;;;;;;;;;;;;
	;Main code
	;;;;;;;;;;;;;;;;;;;;
	DrawGraphicalBar:
		.MainCode
			;insert code here.
		.Restore
			JSL $028AB1		;>Restore the JSL
			JML $00A2EA		;>Continue onwards
			
	;;;;;;;;;;;;;
	;Subroutines
	;;;;;;;;;;;;;
		FindNFreeOAMSlot:
			;Input: $04 = Number of slots open to search for
			;Output: Carry = Set if not enough slots found, Clear if enough slots found
			PHY
			LDY #$0000					;>Open slot counter
			LDX #$0010					;>skip the first four slots
			.loop:						;>to avoid message box conflicts
				CPX #$0200				;\If all slots searched, there is not enough
				BEQ .notEnoughFound			;/open slots being found

				LDA $0201|!base,x			;\If slot used, that isn't empty
				CMP #$F0				;|
				BNE ..notFree				;/
				INY					;>Otherwise if it is unused, count it
				CPY $04					;\If we find n slots that are free, break
				BEQ .enoughFound			;/
				..notFree:
					INX #4				;\Check another slot
					BRA .loop			;/
			.notEnoughFound:
				SEC
				BRA .Done
			.enoughFound:
				CLC
			.Done
				PLY
				RTS
endif