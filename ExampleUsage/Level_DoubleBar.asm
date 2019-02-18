;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;"GraphicalBarELITE.asm" in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

;What this does is display 2 bars overlapping each other, what you see in modern games
;such as street fighter, when a character takes damage, the "secondary" amount of fill
;represents the amount of HP loss (damage indicator).

;How this works: DrawGraphicalBar is executed twice, with the first one having its byte
;table moved into another location to have both being stored and not overwritten. Then,
;when trying to convert the fill values into tile numbers, it check BOTH corresponding
;numbers (corresponding 8x8 bytes) to determine what tile number number (that can show
;2 fills) to use.
;
;For easy testing, up and down on the D-pad adjust FirstFill, left and right adjusts
;SecondFill.
;
;Notes:
;
; -Be very careful, that since layer tiles cannot overlap each other, they are merged
;  into the same 8x8 tile, eating up large amount of graphic space due to combinations.
;  I highly recommend using Sprite OAM, since sprite tiles can literally overlap each
;  other without combining.
;
; --The amount of fill here are assumed 8-bit, because of the reason above.
;
; -This is not to be confused with the flicker between 2 bars every frame (this so-called
;  "transparency effect"), since this can display 2 of them at the same time/frame and
;  not alternate.
;
; -When setting up the 2 bars, they both must have the same settings (left end
;  pieces, middle pieces, length, right end pieces being the same between the two), so that
;  they synchronize on the number of pieces and sections.
;
; -If you plan on making different lengths of the bar, I recommend changing the
;  code (or actually the defines being added by !GraphicalBar_TotalTileUsed) to have FirstBar's output
;  location be moved by a fixed value that is the longest bar you could possibly have in
;  your game to avoid overwriting the other bar. For example: A longest bar with 10 tile
;  bytes total (2 end tiles and 8 for middle length), you would have ("$" sign means hex):
;
;   FirstFillLocation = SecondFillLocation + $0A
;
;  It would look like this (each character represents a byte address location
;  in this ASCII art):
;
;  [========][========]
;
;  So should your bar be shorter, it would look like this ("." symbol means leftover byte):
;
;  [====]....[====]....

incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

main:
.IncrementDecrementTest
..Vertical
	LDA $15				;\Pressing up and down to adjust firstfill value
	BIT.b #%00001000		;|
	BNE ...Up			;|
	BIT.b #%00000100		;|
	BNE ...Down			;/
	BRA ..Horizontal		
	
	...Up
	LDA !Freeram_FirstQuantity
	CMP #!DoubleBar_MaxQuantity
	BEQ ..Horizontal
	INC A
	STA !Freeram_FirstQuantity
	BRA ..Horizontal
	
	...Down
	LDA !Freeram_FirstQuantity
	BEQ ..Horizontal
	DEC A
	STA !Freeram_FirstQuantity
	
	..Horizontal
	LDA $15				;\SecondFill control
	BIT.b #%00000010		;|
	BNE ...Left			;|
	BIT.b #%00000001		;|
	BNE ...Right			;/
	BRA .DisplayFillAmount
	
	...Left
	LDA !Freeram_SecondQuantity
	BEQ .DisplayFillAmount
	DEC A
	STA !Freeram_SecondQuantity
	BRA .DisplayFillAmount
	
	...Right
	LDA !Freeram_SecondQuantity
	CMP #!DoubleBar_MaxQuantity
	BEQ .DisplayFillAmount
	INC A
	STA !Freeram_SecondQuantity
	
.DisplayFillAmount
	;This displays the hex numbers representing the two fills in the bar.
	;Only works with Super Status Bar patch.
	if !StatusBarFormat == $02
		LDA !Freeram_FirstQuantity				;\every 16th number increments the 1st digit.
		LSR #$04						;|
		STA !FirstFillHexValDisplayPos				;/
		LDA !Freeram_FirstQuantity				;\limit it to #$00-#$0F on 2nd digit digit.
		AND #$0F						;|
		STA !FirstFillHexValDisplayPos+(1*!StatusBarFormat)	;/

		LDA !Freeram_SecondQuantity				;\every 16th number increments the 1st digit.
		LSR #$04						;|
		STA !SecondFillHexValDisplayPos				;/
		LDA !Freeram_SecondQuantity				;\limit it to #$00-#$0F on 2nd digit digit.
		AND #$0F						;|
		STA !SecondFillHexValDisplayPos+(1*!StatusBarFormat)	;/
	endif

.GraphicalDoubleBarTest
	if !DoubleBar_DisplayType == 0
		LDA !Freeram_SecondQuantity		;\If secondfill is less than firstfill, don't show secondfill.
		CMP !Freeram_FirstQuantity		;|over firstfill
		BCC .Frame0			;/
		
		LDA $13			;\Frame counter [0-255] MOD 2
		AND.b #%00000001	;/
		BEQ .Frame0
		
		.Frame1
		;Odd frame
		;REP #$20
		LDA !Freeram_SecondQuantity
		BRA .Write
		
		.Frame0
		;Even frame
		;REP #$20
		LDA !Freeram_FirstQuantity
		
		.Write
		STA !Scratchram_GraphicalBar_FillByteTbl
	else
		..GraphicalDoubleBarFirstFill
		LDA !Freeram_FirstQuantity				;\Amount of fill for first fill
		STA !Scratchram_GraphicalBar_FillByteTbl	;|
	endif
	JSR GetPercentageQuantity
	LDA #!Default_LeftPieces				;\Left end pieces
	STA !Scratchram_GraphicalBar_LeftEndPiece		;/
	LDA #!Default_MiddlePieces				;\Middle pieces
	STA !Scratchram_GraphicalBar_MiddlePiece		;/
	LDA.b #!Default_MiddleLength				;\Middle length
	STA !Scratchram_GraphicalBar_TempLength			;/
	LDA #!Default_RightPieces				;\Right end pieces
	STA !Scratchram_GraphicalBar_RightEndPiece		;/
	JSL GraphicalBarELITE_CalculateGraphicalBarPercentage
	if !DoubleBar_RoundAway != 0
		JSR RoundAway
	endif
	if and(notequal(!DoubleBar_DisplayType, 0), equal(!StatusBarFormat, $02))
		LDA $00								;\every 16th number increments the 1st digit.
		LSR #$04							;|
		STA !FirstFillPercentHexValDisplayPos				;/
		LDA $00								;\limit it to #$00-#$0F on 2nd digit digit.
		AND #$0F							;|
		STA !FirstFillPercentHexValDisplayPos+(1*!StatusBarFormat)	;/
	endif
	JSL GraphicalBarELITE_DrawGraphicalBar
	if !DoubleBar_DisplayType == 0
		JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles
	else
		...TransferFirstFillBar ;>FirstFill will be located just after SecondFill in memory address: <SecondFill_Table><FirstFill_Table>
		PHB
		REP #$30
		LDA.w #!GraphicalBar_TotalTileUsed-1									;>Number of bytes to transfer, -1 (because byte 0 is included)
		LDX.w #!Scratchram_GraphicalBar_FillByteTbl							;>Source address
		LDY.w #!Scratchram_GraphicalBar_FillByteTbl+!GraphicalBar_TotalTileUsed					;>Destination address
		MVN (!Scratchram_GraphicalBar_FillByteTbl>>16), (!Scratchram_GraphicalBar_FillByteTbl>>16)	;>Move them
		SEP #$30
		PLB

		..GraphicalDoubleBarSecondFill
		;Thankfully the graphical bar routines does not mess up the scratch RAM inputs (other than !Scratchram_GraphicalBar_FillByteTbl), therefore
		;you only need to set them once.
		LDA !Freeram_SecondQuantity					;\Amount of fill for second fill
		STA !Scratchram_GraphicalBar_FillByteTbl		;/
		JSR GetPercentageQuantity
		JSL GraphicalBarELITE_CalculateGraphicalBarPercentage
		if !DoubleBar_RoundAway != 0
			JSR RoundAway
		endif
		if !StatusBarFormat == $02
			LDA $00								;\every 16th number increments the 1st digit.
			LSR #$04							;|
			STA !SecondFillPercentHexValDisplayPos				;/
			LDA $00								;\limit it to #$00-#$0F on 2nd digit digit.
			AND #$0F							;|
			STA !SecondFillPercentHexValDisplayPos+(1*!StatusBarFormat)	;/
		endif
		JSL GraphicalBarELITE_DrawGraphicalBar					;>Get amount of fill.
		JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTilesDoubleBar
	endif
	LDA.b #!Default_GraphicalBarPosition
	STA $00
	LDA.b #!Default_GraphicalBarPosition>>8
	STA $01
	LDA.b #!Default_GraphicalBarPosition>>16
	STA $02
	if !StatusBar_UsingCustomProperties != 0
		LDA.b #!Default_GraphicalBarProperties
		STA $03
		LDA.b #!Default_GraphicalBarProperties>>8
		STA $04
		LDA.b #!Default_GraphicalBarProperties>>16
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
	;Display both fill percentage when !DoubleBar_DisplayType is 0
	;due to every other frame, doesn't have the info for both at the same frame.
	if !DoubleBar_DisplayType == 0
		.FirstFillPercentageRapidFlicker
		LDA !Freeram_FirstQuantity				;\Amount of fill for first fill
		STA !Scratchram_GraphicalBar_FillByteTbl		;/
		JSR GetPercentageQuantity
		JSL GraphicalBarELITE_CalculateGraphicalBarPercentage
		if !DoubleBar_RoundAway != 0
			JSR RoundAway
		endif
		if !StatusBarFormat == $02
			LDA $00								;\every 16th number increments the 1st digit.
			LSR #$04							;|
			STA !FirstFillPercentHexValDisplayPos				;/
			LDA $00								;\limit it to #$00-#$0F on 2nd digit digit.
			AND #$0F							;|
			STA !FirstFillPercentHexValDisplayPos+(1*!StatusBarFormat)	;/
		endif
		.SecondFillPercentageRapidFlicker
		LDA !Freeram_SecondQuantity					;\Amount of fill for second fill
		STA !Scratchram_GraphicalBar_FillByteTbl		;/
		JSR GetPercentageQuantity
		JSL GraphicalBarELITE_CalculateGraphicalBarPercentage
		if !DoubleBar_RoundAway != 0
			JSR RoundAway
		endif
		if !StatusBarFormat == $02
			LDA $00								;\every 16th number increments the 1st digit.
			LSR #$04							;|
			STA !SecondFillPercentHexValDisplayPos				;/
			LDA $00								;\limit it to #$00-#$0F on 2nd digit digit.
			AND #$0F							;|
			STA !SecondFillPercentHexValDisplayPos+(1*!StatusBarFormat)	;/
		endif
	endif
	RTL
	;-------------------------------------------------------------------------------------------------
	GetPercentageQuantity:
	LDA #$00						;\High byte of above. Should your value here is 8-bit or only 1 byte long,
	STA !Scratchram_GraphicalBar_FillByteTbl+1		;/use [LDA #$00 : STA !Scratchram_GraphicalBar_FillByteTbl+1].
	LDA #!DoubleBar_MaxQuantity				;\Max quantity (example: max HP). Can be a fixed value (#$) or adjustable RAM in-game.
	STA !Scratchram_GraphicalBar_FillByteTbl+2		;/
	LDA #$00						;\High byte above, same format as <Value_high_byte>, so do the same
	STA !Scratchram_GraphicalBar_FillByteTbl+3		;/as that if your value is 8-bit.
	RTS
	if !DoubleBar_RoundAway != 0
		RoundAway:
		;output:
		;$00 = percentage rounded away from empty and full.
		CPY #$00						;\check rounding flags (Y is only #$00 to #$02)
		BEQ .Done						;|
		CPY #$01						;|
		BEQ .RoundedEmpty					;|
		BRA .RoundedFull					;/>Of course, if Y cannot be 0 and 1, it has to be 2, so no extra checks.
		
		.RoundedEmpty
		REP #$20
		INC $00							;>if fill amount is a nonzero less than 0.5, make it display fillvalue = 1 to not display "empty".
		SEP #$20
		BRA .Done						;>and done

		.RoundedFull
		REP #$20
		DEC $00							;>if fill amount is at least Max-0.5 and less than Max, make it display fillvalue = max-1 to not display "full".
		SEP #$20
		
		.Done
		RTS
	endif