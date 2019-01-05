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


 if !sa1 == 0
  !FirstFillHexValDisplayPos           = $7FA036
 else
  !FirstFillHexValDisplayPos           = $404036
 endif
 ;Position of a hex number display of the amount of firstfill.

 if !sa1 == 0
  !SecondFillHexValDisplayPos          = $7FA03C
 else
  !SecondFillHexValDisplayPos          = $40403C
 endif
 ;Same as above, but secondfill.

;RAM testing for how much fill in bar.
 !Freeram_FirstFill = $58
 ;^[1 byte] The amount of fill for the first fill

 !Freeram_SecondFill = $5C
 ;^[1 byte] The amount of fill for the second fill

;Note that this does not use CalculateGraphicalBarPercentage, to ease the simplicity (it is already complex).

main:
.IncrementDecrementTest
..Vertical
	LDA $16				;\Pressing up and down to adjust firstfill value
	BIT.b #%00001000		;|
	BNE ...Up			;|
	BIT.b #%00000100		;|
	BNE ...Down			;/
	BRA ..Horizontal		
	
	...Up
	LDA !Freeram_FirstFill
	INC A
	STA !Freeram_FirstFill
	BRA ..Horizontal
	
	...Down
	LDA !Freeram_FirstFill
	DEC A
	STA !Freeram_FirstFill
	
	..Horizontal
	LDA $16
	BIT.b #%00000010
	BNE ...Left
	BIT.b #%00000001
	BNE ...Right
	BRA .DisplayFillAmount
	
	...Left
	LDA !Freeram_SecondFill
	DEC A
	STA !Freeram_SecondFill
	BRA .DisplayFillAmount
	
	...Right
	LDA !Freeram_SecondFill
	INC A
	STA !Freeram_SecondFill
	
.DisplayFillAmount
	;This displays the hex numbers representing the two fill in the bar.
	;Only works with Super Status Bar patch.
	if !StatusBarFormat == $02
		LDA !Freeram_FirstFill					;\every 16th number increments the 1st digit.
		LSR #$04						;|
		STA !FirstFillHexValDisplayPos				;/
		LDA !Freeram_FirstFill					;\limit it to #$00-#$0F on 2nd digit digit.
		AND #$0F						;|
		STA !FirstFillHexValDisplayPos+(1*!StatusBarFormat)	;/

		LDA !Freeram_SecondFill					;\every 16th number increments the 1st digit.
		LSR #$04						;|
		STA !SecondFillHexValDisplayPos				;/
		LDA !Freeram_SecondFill					;\limit it to #$00-#$0F on 2nd digit digit.
		AND #$0F						;|
		STA !SecondFillHexValDisplayPos+(1*!StatusBarFormat)	;/
	endif

.GraphicalDoubleBarTest

	..GraphicalDoubleBarFirstFill
	LDA !Freeram_FirstFill				;\Amount of fill for first fill
	STA $00						;|
	STZ $01						;/
	LDA #!Default_LeftPieces			;\Left end pieces
	STA !Scratchram_GraphicalBar_LeftEndPiece	;/
	LDA #!Default_MiddlePieces			;\Middle pieces
	STA !Scratchram_GraphicalBar_MiddlePiece	;/
	LDA.b #!Default_MiddleLength			;\Middle length
	STA !Scratchram_GraphicalBar_TempLength		;/
	LDA #!Default_RightPieces			;\Right end pieces
	STA !Scratchram_GraphicalBar_RightEndPiece	;/
	JSL GraphicalBarELITE_DrawGraphicalBar		;>Get amount of fill.

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
	;Thankfully the graphical bar routines does not mess up the scratch RAM inputs, therefore
	;you only need to set them once.
	LDA !Freeram_SecondFill							;\Amount of fill for second fill
	STA $00									;|
	STZ $01									;/
	JSL GraphicalBarELITE_DrawGraphicalBar					;>Get amount of fill.
	JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTilesDoubleBar
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
	endif
	if !Default_LeftwardsBar == 0
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUD			;>Write to status bar
	else
		JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwards
	endif
	RTL