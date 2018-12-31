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


;These (below) are tables to convert whats in !Scratchram_GraphicalBar_FillByteTbl to tile numbers.
;The format here is basically a combination represented as "(FirstFill;SecondFill)" where
;"FirstFill" the amount of fill that overlaps "SecondFill". The amount of fill in SecondFill
;ALWAYS measures from the leftmost of the bar, NOT from where FirstFill ends at. So example:
;an 8x8 byte is "(2;5)", the 2 represent 2 pieces filled with FirstFill, while the 5 is the
;amount of fill for SecondFill. It should look like in-game as 3 pieces (5-2=3) of SecondFill.
;Should SecondFill be less than FirstFill (like (2;1)), it should reuse the same tile as if
;they're equal, this means that the table will have duplicated tile numbers, and increases by 1
;each time FirstFill increases.

;The table here are sorted with increasing SecondFill, and once it gets full, it will increase
;FirstFill by 1 and resets SecondFill, and repeats starting back at increasing SecondFill.
;
;The number of bytes in these tables are (NumbPieces+1)^2. Make sure you have the exact size
;to avoid glitchy tiles.

;Be very careful not to have very large number of pieces, as the number of combinations will
;increase exponentially, potentially won't even be able to fit in the VRAM storage area with
;that many 8x8 tiles.

;Each row represents each possible values of FirstFill, while each column represents SecondFill.
;Hope that makes sense.

;Be extra careful with the bank borders, 2 bar fill data tables are stored with
;!Scratchram_GraphicalBar_FillByteTbl being SecondFill and
;!Scratchram_GraphicalBar_FillByteTbl+!GraphicalBar_TotalTileUsed being FirstFill. Bank border is basically
;the line between $**FFFF and $XXFFFF, since the "reader" only handle 16-bit processing.

GraphicalBar_LeftEnd8x8s:
db $29,$2A,$2B,$2C    ;>(0;0), (0;1), (0;2), (0;3). When FirstFill = 0, and SecondFill is any value
db $2D,$2D,$2F,$35    ;>(1;0), (1;1), (1;2), (1;3). When FirstFill = 1, and SecondFill is any value
db $36,$36,$36,$37    ;>(2;0), (2;1), (2;2), (2;3). When FirstFill = 2, and SecondFill is any value
db $38,$38,$38,$38    ;>(3;0), (3;1), (3;2), (3;3). When FirstFill = 3, and SecondFill is any value
GraphicalBar_Middle8x8s:
db $39,$45,$46,$47,$48,$49,$4B,$4C,$4D ;>(0;0), (0;1), (0;2), (0;3), (0;4), (0;5), (0;6), (0;7), (0;8)
db $4E,$4E,$4F,$50,$51,$52,$53,$54,$55 ;>(1;0), (1;1), (1;2), (1;3), (1;4), (1;5), (1;6), (1;7), (1;8)
db $56,$56,$56,$57,$58,$59,$5A,$5B,$5C ;>(2;0), (2;1), (2;2), (2;3), (2;4), (2;5), (2;6), (2;7), (2;8)
db $5D,$5D,$5D,$5D,$5E,$5F,$60,$61,$62 ;>(3;0), (3;1), (3;2), (3;3), (3;4), (3;5), (3;6), (3;7), (3;8)
db $63,$63,$63,$63,$63,$65,$66,$67,$68 ;>(4;0), (4;1), (4;2), (4;3), (4;4), (4;5), (4;6), (4;7), (4;8)
db $69,$69,$69,$69,$69,$69,$6A,$6B,$6C ;>(5;0), (5;1), (5;2), (5;3), (5;4), (5;5), (5;6), (5;7), (5;8)
db $6D,$6D,$6D,$6D,$6D,$6D,$6D,$6E,$6F ;>(6;0), (6;1), (6;2), (6;3), (6;4), (6;5), (6;6), (6;7), (6;8)
db $71,$71,$71,$71,$71,$71,$71,$71,$72 ;>(7;0), (7;1), (7;2), (7;3), (7;4), (7;5), (7;6), (7;7), (7;8)
db $73,$73,$73,$73,$73,$73,$73,$73,$73 ;>(8;0), (8;1), (8;2), (8;3), (8;4), (8;5), (8;6), (8;7), (8;8)
GraphicalBar_RightEnd8x8s:
db $74,$75,$79,$7A    ;>(0;0), (0;1), (0;2), (0;3)
db $7B,$7B,$7C,$7D    ;>(1;0), (1;1), (1;2), (1;3)
db $7E,$7E,$7E,$7F    ;>(2;0), (2;1), (2;2), (2;3)
db $80,$80,$80,$80    ;>(3;0), (3;1), (3;2), (3;3)

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
	;Thankfully the routine does not mess up the scratch RAM inputs, therefore
	;you only need to set them once.
	LDA !Freeram_SecondFill				;\Amount of fill for second fill
	STA $00						;|
	STZ $01						;/
	JSL GraphicalBarELITE_DrawGraphicalBar		;>Get amount of fill.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Convert fill to 8x8 tile (can be turned into a reusable routine with minor changes).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	..GraphicalDoubleBarConvertToGraphics
	;How this works in relation of using the table (regarding each 8x8 byte section):
	;1) Amount of FirstFill multiply by the number of pieces +1 (example: LeftEndFirstFill*4).
	;   The value you have now is currently the index number for each fill value for FirstFill
	;   while having SecondFill's index as "empty". Basically, this is the " leftmost column"
	;   of the table.
	;
	;2) Load up the corresponding byte/tile (so if it's left end on the first bar, it must be
	;   left end as well for the other bar.) of the second bar. You should have the value for
	;   SecondFill. Then add the value by the value you have in step 1 to get the "row number"
	;   for getting SecondFill's index number. Remember that SecondBar is located before FirstBar
	;   in memory address.
	;
	;So basically the index formula is this:
	;
	; IndexNumber = (FirstFill * (Pieces + 1)) + SecondFill
	;
	;Note that IndexNumber is 8-bit (only used for what tile number for the table, not the byte
	;table containing the amount of fill), because having that much fill will eat up large
	;amounts of 8x8 graphics exponentially (this also means the fill in the bar will also be 8-bit).
	
	PHB									;>Save bank
	PHK									;\Switch bank
	PLB									;/
	if !Setting_GraphicalBar_IndexSize == 0
		LDX #$00
	else
		REP #$10								;>16-bit XY
		LDX #$0000								;>The index for what byte tile position to write.
	endif
	...LeftEndTranslate
	LDA !Scratchram_GraphicalBar_LeftEndPiece				;\Check if left end exist
	BEQ ...MiddleTranslate							;/
	if !sa1 == 0
		INC								;>Pieces + 1
		STA $4202							;>(Pieces + 1) times...
		LDA !Scratchram_GraphicalBar_FillByteTbl+!GraphicalBar_TotalTileUsed	;\...FirstFill (remember Commutative property means same result reguardless of order)
		STA $4203							;/
		JSR WaitCalculation						;>Wait 12 cycles in total (8 is minimum needed)
		LDA $4216							;>Load product (low byte only)
	else
		LDA #$00							;\Multiply mode
		STA $2250							;/
		LDA !Scratchram_GraphicalBar_LeftEndPiece			;\Pieces + 1
		INC								;/
		STA $2251							;\(Pieces + 1) times...
		STZ $2252							;/
		LDA !Scratchram_GraphicalBar_FillByteTbl+!GraphicalBar_TotalTileUsed	;\...FirstFill (remember Commutative property means same result reguardless of order)
		STA $2253							;|
		STZ $2254							;/
		NOP								;\Wait 5 cycles until calculation is done
		BRA $00								;/
		LDA $2306							;>Load product (low byte only)
	endif
	CLC									;\Add by SecondFill
	ADC !Scratchram_GraphicalBar_FillByteTbl				;/
	if !Setting_GraphicalBar_IndexSize != 0
		REP #$20								;\Rid the high byte
		AND #$00FF								;|
		SEP #$20								;/
	endif
	TAY									;>Transfer #$00XX fill value to y
	LDA GraphicalBar_LeftEnd8x8s,y						;\Convert left end
	STA !Scratchram_GraphicalBar_FillByteTbl				;/
	INX									;>next tile

	...MiddleTranslate

	LDA !Scratchram_GraphicalBar_MiddlePiece	;\check if middle exist.
	BEQ ...RightEndTranslate			;|
	LDA !Scratchram_GraphicalBar_TempLength		;|
	BEQ ...RightEndTranslate			;/
	
	LDA !Scratchram_GraphicalBar_TempLength
	STA $00
	
	....Loop
	if !sa1 == 0
		LDA !Scratchram_GraphicalBar_MiddlePiece			;\Pieces + 1
		INC								;/
		STA $4202							;>(Pieces + 1) times...
		LDA !Scratchram_GraphicalBar_FillByteTbl+!GraphicalBar_TotalTileUsed,x	;\...FirstFill (remember Commutative property means same result reguardless of order)
		STA $4203							;/(+1 because it starts on the first middle tile, after the left end)
		JSR WaitCalculation						;>Wait 12 cycles in total (8 is minimum needed)
		LDA $4216							;>Load product (low byte only)
	else
		LDA #$00							;\Multiply mode
		STA $2250							;/
		LDA !Scratchram_GraphicalBar_MiddlePiece			;\Pieces + 1
		INC								;/
		STA $2251							;\(Pieces + 1) times...
		STZ $2252							;/
		LDA !Scratchram_GraphicalBar_FillByteTbl+!GraphicalBar_TotalTileUsed,x	;\...FirstFill (remember Commutative property means same result reguardless of order)
		STA $2253							;|
		STZ $2254							;/
		NOP								;\Wait 5 cycles until calculation is done
		BRA $00								;/
		LDA $2306							;>Load product (low byte only)
	endif
	CLC									;\Add by SecondFill
	ADC !Scratchram_GraphicalBar_FillByteTbl,x				;/
	if !Setting_GraphicalBar_IndexSize != 0
		REP #$20								;\Rid the high byte
		AND #$00FF								;|
		SEP #$20								;/
	endif
	TAY									;>Transfer #$00XX fill value to y
	LDA GraphicalBar_Middle8x8s,y						;\Convert middle tile
	STA !Scratchram_GraphicalBar_FillByteTbl,x				;/
	
	.....Next
	INX						;>Next byte or 8x8 tile
	DEC $00						;\loop until all middles done
	BNE ....Loop					;/
	
	
	...RightEndTranslate
	LDA !Scratchram_GraphicalBar_RightEndPiece
	BEQ ..Done
	if !sa1 == 0
		LDA !Scratchram_GraphicalBar_RightEndPiece
		INC								;>Pieces + 1
		STA $4202							;>(Pieces + 1) times...
		LDA !Scratchram_GraphicalBar_FillByteTbl+!GraphicalBar_TotalTileUsed,x	;\...FirstFill (remember Commutative property means same result reguardless of order)
		STA $4203							;/
		JSR WaitCalculation						;>Wait 12 cycles in total (8 is minimum needed)
		LDA $4216							;>Load product (low byte only)
	else
		LDA #$00							;\Multiply mode
		STA $2250							;/
		LDA !Scratchram_GraphicalBar_RightEndPiece			;\Pieces + 1
		INC								;/
		STA $2251							;\(Pieces + 1) times...
		STZ $2252							;/
		LDA !Scratchram_GraphicalBar_FillByteTbl+!GraphicalBar_TotalTileUsed,x	;\...FirstFill (remember Commutative property means same result reguardless of order)
		STA $2253							;|
		STZ $2254							;/
		NOP								;\Wait 5 cycles until calculation is done
		BRA $00								;/
		LDA $2306							;>Load product (low byte only)
	endif
	CLC									;\Add by SecondFill
	ADC !Scratchram_GraphicalBar_FillByteTbl,x				;/
	if !Setting_GraphicalBar_IndexSize != 0
		REP #$20								;\Rid the high byte
		AND #$00FF								;|
		SEP #$20								;/
	endif
	TAY									;>Transfer #$00XX fill value to y
	LDA GraphicalBar_RightEnd8x8s,y						;\Convert middle tile
	STA !Scratchram_GraphicalBar_FillByteTbl,x				;/
	
	..Done
	SEP #$10								;>8-bit XY
	PLB									;>Restore bank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Write to status bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.TransferBarTileNumberToHud:
if !StatusBarFormat == $01
	if !Leftwards == 0
		LDX.b #!GraphiBar_LeftTileExist+(!Default_MiddleLength*!GraphiBar_MiddleTileExist)+!GraphiBar_RightTileExist-1	;>Start loop counter

		..Loop
		LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Store tile data into status bar tiles
		STA !GraphicalBarPos,x				;/
		DEX						;>Next tile
		BPL ..Loop					;>And loop
	else
		LDX.b #!GraphiBar_LeftTileExist+(!Default_MiddleLength*!GraphiBar_MiddleTileExist)+!GraphiBar_RightTileExist-1	;\Start loop
		LDY #$00						;/

		..Loop
		LDA !Scratchram_GraphicalBar_FillByteTbl,x		;\Transfer scratch to status bar
		STA !GraphicalBarPos,y					;/
		LDA.b #%01000000					;\Tile properties, use +$40 for minimalist status bars, $80 for SMB3. Note that leftwards does
		STA !GraphicalBarPos+$80,y				;/not work on smw's status bar (or future HUD patches that doesn't support tile properties stored in RAM.
		INY							;\Next tile
		DEX							;/
		BPL ..Loop						;>And loop
	endif
else
	if !Leftwards == 0
		LDX.b #((!GraphiBar_LeftTileExist+(!Default_MiddleLength*!GraphiBar_MiddleTileExist)+!GraphiBar_RightTileExist)*2)-2	;>Each 8x8 of SSB has 2 bytes
		LDY.b #(!GraphiBar_LeftTileExist+(!Default_MiddleLength*!GraphiBar_MiddleTileExist)+!GraphiBar_RightTileExist)-1	;>Each 8x8 of scratch is 1 byte each.

		..Loop
		PHX						;>Save SSB index
		TYX						;\LDA $xxxxxx,y does not exist
		LDA !Scratchram_GraphicalBar_FillByteTbl,x	;/
		PLX						;>Restore SSB index
		STA !GraphicalBarPos,x				;>Transfer to status bar tiles
		LDA #%00111000					;\Setup tile properties (bit 6 must be clear)
		STA !GraphicalBarPos+1,x			;/
		
		...Next
		DEY							;\Next tile
		DEX #2							;/
		BPL ..Loop						;>and loop
	else
		LDX.b #((!GraphiBar_LeftTileExist+(!Default_MiddleLength*!GraphiBar_MiddleTileExist)+!GraphiBar_RightTileExist)*2)-2	;>Status bar index
		LDY.b #$00							;>Scratch index

		..Loop
		PHX						;\Transfer to status bar tiles
		TYX						;|
		LDA !Scratchram_GraphicalBar_FillByteTbl,x	;|
		PLX						;|
		STA !GraphicalBarPos,x				;/
		LDA #%01111000					;\Setup tile properties (bit 6 must be set)
		STA !GraphicalBarPos+1,x			;/
		
		...Next
		INY						;\Next tile
		DEX #2						;/
		BPL ..Loop					;>And loop
	endif
endif
	RTL

	if !sa1 == 0
		WaitCalculation:	;>The register to perform multiplication and division takes 8/16 cycles to complete.
		RTS
	endif