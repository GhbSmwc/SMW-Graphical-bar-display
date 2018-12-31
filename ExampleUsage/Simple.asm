;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;"GraphicalBarELITE.asm" in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

;This code measures the player's X position in a horizontal level.
;it measures from the very left side of the area the screen can possibly go
;to the right edge of the stage (last screen). Can be described as "progress"


incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;This is a simple test ASM using the graphical bar.
;best tested using uberasm tool.

;These (below) are tables to convert whats in !Scratchram_GraphicalBar_FillByteTbl to tile numbers.
GraphicalBar_LeftEnd8x8s:
	;    0   1   2   3
	db $36,$37,$38,$39
GraphicalBar_Middle8x8s:
	;    0   1   2   3   4   5   6   7   8
	db $55,$56,$57,$58,$59,$65,$66,$67,$68
GraphicalBar_RightEnd8x8s:
	;    0   1   2   3
	db $50,$51,$52,$53
	

;Be careful not to have the table shorter than the possible fill value, else glitched
;tiles will appear. (so there are 3 tile numbers (0/2 to 2/2) in table, and you have
;a fill value of 3 in the byte, this causes it to use a value past the last valid value
;in the table).
main:
;Get x position percentage in horizontal level.
;This is basically the progress meter.
	REP #$20
	LDA $94							;\Player's X position "progress"
	SEC							;|
	SBC #$0008						;>because the minimum player's X pos is #$0008
	STA !Scratchram_GraphicalBar_FillByteTbl		;/
	SEP #$20
	LDA.b #!Default_MiddleLength				;\Input length (middle)
	STA !Scratchram_GraphicalBar_TempLength			;/
	LDA #$E0						;\The maximum X positon the player can be (right edge of the level)
	STA !Scratchram_GraphicalBar_FillByteTbl+2		;|
	LDA $5E							;|
	DEC							;|
	STA !Scratchram_GraphicalBar_FillByteTbl+3		;/
	LDA.b #!Default_LeftPieces				;\Input amount of pieces in each of the 3 types of sections.
	STA !Scratchram_GraphicalBar_LeftEndPiece		;|
	LDA.b #!Default_MiddlePieces				;|
	STA !Scratchram_GraphicalBar_MiddlePiece		;|
	LDA.b #!Default_RightPieces				;|
	STA !Scratchram_GraphicalBar_RightEndPiece		;/
	JSL GraphicalBarELITE_CalculateGraphicalBarPercentage	;>Get percentage
	JSL GraphicalBarELITE_DrawGraphicalBar			;>get bar values.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Convert fill to 8x8 tile (can be turned into a reusable routine with minor changes).
;
;Now you would translate the table into tile number for each byte (assuming if you have left end
;and middle enabled):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.ConvertFillTo8x8
	PHB						;>Preserve bank (so that table indexing work properly)
	PHK						;>push current bank
	PLB						;>pull out as regular bank

	if !Setting_GraphicalBar_IndexSize == 0
		LDX #$00
	else
		REP #$10								;>16-bit XY
		LDX #$0000								;>The index for what byte tile position to write.
	endif

..LeftEndTranslate
	LDA !Scratchram_GraphicalBar_LeftEndPiece	;\can only be either 0 or the correct number of pieces listed in the table.
	BEQ ..MiddleTranslate				;/
	if !Setting_GraphicalBar_IndexSize == 0
		LDA !Scratchram_GraphicalBar_FillByteTbl
		TAY
	else
		REP #$20
		LDA !Scratchram_GraphicalBar_FillByteTbl
		AND #$00FF
		TAY
		SEP #$20
	endif
	LDA GraphicalBar_LeftEnd8x8s,y
	STA !Scratchram_GraphicalBar_FillByteTbl
	INX						;>next tile

..MiddleTranslate
	LDA !Scratchram_GraphicalBar_MiddlePiece	;\check if middle exist.
	BEQ ..RightEndTranslate				;|
	LDA !Scratchram_GraphicalBar_TempLength		;|
	BEQ ..RightEndTranslate				;/

	if !Setting_GraphicalBar_IndexSize == 0
		LDA !Scratchram_GraphicalBar_TempLength
		STA $00
	else
		REP #$20
		LDA !Scratchram_GraphicalBar_TempLength
		AND #$00FF
		STA $00
	endif
	...Loop
	if !Setting_GraphicalBar_IndexSize == 0
		LDA !Scratchram_GraphicalBar_FillByteTbl,x
		TAY
	else
		LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\amount of filled, indexed
		AND #$00FF					;|
		TAY						;/
		SEP #$20
	endif
	LDA GraphicalBar_Middle8x8s,y			;\amount filled as graphics
	STA !Scratchram_GraphicalBar_FillByteTbl,x	;/
	
	....Next
	INX
	if !Setting_GraphicalBar_IndexSize != 0
		REP #$20
	endif
	DEC $00
	BNE ...Loop
	
	SEP #$20
..RightEndTranslate
	LDA !Scratchram_GraphicalBar_RightEndPiece
	BEQ ..Done
	if !Setting_GraphicalBar_IndexSize == 0
		LDA !Scratchram_GraphicalBar_FillByteTbl,x
		TAY
	else
		REP #$20
		LDA !Scratchram_GraphicalBar_FillByteTbl,x
		AND #$00FF
		TAY
		SEP #$20
	endif
	LDA GraphicalBar_RightEnd8x8s,y
	STA !Scratchram_GraphicalBar_FillByteTbl,x
	
	..Done
	SEP #$30					;>Just in case
	PLB						;>Pull bank
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