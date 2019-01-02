;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;"GraphicalBarELITE.asm" in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

;This is a coin meter to be used as a test for rounding towards empty or full
;("complete") detection. Don't ask me to combine this with double-bar, that
;is probably gonna take a huge amount of space. Best tested with low number of
;pieces for greater chance of rounding.


;The tile numbers representing a rounding are stored in the table that stores
;the amount of fill from 0 to full. This is so that the tile handling is more
;efficient, you wouldn't have 3 codes after "JSL DrawGraphicalBar" (pre-pended
;with "GraphicalBarELITE_" because uberasm tool mode) related to tile
;handling: convert bar to near empty, near full, and converting the amount of
;fill in each bytes to tile numbers.

incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;This is a simple test ASM using the graphical bar.
;best tested using uberasm tool.

 !Default_RoundingIndicate            = 1
 ;^*0 = show amount of fill at minimum of 1 or maximum
 ;  of max-1 should attempts to round towards complete. This
 ;  actually modifies the amount of fill and can be used for
 ;  double bar.
 ; *1 = show "half fill" when attempts to round
 ;  towards complete, note that this option does not count as
 ;  extra pieces, the bar is actually 0 or max and just
 ;  rewrites the tiles to use.


;These (below) are tables to convert whats in !Scratchram_GraphicalBar_FillByteTbl to tile numbers.

;With rounding towards "complete", the last numbers are:
;-tile number when rounded down to empty (should left end exist, apply there)
;-tile number when rounded to full (should right end exist, apply there)
;for GraphicalBar_Middle8x8s when either end tiles are disabled, include
;after the last number: $xx,$yy where $xx is the near-empty and $yy is near-full
;the 
GraphicalBar_LeftEnd8x8s:
	;    0   1   2   3 [*]
	db $36,$37,$38,$39,$29
GraphicalBar_Middle8x8s:
	;    0   1   2   3   4   5   6   7   8 [*] [*]
	db $55,$56,$57,$58,$59,$65,$66,$67,$68,$2A,$2B
GraphicalBar_RightEnd8x8s:
	;    0   1   2   3 [*]
	db $50,$51,$52,$53,$54

;if you edit the number of pieces along with the number of fill graphic tiles here,
;make sure you CTRL+F "Use additional index of the table for near-rounded graphic"
;and change the numbers marked with "[*]" so that they use indexes that represents
;near-complete (near empty or full) graphic tiles, otherwise you end up with garbage
;tiles. The numbers themselves are to be used as indexes (so if the index number is
;#$04, it uses item #$04 (item #$00 exist at the start)) from this table.


;Be careful not to have the table shorter than the possible fill value, else glitched
;tiles will appear. (so there are 3 tile numbers (0/2 to 2/2) in table, and you have
;a fill value of 3 in the byte, this causes it to use a value past the last valid value
;in the table).
main:

.InputRatio
	LDA $0DBF						;\Player's coins as the quantity.
	STA !Scratchram_GraphicalBar_FillByteTbl		;/
	LDA #$00						;>Clear...
	STA !Scratchram_GraphicalBar_FillByteTbl+1		;/the high byte (no STZ because STZ <3 byte address> does not exist).
	STA !Scratchram_GraphicalBar_FillByteTbl+3		;>Also clear the high byte of max quantity
	LDA.b #99						;\Okay, 99 coins is the maximum coins you can have before it rolls over to 0.
	STA !Scratchram_GraphicalBar_FillByteTbl+2		;/
.InputGraphicalBarAttributes
	LDA.b #!Default_LeftPieces				;\Left and right pieces
	STA !Scratchram_GraphicalBar_LeftEndPiece		;|
	LDA.b #!Default_RightPieces				;|
	STA !Scratchram_GraphicalBar_RightEndPiece		;/
	LDA.b #!Default_MiddlePieces				;\Number of pieces in each middle byte/8x8 tile
	STA !Scratchram_GraphicalBar_MiddlePiece		;/
	LDA.b #!Default_MiddleLength				;\length (number of middle tiles)
	STA !Scratchram_GraphicalBar_TempLength			;/
.ConvertToBar
	JSL GraphicalBarELITE_CalculateGraphicalBarPercentage	;>Get ratio/percentage [Here, I assume you are using uberasmTool]
	..RoundingDetect
	if !Default_RoundingIndicate == 0
		CPY #$00						;\check rounding flags (Y is only #$00 to #$02)
		BEQ ..BarWrite						;|
		CPY #$01						;|
		BEQ ..RoundedEmpty					;|
		CPY #$02						;|
		BEQ ..RoundedFull					;/
		
		..RoundedEmpty
		REP #$20
		INC $00							;>if fill amount is exclusively between 0 and 0.5, make it display fillvalue = 1
		SEP #$20
		BRA ..BarWrite						;>and done

		..RoundedFull
		REP #$20
		DEC $00							;>if fill amount is exclusively between max-0.5 and max, make it display fillvalue = max-1
		SEP #$20
		
		..BarWrite
		JSL GraphicalBarELITE_DrawGraphicalBar
	else
		;Use additional index of the table for near-rounded graphic
		PHY							;>push rounding flag (obviously the following routine overwrites Y)
		JSL GraphicalBarELITE_DrawGraphicalBar			;>get bar values.
		PLY							;>pull rounding flag

		CPY #$00 : BEQ ..NoRound				;>Y can only be #$00 to #$02
		CPY #$01 : BEQ ..RoundedEmpty				;>comment line if you want to allow round towards to empty (left end is no longer mandatory to be enabled)
		CPY #$02 : BEQ ..RoundedFull				;>comment line if you want to allow round towards full (right end is no longer mandatory to be enabled)
		;BRA ..NoRound						;>remove comment if any above gets commented so following code doesn't inadvertently gets executed

		..RoundedEmpty
		if !Default_LeftPieces != 0
			LDA #$04					;[*]left end
		else
			LDA #$09					;[*]first middle byte/8x8
		endif
		STA !Scratchram_GraphicalBar_FillByteTbl		;/(remember, the table contains index numbers for tile numbers, not tile numbers directly!!)
		BRA ..NoRound						;>and done
		
		..RoundedFull
		if !Default_RightPieces != 0
			LDA #$04					;[*]right end
		else
			LDA #$0A					;[*]last middle byte/8x8
		endif
		STA !Scratchram_GraphicalBar_FillByteTbl+(!GraphiBar_LeftTileExist+(!GraphiBar_MiddleTileExist*!Default_MiddleLength)+!GraphiBar_RightTileExist)-1
		
	endif
	..NoRound
	JSL GraphiBar_ConvertToTile				;>Convert tiles.
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