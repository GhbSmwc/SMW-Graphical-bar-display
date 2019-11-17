;NOTE: to be used on uberasm tool on any level code as "main" (not init). Also have
;all files in the routine folder in the library folder for this to work (unless you edit the
;JSLs not to call a routine that doesn't have a destination label here).

;This code tests out a range-based bar, where MIN is mapped to 0 and MAX is mapped to
;(MAX-MIN); utilizing my MapRangeToStartAt0 routine. The coin counter acts as an interval
;number.


incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;This is a simple test ASM using the graphical bar.
;best tested using uberasm tool.

main:
;Increment or decrement quantity based on up/down controls:
.Control
	LDA $15                                                 ;\Control quantity based on tapping up and down.
	BIT.b #%00000100                                        ;|
	BNE ..Down                                              ;|
	BIT.b #%00001000                                        ;|
	BNE ..Up                                                ;|
	REP #$30                                                ;|
	BRA +                                                   ;|

	..Down                                                  ;|
	REP #$30                                                ;|
	LDA !Freeram_RangeBasedValue                            ;|
	BEQ +                                                   ;|
	DEC A                                                   ;|
	STA !Freeram_RangeBasedValue                            ;|
	BRA +                                                   ;|

	..Up                                                    ;|
	REP #$30                                                ;|
	LDA !Freeram_RangeBasedValue                            ;|
	CMP.w #210                                              ;|
	BCS +                                                   ;|
	INC A                                                   ;|
	STA !Freeram_RangeBasedValue                            ;/
	
	+
	;This here is where it calculate what interval and how much fill
	;in between two numbers starting from the lower number of the two.
	.IntervalRange
	LDX.w #(RangeTableEnd-RangeTable)-2                    ;\Search what range quantity is on.
	..Loop                                                 ;|
	LDA !Freeram_RangeBasedValue                           ;|Starts at the last index, compares 
	CMP RangeTable,x                                       ;|if quantity >= to the number in the table
	BCS ..IntervalFound                                    ;|and found if true, will have that index.
	DEX #2                                                 ;|
	BPL ..Loop                                             ;|
	INX #2                                                 ;|>Failsafe

	..IntervalFound                                        ;|
	TXA                                                    ;|\Store index*2 here
	STA $14B0|!addr                                        ;|/
	CPX.w #((RangeTableEnd-RangeTable)-4)                  ;|\Check if the range is on the second-last and last value in table
	BCC ..ValidRange                                       ;|/to avoid having another range as last number being the minimum and max being an invalid number.
	LDX.w #((RangeTableEnd-RangeTable)-4)                  ;|>Cap the range if last number reached. (will also display 100% bar once this range be full, as all others will 0 out when at these intervals)
	
	..ValidRange
	LDA RangeTable,x                                       ;|\Miminum
	STA $02                                                ;|/
	+
	..WriteIntervalNumber
	SEP #$20                                               ;|\Take 16-bit interval number, take only the low byte, and write to coin count
	LDA $14B0|!addr                                        ;||(SEP #$20 instead of #$30 to keep X and Y register's high bytes intact)
	LSR                                                    ;||
	STA $0DBF|!addr                                        ;||
	REP #$20                                               ;|/
	
	..InsertMinMax
	INX #2                                                 ;|>Next number in table (minimum X=$0000 after this).
	LDA RangeTable,x                                       ;|\Maximum
	STA $04                                                ;|/
	LDA !Freeram_RangeBasedValue                           ;|\Quantity
	STA $00                                                ;//
	SEP #$30
	
	JSL GraphicalBarOtherRoutines_MapRangeToStartAt0       ;>Convert range
	REP #$20                                               ;\Insert mapped range and value.
	LDA $00                                                ;|
	STA !Scratchram_GraphicalBar_FillByteTbl               ;|
	LDA $04                                                ;|
	STA !Scratchram_GraphicalBar_FillByteTbl+2             ;|
	SEP #$20                                               ;/

	.ProcessGraphicalBar
	LDA.b #!Default_LeftPieces					;\Input amount of pieces in each of the 3 types of sections.
	STA !Scratchram_GraphicalBar_LeftEndPiece			;|
	LDA.b #!Default_MiddlePieces					;|
	STA !Scratchram_GraphicalBar_MiddlePiece			;|
	LDA.b #!Default_RightPieces					;|
	STA !Scratchram_GraphicalBar_RightEndPiece			;/
	JSL GraphicalBarELITE_CalculateGraphicalBarPercentage		;>Get percentage
	JSL GraphicalBarELITE_DrawGraphicalBar				;>get bar values.
	JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
	LDA.b #!Default_GraphicalBar_Pos_Tile
	STA $00
	LDA.b #!Default_GraphicalBar_Pos_Tile>>8
	STA $01
	LDA.b #!Default_GraphicalBar_Pos_Tile>>16
	STA $02
	if !StatusBar_UsingCustomProperties != 0
		LDA.b #!Default_GraphicalBar_Pos_Properties
		STA $03
		LDA.b #!Default_GraphicalBar_Pos_Properties>>8
		STA $04
		LDA.b #!Default_GraphicalBar_Pos_Properties>>16
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
	RTL
	
	;These are the range values. Each number is the minimum of the range
	;and the next number is the maximum. Note that the bar displays
	;from Min to max-1 except the last range.
	;NOTE:
	;-Make sure the numbers are in increasing order and no duplicates.
	;-Make sure all your numbers are in between "RangeTable" and
	; "RangeTableEnd".
	;-The first number must be zero.
	RangeTable:
	dw 0                                   ;>item number 0 ($00)
	dw 10                                  ;>item number 1 ($02)
	dw 30                                  ;>item number 2 ($04)
	dw 60                                  ;>item number 3 ($06)
	dw 100                                 ;>item number 4 ($08)
	dw 150                                 ;>item number 5 ($0A)
	dw 210                                 ;>item number 6 ($0C)
	RangeTableEnd: