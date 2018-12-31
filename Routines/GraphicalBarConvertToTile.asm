incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Convert fill amount in bar to tile numbers.
;Note: This only works with non-double bar graphics.
;
;Input:
; -!Scratchram_GraphicalBar_LeftEndPiece: Number of pieces in left byte (0-255), also
;  the maximum amount of fill for this byte itself. If 0, it's not included in table.
; -!Scratchram_GraphicalBar_MiddlePiece: Same as above but each middle byte.
; -!Scratchram_GraphicalBar_RightEndPiece: Same as above but for right end.
; -!Scratchram_GraphicalBar_TempLength: The length of the bar (only counts
;   middle bytes)
;Output:
; -!Scratchram_GraphicalBar_FillByteTbl to !Scratchram_GraphicalBar_FillByteTbl+x:
;  converted to tile numbers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	GraphicalBar_LeftEnd8x8s:
	;    0   1   2   3
	db $36,$37,$38,$39
	GraphicalBar_Middle8x8s:
	;    0   1   2   3   4   5   6   7   8
	db $55,$56,$57,$58,$59,$65,$66,$67,$68
	GraphicalBar_RightEnd8x8s:
	;    0   1   2   3
	db $50,$51,$52,$53
	;^Tile numbers, ordered from 0 to maximum. This one assumes
	; you're using GFX28, for the SMW's status bar.
	
	ConvertBarFillAmountToTiles:
	PHB						;>Preserve bank (so that table indexing work properly)
	PHK						;>push current bank
	PLB						;>pull out as regular bank

	if !Setting_GraphicalBar_IndexSize == 0
		LDX #$00
	else
		REP #$10								;>16-bit XY
		LDX #$0000								;>The index for what byte tile position to write.
	endif

	.LeftEndTranslate
	LDA !Scratchram_GraphicalBar_LeftEndPiece	;\can only be either 0 or the correct number of pieces listed in the table.
	BEQ .MiddleTranslate				;/
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

	.MiddleTranslate
	LDA !Scratchram_GraphicalBar_MiddlePiece	;\check if middle exist.
	BEQ .RightEndTranslate				;|
	LDA !Scratchram_GraphicalBar_TempLength		;|
	BEQ .RightEndTranslate				;/

	if !Setting_GraphicalBar_IndexSize == 0
		LDA !Scratchram_GraphicalBar_TempLength
		STA $00
	else
		REP #$20
		LDA !Scratchram_GraphicalBar_TempLength
		AND #$00FF
		STA $00
	endif
	..Loop
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
	
	...Next
	INX
	if !Setting_GraphicalBar_IndexSize != 0
		REP #$20
	endif
	DEC $00
	BNE ..Loop
	
	SEP #$20

	.RightEndTranslate
	LDA !Scratchram_GraphicalBar_RightEndPiece
	BEQ .Done
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
	
	.Done
	SEP #$30					;>Just in case
	PLB						;>Pull bank
	RTL