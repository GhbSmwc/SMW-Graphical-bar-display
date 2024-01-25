;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Count tiles. Adopted for pixi.
;
;This had to be duplicated despite already exists in ConvertBarFillAmountToTiles
;because you cannot do this:
;-Have multiple subroutines in the same ASM file, and;
;-Call any subroutine other than the first one (the first opcode) in the ASM file.
;-Call a subroutine that exist in another subroutine file from a subroutine file.
;
;Input:
;-!Scratchram_GraphicalBar_LeftEndPiece,
; !Scratchram_GraphicalBar_MiddlePiece,
; !Scratchram_GraphicalBar_TempLength, and
; !Scratchram_GraphicalBar_RightEndPiece: used to find how many
; tiles.
;Output:
; X = Number of bytes or 8x8 tiles the bar takes up of minus 1
;     For example: 9 total bytes, this routine would output X=$08.
;     Returns X=$FF should not a single tile exist.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
?CountNumberOfTiles:
	LDX #$00
	LDA !Scratchram_GraphicalBar_LeftEndPiece
	BEQ ?+
	INX
	?+
	LDA !Scratchram_GraphicalBar_MiddlePiece
	BEQ ?+
	TXA
	CLC
	ADC !Scratchram_GraphicalBar_TempLength
	TAX
	?+
	LDA !Scratchram_GraphicalBar_RightEndPiece
	BEQ ?+
	INX
	?+
	DEX					;>Subtract by 1 because index 0 exists.
	RTL