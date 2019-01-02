incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This routine directly writes the tile to the status bar.
;Note: This only writes up to 128 (64 if using super status
;bar format) tiles. But it is unlikely you would ever need that
;much tiles, considering that the screen is 32 ($20) 8x8 tiles
;wide and 28 ($1C) 8x8 tiles tall.
;
;Input:
; -$00 to $02: The starting byte address of the status bar (tile number).
; --If you're using SA-1 mode here and using vanilla status bar,
;   the status bar tilemap table is moved to bank $40.
; -!Scratchram_GraphicalBar_LeftEndPiece: Number of pieces in left byte (0-255), also
;  the maximum amount of fill for this byte itself. If 0, it's not included in table.
; -!Scratchram_GraphicalBar_MiddlePiece: Same as above but each middle byte.
; -!Scratchram_GraphicalBar_RightEndPiece: Same as above but for right end.
; -!Scratchram_GraphicalBar_TempLength: The length of the bar (only counts
;   middle bytes)
; -If you are using custom status bar patches that enables editing tile properties in-game,
;  and have set "!StatusBar_UsingCustomProperties" to 1, you have another input:
; --$03 to $05: Same as $00 to $02 but for tile properties instead of tile numbers.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WriteBarToHUD:
	JSR CountNumberOfTiles
	CPX #$FF				;\If 0-1 = (-1), there is no tile to write.
	BEQ .Done				;/(non-existent bar)
	
	if !StatusBarFormat == $01
		TXY
	else
		TXA				;\Have Y = X*2 due to Super Status Bar patch formated for 2 contiguous bytes per tile.
		ASL				;|
		TAY				;/
	endif
	
	.Loop
	LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Write each tile.
	STA [$00],y					;|
	if !StatusBar_UsingCustomProperties
		LDA.b #(!Default_StatusBar_TilePropertiesSetting | (!Default_LeftwardsBar<<6))
		STA [$03],y
	endif
	..Next
	DEX
	DEY #!StatusBarFormat
	BPL .Loop
	
	.Done
	RTL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Same as above, but fills leftwards as opposed to rightwards.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WriteBarToHUDLeftwards:
	JSR CountNumberOfTiles
	CPX #$FF
	BEQ .Done
	LDY #$00
	
	.Loop
	LDA !Scratchram_GraphicalBar_FillByteTbl,x
	STA [$00],y
	
	..Next
	INY #!StatusBarFormat
	DEX
	BPL .Loop
	
	.Done
	RTL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Count tiles.
;Output:
; X = Number of bytes or 8x8 tiles the bar takes up of minus 1
;     (if there is left and right ends in existent and one middle
;     tile, will be 3 tiles, this routine outputs X=$02). Returns
;     X=$FF should not a single tile exist.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CountNumberOfTiles:
	LDX #$00
	LDA !Scratchram_GraphicalBar_LeftEndPiece
	BEQ +
	INX
	+
	LDA !Scratchram_GraphicalBar_MiddlePiece
	BEQ +
	TXA
	CLC
	ADC !Scratchram_GraphicalBar_TempLength
	TAX
	+
	LDA !Scratchram_GraphicalBar_RightEndPiece
	BEQ +
	INX
	+
	DEX					;>Subtract by 1 because index 0 exists.
	RTS