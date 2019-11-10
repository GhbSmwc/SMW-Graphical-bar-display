incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This routine directly writes the tile to the status bar or
;overworld border plus.
;
;Note: This only writes up to 128 (64 if using super status
;bar and OWB+ format) tiles. But it is unlikely you would ever
;need that much tiles, considering that the screen is 32 ($20)
;8x8 tiles wide.
;
;Input:
; -$00 to $02: The starting byte address location of the status bar (tile number).
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
; --$06: The tile properties (YXPCCCTT) you want it to be.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if !OWPlusAndSSBSameFormat == 0
		WriteBarToHUD:
		JSL CountNumberOfTiles
		CPX #$FF				;\If 0-1 = (-1), there is no tile to write.
		BEQ .Done				;/(non-existent bar)
		TXY					;>STA [$xx],x does not exist! Only STA [$xx,x] does but functions differently!

		.Loop
		LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Write each tile.
		STA [$00],y					;/
		if !StatusBar_UsingCustomProperties != 0
			LDA $06
			STA [$03],y
		endif
		
		..Next
		DEX
		DEY
		BPL .Loop
		
		.Done
		RTL
	endif
	
	WriteBarToHUDFormat2:
	JSL CountNumberOfTiles
	CPX #$FF				;\If 0-1 = (-1), there is no tile to write.
	BEQ .Done				;/(non-existent bar)
	TXA					;\Have Y = X*2 due to SSB/OWB+ patch formated for 2 contiguous bytes per tile.
	ASL					;|
	TAY					;/
	
	.Loop
	LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Write each tile.
	STA [$00],y					;/
	if !StatusBar_UsingCustomProperties != 0
		LDA $06
		STA [$03],y
	endif
	
	..Next
	DEX
	DEY #2
	BPL .Loop
	
	.Done
	RTL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Same as above, but fills leftwards as opposed to rightwards.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if !OWPlusAndSSBSameFormat == 0
		WriteBarToHUDLeftwards:
		JSL CountNumberOfTiles
		CPX #$FF
		BEQ .Done
		LDY #$00
		
		.Loop
		LDA !Scratchram_GraphicalBar_FillByteTbl,x
		STA [$00],y
		if !StatusBar_UsingCustomProperties != 0
			LDA $06
			STA [$03],y
		endif
		
		..Next
		INY
		DEX
		BPL .Loop
		
		.Done
		RTL
	endif
	
	WriteBarToHUDLeftwardsFormat2:
	JSL CountNumberOfTiles
	CPX #$FF
	BEQ .Done
	LDY #$00
	
	.Loop
	LDA !Scratchram_GraphicalBar_FillByteTbl,x
	STA [$00],y
	if !StatusBar_UsingCustomProperties != 0
		LDA $06
		STA [$03],y
	endif
	
	..Next
	INY #2
	DEX
	BPL .Loop
	
	.Done
	RTL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Bar extend leftwards (as the length increases, the final/last/
;rightmost tile stays on the same "rightmost" position and the left
;side moves to the left).
;
;How this works: When writing the tiles to the status bar, it first
;uses the "origin" tile, which is the leftmost tile. It is always
;the leftmost tile even when the bar is x-flipped to fill leftwards.
;With a bar that extends leftwards, the left tile is no longer at
;a fixed position, therefore the left tile is calculated by being
;subtracted by a number of tiles towards the left.
;
;Calculates like this:
; BeginningTilePos = DesiredLastTilePos - (NumberOfTiles - 1)
;If using the 2-adjacent bytes per 8x8 tile, uses this instead:
; BeginningTilePos = DesiredLastTilePos - ((NumberOfTiles - 1)*2)
;
;Be careful since this uses Akaginite's simple 16bitNum - 8bitNumb
;it does ((-8bitNumb)+16bitNum) code that does not work with the
;carry flag when 8bitNum is $00, in which there is no safe way
;to handle bank border crosses (a bank is the highest byte of
;the 24-bit address: $XX****, the XX, so avoid things like going
;from $7EFFFF to $7F0000 (made up example)). So avoid having
;status bar positions that would be at different banks. This is
;unlikely though.
;
; Input:
;  $00-$02: the position of the final/last/rightmost tile would be
;           at.
;  $03-$05: Same as above, but for tile properties if applicable.
; Output:
;  $00-$02: the position of the first tile would be at.
;  $03-$05: Same as above, but for tile properties if applicable.
;
; Status bar address write range (when using this routine and
; WriteBarToHUD or WriteBarToHUDLeftwards):
;  [RAMAddressIn00ThatYouEntered -((NumberOfTiles-1)*!StatusBarFormat)] to [RAMAddressIn00ThatYouEntered]
;  Same applies to tile properties.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BarExtendLeftFormat2:
	JSL CountNumberOfTiles
	TXA
	ASL
	TAX
	if !OWPlusAndSSBSameFormat == 0
		BRA +

		BarExtendLeft:
		JSL CountNumberOfTiles
		TXA
		+
	endif
	;^A now holds the number of bytes to go back.
	; This indicates the first tile of the bar to be written.
	
	REP #$21				;\A: -(NumberOfTiles-1)...
	AND #$00FF				;|
	EOR #$FFFF				;|
	INC A					;/
	ADC $00					;>...+ LastTilePos (we are doing LastTilePos - (NumberOfTiles-1))
	STA $00					;>Store difference in $00-$01
	SEP #$20				;
;	LDA $02					;\Handle bank byte (commented out because carry doesn't work like SBC if subtrahend is 0)
;	SBC #$00				;|[(-A) + RAM_00]
;	STA $02					;/
	
	if !StatusBar_UsingCustomProperties != 0
		TXA
		REP #$21				;\-(NumberOfTiles-1)
		AND #$00FF				;|
		EOR #$FFFF				;|
		INC A					;/
		ADC $03					;>+LastTilePos (we are doing LastTilePos - (NumberOfTiles-1))
		STA $03					;>Store difference in $00-$01
		SEP #$20				;
;		LDA $05					;\Handle bank byte
;		SBC #$00				;|
;		STA $05					;/
	endif
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
	RTL