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
;  the status bar tilemap table is moved to bank $40.
; -!Scratchram_GraphicalBar_LeftEndPiece: Number of pieces in left byte (0-255), also
;  the maximum amount of fill for this byte itself. If 0, it's not included in table.
; -!Scratchram_GraphicalBar_MiddlePiece: Same as above but each middle byte.
; -!Scratchram_GraphicalBar_RightEndPiece: Same as above but for right end.
; -!Scratchram_GraphicalBar_TempLength: The length of the bar (only counts
;  middle bytes)
; -If you are using custom status bar patches that enables editing tile properties in-game,
;  and have set "!StatusBar_UsingCustomProperties" to 1, you have another input:
; --$03 to $05: Same as $00 to $02 but for tile properties instead of tile numbers.
; --$06: The tile properties (YXPCCCTT) you want it to be.
;Output:
; -[RAMAddressIn00] to [RAMAddressIn00 + ((NumberOfTiles-1)*TileFormat]: the status bar/OWB+
;  RAM write range.
; -If using SB/OWB+ patch that allows editing YXPCCCTT in-game and have set !StatusBar_UsingCustomProperties
;  to 1:
; --[RAMAddressIn03] to [RAMAddressIn03 + ((NumberOfTiles-1)*TileFormat]: same as above but YXPCCCTT
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
;  [RAMAddressIn00 - ((NumberOfTiles-1)*TileFormat)]
;  to [RAMAddressIn00]. 
;
;  Where RAMAddressIn00 is the address you enter before calling
;  this routine.
;
;  TileFormat is 1 if [TTTTTTTT, TTTTTTTT, ...] and 2 otherwise,
;  similar to !StatusBarFormat, but this also applies to OWB+.
;  Same applies to tile properties.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BarExtendLeftFormat2:
	JSL CountNumberOfTiles
	TXA
	ASL					;>Multiply by 2 due to tile format
	TAX					;>Transfer to X
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
	ADC $00					;>...+ LastTilePos (we are doing LastTilePos - ((NumberOfTiles-1)*TileFormat))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Write static end tiles.
;
;This routine is meant to be used if you have variable-length
;(and position) bar (length and/or position of bar changes in-game)
;as this routine is designed to handle where to position the
;static end tile(s). If you have a fixed-length bar, it is better
;for you to simply write the tiles directly at a fixed location:
;
;	LDA #<TileNumber>
;	STA <StatusBarRAMAddr_TileNumb>
;	LDA #<TileProperties>			;\If you can edit the properties.
;	STA <StatusBarRAMAddr_TileProps>	;/
;
;Where StatusBarRAMAddr_TileNumb and StatusBarRAMAddr_TileProps is
;a RAM location of a tile 1 tile before the left side of the bar
;or 1 tile after the right side of the bar. Repeat this code if you
;wanted both ends.
;
;Note: "Leftside" and "Rightside" terms are literal positions rather
;than the term being inverted when using a leftwards bar, meaning
;if you have the bar X-flipped to fill leftwards, leftside refer
;to the leftside of the bar as opposed to "Leftend" (which is flipped
;to the right).
;
;Input:
;$00 to $02: Same as WriteBarToHUD
;$03 to $05: Same as WriteBarToHUD
;$06: same as WriteBarToHUD (writes tile properties)
;$07: The tile number to write as the static tile.
;
;Note:
; -If not using the extend-left routine:
; --Leftside tile is written at [RAMAddressIn00 - TileFormat]
; --Rightside tile is written at [RAMAddressIn00 + (NumberOfTiles*TileFormat)]
;  otherwise (using extend left, assuming value in RAMAddressIn00 is BEFORE
;  using BarExtendLeft):
; --Leftside tile is written at [RAMAddressIn00 - (NumberOfTiles*TileFormat)]
; --Rightside tile is written at [RAMAddressIn00 + TileFormat]
;
; -If you use this routine with the end tiles being set to non-zero number
;  of pieces, the static tiles are written "past" the end tiles:
;
;   <[===]>
;
;   < and > are static end tiles.
;   [ and ] are fillable end tiles.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if !OWPlusAndSSBSameFormat == 0
		WriteBarStaticTileToHUDLeftside:
			REP #$20
			DEC $00			;\Go to a location 1 tile to the left from the leftside tile.
			if !StatusBar_UsingCustomProperties != 0
				DEC $03			;/
			endif
			SEP #$20
			LDA $07 : STA [$00]		;>Tile number for leftside
			if !StatusBar_UsingCustomProperties != 0
				LDA $06 : STA [$03]		;>Tile properties
			endif
			REP #$20
			INC $00			;\Restore
			if !StatusBar_UsingCustomProperties != 0
				INC $03			;/
			endif
			SEP #$20
			RTL
	endif
	
	WriteBarStaticTileToHUDLeftsideFormat2:
		REP #$20
		DEC $00			;\Go to a location 1 tile to the left from the leftside tile.
		DEC $00			;>DEC twice due to that you have to move 2 bytes over to move over by 1 tile.
		if !StatusBar_UsingCustomProperties != 0
			DEC $03			;/
			DEC $03
		endif
		SEP #$20
		LDA $07 : STA [$00]		;>Tile number for leftside
		if !StatusBar_UsingCustomProperties != 0
			LDA $06 : STA [$03]		;>Tile properties
		endif
		REP #$20
		INC $00			;\Restore
		INC $00
		if !StatusBar_UsingCustomProperties != 0
			INC $03			;/
			INC $03
		endif
			SEP #$20
		RTL
	if !OWPlusAndSSBSameFormat == 0
		WriteBarStaticTileToHUDRightside:
			JSL CountNumberOfTiles
			INX							;After last middle tile.
			TXY
			LDA $07 : STA [$00],y					;>Tile number for rightside
			if !StatusBar_UsingCustomProperties != 0
				LDA.b $06 : STA [$03],y				;>Tile properties 
			endif
			RTL
	endif
WriteBarStaticTileToHUDRightsideFormat2:
	JSL CountNumberOfTiles
	INX							;After last middle tile.
	TXA
	ASL
	TAY
	LDA $07 : STA [$00],y					;>Tile number for rightside
	if !StatusBar_UsingCustomProperties != 0
		LDA.b $06 : STA [$03],y				;>Tile properties 
	endif
	RTL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Write double-ended bar.
;This routine will copy a left-to-right bar and pastes it
;in a location to the left of the original bar. This results
;displaying a bar that fills from the center to the ends.
;
;To be used after calling WriteBarToHUD.
;
;Don't use this with leftwards bar, as this takes the leftmost
;tile on the status bar to handle making a mirrored copy. This
;always SETS the X bit in YXPCCCTT, thus the bar graphic tiles
;must be filling left-to-right by default (in the .bin files).
;
;Input:
; -!Scratchram_GraphicalBar_LeftEndPiece, !Scratchram_GraphicalBar_MiddlePiece,
;   !Scratchram_GraphicalBar_RightEndPiece, and !Scratchram_GraphicalBar_TempLength:
;   Used to determine how many tile bytes.
; -$00-$02: The location of the left-to-right bar (address taken from the first tile)
;   to copy from for the tile numbers.
; -$03-$05: Same as above but tile properties.
;Output:
; -[Address_In_00 - (NumberOfTiles * !StatusBarFormat)] to [Address_In_00 - (1 * !StatusBarFormat)]
;   the area the mirrored copy will be written at, applies to both tile numbers and properties.
;Overwritten:
; -$06-$08: Used for the address of the mirrored copy for tile numbers.
; -$09-$0B: Used for the address of the mirrored copy for tile properties.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
WriteDoubleEndedBar:
	;Get starting address of the flipped bar (write the flipped bar at an address stored in $06 and $09):
		JSL CountNumberOfTiles
		TXY				;>Y = the countdown index
		TXA				;\
		if !StatusBar_UsingCustomProperties != 0
			PHA
		endif
		INC				;|Find where is the leftmost position of the mirrored bar.
		REP #$21			;|(LeftmostPos = LeftToRightBarLeftTilePos - NumberOfTiles)
		AND #$00FF			;|To do 16-bit minus 8-bit, do this instead:
		EOR #$FFFF			;|(LeftmostPos = (-NumberOfTiles) + LeftToRightBarLeftTilePos)
		INC A				;|
		ADC $00				;|
		STA $06				;|
		LDA $02				;|\Bank byte
		STA $08				;|/
		SEP #$20			;/
		if !StatusBar_UsingCustomProperties != 0
			PLA
			TXA				;\Find where is the leftmost position of the mirrored bar.
			INC				;|(LeftmostPos = LeftToRightBarLeftTilePos - NumberOfTiles)
			REP #$21			;|To do 16-bit minus 8-bit, do this instead:
			AND #$00FF			;|(LeftmostPos = (-NumberOfTiles) + LeftToRightBarLeftTilePos)
			EOR #$FFFF			;|
			INC A				;|
			ADC $03				;|
			STA $09				;|
			LDA $05				;|\Bank byte
			STA $0B				;|/
			SEP #$20			;/
		endif
	;Copy tiles from left-to-right bar
		LDX #$00			;>X = the countup index
		.Loop
			..WriteFlippedBarTileNumb
				LDA [$00],y		;>Load a tile starting at the last...
				PHY
				TXY			;>STA [$xx],x don't exist, only STA [$xx,x] which does different.
				STA [$06],y		;>...And then write tile starting on the first.
				PLY
				if !StatusBar_UsingCustomProperties != 0
					..WriteFlippedBarTileProps
						LDA [$03],y		;>Load a tile starting at the last...
						PHY
						TXY			;>STA [$xx],x don't exist, only STA [$xx,x] which does different.
						ORA.b #%01000000	;>Set bit 6 (the X-flip bit)
						STA [$09],y		;>...And then write tile starting on the first.
						PLY
				endif
			..Next
				INX				;>Next tile (addr+1) on the flipped bar.
				DEY				;>Next tile (addr-1) on the left-to-right bar.
				BPL .Loop
	RTL
WriteDoubleEndedBarFormat2:
	;Get starting address of the flipped bar (write the flipped bar at an address stored in $06 and $09):
		JSL CountNumberOfTiles
		TXA
		ASL
		if !StatusBar_UsingCustomProperties != 0
			PHA
		endif
		TAY				;>Y = the countdown index
		INC #2				;\Find where is the leftmost position of the mirrored bar.
		REP #$21			;|(LeftmostPos = LeftToRightBarLeftTilePos - NumberOfTiles)
		AND #$00FF			;|To do 16-bit minus 8-bit, do this instead:
		EOR #$FFFF			;|(LeftmostPos = (-NumberOfTiles) + LeftToRightBarLeftTilePos)
		INC A				;|
		ADC $00				;|
		STA $06				;|
		LDA $02				;|\Bank byte
		STA $08				;|/
		SEP #$20			;/
		if !StatusBar_UsingCustomProperties != 0
			PLA
			INC #2				;\Find where is the leftmost position of the mirrored bar.
			REP #$21			;|(LeftmostPos = LeftToRightBarLeftTilePos - NumberOfTiles)
			AND #$00FF			;|To do 16-bit minus 8-bit, do this instead:
			EOR #$FFFF			;|(LeftmostPos = (-NumberOfTiles) + LeftToRightBarLeftTilePos)
			INC A				;|
			ADC $03				;|
			STA $09				;|
			LDA $05				;|\Bank byte
			STA $0B				;|/
			SEP #$20			;/
		endif
	;Copy tiles from left-to-right bar
		LDX #$00			;>X = the countup index
		.Loop
			..WriteFlippedBarTileNumb
				LDA [$00],y		;>Load a tile starting at the last...
				PHY
				TXY			;>STA [$xx],x don't exist, only STA [$xx,x] which does different.
				STA [$06],y		;>...And then write tile starting on the first.
				PLY
				if !StatusBar_UsingCustomProperties != 0
					..WriteFlippedBarTileProps
						LDA [$03],y		;>Load a tile starting at the last...
						PHY
						TXY			;>STA [$xx],x don't exist, only STA [$xx,x] which does different.
						ORA.b #%01000000	;>Set bit 6 (the X-flip bit)
						STA [$09],y		;>...And then write tile starting on the first.
						PLY
				endif
			..Next
				INX #2				;>Next tile (addr+1) on the flipped bar.
				DEY #2				;>Next tile (addr-1) on the left-to-right bar.
				BPL .Loop
	RTL