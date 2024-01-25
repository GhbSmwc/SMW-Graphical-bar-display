incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

;List of routines.
;NOTE: "Format2" refers to a variation of the original (original, as in without the "Format2")
;that supports Super Status Bar, Overworld Border plus or any other ASM resource
;in which the tile format is [TTTTTTTT, YXPCCCTT, TTTTTTTT, YXPCCCTT...].
;-WriteBarToHUD
;-WriteBarToHUDFormat2
;-WriteBarToHUDLeftwards
;-WriteBarToHUDLeftwardsFormat2
;-WriteBarToHUDVertically
;-WriteBarToHUDVerticallyFormat2
;-BarExtendLeft
;-BarExtendLeftFormat2
;-CountNumberOfTiles
;-WriteBarStaticTileToHUDLeftside
;-WriteBarStaticTileToHUDLeftsideFormat2
;-WriteBarStaticTileToHUDRightside
;-WriteBarStaticTileToHUDRightsideFormat2
;-WriteDoubleEndedBar
;-WriteDoubleEndedBarFormat2
;-SetupStripe
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
; --$06: The tile properties (YXPCCCTT) you want it to be. Note: This does not automatically
;   modify the X-bit flip flag. You need to flip them yourself for this routine alone for flipped bars.
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
;Note that this is still "left anchored", meaning the address
;to write your bar on would be the left side where the fill
;is at when full.
;
;NOTE: does not reverse the order of data in
;!Scratchram_GraphicalBar_FillByteTbl, it simply writes to the HUD
;in reverse order.
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
;Write graphical bar vertically
;Input;
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
; --$06: The tile properties (YXPCCCTT) you want it to be. Note: This does not automatically
;   modify the Y-bit flip flag. You need to flip them yourself for this routine alone for flipped bars.
; -$07: X = $00 for upwards, X = $02 for downwards, don't use any other values.
;Output:
; -[RAMAddressIn00]-(X*32*Format) where X increases from 0 to NumberOfTiles-1 for upwards, [RAMAddressIn00]+(X*32*Format) where X increases from 0 to NumberOfTiles-1 for downwards:
;  the tiles written to the status bar
; -If using SB/OWB+ patch that allows editing YXPCCCTT in-game and have set !StatusBar_UsingCustomProperties
;  to 1:
; --[RAMAddressIn03]-(X*32*Format) where X increases from 0 to NumberOfTiles-1:
;   the tile properties written:
; -$00 to $02: The address after writing the last tile (as if writing the amount of tiles plus 1), can be used
;  for writing static end tile where the fill ends at.
; -$03 to $05: The address after writing the last tile (as if writing the amount of tiles plus 1), can be used
;  for writing static end tile where the fill ends at.
;
;NOTE: this only works with status bar having a width of 32 8x8 tiles. So far at the time of writing this
;is that the super status bar and SMB3 status bar patches are the only status bar patches that offer
;tile property modification and have more than 1 contiguous row that each have the same number of tiles.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if !OWPlusAndSSBSameFormat == 0
		WriteBarToHUDVertically:
			PHB					;\Adjust bank so that 16-bit table addressing works properly
			PHK					;|
			PLB					;/
			JSL CountNumberOfTiles			;X = number of tiles, -1
			CPX #$FF				;\If 0-1 = (-1), there is no tile to write.
			BEQ .Done				;/(non-existent bar)
			TXY					;>Move to Y (countdown loop)
			LDX #$00				;>X, unlike in WriteBarToHUD, increases, not decreases
			;note to self: indexes cannot be negative and point to addresses byte before the address $xxxxxx
			;in "LDA/STA $XXXXXXX,X/Y". So we have to directly modify the $XXXXXXX.
			;Also, upwards is a "negative direction" but most games displaying vertical health bars increase
			;direction is upwards.
			
			.Loop
				LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Write each tile.
				STA [$00]					;/
				if !StatusBar_UsingCustomProperties != 0
					LDA $06
					STA [$03]
				endif
				PHX			;>Preserve X
				LDX $07
				REP #$20						;\Go to the row above
				LDA $00							;|
				CLC							;|
				ADC .WriteBarToHUDVerticallyUpDownDisplacement,x	;|
				STA $00							;|
				if !StatusBar_UsingCustomProperties != 0
					LDA $03							;|
					CLC							;|
					ADC .WriteBarToHUDVerticallyUpDownDisplacement,x	;|
					STA $03							;|
				endif
				SEP #$20						;/
				PLX			;>Restore X
				..Next
					INX
					DEY
					BPL .Loop
			
			.Done
				PLB
				RTL
		.WriteBarToHUDVerticallyUpDownDisplacement
		dw -32			;>RAM $07 = $00
		dw 32			;>RAM $07 = $02
	endif
	WriteBarToHUDVerticallyFormat2:
		PHB					;\Adjust bank so that 16-bit table addressing works properly
		PHK					;|
		PLB					;/
		JSL CountNumberOfTiles			;X = number of tiles, -1
		CPX #$FF				;\If 0-1 = (-1), there is no tile to write.
		BEQ .Done				;/(non-existent bar)
		TXY					;>Move to Y (countdown loop)
		LDX #$00				;>X, unlike in WriteBarToHUD, increases, not decreases
		;note to self: indexes cannot be negative and point to addresses byte before the address $xxxxxx
		;in "LDA/STA $XXXXXXX,X/Y". So we have to directly modify the $XXXXXXX.
		;Also, upwards is a "negative direction" but most games displaying vertical health bars increase
		;direction is upwards.
		
		.Loop
			LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Write each tile.
			STA [$00]					;/
			if !StatusBar_UsingCustomProperties != 0
				LDA $06
				STA [$03]
			endif
			PHX			;>Preserve X
			LDX $07
			REP #$20						;\Go to the row above
			LDA $00							;|
			CLC							;|
			ADC .WriteBarToHUDVerticallyUpDownDisplacementFormat2,x	;|
			STA $00							;|
			if !StatusBar_UsingCustomProperties != 0
				LDA $03							;|
				CLC							;|
				ADC .WriteBarToHUDVerticallyUpDownDisplacementFormat2,x	;|
				STA $03							;|
			endif
			SEP #$20						;/
			PLX			;>Restore X
			..Next
				INX
				DEY
				BPL .Loop
		
		.Done
			PLB
			RTL
	.WriteBarToHUDVerticallyUpDownDisplacementFormat2
	dw -64			;>RAM $07 = $00
	dw 64			;>RAM $07 = $02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Bar extend leftwards (as the length increases, the final/last/
;rightmost tile stays on the same "rightmost" position and the left
;side moves to the left).
;
;Stripe image note:
; Don't use this for stripe image! Because stripe image explicitly
; stores the XY position (in the first-two bytes of the stripe
; header) rather than having the XY position based what RAM to write
; the bar tiles (like OWB+/SSB+/SMB3SB patches) you would want to
; modify the positions during the writing of the stripe header.
;
; Thankfully I made "SetupStripe", have its X or Y position,
; then subtract by (length-1), which is obtained from "CountNumberOfTiles"
; then store that as the new XY position. See the example in
; "UberasmTool_ExampleUsage/Level_Simple_UsingStripe.asm"
;
;How this routine works: When writing the tiles to the status bar,
;it first uses the "origin" tile, which is the leftmost tile. It is
;always the leftmost tile even when the bar is x-flipped to fill
;leftwards. With a bar that extends leftwards, the left tile is no
;longer at a fixed position, therefore the left tile is calculated
;by being subtracted by a number of tiles towards the left.
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Easy stripe setup-er 2.1. Sets up stripe header, Updates length of stripe,
;and writes the terminating byte. You only need to write the tile data
;afterwards.
;
;-$00: X position (%00XXXXXX, only bits 0-5 used, ranges from 0-63 ($00-$3F))
;-$01: Y position (%00YYYYYY, only bits 0-5 used, ranges from 0-63 ($00-$3F))
;-$02: What layer:
;  $02 = Layer 1
;  $03 = Layer 2
;  $05 = Layer 3
;-$03: Direction and RLE: %DR00000000
;  D = Direction: 0 = horizontal (rightwards), 1 = vertical (downwards)
;  R = RLE: 0 = no (manually write different tiles), 1 = yes (write one
;   tile multiple times, based on input $04-$05).
;-$04 to $05 (16-bit): Number of tiles, minus 1 (a value of 2 here means 3
;  tiles). (If RLE is used, this is how many times a tile is repeated).
;Output:
;-$7F837B-$7F837C: Updated length of stripe data.
;-X register (16-bit, XY registers are 16-bit): The index position of where
; to write tile data (starting at $7F837D+4,x)
;Destroyed:
;-$06-$08: Used when not using RLE, to calculate the terminating byte location.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;note to self
; $7F837B = Length of stripe, counting header and tile data, but not the terminating byte.
; $7F837D+0,x = EHHHYXyy
; $7F837D+1,x = yyyxxxxx
; $7F837D+2,x = DRllllll
; $7F837D+3,x = LLLLLLLL
; $7F837D+4,x = Tile, number
; $7F837D+5,x = Tile properties
; $7F837D+6,x = Terminating byte
SetupStripe:
	.GetWhereToSafelyWriteStripe
		REP #$30		;>16-bit AXY
		LDA $7F837B		;\LDX $XXXXXX does not exist so we need LDA $XXXXXX : TAX to
		TAX			;/get RAM values stored in bank $7F into X register.
	.StartWithBlankHeaderInitally
		LDA #$0000		;\Clear everything out first
		STA $7F837D+0,x		;|
		STA $7F837D+2,x		;/
		SEP #$20
	.Xposition
		LDA $00			;\X bit 0-4
		AND.b #%00011111	;|
		ORA $7F837D+1,x		;|
		STA $7F837D+1,x		;/
		LDA $00			;\X bit 5
		AND.b #%00100000	;|
		LSR #3			;|
		ORA $7F837D+0,x		;|
		STA $7F837D+0,x		;/
	.Yposition
		LDA $01			;\Y bit 0-2
		AND.b #%00000111	;|
		ASL #5			;|
		ORA $7F837D+1,x		;|
		STA $7F837D+1,x		;/
		LDA $01			;\Y bit 3-4
		AND.b #%00011000	;|
		LSR #3			;|
		ORA $7F837D+0,x		;|
		STA $7F837D+0,x		;/
		LDA $01			;\Y bit 5
		AND.b #%00100000	;|
		LSR #2			;|
		ORA $7F837D+0,x		;|
		STA $7F837D+0,x		;/
	.WhatLayer
		LDA $02
		AND.b #%00000111
		ASL #4
		ORA $7F837D+0,x
		STA $7F837D+0,x
	.Direction
		LDA $03
		AND.b #%11000000	;>Failsafe
		ORA $7F837D+2,x
		STA $7F837D+2,x
	.Length
		AND.b #%01000000
		BEQ ..NoRLE
		
		..RLE
			REP #$21		;REP #$21 is 8-bit A with carry cleared
			TXA			;\Update length of stripe. 6 because 2 bytes of 1 tile plus 4 bytes of header)
			ADC #$0006		;|
			STA $7F837B		;/
			SEP #$20		;>8-bit A
			LDA #$FF		;\Terminating byte
			STA $7F837D+6,x		;/
			REP #$20
			LDA $04			;\NumberOfBytes = (NumberOfTiles-1)*2
			ASL			;|
			SEP #$20		;/
			BRA ..Write
		..NoRLE
			REP #$21		;REP #$21 is 8-bit A with carry cleared
			LDA $04			;\Length = 4+(NumberOfTiles*2) = ((NumberOfTiles-1)*2) + 6
			ASL			;|
			CLC			;|
			ADC #$0006		;/
			CLC			;\plus the current length
			ADC $7F837B		;/
			STA $7F837B		;>And that is our new length
			SEP #$20		;>8-bit A
			LDA #$7F		;\Bank byte
			STA $08			;/
			REP #$20		;\4+(NumberOfTiles*2)...
			LDA $04			;|
			INC			;|
			ASL			;|
			CLC			;|>Just in case
			ADC.w #$837D+4		;|
			STA $06			;/
			TXA			;\Plus index ($7F837D+(NumberOfBytesSinceHeader),x is equivalent to $7F837D + NumberOfBytesSinceHeader + X_index)
			CLC			;|
			ADC $06			;|
			STA $06			;/
			SEP #$20
			LDA #$FF		;\Write terminate byte here.
			STA [$06]		;/
			REP #$20
			LDA $04			;\NumberOfBytes = (NumberOfTiles*2)-1
			INC			;|
			ASL			;|
			DEC			;|
			SEP #$20		;/
		..Write
			STA $7F837D+3,x		;\Write length bits
			XBA			;|
			AND.b #%00111111	;|
			ORA $7F837D+2,x		;|
			STA $7F837D+2,x		;/
	.Done
		RTL