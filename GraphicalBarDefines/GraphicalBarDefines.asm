;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This ASM file contains defines relating only to the subroutines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SA-1 handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Only include this if there is no SA-1 detection, such as including this
;in a (seperate) patch.
if defined("sa1") == 0
	!dp = $0000
	!addr = $0000
	!sa1 = 0
	!gsu = 0

	if read1($00FFD6) == $15
		sfxrom
		!dp = $6000
		!addr = !dp
		!gsu = 1
	elseif read1($00FFD5) == $23
		sa1rom
		!dp = $3000
		!addr = $6000
		!sa1 = 1
	endif
endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Graphical bar defines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Ram stuff (note that ram banks $7E/$7F cannot be accessed when
	;sa-1 mode is running, so use banks $40/$41).
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;Bar attributes. These are RAM addresses that sets the number of pieces
		;and length of the middle. Not to be confused with default values
		;in StatusBarSettings where it mentions (excluding the outer brackets):
		;-[Tile settings (length does not apply to [ExtendLeftwards.asm] as that is variable in-game)]
		;Since that is the actual number of pieces, while this is the RAM address locations that stores
		;them in case if you have 2+ bars with different attributes.
			if !sa1 == 0
				!Scratchram_GraphicalBar_LeftEndPiece   = $60		;>normal ROM
			else
				!Scratchram_GraphicalBar_LeftEndPiece   = $60	;>SA-1 ROM
			endif
				;^[1 byte] number of pieces on the left end byte/8x8 tile.

			if !sa1 == 0
				!Scratchram_GraphicalBar_MiddlePiece    = $61
			else
				!Scratchram_GraphicalBar_MiddlePiece    = $61
			endif
				;^[1 byte] number of pieces on each middle byte/8x8 tile.

			if !sa1 == 0
				!Scratchram_GraphicalBar_RightEndPiece  = $62
			else
				!Scratchram_GraphicalBar_RightEndPiece  = $62
			endif
				;^[1 byte] number of pieces on the right end byte/8x8 tile.

			if !sa1 == 0
				!Scratchram_GraphicalBar_TempLength  = $7F8449
			else
				!Scratchram_GraphicalBar_TempLength  = $404140
			endif
				;^[1 byte] how many middle bytes/8x8 to be written on the bar. This is
				;basically the length of the bar.
		;Fill byte table:
			if !sa1 == 0
				!Scratchram_GraphicalBar_FillByteTbl = $7F844A
			else
				!Scratchram_GraphicalBar_FillByteTbl = $404141
			endif
				;^[>= 4 bytes] Used to hold the fill amount for each
				; byte to be converted into tile numbers to be used for display.
				; The amount of bytes used is:
				;
				; BytesUsed = LeftExist + (MiddleExist*Length) + RightExist
				;
				; where any variable with "exist" in name is either 0
				; (pieces is 0) or 1 (pieces is nonzero).
				;
				; Also used for calculating the percentage:
				;  +$00 to +$01 = quantity
				;  +$02 to +$03 = max quantity
		;Misc RAM use
			;RAM testing for Range-based bar ("Level_RangeBased.asm"):
				if !sa1 == 0
					!Freeram_RangeBasedValue = $0DDB|!addr
				else
					!Freeram_RangeBasedValue = $0DDB|!addr
				endif
					;^[2 bytes] The value to test what range it is in (quantity).
	
				if !sa1 == 0
					!Scratchram_WhatRange = $14B0|!addr
				else
					!Scratchram_WhatRange = $14B0|!addr
				endif
					;^[2 bytes] Range number of what interval the quantity is in.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Graphical bar Settings
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		!Setting_GraphicalBar_IndexSize = 0
			;^0 = 8-bit indexing for byte table, 1 = 16-bit. Only set this to 1
			; if you, somehow wanted to have a middle length of 255 middle bytes/8x8
			; (which is extremely unlikely), and have any bar ends enabled
			; (this would have the index value being at $0100 or $0101).
			;
			; each byte/tile consumes an index, so if this is 0, and you have both ends
			; enabled, your actual middle's maximum is 253 (because 253 middle bytes
			; plus 2 ends = 255 total bytes used up).
			;
			; Do note that most routines would assume there are no more than 255 tile bytes,
			; reasons being:
			; -The loops' end uses BPL, so it will end if the index value is #$80-#$FF.
			; -If you are using a 2 adjacent bytes per 8x8 tile (!StatusBarFormat = $02),
			;  the tile processing uses ASL (multiply index by 2) and therefore "overflow"
			;  should the number being multiplyed by 2 be >= #$80. Thus the limit, along
			;  above limitation, is 64 tile bytes (indexes $00 to $3F).
			;
			; But this is highly unlikely as the screen is 32 8x8 tiles wide.

		!Setting_GraphicalBar_SNESMathOnly = 0
			;^Info follows:
			;-Set this to 0 if any of your code AT LEAST calls the graphical bar routine under the SA-1 processor;
			; should you ever have one code calls this using SA-1 and the other calls using SNES, or ALL calls
			; using SA-1.
			;-Set this to 1 if *all* codes that call graphical bar routine are not using SA-1 processor.
			;
			;The reason for this is because if the user only uses the graphical bar routine on a SA-1
			;ROM, but never processed the routine by SA-1, using SA-1's math registers is useless as
			;the SNES's 8-bit math registers ($4202, $4203, $4216-$4217) become available for 8-bit*8-bit = 16-bit.
			;
			;Things to note:
			;
			;-SNES' math handles 8bit*8bit = 16bit numbers, all unsigned. This will be unavailable to
			; be used if processing SA-1.
			;-SA-1's math are 16bit*16bit = 32bit, all *signed*. The register is always available
			; to use regardless if SNES or SA-1 being used.

		!Setting_Beta32bitMultiplication = 0
			;^In case if you are editing this code to handle 32 bit for quantity, set this to 1.
			; This is a beta code.

		!Setting_GraphicalBar_SecondFillByteTableOffset          = 32
			;^Only used when using a double-bar via "differently colored fill".
			; This creates another "!Scratchram_GraphicalBar_FillByteTbl" plus n (the value in
			; this define) bytes from the original fill byte table. This is also the maximum number of 8x8
			; tile bytes you are going to use (as in, if you have multiple bars with
			; their own lengths, the one the longest plus any of the two existing end
			; tiles is this number) for your entire game. Be careful not to make it
			; possible for any bars to have more tiles than this number, else the first n tiles
			; will be overwritten and glitched out.
			;
			; Here is what it looks like using ASCII art (each character within brackets is a tile byte) using 9 tiles out of max of 32:
			;  SecondFill                     FirstFill
			;     |                               |
			;     V                               V
			; [<=======>.......................<=======>]
			;When you keep lengthening the bar:
			; [<========>......................<========>] (10 tiles out of 32 used)
			; [<=========>.....................<=========>] (11 tiles out of 32 used)
			; [<==========>....................<==========>] (12 tiles out of 32 used)
			;After a while:
			; [<============================>..<============================>] (30 tiles out of 32 used)
			; [<=============================>.<=============================>] (31 tiles out of 32 used)
			; [<==============================><==============================>] (32 tiles out of 32 used)
			; [<===============================>===============================>] (33 tiles out of 32 used, left end of FirstFill is overwritten by SecondFill's right end)
			;And longer will start overwriting additional tiles of FirstFill.
			;
			; < is left end.
			; = is middle tile.
			; > is right end.
			; . is unused RAM that can be garbage or used when bar is extended.



;Print descriptions (if you have trouble tracking your RAM address)
	!Setting_GraphicalBar_Debug_DisplayRAMUsage = 0
		;^0 = no, 1 = display RAM usage on console window.
	
	;print onto console window
		if !Setting_GraphicalBar_Debug_DisplayRAMUsage != 0
			print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
			print ";Graphical bar routine RAM usage"
			print ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
			print "-Left end piece: $", hex(!Scratchram_GraphicalBar_LeftEndPiece)
			print "-Middle piece: $", hex(!Scratchram_GraphicalBar_MiddlePiece)
			print "-Right end piece: $", hex(!Scratchram_GraphicalBar_RightEndPiece)
			print "-Middle length: $", hex(!Scratchram_GraphicalBar_TempLength)
			print "-Fill byte table: $", hex(!Scratchram_GraphicalBar_FillByteTbl), " to $", hex(!Scratchram_GraphicalBar_FillByteTbl+31), " (at max length)"
			print "-Fill byte table (double bar, 2 fills): $", hex(!Scratchram_GraphicalBar_FillByteTbl+!Setting_GraphicalBar_SecondFillByteTableOffset), " to $", hex(!Scratchram_GraphicalBar_FillByteTbl+31+!Setting_GraphicalBar_SecondFillByteTableOffset), " (at max length)"
		endif