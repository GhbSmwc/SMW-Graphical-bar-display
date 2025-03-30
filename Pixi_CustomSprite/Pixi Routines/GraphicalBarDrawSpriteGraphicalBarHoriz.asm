incsrc "../GraphicalBarDefines/SpriteOAMSettings.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This writes the graphical bar tiles to OAM (horizontal).
;
;To be used for “normal sprites” only (in this case, pixi).
;
;Note: If you are combining this with sprites using 2 different
;tile sizes (8x8 and 16x16), you'll have to manually set the tile
;sizes for $0460 (and also have Y=$FF when finishing the OAM write)
;
;Before calling this subroutine:
; - Call GetDrawInfo to obtain the Y index of which OAM to use (increments of 4).
; - XY register must be 16-bit (REP #$10)
;
;After calling this subroutine:
; - It is not recommended to call GetDrawInfo and finishing the OAM routine more than once.
;   Therefore having the sprite GFX and graphical bar GFX together after GetDrawInfo and
;   before FinishOAMWrite is better suited.
; - When finishing the OAM write, make sure you add the total number (without the minus 1s on EACH)
;   number of OAM tiles used, and THEN subtract by 1 (don't subtract by 1 for the sprite itself
;   and the graphical bar, you'll total the subtraction by 2 instead of 1).
; - X index is used for which tile byte of the bar, so make sure you restore X in some way (PHX,
;   call this subroutine, PLX or use LDX $15E9).
;
;Input:
; - !Scratchram_GraphicalBar_FillByteTbl to !Scratchram_GraphicalBar_FillByteTbl + (NumberOfTiles -1)
;   the tile numbers to write.
; - Y index: The OAM index (increments of 4)
; - $02: X position (where the fill start at)
; - $03: Y position (where the fill start at)
; - $04 to $05: Number of tiles to write
; - $06: Direction of increasing fill:
; -- #$00 = left to right
; -- #$01 = right to left (YXPPCCCT's X bit being set)
; - $07: Properties (YXPPCCCT).
;Output:
; - Y index: The OAM index after writing the last tile of the bar.
;Destroyed:
; - $08: Displacement of each tile during processing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
?DrawSpriteGraphicalBarHoriz:
	?.HorizontalBar
		LDX #$00
		LDA $02							;\Initialize displacement loop
		STA $08							;/
		?..OAMLoop
			LDA $08						;\X pos
			STA $0300|!addr,y				;/
			
			LDA $03						;\Y pos
			STA $0301|!addr,y				;/
			
			LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Tile number
			STA $0302|!addr,y				;/
			
			?...HandleXFlip
				LDA $06
				BEQ ?....NoFlip
				?....Flip
					LDA $07
					ORA.b #%01000000
					BRA ?....Write
				?....NoFlip
					LDA $07
				?....Write
					STA $0303|!addr,y		;>YXPPCCCT
			?...HandleTileSize
				PHY			;\Set tile size to 8x8.
				TYA			;|
				LSR #2			;|
				TAY			;|
				LDA $0460|!addr,y	;|
				AND.b #%11111101	;|
				STA $0460|!addr,y	;|
				PLY			;/
		?..Next
			LDA $06
			BEQ ?...NoFlip
			?...Flip
				LDA $08			;\Move tile X position by 8 pixels
				SEC			;|
				SBC #$08		;/
				BRA ?...Write
			?...NoFlip
				LDA $08			;\Move tile X position by 8 pixels
				CLC			;|
				ADC #$08		;/
			?...Write
				STA $08			;
			INY			;\Next OAM slot
			INY			;|
			INY			;|
			INY			;/
			INX			;>Next graphical bar slot
			CPX $04			;>...so it doesn't need to execute the subroutine repeatedly.
			BCC ?..OAMLoop
			RTL