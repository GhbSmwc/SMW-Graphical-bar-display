
incsrc "GraphicalBarDefines/SpriteOAMSettings.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This writes the graphical bar tiles to OAM (horizontal).
;
;Note: Not to be used for “normal sprites” (the generally
;interactable sprites such as SMW or pixi sprites using 12 (22 for
;SA-1) slots). This writes OAM directly like most sprite status bar
;patches. Instead, use DrawSpriteGraphicalBarHoriz instead.
;
;Input
; -$00 to $01: X position, relative to screen border
; -$02 to $03: Y position, same as above but Y position
; -$04 to $05: Number of tiles to write
; -$06: Direction of increasing fill:
;  -#$00 = left to right
;  -#$01 = right to left (YXPPCCCT's X bit being set)
; -$07: Properties (YXPPCCCT).
;Destroyed:
; $08 to $09: Displacement of each tile during processing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TileDisplacement:
	dw $0008
	dw $FFF8
DrawOamGraphicalBarHoriz:
	PHB
	PHK
	PLB
	REP #$10
	LDX $04				;>Load number of tiles as 16-bit X
	PHX
	JSR FindNFreeOAMSlot		;>Check if enough slots are available
	PLX
	BCC +
	JMP .Done			;>If no slots available, don't write any of the graphical bar (failsafe).
	+
	LDX #$0000			;>Start loop
	.DrawBar
		REP #$20
		LDA $00				;\Store the initial tile pos in $08 (this makes writing each tile in each 8 pixels to the right)
		STA $08				;/
		SEP #$20
		LDY.w #!GraphicalBar_OAMSlot*4
		..OAMLoop
			;Check if OAM is used by something else, if yes, pick another OAM slot
			...CheckOAMUsed
				LDA $0201|!addr,y
				CMP #$F0
				BEQ ....NotUsed		;>If Y pos is #$F0 (offscreen), it is not used
				
				....Used	;>Otherwise if used, check next slot.
					INY
					INY
					INY
					INY
					BRA ...CheckOAMUsed
				....NotUsed
			;Screen and positions
			...CheckIfOnScreen
				REP #$20	;\If offscreen, go to next tile of the graphical bar, and reuse the same OAM index (don't hog the slots for nothing)
				LDA $08		;|\X position
				CMP #$FFF8+1	;||
				SEP #$20	;||
				BMI ...Next	;||
				REP #$20	;||
				CMP #$0100	;||
				SEP #$20	;||
				BPL ...Next	;|/
				REP #$20	;|
				LDA $02		;|\Y position
				CMP #$FFF8+1	;||
				SEP #$20	;||
				BMI ...Next	;||
				REP #$20	;||
				CMP #$00E0	;||
				SEP #$20	;||
				BPL ...Next	;//
			...XPos
				LDA $08			;\Low 8 bits
				STA $0200|!addr,y	;/
				REP #$30		;>Because we are transferring Y (16-bit) to A (8-bit), it's best to have both registers 16-bit.
				TYA			;>TYA : LSR #4 TAY converts the Y slot index (increments of 4) into slot number (increments of 1)
				LSR #2			;\Handle 9th bit X position
				PHY			;|
				TAY			;|
				LDA $09			;|
				SEP #$20		;|
				AND.b #%00000001	;|
				STA $0420|!addr,y	;/
				PLY
			...YPos
				LDA $02						;\Y pos
				STA $0201|!addr,y				;/
			...TileNumber
				LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Tile number
				STA $0202|!addr,y				;/
			...TileProps
				LDA $06
				BNE ....XFlip
				
				....NoXFlip
					LDA $07
					BRA ....Write
				....XFlip
					LDA $07
					ORA.b #%01000000
				....Write
					STA $0203|!addr,y		;>YXPPCCCT
			...NextOamSlotAndBarTile
				INY			;\Next OAM slot (only next if the OAM tile is onscreen)
				INY			;|
				INY			;|
				INY			;/
			...Next
				PHX
				LDX #$0000
				LDA $06
				BEQ ....NoXFlip
				....XFlip
					INX #2
				....NoXFlip
				REP #$20			;\Move tile position by 8 pixels
				LDA $08				;|
				CLC				;|
				ADC.w TileDisplacement,x	;|
				PLX
				STA $08				;|
				SEP #$20			;/
				INX				;>Next graphical bar slot
				CPX $04				;\Loop until all graphical bar tiles are written.
				BCS +
				JMP ..OAMLoop
				+
	.Done
		SEP #$30				;>Set AXY to 8-bit just in case.
		PLB
		RTL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This writes the graphical bar tiles to OAM (vertical).
;
;Note: Not to be used for “normal sprites” (the generally
;interactable sprites such as SMW or pixi sprites using 12 (22 for
;SA-1) slots). This writes OAM directly like most sprite status bar
;patches.
;
;Input
; -$00 to $01: X position, relative to screen border
; -$02 to $03: Y position, same as above but Y position
; -$04 to $05: Number of tiles to write
; -$06: Direction of increasing fill:
;  -#$00 = bottom to top
;  -#$01 = top to bottom (YXPPCCCT's Y bit being set)
; -$07: Properties (YXPPCCCT).
;Destroyed:
; $08 to $09: Displacement of each tile during processing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawOamGraphicalBarVert:
	PHB
	PHK
	PLB
	REP #$10
	LDX $04				;>Load number of tiles as 16-bit X
	PHX
	JSR FindNFreeOAMSlot		;>Check if enough slots are available
	PLX
	BCC +
	JMP .Done			;>If no slots available, don't write any of the graphical bar (failsafe).
	+
	LDX #$0000			;>Start loop
	.DrawBar
		REP #$20
		LDA $02				;\Store the initial tile pos in $08 (this makes writing each tile in each 8 pixels to the right)
		STA $08				;/
		SEP #$20
		LDY.w #!GraphicalBar_OAMSlot*4
		..OAMLoop
			;Check if OAM is used by something else, if yes, pick another OAM slot
			...CheckOAMUsed
				LDA $0201|!addr,y
				CMP #$F0
				BEQ ....NotUsed		;>If Y pos is #$F0 (offscreen), it is not used
				
				....Used	;>Otherwise if used, check next slot.
					INY
					INY
					INY
					INY
					BRA ...CheckOAMUsed
				....NotUsed
			;Screen and positions
			...CheckIfOnScreen
				REP #$20	;\If offscreen, go to next tile of the graphical bar, and reuse the same OAM index (don't hog the slots for nothing)
				LDA $00		;|\X position
				CMP #$FFF8+1	;||
				SEP #$20	;||
				BMI ...Next	;||
				REP #$20	;||
				CMP #$0100	;||
				SEP #$20	;||
				BPL ...Next	;|/
				REP #$20	;|
				LDA $08		;|\Y position
				CMP #$FFF8+1	;||
				SEP #$20	;||
				BMI ...Next	;||
				REP #$20	;||
				CMP #$00E0	;||
				SEP #$20	;||
				BPL ...Next	;//
			...XPos
				LDA $00			;\Low 8 bits
				STA $0200|!addr,y	;/
				REP #$30		;>Because we are transferring Y (16-bit) to A (8-bit), it's best to have both registers 16-bit.
				TYA			;>TYA : LSR #4 TAY converts the Y slot index (increments of 4) into slot number (increments of 1)
				LSR #2			;\Handle 9th bit X position
				PHY			;|
				TAY			;|
				LDA $01			;|
				SEP #$20		;|
				AND.b #%00000001	;|
				STA $0420|!addr,y	;/
				PLY
			...YPos
				LDA $08						;\Y pos
				STA $0201|!addr,y				;/
			...TileNumber
				LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Tile number
				STA $0202|!addr,y				;/
			...TileProps
				LDA $06
				BNE ....YFlip
				
				....NoYFlip
					LDA $07
					BRA ....Write
				....YFlip
					LDA $07
					ORA.b #%10000000
				....Write
					STA $0203|!addr,y		;>YXPPCCCT
			...NextOamSlotAndBarTile
				INY			;\Next OAM slot (only next if the OAM tile is onscreen)
				INY			;|
				INY			;|
				INY			;/
			...Next
				PHX
				;Unlike the horizontal version, where default is left-to-right, which is a positive direction,
				;vertical, however, going upwards (a default "normal" direction) is a negative direction because
				;most computers, including the SNES, have the Y-coordinate oriented downwards.
				LDX #$0002
				LDA $06
				BEQ ....NoYFlip
				....YFlip
					DEX #2
				....NoYFlip
				REP #$20			;\Move tile position by 8 pixels
				LDA $08				;|
				CLC				;|
				ADC.w TileDisplacement,x	;|
				PLX
				STA $08				;|
				SEP #$20			;/
				INX				;>Next graphical bar slot
				CPX $04				;\Loop until all graphical bar tiles are written.
				BCS +
				JMP ..OAMLoop
				+
	.Done
		SEP #$30				;>Set AXY to 8-bit just in case.
		PLB
		RTL

FindNFreeOAMSlot:
	;Input: $04 = Number of slots open to search for
	;Output: Carry = Set if not enough slots found, Clear if enough slots found
	PHY
	LDY.w #$0000					;>Open slot counter
	LDX.w #!GraphicalBar_OAMSlot*4			;>skip the first four slots
	.loop:						;>to avoid message box conflicts
		CPX #$0200				;\If all slots searched, there is not enough
		BEQ .notEnoughFound			;/open slots being found

		LDA $0201|!addr,x			;\If slot used, that isn't empty
		CMP #$F0				;|
		BNE ..notFree				;/
		INY					;>Otherwise if it is unused, count it
		CPY $04					;\If we find n slots that are free, break
		BEQ .enoughFound			;/
		..notFree:
			INX #4				;\Check another slot
			BRA .loop			;/
	.notEnoughFound:
		SEC
		BRA .Done
	.enoughFound:
		CLC
	.Done
		PLY
		RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This writes the graphical bar tiles to OAM (horizontal).
;
;To be used for “normal sprites” only.
;
;Note: If you are combining this with sprites using 2 different
;tile sizes (8x8 and 16x16), you'll have to manually set the tile
;sizes for $0460 (and also have Y=$FF when finishing the OAM write)
;
;Before calling this subroutine:
;-Call GetDrawInfo to obtain the Y index of which OAM to use (increments of 4).
;-XY register must be 16-bit (REP #$10)
;
;After calling this subroutine:
;-It is not recommend to call GetDrawInfo and finishing the OAM routine more than once.
; Therefore having the sprite GFX and graphical bar GFX together after GetDrawInfo and
; before FinishOAMWrite is better suited.
;-When finishing the OAM write, make sure you add the total number (without the minus 1s on EACH)
; number of OAM tiles used, and THEN subtract by 1 (don't subtract by 1 for the sprite itself
; and the graphical bar, you'll total the subtraction by 2 instead of 1).
;-X index is used for which tile byte of the bar, so make sure you restore X in some way (PHX,
; call this subroutine, PLX or use LDX $15E9).
;
;Input:
; -Y index: The OAM index (increments of 4)
; -$02: X position
; -$03: Y position
; -$04 to $05: Number of tiles to write
; -$06: Direction of increasing fill:
;  -#$00 = left to right
;  -#$01 = right to left (YXPPCCCT's X bit being set)
; -$07: Properties (YXPPCCCT).
;Output:
; -Y index: The OAM index after writing the last tile of the bar.
;Destroyed:
; $08: Displacement of each tile during processing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawSpriteGraphicalBarHoriz:
	.HorizontalBar
		LDX #$00
		LDA $02							;\Initialize displacement loop
		STA $08							;/
		..OAMLoop
			LDA $08						;\X pos
			STA $0300|!addr,y				;/
			
			LDA $03						;\Y pos
			STA $0301|!addr,y				;/
			
			LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Tile number
			STA $0302|!addr,y				;/
			
			...HandleXFlip
				LDA $06
				BEQ ....NoFlip
				....Flip
					LDA $07
					ORA.b #%01000000
					BRA ....Write
				....NoFlip
					LDA $07
				....Write
					STA $0303|!addr,y		;>YXPPCCCT
			...HandleTileSize
				PHY			;\Set tile size to 8x8.
				TYA			;|
				LSR			;|
				TAY			;|
				LDA $0460|!addr,y	;|
				AND.b #%11111101	;|
				STA $0460|!addr,y	;|
				PLY			;/
		..Next
			LDA $06
			BEQ ...NoFlip
			...Flip
				LDA $08			;\Move tile X position by 8 pixels
				SEC			;|
				SBC #$08		;/
				BRA ...Write
			...NoFlip
				LDA $08			;\Move tile X position by 8 pixels
				CLC			;|
				ADC #$08		;/
			...Write
				STA $08			;
			INY			;\Next OAM slot
			INY			;|
			INY			;|
			INY			;/
			INX			;>Next graphical bar slot
			CPX $04			;>...so it doesn't need to execute the subroutine repeatedly.
			BCC ..OAMLoop
			RTL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This writes the graphical bar tiles to OAM (vertical).
;
;To be used for “normal sprites” only.
;
;Note: If you are combining this with sprites using 2 different
;tile sizes (8x8 and 16x16), you'll have to manually set the tile
;sizes for $0460 (and also have Y=$FF when finishing the OAM write)
;
;Before calling this subroutine:
;-Call GetDrawInfo to obtain the Y index of which OAM to use (increments of 4).
;-XY register must be 16-bit (REP #$10)
;
;After calling this subroutine:
;-It is not recommend to call GetDrawInfo and finishing the OAM routine more than once.
; Therefore having the sprite GFX and graphical bar GFX together after GetDrawInfo and
; before FinishOAMWrite is better suited.
;-When finishing the OAM write, make sure you add the total number (without the minus 1s on EACH)
; number of OAM tiles used, and THEN subtract by 1 (don't subtract by 1 for the sprite itself
; and the graphical bar, you'll total the subtraction by 2 instead of 1).
;-X index is used for which tile byte of the bar, so make sure you restore X in some way (PHX,
; call this subroutine, PLX or use LDX $15E9).
;
;Input:
; -Y index: The OAM index (increments of 4)
; -$02: X position
; -$03: Y position
; -$04 to $05: Number of tiles to write
; -$06: Direction of increasing fill:
;  -#$00 = bottom to top
;  -#$01 = top to bottom
; -$07: Properties (YXPPCCCT).
;Output:
; -Y index: The OAM index after writing the last tile of the bar.
;Destroyed:
; $08: Displacement of each tile during processing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawSpriteGraphicalBarVert:
	.verticalBar
		LDX #$00
		LDA $03							;\Initialize displacement loop
		STA $08							;/
		..OAMLoop
			LDA $02						;\X pos
			STA $0300|!addr,y				;/
			
			LDA $08						;\Y pos
			STA $0301|!addr,y				;/
			
			LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Tile number
			STA $0302|!addr,y				;/
			
			...HandleYFlip
				LDA $06
				BEQ ....NoFlip
				....Flip
					LDA $07
					ORA.b #%10000000
					BRA ....Write
				....NoFlip
					LDA $07
				....Write
					STA $0303|!addr,y		;>YXPPCCCT
			...HandleTileSize
				PHY			;\Set tile size to 8x8.
				TYA			;|
				LSR			;|
				TAY			;|
				LDA $0460|!addr,y	;|
				AND.b #%11111101	;|
				STA $0460|!addr,y	;|
				PLY			;/
		..Next
			LDA $06
			BEQ ...NoFlip
			...Flip
				LDA $08			;\Move tile Y position by 8 pixels
				CLC			;|
				ADC #$08		;/
				BRA ...Write
			...NoFlip
				LDA $08			;\Move tile Y position by 8 pixels
				SEC			;|
				SBC #$08		;/
			...Write
				STA $08			;
			INY			;\Next OAM slot
			INY			;|
			INY			;|
			INY			;/
			INX			;>Next graphical bar slot
			CPX $04			;>...so it doesn't need to execute the subroutine repeatedly.
			BCC ..OAMLoop
			RTL