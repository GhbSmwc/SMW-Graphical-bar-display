	incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
	incsrc "../GraphicalBarDefines/SpriteOAMSettings.asm"
	incsrc "../GraphicalBarDefines/StatusBarSettings.asm"
	;Subroutines exclusively here
		;NOTE: To be used by the shared subroutines patch and not by uberasm tool. This ASM
		;code is meant to be JSL-JML (JSL to one of the JMLs in the list of subroutines in shared subroutines)
		;from pixi sprites and the included patch "Patch_DrawSpriteGraphicalBar/DrawOamPatch.asm".
		;
		;List of routines
		;-DrawOamGraphicalBarHoriz    ;\These are for things like patches that write to OAM directly without using
		;-DrawOamGraphicalBarVert     ;/anything related to the 12/22 sprite slots, like the sprite status bar.
		;-FindNFreeOAMSlot
		;
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;This writes the graphical bar tiles to OAM (horizontal).
		;
		;Note: Not to be used for “normal sprites” (the generally
		;interactable sprites such as SMW or pixi sprites using 12 (22 for
		;SA-1) slots). This writes OAM directly like most sprite status bar
		;patches. Instead, use DrawSpriteGraphicalBarHoriz instead.
		;
		;Input
		; -!Scratchram_GraphicalBar_FillByteTbl to !Scratchram_GraphicalBar_FillByteTbl + (NumberOfTiles -1)
		;  the tile numbers to write.
		; -$00 to $01: X position, relative to screen border
		; -$02 to $03: Y position, same as above but Y position
		; -$04 to $05: Number of tiles to write
		; -$06: Direction of increasing fill:
		;  -#$00 = left to right
		;  -#$01 = right to left (YXPPCCCT's X bit being set)
		; -$07: Properties (YXPPCCCT).
		;Destroyed:
		; -$08 to $09: Displacement of each tile during processing. Once finished
		;  this will be the tile after the final tile, can be used for placing
		;  static end tile here.
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
				LDA $00				;\Store the initial tile pos in $08 (this makes writing each tile in each 8 pixels to the right/left)
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
						ADC.w .TileDisplacement,x	;|
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
			.TileDisplacement
				dw $0008
				dw $FFF8
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;This writes the graphical bar tiles to OAM (vertical).
		;
		;Note: Not to be used for “normal sprites” (the generally
		;interactable sprites such as SMW or pixi sprites using 12 (22 for
		;SA-1) slots). This writes OAM directly like most sprite status bar
		;patches.
		;
		;Input
		; -!Scratchram_GraphicalBar_FillByteTbl to !Scratchram_GraphicalBar_FillByteTbl + (NumberOfTiles -1)
		;  the tile numbers to write.
		; -$00 to $01: X position, relative to screen border
		; -$02 to $03: Y position, same as above but Y position
		; -$04 to $05: Number of tiles to write
		; -$06: Direction of increasing fill:
		;  -#$00 = bottom to top
		;  -#$01 = top to bottom (YXPPCCCT's Y bit being set)
		; -$07: Properties (YXPPCCCT).
		;Destroyed:
		; -$08 to $09: Displacement of each tile during processing. Once finished
		;  this will be the tile after the final tile, can be used for placing
		;  static end tile here.
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
				LDA $02				;\Store the initial tile pos in $08 (this makes writing each tile in each 8 pixels up/down)
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
						REP #$20						;\Move tile position by 8 pixels
						LDA $08							;|
						CLC							;|
						ADC.w DrawOamGraphicalBarHoriz_TileDisplacement,x	;|
						PLX
						STA $08							;|
						SEP #$20						;/
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