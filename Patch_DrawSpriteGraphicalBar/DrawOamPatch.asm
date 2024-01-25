;This is the patch version that draws a OAM-based sprite graphical bar, without taking up any sprite slots.

;To get this inserted in your game, make needed changes in the defines, inserted the appropriate graphics and insert via LM,
;and then run this patch via asar.

;Don't move this ASM file (the one you are reading right now), and/or rename/move/delete the ASM files in the routines
;folder, into a subdirectory or parent directory (main/sub folder), else you get define/missing ASM files error. This
;ASM file uses [incsrc <file path>] to include necessary components: ASM files in "GraphicalBarDefines" and "Routines" folder,
;both should be in the main directory of the graphical bar pack.

;Note: The tile table to use is the same as SimpleTest pixi sprite (SMW-Graphical-bar-display/ExGraphics/Sprite/SimpleTest/TileTable.asm)

;This patch is based on:
;-The mega man X HP bar by anonimzwx (https://www.smwcentral.net/?p=section&a=details&id=13994 )
;-The DKR status bar by Ladida, WhiteYoshiEgg, and lx5 (https://www.smwcentral.net/?p=section&a=details&id=24026 )
;and also the suggestion by lx5: https://discord.com/channels/161245277179609089/161247652946771969/827647409429151816

;And yes, this may conflict with some sprite HUD patches because $00A2E6 is somewhat a common address to use.

;You'd think this can be converted to just an uberasm tool code, but this is wrong. Uberasm tool code runs in between after transferring OAM
;RAM ($0200-$041F and $0420-$049F) to SNES register (the code at $008449 does this) and before calling $7F8000 (clears OAM slots), so
;therefore, writing OAM on uberasm tool will get cleared before drawn, therefore, not show up.

;The code here is largely the same as the layer 3 version, but the only difference is that instead of writing to layer 3, it
;is writing to OAM instead, therefore the final steps to draw a graphical bar is different.

;Don't touch these unless you know what you're doing
	;Get defines.
		incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
		incsrc "../GraphicalBarDefines/SpriteOAMSettings.asm"
	;SA-1
		!dp = $0000
		!addr = $0000
		!bank = $800000
		!sa1 = 0
		!gsu = 0

		if read1($00FFD6) == $15
			sfxrom
			!dp = $6000
			!addr = !dp
			!bank = $000000
			!gsu = 1
		elseif read1($00FFD5) == $23
			sa1rom
			!dp = $3000
			!addr = $6000
			!bank = $000000
			!sa1 = 1
		endif
	;Handle bar direction
		!GraphicalBar_OAMXFlip = 0
		!GraphicalBar_OAMYFlip = 0
		if !PatchSprite_Direction == 1
			!GraphicalBar_OAMXFlip = 1
		elseif !PatchSprite_Direction == 3
			!GraphicalBar_OAMYFlip = 1
		endif
;Patch stuff
if !PatchSprite_Uninstall == 0
	org $00A2E6				;>$00A2E6 is the code that runs at the end of the frame, after ALL sprite tiles are written.
	autoclean JML GraphicalBarTest
else
	if read4($00A2E6) != $028AB122			;22 B1 8A 02 -> JSL.L CODE_028AB1
		autoclean read3($00A2E6+1)
	endif
	org $00A2E6
	JSL $028AB1
endif

if !PatchSprite_Uninstall == 0
	freecode
	;;;;;;;;;;;;;;;;;;;;
	;Main code
	;;;;;;;;;;;;;;;;;;;;
	GraphicalBarTest:
		.RestoreOverwrittenCode
			JSL $028AB1		;>Restore the JSL (we write our own OAM after all sprite OAM of SMW are finished)
		.MainCode
				PHB		;\In case if you are going to use tables using 16-bit addressing
				PHK		;|
				PLB		;/
			..InputRatio
				LDA $14							;\Quantity
				STA !Scratchram_GraphicalBar_FillByteTbl		;/
				LDA #$00						;\High byte quantity
				STA !Scratchram_GraphicalBar_FillByteTbl+1		;/
				LDA #$FF						;\Max quantity
				STA !Scratchram_GraphicalBar_FillByteTbl+2		;/
				LDA #$00						;\High byte of max quantity
				STA !Scratchram_GraphicalBar_FillByteTbl+3		;/
			..InputGraphicalBarAttributes
				LDA.b #!Default_PatchSprite_LeftEndPieces		;\Left end normally have 3 pieces.
				STA !Scratchram_GraphicalBar_LeftEndPiece		;/
				LDA.b #!Default_PatchSprite_MiddlePieces		;\Number of pieces in each middle byte/8x8 tile
				STA !Scratchram_GraphicalBar_MiddlePiece		;/
				LDA.b #!Default_PatchSprite_RightEndPieces		;\Right end
				STA !Scratchram_GraphicalBar_RightEndPiece		;/
				LDA.b #!Default_PatchSprite_MiddleLength		;\length (number of middle tiles)
				STA !Scratchram_GraphicalBar_TempLength			;/
			..ConvertToBar
				JSL CalculateGraphicalBarPercentage				;>Get percentage
				JSL RoundAwayEmptyFull
				JSL DrawGraphicalBarSubtractionLoopEdition			;>get bar values.
				LDA #$01							;\Use Level-sprite tileset
				STA $00								;/
				JSL ConvertBarFillAmountToTiles				;>Convert tiles.
				JSL CountNumberOfTiles		;>Have this OUTSIDE the loop and have the information of how many tiles in $02...
				INX
				STX $04				;\Store number of tiles in $04.
				STZ $05				;/
			..WriteOAM
				REP #$20			;\Write position
				if !PatchSprite_BarOnPlayer == 0
					LDA.w #!PatchSprite_BarXPos	;|
					STA $00				;|
					LDA.w #!PatchSprite_BarYPos	;|
					STA $02				;|
				else
					LDA $7E
					if !PatchSprite_BarXPos != 0
						CLC
						ADC.w #!PatchSprite_BarXPos
					endif
					STA $00
					LDA $80
					if !PatchSprite_BarYPos != 0
						CLC
						ADC.w #!PatchSprite_BarYPos
					endif
					STA $02
				endif
				SEP #$20			;/
				if !PatchSprite_Direction < 2
					LDA.b #!GraphicalBar_OAMXFlip
				else
					LDA.b #!GraphicalBar_OAMYFlip
				endif
				STA $06
				LDA.b #(!GraphicalBar_OAMYFlip<<7)+(!GraphicalBar_OAMXFlip<<6)+(3<<4)+(!PatchSprite_Palette<<1)+(!PageNum<<0)
				STA $07
				if !PatchSprite_Direction < 2
					JSL DrawOamGraphicalBarHoriz
				else
					JSL DrawOamGraphicalBarVert
				endif
		.Restore
			SEP #$30
			PLB
			JML $00A2EA		;>Continue onwards
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Subroutines below
	;For the sake of simplicity, I made it so it goes to the parent directory,
	;then into a subfolder "Routines", and to the ASM files there.
	;
	;No need of shared subroutines or copying of the subroutines here.
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;Include commonly used subroutines
			incsrc "../Routines/GraphicalBarConvertToTile.asm"
			incsrc "../Routines/GraphicalBarELITE.asm"
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
endif