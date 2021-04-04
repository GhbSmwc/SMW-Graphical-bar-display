;This is the patch version that draws a OAM-based sprite graphical bar, without taking up any sprite slots.

;This patch is based on:
;-The mega man X HP bar by anonimzwx (https://www.smwcentral.net/?p=section&a=details&id=13994 )
;-The DKR status bar by Ladida, WhiteYoshiEgg, and lx5 (https://www.smwcentral.net/?p=section&a=details&id=24026 )
;and also the suggestion by lx5: https://discord.com/channels/161245277179609089/161247652946771969/827647409429151816

;And yes, this may conflict with some sprite HUD patches because $00A2E6 is somewhat a common address to use.

;You'd think this can be converted to just an uberasm tool code, but this is wrong. Uberasm tool code runs in between after transferring OAM
;RAM ($0200-$041F and $0420-$049F) to SNES register (the code at $008449 does this) and before calling $7F8000 (clears OAM slots), so
;therefore, writing OAM on uberasm tool will get cleared before drawn.

;To use, have the shared subroutines patch, and defines ready:
;Shared subroutines patch should have these:
; SharedSub.asm:
;  Obtaining defines (place under other incsrc's):
;   incsrc "GraphicalBarDefines/GraphicalBarDefines.asm" ;>Get graphical bar defines.
;   incsrc "GraphicalBarDefines/SpriteOAMSettings.asm"
;  Subroutine list:
;   ;; Graphical bar
;   autoclean JML CalculateGraphicalBarPercentage
;   autoclean JML RoundAwayEmpty
;   autoclean JML RoundAwayFull
;   autoclean JML RoundAwayEmptyFull
;   autoclean JML DrawGraphicalBar
;   autoclean JML DrawGraphicalBarSubtractionLoopEdition
;   autoclean JML ConvertBarFillAmountToTiles
;   autoclean JML ConvertBarFillAmountToTilesDoubleBar
;   autoclean JML ConvertBarFillAmountToTilesEdgeOverMultipleTiles
;   autoclean JML CountNumberOfTiles
; SubroutineDefs.asm
;  ; Graphical bar
;  %SetDefine(CalculateGraphicalBarPercentage)
;  %SetDefine(RoundAwayEmpty)
;  %SetDefine(RoundAwayFull)
;  %SetDefine(RoundAwayEmptyFull)
;  %SetDefine(DrawGraphicalBar)
;  %SetDefine(DrawGraphicalBarSubtractionLoopEdition)
;  %SetDefine(ConvertBarFillAmountToTiles)
;  %SetDefine(ConvertBarFillAmountToTilesDoubleBar)
;  %SetDefine(ConvertBarFillAmountToTilesEdgeOverMultipleTiles)
;  %SetDefine(CountNumberOfTiles)
; Like what the readme says, make sure the orders of the subroutines list and the defines of the subroutines match!
;
;And have both the copies of "GraphicalBarDefines" and "SharedSub_Defines" folder at the
;same directory as this ASM file you're reading.

;Don't touch these unless you know what you're doing
	;Get defines
		incsrc "SharedSub_Defines/SubroutineDefs.asm"
		incsrc "GraphicalBarDefines/GraphicalBarDefines.asm"
		incsrc "GraphicalBarDefines/SpriteOAMSettings.asm"
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
	autoclean JML DrawGraphicalBar
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
	DrawGraphicalBar:
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
				JSL !CalculateGraphicalBarPercentage				;>Get percentage
				JSL !RoundAwayEmptyFull
				JSL !DrawGraphicalBar						;>get bar values.
				LDA #$01							;\Use Level-sprite tileset
				STA $00								;/
				JSL !ConvertBarFillAmountToTiles				;>Convert tiles.
				JSL !CountNumberOfTiles		;>Have this OUTSIDE the loop and have the information of how many tiles in $02...
				INX
				STX $06				;>Store number of tiles in $06.
				STZ $07
				REP #$10
				LDX $06				;>Load number of tiles as 16-bit X
				PHX
				JSR FindNFreeOAMSlot
				PLX
				BCC +
				JMP .Restore			;>Not slots available, don't write any of the graphical bar (failsafe).
				+
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
				;$00-$01 = X pos
				;$02-$03 = Y pos
				;$04-$05 = Displacement of each tile (increments or decrements by 8 for every tile)
				;          displacement in whatever was set in !PatchSprite_Direction.
				;$06-$07 = Number of tiles to write.
				;Y index = OAM index (inc by 4)
				;X index = which tile of the graphical bar
				LDX #$0000			;>Start loop
				...DrawBar
					REP #$20
					if !PatchSprite_Direction < 2
						LDA $00				;\Store the initial tile pos in $04 (this makes writing each tile in each 8 pixels to the right)
					else
						LDA $02
					endif
					STA $04				;/
					SEP #$20
					LDY.w #!OAMSlot*4
					....OAMLoop
						;Check if OAM is used by something else, if yes, pick another OAM slot
							.....CheckOAMUsed
								LDA $0201|!addr,y
								CMP #$F0
								BEQ ......NotUsed
								
								......Used
									INY
									INY
									INY
									INY
									BRA .....CheckOAMUsed
								......NotUsed
						;Screen and positions
							.....CheckIfOnScreen
								REP #$20	;\If offscreen, go to next tile of the graphical bar, and reuse the same OAM index (don't hog the slots for nothing)
								if !PatchSprite_Direction < 2
									LDA $04		;|
								else
									LDA $00		;|
								endif
								CMP #$FFF8+1	;|
								SEP #$20	;|
								BMI ....Next	;|
								REP #$20	;|
								CMP #$0100	;|
								SEP #$20	;|
								BPL ....Next	;|
								REP #$20	;|
								if !PatchSprite_Direction < 2
									LDA $02		;|
								else
									LDA $04		;|
								endif
								CMP #$FFF8+1	;|
								SEP #$20	;|
								BMI ....Next	;|
								REP #$20	;|
								CMP #$00E0	;|
								SEP #$20
								BPL ....Next	;/
							.....XPos
								if !PatchSprite_Direction < 2
									LDA $04			;\Low 8 bits
								else
									LDA $00
								endif
								STA $0200|!addr,y	;/
								REP #$30
								TYA
								LSR #2			;\Handle 9th bit X position
								PHY			;|
								TAY			;|
								if !PatchSprite_Direction < 2
									LDA $05			;|
								else
									LDA $01
								endif
								SEP #$20
								AND.b #%00000001	;|
								STA $0420|!addr,y	;/
								PLY
							.....YPos
								if !PatchSprite_Direction < 2
									LDA $02						;\Y pos
								else
									LDA $04
								endif
								STA $0201|!addr,y				;/
						;Tile stuff
							LDA !Scratchram_GraphicalBar_FillByteTbl,x	;\Tile number
							STA $0202|!addr,y				;/
							;YXPPCCCT
							;(!Yflip<<7)+(!XFlip<<6)+(!Priority<<4)+(!PatchSprite_Palette<<1)+(!PageNum<<0)
							;!PageNum: (0-1) - page number
							;!Palette: (0-7) - palette
							;!Priority: (0-3) - priority
							;!XFlip: (0-1) - X flip
							;!YFlip: (0-1) - Y flip
								LDA.b #(!GraphicalBar_OAMYFlip<<7)+(!GraphicalBar_OAMXFlip<<6)+(3<<4)+(!PatchSprite_Palette<<1)+(!PageNum<<0)
								STA $0203|!addr,y		;>YXPPCCCT
					....NextOamSlotAndBarTile
						INY			;\Next OAM slot (only next if the OAM tile is onscreen)
						INY			;|
						INY			;|
						INY			;/
					....Next
						REP #$20			;\Move tile position by 8 pixels
						LDA $04				;|
						if or(equal(!PatchSprite_Direction, 0),equal(!PatchSprite_Direction, 3))
							CLC			;|
							ADC #$0008		;|
						else
							SEC			;|
							SBC #$0008		;|
						endif
						STA $04				;|
						SEP #$20			;/
						INX				;>Next graphical bar slot
						CPX $06				;\Loop until all graphical bar tiles are written.
						BCC ....OAMLoop			;/
				...Done
		.Restore
			SEP #$30
			PLB
			JML $00A2EA		;>Continue onwards
			
	;;;;;;;;;;;;;
	;Subroutines
	;;;;;;;;;;;;;
		FindNFreeOAMSlot:
			;Input: $06 = Number of slots open to search for
			;Output: Carry = Set if not enough slots found, Clear if enough slots found
			PHY
			LDY.w #$0000					;>Open slot counter
			LDX.w #!OAMSlot*4					;>skip the first four slots
			.loop:						;>to avoid message box conflicts
				CPX #$0200				;\If all slots searched, there is not enough
				BEQ .notEnoughFound			;/open slots being found

				LDA $0201|!addr,x			;\If slot used, that isn't empty
				CMP #$F0				;|
				BNE ..notFree				;/
				INY					;>Otherwise if it is unused, count it
				CPY $06					;\If we find n slots that are free, break
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