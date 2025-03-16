;This is the patch version that draws a OAM-based sprite graphical bar, without taking up any sprite slots. Note: This is being made
;before UberASM tool 2.0's level "end:" feature was introduced (released on SMWC on 2024-08-02).

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
	;Invoke SA-1
		macro invoke_sa1(label)
			LDA.b #<label>
			STA $3180
			LDA.b #<label>>>8
			STA $3181
			LDA.b #<label>>>16
			STA $3182
			JSR $1E80
		endmacro
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
	org $00A2E6				;>$00A2E6 is the code that runs at the end of the frame, after ALL sprite tiles are written. Not executed if game is paused, along with the OAM clear routine.
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
			if !CPUMode != 0
				%invoke_sa1(.MainCode)
				JMP .BackToSMW
			endif
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
			if !CPUMode != 0
				RTL
			endif
		.BackToSMW
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
			incsrc "../Routines/GraphicalBarWriteToOAM.asm"
endif