;This is similar to [Level_DoubleBar.asm] but SecondFill is stored as a percentage than as a secondary quantity. Meaning
;it holds the number of units in the bar (or pixels) filled. This is useful if you wanted the second fill to follow
;first fill at a constant rate regardless of maximum amount.


incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/StatusBarSettings.asm"

main:
	.IncrementDecrementTest
		..HandleFirstFill
			;Controller to test
				LDA $15				;\Pressing up and down to adjust FirstFill value
				BIT.b #%00001000		;|
				BNE ...Up			;|
				BIT.b #%00000100		;|
				BNE ...Down			;/
				BRA ...Done		
				
				...Up
					LDA !Freeram_FirstQuantity		;\Avoid incrementing past max
					CMP #!DoubleBar_MaxQuantity		;|
					BEQ ...Done				;/
					INC A
					STA !Freeram_FirstQuantity
					if !Setting_DoubleBar_FillMode != 0
						LDA.b #!Setting_DoubleBar_FillMode	;\Anytime there is a change on FirstFill, you apply a delay
						STA !Freeram_SecondQuantityDelay	;/
					endif
					BRA ...Done
				
				...Down
					LDA !Freeram_FirstQuantity		;\Avoid decrementing past 0.
					BEQ ...Done				;/
					DEC A
					STA !Freeram_FirstQuantity
					if !Setting_DoubleBar_FillMode != 0
						LDA.b #!Setting_DoubleBar_FillMode	;\Anytime there is a change on FirstFill, you apply a delay
						STA !Freeram_SecondQuantityDelay	;/
					endif
				...Done
	.SecondFillFollowsFirstFill
		..GetFirstFillPercentage
			;First we need SecondFill's percentage amount
				LDA.b #!Default_MiddleLength					;\Input length (middle)
				STA !Scratchram_GraphicalBar_TempLength				;/
				LDA.b #!Default_LeftPieces					;\Input amount of pieces in each of the 3 types of sections.
				STA !Scratchram_GraphicalBar_LeftEndPiece			;|
				LDA.b #!Default_MiddlePieces					;|
				STA !Scratchram_GraphicalBar_MiddlePiece			;|
				LDA.b #!Default_RightPieces					;|
				STA !Scratchram_GraphicalBar_RightEndPiece			;/
				LDA !Freeram_FirstQuantity					;\Insert quantity
				STA !Scratchram_GraphicalBar_FillByteTbl			;/
				LDA #$00							;\Zero out the high bytes
				STA !Scratchram_GraphicalBar_FillByteTbl+1			;|
				STA !Scratchram_GraphicalBar_FillByteTbl+3			;/
				LDA.b #!DoubleBar_MaxQuantity					;\Max quantity
				STA !Scratchram_GraphicalBar_FillByteTbl+2			;/
				JSL GraphicalBarELITE_CalculateGraphicalBarPercentage		;>Get percentage
		..SecondFillIncrementsDecrementsToFirst
			;Since SecondFill is already a percentage amount, we don't need [CalculateGraphicalBarPercentage].
			;We can then compare two percentage values directly.
			;	$00-$01: SecondFill percentage
			;	!Freeram_SecondQuantity: SecondFill percentage
			;Note: All percentage values here are 8-bit! Unless you make !Freeram_SecondQuantity
			;16-bit. But it is very unlikely because it is unlikely you would have a bar with more
			;than 255 pieces (or pixels) of percentage.
				if !DoubleBar_DisplayIncrease != 0
					LDA !Freeram_SecondQuantityDelay	;\When the delay timer is active, SecondFill won't change.
					BEQ +					;|
					DEC					;|
					STA !Freeram_SecondQuantityDelay	;|
					BRA ...StayFrozen			;|
					+					;/
				endif
				LDA !Freeram_SecondQuantity	;\Determine rather or not SecondFill to increase, decrease or don't change at all.
				CMP $00				;|
				BEQ ...Same			;|
				BCC ...Increment		;/>If SecondFill less than FirstFill, increment towards FirstFill
				
				...Decrement
					if !DoubleBar_DisplayIncrease == 0
						LDA !Freeram_SecondQuantityDelay	;\Only freeze by timer when decrementing, while increment does not need a timer.
						BEQ +					;|
						DEC					;|
						STA !Freeram_SecondQuantityDelay	;|
						BRA ...StayFrozen			;|
						+					;/
						
						LDA !Freeram_SecondQuantity
					endif
					DEC A
					BRA ...Write
				
				...Increment
					if !DoubleBar_DisplayIncrease == 0
						LDA $00
					else
						INC
					endif
				...Write
					STA !Freeram_SecondQuantity
					
				...StayFrozen
				...Same
		..ConvertToBar
			if !DoubleBar_DisplayType == 0
				;Rapid flicker
					LDA $13
					AND.b #%00000001
					BEQ ...ShowFirstFill
					
					...ShowSecondFill			;\Every 2nd frame of the 2-frame-mod-loop, show secondfill
						LDA !Freeram_SecondQuantity	;/
						STA $00
					...ShowFirstFill			;>$00 already contains the FirstFill
			else
				;Separate graphics
			endif
			JSL GraphicalBarELITE_DrawGraphicalBar				;>get bar values.
			JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert tiles.
			LDA.b #!Default_GraphicalBar_Pos_Tile
			STA $00
			LDA.b #!Default_GraphicalBar_Pos_Tile>>8
			STA $01
			LDA.b #!Default_GraphicalBar_Pos_Tile>>16
			STA $02
			if !StatusBar_UsingCustomProperties != 0
				LDA.b #!Default_GraphicalBar_Pos_Properties
				STA $03
				LDA.b #!Default_GraphicalBar_Pos_Properties>>8
				STA $04
				LDA.b #!Default_GraphicalBar_Pos_Properties>>16
				STA $05
				if !Default_LeftwardsBar == 0
					LDA.b #!Default_StatusBar_TilePropertiesSetting			;\Properties
				else
					LDA.b #(!Default_StatusBar_TilePropertiesSetting|(!Default_LeftwardsBar<<6))
				endif
				STA $06								;/
			endif
			if !Default_LeftwardsBar == 0
				if !StatusBarFormat = $01
					JSL GraphicalBarWriteToStatusBar_WriteBarToHUD			;>Write to status bar
				else
					JSL GraphicalBarWriteToStatusBar_WriteBarToHUDFormat2		;>Write to status bar
				endif
			else
				if !StatusBarFormat = $01
					JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwards
				else
					JSL GraphicalBarWriteToStatusBar_WriteBarToHUDLeftwardsFormat2
				endif
			endif
	RTL