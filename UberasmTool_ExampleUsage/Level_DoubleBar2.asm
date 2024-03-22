;This is similar to [Level_DoubleBar.asm] but SecondFill (!Freeram_SecondQuantity) is stored as a percentage than as
;a secondary quantity. Meaning it holds the number of units in the bar (or pixels) filled. This is useful if you
;wanted the second fill to follow first fill at a constant rate regardless of maximum amount, but SecondFill
;will also display as if FirstFill changes should the maximum changes.

;This only supports SecondFill following FirstFill, as to test the rate in which one bar follows another.
;Also, the dealy MUST be a nonzero value since having huge MaxQuantity makes the bar representing current value
;move so slow that the previous value display ("following" bar) catch up easily, making it impossible to see
;the second bar.

;While having !DoubleBar_DisplayIncrease set to 1, If you look at the codes at [...HandleSwappableFills], it has
;only one check of which of the two FillPercentages is greater and duplicate code to write to $00 before writing to
;GraphicalBarELITE_DrawGraphicalBar, which is different than [Level_DoubleBar.asm] which has two checks, each
;comparing the two literal quantities but having a single code to use GraphicalBarELITE_DrawGraphicalBar. You may be
;asking, what's up with the inconsistency? Well, the other one has longer code, including executing BOTH
;GraphicalBarELITE_CalculateGraphicalBarPercentage, so I think it is better to have no duplicate of long codes and
;have two checks, while this one have duplicate of shorter codes, with a single check of the two fill percentage.

!SafeDelay = clamp(!Setting_DoubleBar_FillMode, 1, 255)


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
					LDA.b #!SafeDelay	;\Anytime there is a change on FirstFill, you apply a delay
					STA !Freeram_SecondQuantityDelay	;/
					BRA ...Done
				
				...Down
					LDA !Freeram_FirstQuantity		;\Avoid decrementing past 0.
					BEQ ...Done				;/
					DEC A
					STA !Freeram_FirstQuantity
					LDA.b #!SafeDelay	;\Anytime there is a change on FirstFill, you apply a delay
					STA !Freeram_SecondQuantityDelay	;/
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
				LDA #$00							;\Zero out the high byte of quantity
				STA !Scratchram_GraphicalBar_FillByteTbl+1			;/
				JSR GetMaxQuantityPercentage
				JSL GraphicalBarELITE_CalculateGraphicalBarPercentage		;>Get percentage
				if !DoubleBar_RoundAway != 0
					JSL GraphicalBarELITE_RoundAwayEmptyFull
				endif
		..SecondFillIncrementsDecrementsToFirst
			;Since SecondFill is already a percentage amount, we don't need [CalculateGraphicalBarPercentage].
			;We can then compare two percentage values directly.
			;	$00-$01: FirstQuantity percentage
			;	!Freeram_SecondQuantity: SecondQuantity percentage
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
					;WriteGraphicalBar
						JSL GraphicalBarELITE_DrawGraphicalBar				;>get bar values.
						STZ $00								;>Use Level-layer3 tileset
						JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTiles	;>Convert to tiles.
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
			else
				;Separate graphics
					if !DoubleBar_DisplayIncrease == 0
						...FirstQuantity
							JSL GraphicalBarELITE_DrawGraphicalBar
							....TransferFirstFillBar
								PHB
								REP #$30
								LDA.w #!Setting_GraphicalBar_SecondFillByteTableOffset-1					;>Number of bytes to transfer, -1 (because byte 0 is included)
								LDX.w #!Scratchram_GraphicalBar_FillByteTbl							;>Source address
								LDY.w #!Scratchram_GraphicalBar_FillByteTbl+!Setting_GraphicalBar_SecondFillByteTableOffset	;>Destination address
								MVN (!Scratchram_GraphicalBar_FillByteTbl>>16), (!Scratchram_GraphicalBar_FillByteTbl>>16)	;>Move them
								SEP #$30
								PLB
						...SecondQuantity
							LDA !Freeram_SecondQuantity
							STA $00
							STZ $01
							JSL GraphicalBarELITE_DrawGraphicalBar
							JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTilesDoubleBar	;>Convert to tiles.
					else
						...HandleSwappableFills
							;We can display increase (such as healing) by swapping FirstQuantity and SecondQuantity when written into
							;$00 prior calling DrawGraphicalBar. In this event, FirstFill ends up displaying as SecondQuantity (i.e increased current HP)
							;and SecondFill displaying as FirstQuantity (Previous HP that was lower then current HP).
								LDA $00
								CMP !Freeram_SecondQuantity
								BCS ....SwapThem
								....NoSwap
									.....FirstQuantity
										JSL GraphicalBarELITE_DrawGraphicalBar
										......TransferFirstFillBar
											PHB
											REP #$30
											LDA.w #!Setting_GraphicalBar_SecondFillByteTableOffset-1					;>Number of bytes to transfer, -1 (because byte 0 is included)
											LDX.w #!Scratchram_GraphicalBar_FillByteTbl							;>Source address
											LDY.w #!Scratchram_GraphicalBar_FillByteTbl+!Setting_GraphicalBar_SecondFillByteTableOffset	;>Destination address
											MVN (!Scratchram_GraphicalBar_FillByteTbl>>16), (!Scratchram_GraphicalBar_FillByteTbl>>16)	;>Move them
											SEP #$30
											PLB
									.....SecondQuantity
										LDA !Freeram_SecondQuantity
										STA $00
										STZ $01
										JSL GraphicalBarELITE_DrawGraphicalBar
										JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTilesDoubleBar	;>Convert to tiles.
									BRA ....WriteToTableDone
								....SwapThem
									.....SecondQuantity
										LDA $00										;\Preserve FirstQuantity
										PHA										;/
										LDA !Freeram_SecondQuantity							;\SecondQuantityPercentage
										STA $00										;/
										STZ $01
										JSL GraphicalBarELITE_DrawGraphicalBar					;>FirstFill
										......TransferFirstFillBar
											PHB
											REP #$30
											LDA.w #!Setting_GraphicalBar_SecondFillByteTableOffset-1					;>Number of bytes to transfer, -1 (because byte 0 is included)
											LDX.w #!Scratchram_GraphicalBar_FillByteTbl							;>Source address
											LDY.w #!Scratchram_GraphicalBar_FillByteTbl+!Setting_GraphicalBar_SecondFillByteTableOffset	;>Destination address
											MVN (!Scratchram_GraphicalBar_FillByteTbl>>16), (!Scratchram_GraphicalBar_FillByteTbl>>16)	;>Move them
											SEP #$30
											PLB
									.....FirstQuantity
										PLA									;\FirstQuantityPercentage
										STA $00									;/
										STZ $01
										JSL GraphicalBarELITE_DrawGraphicalBar					;>SecondFill
										JSL GraphicalBarConvertToTile_ConvertBarFillAmountToTilesDoubleBar	;>Convert to tiles.
								....WriteToTableDone
					endif
					;WriteToStatusbar
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
			endif
	RTL
	
GetMaxQuantityPercentage:
	LDA #!DoubleBar_MaxQuantity				;\Max quantity (example: max HP). Can be a fixed value (#$) or adjustable RAM in-game.
	STA !Scratchram_GraphicalBar_FillByteTbl+2		;/
	LDA #$00						;\High byte above, same format as you would do for quantity, so do the same
	STA !Scratchram_GraphicalBar_FillByteTbl+3		;/as that if your value is 8-bit.
	RTS