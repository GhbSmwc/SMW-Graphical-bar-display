incsrc "../GraphicalBarDefines/SpriteOAMSettings.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Convert amount of fill to each fill per byte, repeated subtraction edition.
;
;Same as the other version, however does not use multiplication and division
;routines. In fact, this alone does not use any other subroutines AT ALL.
;It works by:
;
;(1) Taking the given/remaining fill amount, compares or subtracts by the
;    maximum amount given for each byte in table: [RemainingFill - Maximum = Difference]
;(1.1) If “Difference” becomes negative (RemainingFill < Maximum) “RemainingFill”
;      (prior this subtraction into the negative) is copied and stored into
;      the byte in the table array and then “RemainingFill” is set to 0.
;      In simple terms, use all the rest if remaining fill is small.
;(1.2) If zero or positive number occurs (RemainingFill >= Maximum),
;      a byte in the table array is set to “Maximum”, “RemainingFill” is set to
;      “Difference” (as in, RemainingFill := RemainingFill - Maximum).
;      In simple terms, remaining amount deducted to “completely fill” a byte
;      in table array.
; (2) Index for tale array increases, and repeat back to step (1)
;
;^Essentially, you are transferring a given amount and “distributing” a given value
; to each consecutive byte in the table. This is division in the form of repeated
; subtraction. Much lighter than the other version.
;
;Input:
; -$00 to $01: The amount of fill for the WHOLE bar.
; -!Scratchram_GraphicalBar_LeftEndPiece: Number of pieces in left byte (0-255), also
;  the maximum amount of fill for this byte itself. If 0, it's not included in table.
; -!Scratchram_GraphicalBar_MiddlePiece: Same as above but each middle byte.
; -!Scratchram_GraphicalBar_RightEndPiece: Same as above but for right end.
; -!Scratchram_GraphicalBar_TempLength: The length of the bar (only counts
;   middle bytes)
;Output:
; -!Scratchram_GraphicalBar_FillByteTbl to !Scratchram_GraphicalBar_FillByteTbl+EndAddress:
;  A table array containing the amount of fill for each byte, the address it ends at is:
;
;  EndAddress = (L + MLength + R) - 1
;
;  -L and R are 0 if set to 0 number of pieces, 1 otherwise on any nonzero values.
;  -MLength is how many middle tiles.
;
; -$00 to $01: The leftover fill amount. If bar isn't full, it will be #$0000, otherwise its
;  [RemainingFill = OriginalFill - EntireBarCapicity]. (overall calculation: RemainingFill = max((InputFillAmount - BarMaximumFull), 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
?DrawGraphicalBarSubtractionLoopEdition:
		LDX #$00
	?.Leftend
		LDA !Scratchram_GraphicalBar_LeftEndPiece       ;\If left end does not exist, skip
		BEQ ?.Middle                                     ;/
		LDA $00                                         ;\Fillamount = Fillamount - MaxAmount (without writing to $00)
		SEC                                             ;|(SBC clears carry if an unsigned underflow occurs (x < 0))
		SBC !Scratchram_GraphicalBar_LeftEndPiece       ;|
		LDA $01                                         ;|
		SBC #$00                                        ;/
		BCC ?..NotFull                                   ;>If Fillamount < MaxAmount, use all remaining fill amount of $00.
		
		?..Full ;>Otherwise set the byte to max, and deduct the remaining fill amount by maximum.
			LDA !Scratchram_GraphicalBar_LeftEndPiece       ;\Full left end.
			STA !Scratchram_GraphicalBar_FillByteTbl        ;/
			LDA $00                                         ;\Fill amount deducted.
			SEC                                             ;|
			SBC !Scratchram_GraphicalBar_LeftEndPiece       ;|
			STA $00                                         ;|
			LDA $01                                         ;|
			SBC #$00                                        ;|
			STA $01                                         ;/
			BRA ?..NextByte
		
		?..NotFull
			LDA $00                                         ;\Take all the rest of $00.
			STA !Scratchram_GraphicalBar_FillByteTbl        ;|
			STZ $00                                         ;|
			STZ $01                                         ;/
		
		?..NextByte
			INX                                             ;>Next tile byte
	?.Middle
		LDA !Scratchram_GraphicalBar_MiddlePiece        ;\If middle does not exist, skip
		BEQ ?.RightEnd                                   ;|
		LDA !Scratchram_GraphicalBar_TempLength         ;|
		BEQ ?.RightEnd                                   ;/
		
		LDA !Scratchram_GraphicalBar_TempLength         ;\Loop counter for number of middle tiles.
		TAY                                             ;/
		
		?..LoopMiddleTiles
			LDA $00                                         ;\Fillamount = Fillamount - MaxAmount (without writing to $00)
			SEC                                             ;|(SBC clears carry if an unsigned underflow occurs (x < 0))
			SBC !Scratchram_GraphicalBar_MiddlePiece        ;|
			LDA $01                                         ;|
			SBC #$00                                        ;/
			BCC ?...NotFull                                   ;>If Fillamount < MaxAmount, use all remaining fill amount of $00.
		
			?...Full ;>Otherwise set the byte to max, and deduct the remaining fill amount by maximum.
				LDA !Scratchram_GraphicalBar_MiddlePiece        ;\Full middle tile.
				STA !Scratchram_GraphicalBar_FillByteTbl,x      ;/
				LDA $00                                         ;\Fill amount deducted.
				SEC                                             ;|
				SBC !Scratchram_GraphicalBar_MiddlePiece        ;|
				STA $00                                         ;|
				LDA $01                                         ;|
				SBC #$00                                        ;|
				STA $01                                         ;/
				BRA ?...NextByte
			
			?...NotFull
				LDA $00                                         ;\Take all the rest of $00.
				STA !Scratchram_GraphicalBar_FillByteTbl,x      ;|
				STZ $00                                         ;|
				STZ $01                                         ;/
			
			?...NextByte
				INX                                             ;>Next middle tile or to the right end.
				DEY                                             ;\Loop till all middle tiles done.
				BNE ?..LoopMiddleTiles                           ;/
	?.RightEnd
		LDA !Scratchram_GraphicalBar_RightEndPiece      ;\If right end does not exist, skip
		BEQ ?.Done                                       ;/

		LDA $00                                         ;\Fillamount = Fillamount - MaxAmount (without writing to $00)
		SEC                                             ;|(SBC clears carry if an unsigned underflow occurs (x < 0))
		SBC !Scratchram_GraphicalBar_RightEndPiece      ;|
		LDA $01                                         ;|
		SBC #$00                                        ;/
		BCC ?..NotFull                                   ;>If Fillamount < MaxAmount, use all remaining fill amount of $00.
		
		?..Full ;>Otherwise set the byte to max, and deduct the remaining fill amount by maximum.
			LDA !Scratchram_GraphicalBar_RightEndPiece      ;\Full right end.
			STA !Scratchram_GraphicalBar_FillByteTbl,x      ;/
			LDA $00                                         ;\Fill amount deducted.
			SEC                                             ;|
			SBC !Scratchram_GraphicalBar_RightEndPiece      ;|
			STA $00                                         ;|
			LDA $01                                         ;|
			SBC #$00                                        ;|
			STA $01                                         ;/
			BRA ?.Done
		
		?..NotFull
			LDA $00                                         ;\Take all the rest of $00.
			STA !Scratchram_GraphicalBar_FillByteTbl,x      ;|
			STZ $00                                         ;|
			STZ $01                                         ;/
	?.Done
		RTL