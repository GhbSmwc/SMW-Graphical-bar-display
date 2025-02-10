?GetMaxBarInAForRoundToMaxCheck:
	;Must be called with 16-bit A.
	;Get the full number of pieces (for checking if rounding a number between Max-1 and Max to Max.)
	;Output: A (16-bit): Maximum fill amount (processor flag for A is 8-bit though)
	;Destroys:
	; -$00-$07 in LoROM
	; -$04-$05 in SA-1
	if !CPUMode == 0
		LDA !Scratchram_GraphicalBar_MiddlePiece	;\Get amount of pieces in middle
		AND #$00FF					;|
		STA $00						;|
		LDA !Scratchram_GraphicalBar_TempLength		;|
		AND #$00FF					;|
		STA $02						;/
		SEP #$20
		%MathMul16_16()				;>[$04-$07: Product]
	else
		SEP #$20
		LDA !Scratchram_GraphicalBar_MiddlePiece
		STA $4202
		LDA !Scratchram_GraphicalBar_TempLength
		STA $4203
		XBA						;\Wait 8 cycles (XBA takes 3, NOP takes 2) for calculation
		XBA						;|
		NOP						;/
		LDA $4216					;\[$04-$07: Product]
		STA $04						;|
		LDA $4217					;|
		STA $05						;/
	endif
	;add the 2 ends tiles amount (both are 8-bit, but results 16-bit)
	
	;NOTE: should the fill amount be exactly full OR greater, Y will be #$00.
	;This is so that greater than full is 100% treated as exactly full.
	LDA #$00					;\A = $YYXX, (initially YY is $00)
	XBA						;/
	LDA !Scratchram_GraphicalBar_LeftEndPiece	;\get total pieces
	CLC						;|\carry is set should overflow happens (#$FF -> #$00)
	ADC !Scratchram_GraphicalBar_RightEndPiece	;//
	XBA						;>A = $XXYY
	ADC #$00					;>should that overflow happen, increase the A's upper byte (the YY) by 1 ($01XX)
	XBA						;>A = $YYXX, addition maximum shouldn't go higher than $01FE. A = 16-bit total ends pieces
	REP #$20
	CLC						;\plus middle pieces = full amount
	ADC $04						;/
	RTL