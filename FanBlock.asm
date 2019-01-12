;Behaves $025

incsrc "../TOHFanDefines/Defines.asm"

db $42
JMP MarioBelow : JMP MarioAbove : JMP MarioSide
JMP SpriteV : JMP SpriteH : JMP MarioCape : JMP MarioFireball
JMP TopCorner : JMP BodyInside : JMP HeadInside

MarioBelow:
MarioAbove:
MarioSide:
TopCorner:
BodyInside:
HeadInside:
	REP #$20			;\If mario's block hitbox is not at least a
	LDA $9A				;|pixel overlapping the block's hitbox,
	AND #$FFF0			;|then return.
	SEC				;|
	SBC #$000E			;|
	CMP $94				;|
	BPL return16bit			;|
	CLC				;|
	ADC #$001A			;|> this means #$0C if starting at #$00
	CMP $94				;|
	BMI return16bit			;|
	SEP #$20			;/

	LDA !Freeram_TowerOfHeavenFanned	;\Set I bit to fling player upwards.
	ORA.b #%00000001			;|
	STA !Freeram_TowerOfHeavenFanned	;/
SpriteV:
SpriteH:
MarioCape:
MarioFireball:
return16bit:
	SEP #$20
	RTL

print "A fan or wind from Tower of Heaven."