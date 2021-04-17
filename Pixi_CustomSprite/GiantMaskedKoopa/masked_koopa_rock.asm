;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Golem Rock, by Dispari Scuro
;; With help from smkdan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!FallSFX = $28              ; Sound effect to play when the sprite is falling
!FallBank = $1DF9

!Y_Speed = $D0              ; The sprite's vertical speed.

print "INIT ",pc
    RTL

print "MAIN ",pc
    PHB : PHK : PLB
    CMP #$09
    BCS HandleStunned
    JSR SpriteCode
    PLB
    RTL

HandleStunned:
    LDA !1540,x
    BEQ +
        LDA #$1F
        STA !1540,x
+   LDA !167A,x             ; Set to interact with Mario
    AND #$7F
    STA !167A,x

    LDY !15EA,x             ; Replace Goomba's tile
    PHX
    LDX.w Tilemap
    LDA $0302|!Base2,y
    CMP #$A8
    BEQ +
        LDX.w Tilemap+1
+   TXA
    STA $0302|!Base2,y
    PLX
    PLB
    RTL

Return:
    RTS

SpriteCode:
    LDA !1540,x
    CMP #$00
    BEQ +
        LDA #$FF
        STA !1540,x
+   JSR Graphics                ; graphics routine
    LDA !14C8,x                 ; \ 
    CMP #$08                    ;  | if status = 8, continue
    BEQ NormalCode              ; /

    STZ !B6,x
    STZ !AA,x
    STZ !14C8,x
    LDA !D8,x                   ; \ 
    STA $98                     ;  |
    LDA !14D4,x                 ;  |
    STA $99                     ;  | Grab X/Y location of sprite...
    LDA !E4,x                   ;  | ...to store for shatter block location.
    STA $9A                     ;  |
    LDA !14E0,x                 ;  |
    STA $9B                     ; /
    PHB                         ; preserve current bank
    LDA #$02                    ; push 02
    PHA
    PLB                         ; bank = 02
    LDA #$00                    ; default shatter
    JSL $028663|!BankB          ; shatter block
    PLB                         ; restore bank
    RTS

NormalCode:
    LDA $9D                     ; \ if sprites locked, return
    BNE Return                  ; /
    LDA #$00
    %SubOffScreen()             ; handle off screen situation

    LDA !1588,x                 ; if on the ground, reset the turn counter
    AND #$04
    BEQ InAir

    LDA !151C,x
    INC A
    AND #$03
    STA !151C,x
    LDA.b #!Y_Speed             ; | Sprite jumping
    STA !AA,x                   ; /
    LDA #$04                    ; \ status = 4 (being killed by spin jump)
    STA !14C8,x                 ; /

    STZ !B6,x
    STZ !AA,x
    STZ !14C8,x
    LDA !D8,x                   ; \ 
    STA $98                     ;  |
    LDA !14D4,x                 ;  |
    STA $99                     ;  | Grab X/Y location of sprite...
    LDA !E4,x                   ;  | ...to store for shatter block location.
    STA $9A                     ;  |
    LDA !14E0,x                 ;  |
    STA $9B                     ; /
    PHB                         ; preserve current bank
    LDA #$02                    ; push 02
    PHA
    PLB                         ; bank = 02
    LDA #$00                    ; default shatter
    JSL $028663|!BankB          ; shatter block
    PLB                         ; restore bank

    RTS

InAir:
    JSL $01802A|!BankB          ; update position based on speed values

    JSL $018032|!BankB          ; interact with sprites
    JSL $01A7DC|!BankB          ; check for mario/sprite contact (carry set = contact)
    BCC Return2                 ; return if no contact
    %SubVertPos()
    LDA $0F                     ; \ if mario isn't above sprite, and there's vertical contact...
    CMP #$E6                    ;  |     ... sprite wins
    BPL SpriteWins              ; /
    LDA $7D                     ; \if mario speed is upward, return
    BMI Return2                 ; /
    LDA !extra_prop_1,x
    AND #$10
    BEQ +
        LDA $140D|!Base2        ; \ if mario is spin jumping, goto SpinKill
        BNE SpinKill            ; /
+   LDA $187A|!Base2
    BNE RideSprite
    LDA !extra_prop_1,x
    AND #$20
    BEQ RideSprite
    BIT $16                     ; Don't pick up sprite if not pressing button
    BVC RideSprite
    LDA #$0B                    ; Sprite status = Carried
    STA !14C8,x
    LDA #$F0                    ; Set time until recovery
    STA !1540,x
    RTS

RideSprite:
    LDA #$01                    ; \ set "on sprite" flag
    STA $1471|!Base2            ; /
    LDA #$06                    ; \ set riding sprite
    STA !154C,x                 ; /
    STZ $7D                     ; y speed = 0
    LDA #$E1                    ; \ 
    LDY $187A|!Base2            ;  | mario's y position += E1 or D1 depending if on yoshi
    BEQ +                       ;  |
    LDA #$D1                    ;  |
+   CLC : ADC !D8,x             ;  |
    STA $96                     ;  |
    LDA !14D4,x                 ;  |
    ADC #$FF                    ;  |
    STA $97                     ; /
    LDY #$00                    ; \ 
    LDA $1491|!Base2            ;  | $1491 == 01 or FF, depending on direction
    BPL +                       ;  | set mario's new x position
        DEY                     ;  |
+   CLC : ADC $94               ;  |
    STA $94                     ;  |
    TYA                         ;  |
    ADC $95                     ;  |
    STA $95                     ; /
    RTS

SpriteWins:
    LDA !154C,x                 ; \ if riding sprite...
    ORA !15D0,x                 ;  |   ...or sprite being eaten...
    BNE Return2                 ; /   ...return
    LDA $1490|!Base2            ; \ if mario star timer > 0, goto HasStar
    BNE HasStar                 ; / NOTE: branch to Return2 to disable star killing
    JSL $00F5B7|!BankB          ; hurt mario
Return2:
    RTS

SpinKill:
    JSR StompPoints             ; give Mario points
    LDA #$F8
    STA $7D
    JSL $01AB99|!BankB          ; display contact graphic
    LDA #$04                    ; \ status = 4 (being killed by spin jump)
    STA !14C8,x                 ; /
    LDA #$1F                    ; \ set spin jump animation timer
    STA !1540,x                 ; /
    JSL $07FC3B|!BankB          ; show star animation
    LDA.b #!FallSFX             ; \ play sound effect
    STA.w !FallBank|!Base2      ; /
    RTS

HasStar:
    LDA #$02                    ; \ status = 2 (being killed by star)
    STA !14C8,x                 ; /
    LDA #$D0                    ; \ set y speed
    STA !AA,x                   ; /
    STZ !B6,x                   ; set x speed
    INC $18D2|!Base2            ; increment number consecutive enemies killed
    LDA $18D2|!Base2            ; \ 
    CMP #$08                    ;  | if consecutive enemies stomped >= 8, reset to 8
    BCC +                       ;  |
        LDA #$08                ;  |
        STA $18D2|!Base2        ; /
+   JSL $02ACE5|!BankB          ; give mario points
    LDY $18D2|!Base2            ; \ 
    CPY #$08                    ;  | if consecutive enemies stomped < 8 ...
    BCS +                       ;  |
        LDA StarSounds,y        ;  |    ... play sound effect
        STA $1DF9|!Base2        ; /
+   RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite graphics routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Tilemap:
    db $A6,$A6

Graphics:
    %GetDrawInfo()              ; sets y = OAM offset
    LDA $14                     ; \ 
    LSR #3                      ;  |
    CLC : ADC $15E9|!Base2      ;  |
    AND #$01                    ;  |
    STA $03                     ;  | $03 = index to frame start (0 or 1)
    PHX                         ; /

    LDA !14C8,x
    CMP #$02
    BNE +
        STZ $03
        LDA !15F6,x
        ORA #$80
        STA !15F6,x
+   LDA $00                     ; \ tile x position = sprite x location ($00)
    STA $0300|!Base2,y          ; /

    LDA $01                     ; \ tile y position = sprite y location ($01)
    STA $0301|!Base2,y          ; /

    LDA !15F6,x                 ; tile properties xyppccct, format
    ORA $64                     ; add in tile priority of level
    STA $0303|!Base2,y          ; store tile properties

    LDX $03                     ; \ store tile
    LDA Tilemap,x               ;  |
    STA $0302|!Base2,y          ; /

    INY #4

    PLX                         ; pull, X = sprite index
    LDY #$02                    ; \ 460 = 2 (all 16x16 tiles)
    LDA #$00                    ;  | A = (number of tiles drawn - 1)
    JSL $01B7B3|!BankB          ; / don't draw if offscreen
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; points routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

StarSounds:
    db $00,$13,$14,$15,$16,$17,$18,$19

StompPoints:
    PHY
    LDA $1697|!Base2            ; \ 
    CLC : ADC !1626,x           ; / some enemies give higher pts/1ups quicker??
    INC $1697|!Base2            ; increase consecutive enemies stomped
    TAY
    INY
    CPY #$08                    ; \ if consecutive enemies stomped >= 8 ...
    BCS +                       ; /    ... don't play sound
        LDA StarSounds,y        ; \ play sound effect
        STA $1DF9|!Base2        ; /
+   TYA                         ; \ 
    CMP #$08                    ;  | if consecutive enemies stomped >= 8, reset to 8
    BCC +                       ;  |
        LDA #$08                ; /
+   JSL $02ACE5|!BankB          ; give Mario points
    PLY
    RTS

