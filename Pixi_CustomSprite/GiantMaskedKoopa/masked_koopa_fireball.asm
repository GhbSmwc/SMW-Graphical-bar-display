;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Purple Critter's Projectile
; by Sonikku
; Description: If inserted to a level normally, it is stationary. It is meant to
; be shot from PurpleCritter.cfg, where it aims this sprite at Mario.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!WallSFX = $01                  ; Sound effect to play when the fireball hits a wall
!WallBank = $1DF9

print "INIT ",pc
    RTL

print "MAIN ",pc
    PHB : PHK : PLB
    JSR SpriteCode
    PLB
    RTL

Return:
    RTS

SpriteCode:
    JSR Graphics
    LDA #$00
    %SubOffScreen()
    LDA $9D                     ;\ if locked, don't move
    BNE Return                  ;/
    JSL $01801A|!BankB          ; > Update y position w/o gravity
    JSL $018022|!BankB          ; > same as above but x
    JSL $019138|!BankB          ; > Interact objects
    JSL $01A7DC|!BankB          ; > Mario contact
    BCC +                       ; > If not touching,
        JSL $00F5B7|!BankB      ; > Hurt player
+   LDA !186C,x                 ;\ Off screen stuff
    ORA !15A0,x                 ; |
    BNE Kill                    ;/
    LDA !1588,x                 ;\ If not toucing surface, return
    BEQ Return                  ;/
    LDA.b #!WallSFX             ;\ sfx
    STA.w !WallBank|!Base2      ;/

    LDY #$03                    ; > Number of smoke slots -1 (0-3 inclusive)
-   LDA $17C0|!Base2,y          ;\ Search for free smoke slot index.
    BEQ +                       ; |
    DEY                         ; |
    BPL -                       ; |
    RTS                         ;/

+   LDA #$01                    ;\ smoke type
    STA $17C0|!Base2,y          ;/
    LDA #$1B                    ;\ Existence timer
    STA $17CC|!Base2,y          ;/
    LDA !D8,x                   ;\ set smoke y position
    CLC : ADC #$04              ; |
    STA $17C4|!Base2,y          ;/
    LDA !E4,x                   ;\ set smoke x position
    CLC : ADC #$04              ; |
    STA $17C8|!Base2,y          ;/

Kill:
    STZ !14C8,x                 ; > Erase fireball sprite
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sprite graphics routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Tilemap:
    db $84,$86,$A4,$86

Graphics:
    LDA !B6,x                   ;\  Fix a bug where the fireball is facing backwards when
    BPL +                       ; | shooting leftwards.
    LDA #$01                    ; |
    BRA Skip                    ; |

+   LDA #$00                    ; |
Skip:
    STA !157C,x                 ;/
    %GetDrawInfo()              ; sets y = OAM offset
    LDA !157C,x                 ; \ $02 = direction
    STA $02                     ; /
    LDA $14                     ; \ 
    LSR #3                      ;  |
    CLC : ADC $15E9|!Base2      ;  |
    AND #$03                    ;  |
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
    LDX $02                     ; \ if direction == 0...
    BNE +                       ;  |
        ORA #$40                ; /    ...flip tile
+   ORA $64                     ; add in tile priority of level
    STA $0303|!Base2,y          ; store tile properties

    LDX $03                     ; \ store tile
    LDA Tilemap,x               ;  |
    STA $0302|!Base2,y          ; /

    INY                         ; \ increase index to sprite tile map ($300)...
    INY                         ;  |    ...we wrote 1 16x16 tile...
    INY                         ;  |    ...sprite OAM is 8x8...
    INY                         ; /    ...so increment 4 times

    PLX                         ; pull, X = sprite index
    LDY #$02                    ; \ 460 = 2 (all 16x16 tiles)
    LDA #$00                    ;  | A = (number of tiles drawn - 1)
    JSL $01B7B3|!BankB          ; / don't draw if offscreen
    RTS
