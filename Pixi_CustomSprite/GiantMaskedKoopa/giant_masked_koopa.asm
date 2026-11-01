incsrc "../GraphicalBarDefines/GraphicalBarDefines.asm"
incsrc "../GraphicalBarDefines/SpriteOAMSettings.asm"

;Note, to view my changes and other info for the graphical bar, CTRL+F “[GraphicalBar_For_HP]” (without quotes).
;See graphics at ExGraphics/Sprite/GiantMaskedKoopaTest and you'll need to replace the table of
;tile numbers due to tile relocation.
;When displaying previous HP is enabled (displaying a double-bar to show damage):
; $1558 (decrements itself per frame): is the amount of delay before the bar's SecondFill decreases to FirstFill
; $1570: is the SecondFill amount displaying its previous HP percentage.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bowser Jr., by dahnamics
;;
;; Credit: mikeyk (Shy Guy), Yoshicookiezeus (Thwomp boss v. 2.0), Schwa (sledge_ex)
;;
;; Description: A Shy Guy with many available configurations.
;;
;; Note: When rideable, clipping tables values should be: 03 0A FE 0E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!FireballNumber = $03       ; Custom sprite number for the fireball (from list.txt).
!RockNumber = $04           ; Custom sprite number for the rock (from list.txt).
!NormalNumber1 = $00        ; The first normal sprite to spawn when a green shell throws 2 sprites.
!NormalNumber2 = $00        ; The second normal sprite to spawn when a green shell throws 2 sprites.

; How much hits to defeat. Do note that the hit counter, $1534, increments.
!HitPoints = 9

; How much time should pass between two jumps.
; The lower this is, the more often the Koopa jumps ($C0: default).
!JumpPeriod = $C0

; Amount of time to shake the screen for when landing from a jump.
!ShakeTime = $18

; The sprite's vertical speed during a jump.
!RiseSpeed = $C0

; During wall climb, if the Y position of the Koopa reaches this value, will revert to walking.
; The lower the value, the higher. Here is the format: $XXyz. Uppercase XX is what screen number
; (or subscreen in a horizontal level), while lowercase Y is what 16x16 block, and z is pixels
; within the 16x16 block).
!ClimbingPeak = $0110

; Amount of time (in frames; 60 is 1 second) before the Koopa lunges into the wall after defeat.
!DeathWait = 120

; The sound effect (if any) to play when the boss jumps.
; Be default, it's the jump sound effect used by the game (changed by AddmusicK).
; Set the define below to 1 to enable.
!EnableJumpSFX = 0
!JumpSFX = read1($00D65E+$01)
!JumpBank = read2($00D660+$01)

; The sound effect to play when the boss lands on the ground after a jump.
!GroundSFX = $09
!GroundBank = $1DFC

; The sound effect to play when a rock falls.
!RockSFX = $07
!RockBank = $1DF9

; The sound effect to play when shooting fire.
!FireSFX = $17
!FireBank = $1DFC

; The sound effect to play when spawning the two normal sprites.
!KoopaSFX = $20
!KoopaBank = $1DF9

; The sound effect to play when the player jumps on top of the sprite's shell.
!ContactSFX = $02
!ContactBank = $1DF9

; The sound effect to play when the sprite gets hurt.
!HurtSFX = $28
!HurtBank = $1DFC

; The sound effect to play when the sprite dies after hitting a wall
!DeathSFX = $08
!DeathBank = $1DF9

; The music to play for the goal (if the extra bit is set).
; Set to $03 is you're using AddmusicK!
!GoalMusic = $03

; The sprite's horizontal speed.
X_Speed:        db $20,$E0

; The sprite's horizontal speed when lunging at Mario after death.
Lunge_X_Speed:  db $40,$C0

; The sprite's maximum horizontal speed.
X_Max:          db $20,$D0

; The sprite's horizontal acceleration.
X_Accel:        db $02,$FE

; Names for sprite tables and subroutines, do not touch.
!sprite_state = !C2             ; What actions the sprite is doing.
!sprite_timer = !1504           ; Used as a misc timer, like when will it hop.
!sprite_earthquake = !151C      ; So that it earthquakes for 1 frame
!sprite_freeze_timer = !1540
!sprite_stun_timer = !1564
!sprite_direction = !157C
!sprite_smash_status = !1602

!GetSpriteClippingA = $03B69F|!BankB
!CheckForContact = $03B72B|!BankB
!GetSpriteClippingB = $03B6E5|!BankB
!ShowSprContactGfx = $01AB72|!BankB

NextState:      db $05,$03,$04,$06

print "INIT ",pc
    LDA #$01                    ;\ Prevent premature earthquake
    STA !sprite_earthquake,x    ;/
    LDA !E4,x                   ;\ Move sprite 8 pixels down
    CLC : ADC #$08              ; |
    STA !E4,x                   ;/
    LDA !14D4,x                 ;\ Fix an overflow, this time, uses 2 bytes of the position
    ADC #$00                    ; |
    STA !14D4,x                 ;/

    CLC : ADC #$32              ; |
    STA !sprite_timer,x         ;/

    PHY
    %SubHorzPos()
    TYA
    STA !sprite_direction,x
    PLY
    LDA !sprite_blocked_status,x    ; if on the ground, reset the turn counter
    ORA #$04
    STA !sprite_blocked_status,x
    RTL

print "MAIN ",pc
    PHB : PHK : PLB
    JSR SpriteCode
    PLB
    RTL

Return:
    RTS
SpriteCode:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graphics based on sprite state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    LDA !sprite_state,x         ;\ Palette for when the koopa is walking or waiting
    CMP #$01                    ; |
    BNE RedShell                ; |
    LDA #$04                    ; |
    BRA Continue                ;/

RedShell:
    LDA !sprite_state,x         ;\ Palette for when the koopa is shooting fireballs
    CMP #$03                    ; |
    BNE YellowShell             ; |
    LDA #$01                    ; |
    BRA Continue                ;/

YellowShell:
    LDA !sprite_state,x         ;\ Palette when dropping rocks.
    CMP #$05                    ; |
    BNE BlueShell               ; |
    LDA #$02                    ; |
    BRA Continue                ;/

BlueShell:
    LDA !sprite_state,x         ;\ Palette for wall climbing
    CMP #$06                    ; |
    BNE GreenShell              ; |
    LDA #$03                    ; |
    BRA Continue                ;/

GreenShell:
    LDA #$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HUGE contact routine below ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Continue:
    JSR Graphics
    LDA !14C8,x                 ; return if sprite status != 8
    CMP #$08                    ;\ If sprite status isn't normal (like dying), return
    BNE Return                  ;/
    LDA $9D                     ;\ If freeze time is set, return
    BNE Return                  ;/
    LDA #$00
    %SubOffScreen()             ; handle off screen situation
    LDA !sprite_x_low,x         ;\ Preserve normal X position
    PHA                         ; |
    LDA !sprite_x_high,x        ; |
    PHA                         ;/
    LDA !sprite_y_low,x         ;\ Preserve normal Y position
    PHA                         ; |
    LDA !sprite_y_high,x        ; |
    PHA                         ;/

    JSL $018032|!BankB          ; interact with other sprites

    LDA !sprite_state,x         ;\  If koopa is walking, then use the 32x40
    CMP #$01                    ; | hitbox for mairo.
    BEQ StandingUp              ;/
    LDA !sprite_y_low,x         ;\ Adjust Y pos...
    CLC : ADC #$08              ; |
    STA !sprite_y_low,x         ;/
    LDA !sprite_y_high,x        ;\ High byte
    ADC #$00                    ; |
    STA !sprite_y_high,x        ;/

    LDA #$23                    ;\ Modify clipping (32x32)
    STA !1662,x                 ;/
    JSL $01A7DC|!BankB          ; interact with Mario (with shifted position)
    BRA ClippingDone

StandingUp:
    LDA !sprite_x_low,x         ;\ Adjust X pos...
    CLC : ADC #$08              ;|
    STA !sprite_x_low,x         ;/
    LDA !sprite_x_high,x        ;\ high byte
    ADC #$00                    ;|
    STA !sprite_x_high,x        ;/
    LDA #$28                    ;\ Modify clipping (32x40)
    STA !1662,x                 ;/
    JSL $01A7DC|!BankB          ; interact with Mario (with shifted position)
ClippingDone:
    PLA                         ;\ Restore Y pos
    STA !sprite_y_high,x        ; |
    PLA                         ; |
    STA !sprite_y_low,x         ;/
    PLA                         ;\ Restore X pos
    STA !sprite_x_high,x        ; |
    PLA                         ; |
    STA !sprite_x_low,x         ;/

    BCC NoContact               ; return if no contact
    LDA !154C,x                 ;\ If invulnerability is running, can't touch
    BNE NoContact               ;/
    LDA #$08                    ;\ sprite invincibility timer = $08
    STA !154C,x                 ;/
    LDA !sprite_y_high,x        ;\ This basically checks if Mario is in the upper section of the
    XBA                         ;| sprite's hitbox. If Mario is below or sprite is above Mario, assumes
    LDA !sprite_y_low,x         ;| touching the sides and bottom and hurts the player.
    REP #$20                    ;|
    SEC                         ;| \ Move the line between "stomps counts" and "boss hurts mario" upwards.
    SBC #$0028                  ;| /
    CMP $96                     ;/
    SEP #$20
    BMI SpriteWins              ; If sprite's "hurtbox" is much higher up than mairo, damage mario.

    JSL $01AA33|!BankB          ; set Mario Y speed
    JSL $01AB99|!BankB          ; display contact graphic

    LDA !sprite_state,x         ;\ If the sprite is not walking, its invulnerable
    CMP #$01                    ; |
    BNE NoCount                 ;/

    JSR StompPoints             ; give Mario points
    LDA.b #!HurtSFX             ;\ sound effect
    STA.w !HurtBank|!addr       ;/
    LDA #$A0                    ;\ Set ThrowFire timer
    STA !sprite_stun_timer,x    ;/
    INC !1534,x                 ; increment sprite hit counter (this counts the damage up by 1)
	if !GiantMaskedKoopa_DisplayPreviousHP != 0	;[GraphicalBar_For_HP] everytime the boss takes damage, set delay to show how much HP percentage loss.
		LDA #!GiantMaskedKoopa_PrevHPDelay
		STA !1558,x
	endif
    LDA !1534,x                               ;\ If it has incremented to the "max", do death animation.
    CMP.b #!HitPoints                         ; |
    BCS SpriteDead                            ;/

    LDA #$02                    ;\ Sprite retreats into its mask
    STA !sprite_state,x         ;/
    BRA NewReturn

SpriteDead:
    LDA #$07                    ;\ Initalize death countdown
    STA !sprite_state,x         ;/
    LDA.b #!DeathWait           ;\ Set timer before lunging at the player
    STA !sprite_freeze_timer,x  ;/
NoCount:
    LDA.b #!ContactSFX          ;\ sound effect, like trying to normal-jump a flashing kamikaze
    STA.w !ContactBank|!Base2   ;/ koopa shell.
NewReturn:
    RTS

SpriteWins:
    LDA $1497|!Base2            ;\ If Mario is invincible or riding Yoshi, return
    ORA $187A|!Base2            ; |
    BNE NoContact               ;/
    %SubHorzPos()               ;\ Switch direction.
    TYA                         ; |
    STA !sprite_direction,x     ;/
    JSL $00F5B7|!BankB          ; hurt Mario
NoContact:
    PHX                         ; Preserve x since its reserved for current sprite slot.

    LDA !sprite_state,x         ; Load what points to what loaction
    ASL                         ; Multiply by 2 because the address are 16-bit (2 bytes), using dw
    TAX                         ; Transfer to x register
    JMP (States,x)

States:
    dw Waiting                  ; $00 (x=$00)
    dw Walking                  ; $01 (x=$02)
    dw Spinning                 ; $02 (x=$04)
    dw ThrowFire                ; $03 (x=$06)
    dw TossKoopa                ; $04 (x=$08)
    dw SpawnRocks               ; $05 (x=$0A)
    dw Rising                   ; $06 (x=$0C)
    dw Dead                     ; $07 (x=$0E)
    dw Countdowns               ; $08 (x=$10)

;-----------------------------------------------------------------------------------------
; state 0
;-----------------------------------------------------------------------------------------

Waiting:
    PLX
    LDA !sprite_off_screen_vert,x   ; fall if offscreen vertically
    BNE SetWalking

    LDA !sprite_off_screen_horz,x   ; return if offscreen horizontally
    BNE Return0

    %SubHorzPos()                   ; determine if Mario is close and act accordingly
    TYA
    STA !sprite_direction,x

    LDA !sprite_blocked_status,x    ;\  If sprite has its left or right blocked status set,
    AND #$03                        ; | then flip its left/right direction.
    BEQ +                           ; |
        LDA !sprite_direction,x     ; |
        EOR #$01                    ; |
        STA !sprite_direction,x     ;/
+   LDA !sprite_blocked_status,x    ; run the subroutine if the sprite is in the air...
    ORA !sprite_state,x             ; ...and not already turning
    BNE +
        JSR ChangeDirection
        LDA #$01
        STA !sprite_state,x         ; [GHB's fix], that should not store to original Y position
+   LDA !sprite_blocked_status,x    ;\ if on the ground, reset the turn counter
    AND #$04                        ; |
    BEQ Return0                     ;/
    STZ !sprite_state,x             ;\ Hang in the air
    STZ !sprite_speed_y,x           ;/
    BRA XTime

XTime:
    STZ !sprite_speed_x,x           ; Clear X speed
    BCS Return0

    LDA $0E                         ;\ 
    CLC : ADC #$80                  ; |
    CMP #$50                        ; |
    BCS Return0                     ;/

; Basically, those two above determine if the sprite should "wake up" to initiate the boss fight.

SetWalking:
    INC !sprite_state,x             ; Switch state to walking.
    JMP Walking_PullXDone           ; Run walking routine

Return0:
    JSL $01802A|!BankB              ; update position based on speed values (gravity + layer interaction)
    RTS

;-----------------------------------------------------------------------------------------
; state 1, walking (Mario can damage the boss in this state)
;-----------------------------------------------------------------------------------------

Walking:
    PLX
Walking_PullXDone:
    LDA !sprite_blocked_status,x    ;\ If not on ground, return (prevent air quakes).
    AND #$04                        ; |
    BEQ .AirClear                   ;/

    LDA !sprite_speed_y,x           ;\ so it doesn't get confused between jumping and landing
    CMP #$08                        ; |
    BMI .EarthquakeDone             ;/
    LDA !sprite_earthquake,x        ;\ If earthquake already set, don't run this again.
    BNE .EarthquakeDone             ;/
    INC !sprite_earthquake,x        ; And flag it
    LDA.b #!ShakeTime               ;\ shake ground
    STA $1887|!Base2                ;/
    LDA $77                         ;\  \ If Mario is on the ground...
    AND #$04                        ; | /
    BEQ .NoLockMario                ; | Then no stun.
    LDA $1497|!Base2                ; | OR if Mario is invulnerable after a hit...
    BNE .NoLockMario                ; | then no stun (comment it out if stun while immume).
    LDA.b #!ShakeTime               ; |
    STA $18BD|!Base2                ;/
.NoLockMario
    LDA.b #!GroundSFX               ;\ play sound effect
    STA.w !GroundBank|!Base2        ;/

	LDA #$F0                         ;\X offset
	STA $00                          ;/
	LDA #$02                         ;\Y offset
	STA $01                          ;/
	LDA #$1B                         ;\Timer
	STA $02                          ;/
	LDA #$01                         ;>Smoke type
	%SpawnSmoke()
	
	LDA #$10                         ;\X offset
	STA $00                          ;/
	LDA #$01                         ;>Smoke type
	%SpawnSmoke()


    BRA .EarthquakeDone
.AirClear
    STZ !sprite_earthquake,x
.EarthquakeDone
    LDA !sprite_blocked_status,x    ;\  If sprite has its left or right blocked status
    AND #$03                        ; | set, then flip horizontal direction.
    BEQ +                           ; |
        LDA !sprite_direction,x     ; |
        EOR #$01                    ; |
        STA !sprite_direction,x     ;/
+   LDY !sprite_direction,x         ;\ set x speed based on direction
    LDA X_Speed,y                   ; |
    STA !sprite_speed_x,x           ;/
    JSR Hop                         ; Jump to custom code
    JSL $01802A|!BankB              ; update position based on speed values (gravity + layer interaction)
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Hop:
    LDA.b #!JumpPeriod              ;\ If timer less than C0..
    CMP !sprite_timer,x             ; |
    BCS IncreaseHop                 ;/ then Increase, and return.
    STZ !sprite_timer,x             ; Reset timer so it doesn't overflow (just in case).
    LDA !sprite_blocked_status,x    ;\ If not on ground, return (prevent air jumping).
    AND #$04                        ; |
    BEQ Return68                    ;/
    LDA #$C0                        ;\ Set jump up-speed.
    STA !sprite_speed_y,x           ;/

    if !EnableJumpSFX == 1
        LDA.b #!JumpSFX
        STA.w !JumpBank|!Base2
    endif
Return68:
    RTS
IncreaseHop:
    INC !sprite_timer,x             ; Increase timer.
    RTS

;-----------------------------------------------------------------------------------------
; state 2 (chasing mario in its shell)
;-----------------------------------------------------------------------------------------

Spinning:
    PLX
    %SubHorzPos()                   ;\  Determine if Mario is either left or right of the sprite.
    TYA                             ; | And that the sprite will direct itself towards Mario
    LDA X_Max,y                     ; | \ Prevent over-acceleration.
    CMP !sprite_speed_x,x           ; |  |
    BEQ +                           ;/  /
        LDA !sprite_speed_x,x       ;\ In laws of physics, acceleration is simply the change in velocity.
        CLC : ADC X_Accel,y         ; |
        STA !sprite_speed_x,x       ;/
+   LDA !sprite_blocked_status,x    ;\ If not on the ground, then detect air
    AND #$04                        ; |
    BEQ +                           ;/
        STZ !sprite_speed_y,x       ; Zero Y speed to prevent falling through the ground.
+   LDA !sprite_blocked_status,x    ;\ If not contacting the wall, keep going in that direction.
    AND #$03                        ; |
    BEQ InAir                       ;/

    LDA !sprite_speed_x,x           ;\ Otherwise flip x speed
    EOR #$FF : INC A                ; |
    STA !sprite_speed_x,x           ;/

    LDA $77                         ;\ If Mario is not on ground, then switch state
    AND #$04                        ; |
    BEQ +                           ; |
        LDA #$20                    ; |
        STA $18BD|!Base2            ;/
+   PHX                             ;\ Execute random number generation for switching sprite state
    LDA #$03                        ; |
    JSL Random                      ; |
    TAX                             ; |
    LDA NextState,x                 ; |
    PLX                             ; |
    STA !sprite_state,x             ;/
InAir:
    JSL $01802A|!BankB              ; update position based on speed values (gravity + layer interaction)
    RTS

;------------------------------------------------------------------------------------------

Random:
    PHX
    PHP
    SEP #$30
    PHA
    JSL $01ACF9|!BankB              ; Random number generation routine
    PLX
    CPX #$FF                        ;\ Handle glitch if max is FF
    BNE .normal                     ; |
    LDA $148B|!Base2                ; |
    BRA .end                        ;/

.normal
    INX                             ; Amount in plus 1
    LDA $148B|!Base2

    if !SA1 == 0
        STA $4202                   ;\ Multiply with hardware regsisters
        STX $4203                   ; |
        NOP #4                      ;/
        LDA $4217
    else
        STZ $2250                   ; Set SA-1 multiplication mode
        REP #$20                    ; Accum (16-bit)
        AND #$00FF
        STA $2251
        TXA
        AND #$00FF
        STA $2253
        NOP                         ; Wait 2 cycles (+SEP = 5) for the multiplication to finish.
        SEP #$20                    ; Accum (8-bit)
        LDA $2307
    endif

.end
    PLP
    PLX
    RTL

;-----------------------------------------------------------------------------------------
; state 3 (red shell throwing fireballs at mario)
;-----------------------------------------------------------------------------------------
ThrowFire:
    PLX
    LDA !sprite_blocked_status,x        ;\  if sprite is in contact with an object...
    AND #$03                            ; | flip the direction status
    BEQ +                               ; |
        LDA !sprite_direction,x         ; |
        EOR #$01                        ; |
        STA !sprite_direction,x         ;/
+   STZ !sprite_speed_x,x
    JSR +                               ; jump to custom code
    JSL $01802A|!BankB                  ; update position based on speed values (gravity + layer interaction)
+   LDA #$C0                            ;\  If timer isn't A0..
    CMP !1528,x                         ; |
    BNE IncreaseSpawn                   ; | Increase it.
    STZ !1528,x                         ;/  Reset timer.
    LDA #$01                            ;\ Switch to walking
    STA !sprite_state,x                 ;/
    STA !sprite_earthquake,x            ; And don't earthquake upon switching back
    RTS

IncreaseSpawn:
    LDA !1528,x                         ;\ If timer = #$20, shoot a fireball
    CMP #$20                            ; |
    BNE +                               ;/
        JSR HammerThrow                 ; shoot fireball subroutine
        BRA IncrementTime               ; Increment timer and return
+   CMP #$40                            ;\ If timer = #$40, shoot 2 fireballs
    BNE +                               ; |
        JSR HammerThrow                 ;/
        BRA IncrementTime               ; Increment timer and return
+   CMP #$60                            ;\ If timer = #$60, shoot 3 fireballs
    BNE +                               ; |
        JSR HammerThrow                 ;/
        BRA IncrementTime               ; Increment tiemr and return
+   CMP #$80                            ;\ If timer = #$80, shoot 4 fireballs
    BNE +                               ; |
        JSR HammerThrow                 ;/
        BRA IncrementTime               ; increment timer and return.
+   CMP #$A0                            ;\ If one last time, return
    BNE IncrementTime                   ;/
    JSR HammerThrow                     ; Shoot 1 extra fireball?
IncrementTime:
    INC !1528,x                         ; Increase timer
    RTS

;-----------------------------------------------------------------------------------------
; state 4 (throw 2 green beach koopas).
;-----------------------------------------------------------------------------------------

TossKoopa:
    PLX
    LDA !sprite_blocked_status,x    ;\ if sprite is in contact with an object...
    AND #$03                        ; | flip the direction status
    BEQ +                           ; |
        LDA !sprite_direction,x     ; |
        EOR #$01                    ; |
        STA !sprite_direction,x     ;/
+   LDY !sprite_direction,x         ;\ set x speed based on direction
    LDA X_Speed,y                   ; |
    STA !sprite_speed_x,x           ;/
    JSR KoopaToss                   ; Jump to custom code
    JSL $01802A|!BankB              ; update position based on speed values (gravity + layer interaction)
KoopaToss:
    LDA #$80                        ;\ If timer isn't 80..
    CMP !1528,x                     ;/
    BNE IncreaseToss                ; Increase it.
    STZ !1528,x                     ; Reset timer.
    JSR HammerThrow3                ;\ Again, the author didn't rename the labels, but doesn't matter
    JSR HammerThrow4                ;/ they spawn koopas
    LDA #$01                        ;\ switch to walking state
    STA !sprite_state,x             ;/
    STA !sprite_earthquake,x        ; And don't earthquake upon switching back
    RTS

IncreaseToss:
    INC !1528,x                     ; Increase timer.
    RTS

;-----------------------------------------------------------------------------------------
; state 5 (yellow shell dropping rocks)
;-----------------------------------------------------------------------------------------

SpawnRocks:
    PLX
    LDA !sprite_blocked_status,x    ;\ if sprite is in contact with an object...
    AND #$03                        ; | flip the direction status
    BEQ +                           ; |
        LDA !sprite_direction,x     ; |
        EOR #$01                    ; |
        STA !sprite_direction,x     ;/
+   STZ !sprite_speed_x,x           ; Stop horizontally
    JSR +                           ; jump to custom code
    JSL $01802A|!BankB              ; update position based on speed values (gravity + layer interaction)
+   LDA #$C0                        ; if timer isn't C0..
    CMP !1528,x
    BNE IncreaseSpawn2              ; increase it.
    STZ !1528,x                     ; reset timer.
    LDA #$01                        ;\ Switch to walking
    STA !sprite_state,x             ;/
    STA !sprite_earthquake,x        ; and don't earthquake upon switching back
    RTS

IncreaseSpawn2:
    LDA !1528,x                     ; Check if the timer is greater than $20
    CMP #$20                        ;\ Same as the fireballs, if the timer is
    BNE +                           ; | divisible by #$20 but not higher than #$A0,
        JSR HammerThrow2            ; | spawn a rock
        BRA IncrementTime2          ; |

+   CMP #$40                        ; |
    BNE +                           ; |
        JSR HammerThrow2            ; |
        BRA IncrementTime2          ; |

+   CMP #$60                        ; |
    BNE +                           ; |
        JSR HammerThrow2            ; |
        BRA IncrementTime2          ; |

+   CMP #$80                        ; |
    BNE +                           ; |
        JSR HammerThrow2            ; |
        BRA IncrementTime2          ; |

+   CMP #$A0                        ; |
    BNE IncrementTime2              ; |
    JSR HammerThrow2                ;/
IncrementTime2:
    INC !1528,x                     ; increase timer.
    RTS

;-----------------------------------------------------------------------------------------
; state 6 (blue shell that wall climbs)
;-----------------------------------------------------------------------------------------

Rising:
    PLX
    LDA !sprite_freeze_timer,x  ; if we're still waiting on the ground, return
    BNE Return7

    LDA !14D4,x                 ;\ Transfer sprite Y high position to high byte of the A register.
    XBA                         ;/ (A= $YYyy), note that A has a high byte, even in 8-bit mode, unlike x or y register.
    LDA !D8,x                   ; Load low byte of Y position
    REP #$20                    ; 16-bit A (placed AFTER the LDA so it doesn't load 2 slots at $D8).
    CMP.w #!ClimbingPeak        ;\ If the sprite is below the climbing peak, keep going up.
    SEP #$20                    ; | (as the Y position increases, the sprite moves downwards, thats how positioning works).
    BPL Rise                    ;/

    LDA #$01                    ;\ Switch to walking.
    STA !sprite_state,x         ;/
    RTS

Rise:
    LDY !1528,x
    LDA.b #!RiseSpeed           ;\ set rising speed
    STA !sprite_speed_y,x       ;/
    JSL $01801A|!BankB          ; apply speed
Return7:
    RTS

;-----------------------------------------------------------------------------------------
; state 7 timer before lunging at the player
;-----------------------------------------------------------------------------------------

Dead:
    PLX
    LDA !sprite_freeze_timer,x
    BNE Return10

    STZ !sprite_speed_x,x           ; clear X speed
    STZ !sprite_speed_y,x           ; clear Y speed

    INC !sprite_smash_status,X      ;\ if smash status incremented to #$02, then switch state.
    LDA !sprite_smash_status,x      ; |
    CMP #$02                        ; |
    BCC +                           ;/
        INC !sprite_state,x         ; change state to dying
        %SubHorzPos()               ;\ Update direction UNTIL the sprite lunges (direct to mario)
        TYA                         ; |
        STA !sprite_direction,x     ;/
+   JSL $01801A|!BankB              ; Don't float in the air if stomped in air.
Return10:
    RTS

;-----------------------------------------------------------------------------------------
; state 8 (about to die)
;-----------------------------------------------------------------------------------------

Countdowns:
    PLX
    LDA !sprite_blocked_status,x    ;\ if sprite is in contact with an object...
    AND #$03                        ; | then die
    BEQ NoObjectContact             ; |
    LDA #$04                        ; |
    STA !14C8,x                     ;/
    JSL $07FC3B|!BankB              ; show star animation
    LDA.b #!DeathSFX                ;\ play sound effect
    STA.w !DeathBank|!Base2         ;/

    %BEC(NoObjectContact)

    DEC $13C6|!Base2            ; prevent Mario from walking at the level end
    LDA #$FF                    ; \ set goal
    STA $1493|!Base2            ; /
    LDA.b #!GoalMusic           ; \ set ending music
    STA $1DFB|!Base2            ; /
    RTS

NoObjectContact:
    LDY !sprite_direction,x     ;\ set x speed based on direction
    LDA Lunge_X_Speed,y         ; |
    STA !sprite_speed_x,x       ;/
    JSL $01802A|!BankB          ; update position based on speed values (gravity + layer interaction)
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; hammer routine (spawns fireballs actually)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HitboxOffsets:  dw $0000,$0010  ; 16x32hitbox, 16x16hitbox
X_Offset:       db $06,$FA
X_Offset_2:     db $00,$FF

HammerThrow:
    LDA.b #!SprSize-3           ;>Fireballs only interacts with sprite slots 0 to !SprSize-3. See $02A0B8.
    STA $04
    LDA.b #!FireballNumber      ;> Fireball's sprite number
    SEC                         ; Custom sprite mode
    %SpawnSpriteSafe()
    BCC +
        JMP .Return             ; If all slots indexes taken, return (not even a sound)
+   LDA.b #!FireSFX             ;\ sound effect
    STA.w !FireBank|!Base2      ;/

    PHY                         ; Save y index of the new sprite
    LDA !sprite_direction,x     ;\ Transfer facing to X position the fireball spawns
    TAY                         ;/
    LDA !E4,x                   ;\ set xlo position for new sprite
    CLC : ADC X_Offset,y        ; |
    PLY                         ; | Get new sprite's y index
    STA.w !E4,y                 ;/ And set the new sprite's position

    PHY                         ; Save y index of the new sprite
    LDA !sprite_direction,x     ;\ 
    TAY                         ;/
    LDA !14E0,x                 ;\ set xhi position for new sprite
    ADC X_Offset_2,y            ; |
    PLY                         ; | Get new sprite's y index
    STA !14E0,y                 ;/

    LDA !sprite_y_low,x         ;\ set y position for new sprite
    SEC                         ; | (y position of generator - 1)
    SBC #$0E                    ; |
    STA.w !D8,y                 ; |
    LDA !14D4,x                 ; |
    SBC #$00                    ; |
    STA !14D4,y                 ;/

    ; setup aiming input
    LDA !sprite_x_high,y        ;\ find horizontal distance (signed, D = S - M)
    XBA                         ;|
    LDA.w !sprite_x_low,y       ;|
    REP #$20                    ;|
    SEC : SBC $94               ;|
    STA $00                     ;|
    SEP #$20                    ;/

    PHX                         ; Save GMK's slot index
    LDA $19                     ;\ If small Mario, 16x16 hitbox, aim for bottom of 16x32
    BEQ .16x16Hitbox            ;/
    LDA $73                     ;\ While big, if crouching, aim for bottom of 16x32
    BNE .16x16Hitbox            ;/

;16x32Hitbox
    LDX #$00                    ; Load for aiming towards the bottom half of Mario
    BRA +
.16x16Hitbox
    LDX #$02                    ; Load for aiming towards the top half of mario
+   LDA !sprite_y_high,y        ;\ Same as above but vertical.
    XBA                         ; |
    LDA.w !sprite_y_low,y       ; |
    REP #$20                    ; |
    SEC : SBC $96               ; |
    SEC : SBC HitboxOffsets,x   ; |
    STA $02                     ; |
    SEP #$30                    ;/
    PLX                         ; Restore GMK's slot index

    LDA #$20                            ; Speed of fireball, including diagonals.
    %Aiming()                           ; Calculate the velocity.
    LDA $00                             ;\ set X speed
    STA.w !sprite_speed_x,y             ;/
    LDA $02                             ;\ set Y speed
    STA.w !sprite_speed_y,y             ;/
.Return
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; hammer routine (spawns rocks actually)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HammerThrow2:
    LDA.b #!SprSize-3           ;>Fireballs only interacts with sprite slots 0 to !SprSize-3. See $02A0B8.
    STA $04
    LDA.b #!RockNumber          ;\ Rock sprite number
    SEC                         ; Custom sprite mode
    %SpawnSpriteSafe()
    BCS Return20                ; If indexes full, return

    LDA.b #!RockSFX             ;\play the crumbling sound effect
    STA.w !RockBank|!Base2      ;/

    LDA $94                     ;\ Sprite appears at Mario's X position
    STA.w !E4,y                 ; |
    LDA $95                     ; |
    STA !14E0,y                 ;/
    LDA #$00                    ;\ rock falls straight down onto player
    STA !B6,y                   ; |
    LDA #$40                    ; |
    STA !AA,y                   ;/

    LDA !sprite_y_low,x         ;\ set y position for new sprite
    SEC                         ; |(y position of generator - 1)
    SBC #$F0                    ; |
    STA.w !D8,y                 ; |
    LDA !14D4,x                 ; |
    SBC #$00                    ; |
    STA !14D4,y                 ;/
Return20:
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;hammer routine (spawns koopas actually)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

X_Offset_3:     db $F0,$10
X_Offset_4:     db $FF,$00
X_ThrowSpeed2:  db $18,$E8

Return27:
    RTS

HammerThrow3:
    LDA !sprite_off_screen_horz,x   ;\ don't spawn if off screen
    ORA !sprite_off_screen_vert,x   ; |
    ORA !15D0,x                     ; |
    BNE Return27                    ;/

    LDA.b #!SprSize-3               ;>Fireballs only interacts with sprite slots 0 to !SprSize-3. See $02A0B8.
    STA $04
    LDA.b #!NormalNumber1           ; sprite number
    CLC                             ; Clear carry for normal sprites
    %SpawnSpriteSafe()
    BCS Return27                    ; If all slots full, return (don't play sfx while fail to spawn)

    LDA.b #!KoopaSFX                ;\ SFX for tossing a koopa
    STA.w !KoopaBank|!Base2         ;/

    PHY                         ;\ Depending on where the koopas are spawn,
    LDA !sprite_direction,x     ; | it relies on GMK's current facing direction.
    TAY                         ; |
    LDA !E4,x                   ; |
    CLC : ADC X_Offset_3,y      ; |
    PLY                         ; |
    STA.w !E4,y                 ;/

    PHY                         ;\ Same as above but for high byte
    LDA !sprite_direction,x     ; |
    TAY                         ; |
    LDA !14E0,x                 ; |
    ADC X_Offset_4,y            ; |
    PLY                         ; |
    STA !14E0,y                 ;/

    LDA !sprite_y_low,x         ;\ set y position for new sprite
    SEC                         ; | (y position of generator - 1).
    SBC #$0E                    ; |
    STA.w !D8,y                 ; |
    LDA !14D4,x                 ; |
    SBC #$00                    ; |
    STA !14D4,y                 ;/

    PHY                         ;\ set X speed for each koopa based on GMK facing
    LDA !sprite_direction,x     ; |
    TAY                         ; |
    LDA X_ThrowSpeed2,y         ; |
    PLY                         ; |
    STA.w !B6,y                 ;/

    LDA #$C0                    ;\ set Y speed
    STA.w !AA,y                 ;/
Return28:
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; hammer routine (spawns koopas actually),
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

X_Offset_5:     db $F0,$10
X_Offset_6:     db $FF,$00
X_ThrowSpeed3:  db $E8,$18

Return23:
    RTS

HammerThrow4:
    LDA !sprite_off_screen_horz,x   ;\ don't spawn if off screen
    ORA !sprite_off_screen_vert,x   ; |
    ORA !15D0,x                     ; |
    BNE Return23                    ;/

    LDA.b #!SprSize-3               ;>Fireballs only interacts with sprite slots 0 to !SprSize-3. See $02A0B8.
	STA $04
    LDA.b #!NormalNumber2
    CLC                             ; Clear carry for normal sprite
    %SpawnSpriteSafe()
    BCS Return23                    ; If all slots full, return (don't play sfx while fail to spawn)

    PHY                             ;\ Depending on where the koopas are spawn,
    LDA !sprite_direction,x         ; | it relies on GMK's current facing direction.
    TAY                             ; |
    LDA !E4,x                       ; |
    CLC : ADC X_Offset_5,y          ; |
    PLY                             ; |
    STA.w !E4,y                     ;/

    PHY                             ;\ Same as above but for high byte
    LDA !sprite_direction,x         ; |
    TAY                             ; |
    LDA !14E0,x                     ; |
    ADC X_Offset_6,y                ; |
    PLY                             ; |
    STA !14E0,y                     ;/

    LDA !sprite_y_low,x             ;\ set y position for new sprite
    SEC                             ; | (y position of generator - 1)
    SBC #$0E                        ; |
    STA.w !D8,y                     ; |
    LDA !14D4,x                     ; |
    SBC #$00                        ; |
    STA !14D4,y                     ;/

    PHY                             ;\ set X speed for each koopa based on GMK facing
    LDA !sprite_direction,x         ; |
    TAY                             ; |
    LDA X_ThrowSpeed3,y             ; |
    PLY                             ; |
    STA.w !B6,y                     ;/

    LDA #$C0                        ;\ set Y speed
    STA.w !AA,y                     ;/
Return22:
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; graphics routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Properties: dw .green,.fire,.rocks,.jumping,.walking
.green      db $7B,$3B
.fire       db $79,$39
.rocks      db $75,$35
.jumping    db $77,$37
.walking    db $7B,$3B

X_Disp: dw .green,.fire,.rocks,.jumping,.walking
.green
    db $F8,$08,$F8,$08      ; Spin 1 Right
    db $F8,$08,$F8,$08      ; Spin 2 Right
    db $F8,$08,$F8,$08      ; Spin 3 Right
    db $F8,$08,$F8,$08      ; Spin 4 Right
    db $08,$F8,$08,$F8      ; Spin 1 Left
    db $08,$F8,$08,$F8      ; Spin 2 Left
    db $08,$F8,$08,$F8      ; Spin 3 Left
    db $08,$F8,$08,$F8      ; Spin 4 Left
.fire
    db $F8,$08,$F8,$08      ; Spin 1 Right
    db $F8,$08,$F8,$08      ; Spin 2 Right
    db $F8,$08,$F8,$08      ; Spin 3 Right
    db $F8,$08,$F8,$08      ; Spin 4 Right
    db $08,$F8,$08,$F8      ; Spin 1 Left
    db $08,$F8,$08,$F8      ; Spin 2 Left
    db $08,$F8,$08,$F8      ; Spin 3 Left
    db $08,$F8,$08,$F8      ; Spin 4 Left
.rocks
    db $F8,$08,$F8,$08      ; Spin 1 Right
    db $F8,$08,$F8,$08      ; Spin 2 Right
    db $F8,$08,$F8,$08      ; Spin 3 Right
    db $F8,$08,$F8,$08      ; Spin 4 Right
    db $08,$F8,$08,$F8      ; Spin 1 Left
    db $08,$F8,$08,$F8      ; Spin 2 Left
    db $08,$F8,$08,$F8      ; Spin 3 Left
    db $08,$F8,$08,$F8      ; Spin 4 Left
.jumping
    db $F8,$08,$F8,$08      ; Spin 1 Right
    db $F8,$08,$F8,$08      ; Spin 2 Right
    db $F8,$08,$F8,$08      ; Spin 3 Right
    db $F8,$08,$F8,$08      ; Spin 4 Right
    db $08,$F8,$08,$F8      ; Spin 1 Left
    db $08,$F8,$08,$F8      ; Spin 2 Left
    db $08,$F8,$08,$F8      ; Spin 3 Left
    db $08,$F8,$08,$F8      ; Spin 4 Left
.walking
    db $F0,$00,$F0,$00,$F0,$00,$F0,$00
    db $F0,$00,$F0,$00,$F0,$00,$F0,$00
    db $10,$00,$10,$00,$10,$00,$10,$00
    db $10,$00,$10,$00,$10,$00,$10,$00

Y_Disp: dw .green,.fire,.rocks,.jumping,.walking
.green
    db $F0,$F0,$00,$00      ; Spin 1 Right
    db $F0,$F0,$00,$00      ; Spin 2 Right
    db $F0,$F0,$00,$00      ; Spin 3 Right
    db $F0,$F0,$00,$00      ; Spin 4 Right
    db $F0,$F0,$00,$00      ; Spin 1 Left
    db $F0,$F0,$00,$00      ; Spin 2 Left
    db $F0,$F0,$00,$00      ; Spin 3 Left
    db $F0,$F0,$00,$00      ; Spin 4 Left
.fire
    db $F0,$F0,$00,$00      ; Spin 1 Right
    db $F0,$F0,$00,$00      ; Spin 2 Right
    db $F0,$F0,$00,$00      ; Spin 3 Right
    db $F0,$F0,$00,$00      ; Spin 4 Right
    db $F0,$F0,$00,$00      ; Spin 1 Left
    db $F0,$F0,$00,$00      ; Spin 2 Left
    db $F0,$F0,$00,$00      ; Spin 3 Left
    db $F0,$F0,$00,$00      ; Spin 4 Left
.rocks
    db $F0,$F0,$00,$00      ; Spin 1 Right
    db $F0,$F0,$00,$00      ; Spin 2 Right
    db $F0,$F0,$00,$00      ; Spin 3 Right
    db $F0,$F0,$00,$00      ; Spin 4 Right
    db $F0,$F0,$00,$00      ; Spin 1 Left
    db $F0,$F0,$00,$00      ; Spin 2 Left
    db $F0,$F0,$00,$00      ; Spin 3 Left
    db $F0,$F0,$00,$00      ; Spin 4 Left
.jumping
    db $F0,$F0,$00,$00      ; Spin 1 Right
    db $F0,$F0,$00,$00      ; Spin 2 Right
    db $F0,$F0,$00,$00      ; Spin 3 Right
    db $F0,$F0,$00,$00      ; Spin 4 Right
    db $F0,$F0,$00,$00      ; Spin 1 Left
    db $F0,$F0,$00,$00      ; Spin 2 Left
    db $F0,$F0,$00,$00      ; Spin 3 Left
    db $F0,$F0,$00,$00      ; Spin 4 Left
.walking
    db $D0,$D0,$E0,$E0,$F0,$F0,$00,$00
    db $D0,$D0,$E0,$E0,$F0,$F0,$00,$00
    db $D0,$D0,$E0,$E0,$F0,$F0,$00,$00
    db $D0,$D0,$E0,$E0,$F0,$F0,$00,$00

Tilemap: dw .green,.fire,.rocks,.jumping,.walking
.green
    db $04,$06,$08,$0A      ; Spin 1 Right
    db $0C,$0E,$2C,$2E      ; Spin 2 Right
    db $4C,$4E,$6C,$6E      ; Spin 3 Right
    db $80,$82,$A0,$A2      ; Spin 4 Right
    db $04,$06,$08,$0A      ; Spin 1 Left
    db $0C,$0E,$2C,$2E      ; Spin 2 Left
    db $4C,$4E,$6C,$6E      ; Spin 3 Left
    db $80,$82,$A0,$A2      ; Spin 4 Left
.fire
    db $04,$06,$08,$0A      ; Spin 1 Right
    db $0C,$0E,$2C,$2E      ; Spin 2 Right
    db $4C,$4E,$6C,$6E      ; Spin 3 Right
    db $80,$82,$A0,$A2      ; Spin 4 Right
    db $04,$06,$08,$0A      ; Spin 1 Left
    db $0C,$0E,$2C,$2E      ; Spin 2 Left
    db $4C,$4E,$6C,$6E      ; Spin 3 Left
    db $80,$82,$A0,$A2      ; Spin 4 Left
.rocks
    db $04,$06,$08,$0A      ; Spin 1 Right
    db $0C,$0E,$2C,$2E      ; Spin 2 Right
    db $4C,$4E,$6C,$6E      ; Spin 3 Right
    db $80,$82,$A0,$A2      ; Spin 4 Right
    db $04,$06,$08,$0A      ; Spin 1 Left
    db $0C,$0E,$2C,$2E      ; Spin 2 Left
    db $4C,$4E,$6C,$6E      ; Spin 3 Left
    db $80,$82,$A0,$A2      ; Spin 4 Left
.jumping
    db $04,$06,$08,$0A      ; Spin 1 Right
    db $0C,$0E,$2C,$2E      ; Spin 2 Right
    db $4C,$4E,$6C,$6E      ; Spin 3 Right
    db $80,$82,$A0,$A2      ; Spin 4 Right
    db $04,$06,$08,$0A      ; Spin 1 Left
    db $0C,$0E,$2C,$2E      ; Spin 2 Left
    db $4C,$4E,$6C,$6E      ; Spin 3 Left
    db $80,$82,$A0,$A2      ; Spin 4 Left
.walking
    db $00,$02,$20,$22,$40,$42,$60,$62
    db $00,$02,$24,$26,$44,$46,$64,$66
    db $00,$02,$20,$22,$40,$42,$60,$62
    db $00,$02,$24,$26,$44,$46,$64,$66

;3/8/3 LevelSprite GiantMaskedKoopa Layout
		BarTilesLeft:
		;Left end fill amount tile numbers:
		db $88		;>Fill amount/index: $00
		db $89		;>Fill amount/index: $01
		db $8A		;>Fill amount/index: $02
		db $8B		;>Fill amount/index: $03
		BarTilesMiddle:
		;Middle fill amount tile numbers
		db $8C		;>Fill amount/index: $00
		db $8D		;>Fill amount/index: $01
		db $8E		;>Fill amount/index: $02
		db $8F		;>Fill amount/index: $03
		db $98		;>Fill amount/index: $04
		db $99		;>Fill amount/index: $05
		db $9A		;>Fill amount/index: $06
		db $9B		;>Fill amount/index: $07
		db $9C		;>Fill amount/index: $08
		BarTilesRight:
		;Right end fill amount tile numbers:
		db $9D		;>Fill amount/index: $00
		db $9E		;>Fill amount/index: $01
		db $9F		;>Fill amount/index: $02
		db $A8		;>Fill amount/index: $03

GraphicalBarXDisp:	;[GraphicalBar_For_HP]
	;The origin of the sprites XY position isn't centered with the image of the sprite,
	;so the bar's offset from the sprite's origin have to adjust to remain centered with the sprite.
	db !Default_GiantMaskedKoopa_GraphicalBar_XPosOffset_StandFaceRight	;>Facing right
	db !Default_GiantMaskedKoopa_GraphicalBar_XPosOffset_StandFaceLeft	;>Facing left
Graphics:
	;[GraphicalBar_For_HP]
	;It's best to have this graphical bar code at the very start of "Graphics" so that branches would not skip
	;over this code and cause the bar to disappear.
	STA $0F		;>You cannot push, call GetDrawInfo, then pull, because GetDrawInfo contains a code that destroy the return address,
			;which will crash the game when the sprite goes offscreen.
			; This is the graphics "image state" (what it shows), separate from "Behavior State" (what it does physically) as
			;some of the frames are walking and the other is in its shell:
			;A < $04 means in shell
			;A >= $04 means standing upright
			;Since this is used before and after calling %GetDrawInfo(), I choose to store it at $0F, an unused RAM at the moment.
	;Here is the newly added code, because this is before GetDrawInfo, we can only get the bar values here and will do the OAM
	;after calling GetDrawInfo.
	;To obtain the graphical bar data, (like the amount of fill, the attributes and all that stuff), is virtually no different
	;compared to the layer 3 version. The only difference is writing to OAM as opposed to layer 3 tiles.
		.InputRatio
			;Quantity (HP)
			;$1534 is the damage counter (increase by 1 on each hit, and dies when reaching a certain value (!HitPoints)),
			;therefore this sprite uses an "inverted health system".
			;To convert damage count to HP: HP = NumberOfHitsToKill - DamageCount
			;I could use InvertQuantity, but because the sprite always take 1 damage from stomps and no other types of damage exists,
			;and that its damage counter and !HitPoints (the fixed number of hits to defeat) are 8-bit, it is not needed, as this is faster.
				LDA #!HitPoints					;\HP = NumberOfHitsToKill - DamageCount
				SEC						;|
				SBC !1534,x					;|
				STA !Scratchram_GraphicalBar_FillByteTbl	;/Store HP into quantity.
			LDA #$00						;>Set these following data to 0 since HP is 8-bit
			STA !Scratchram_GraphicalBar_FillByteTbl+1		;>High byte quantity
			STA !Scratchram_GraphicalBar_FillByteTbl+3		;>High byte max quantity
			LDA #!HitPoints						;\Max quantity
			STA !Scratchram_GraphicalBar_FillByteTbl+2		;/
		.InputGraphicalBarAttributes
			LDA.b #!Default_GiantMaskedKoopa_GraphicalBar_LeftEndPieces		;\Left end normally have 3 pieces.
			STA !Scratchram_GraphicalBar_LeftEndPiece				;/
			LDA.b #!Default_GiantMaskedKoopa_GraphicalBar_MiddlePieces		;\Number of pieces in each middle byte/8x8 tile
			STA !Scratchram_GraphicalBar_MiddlePiece				;/
			LDA.b #!Default_GiantMaskedKoopa_GraphicalBar_RightEndPieces		;\Right end
			STA !Scratchram_GraphicalBar_RightEndPiece				;/
			LDA.b #!Default_GiantMaskedKoopa_GraphicalBar_MiddleLength		;\length (number of middle tiles)
			STA !Scratchram_GraphicalBar_TempLength					;/
		.ConvertToBar
			PHX								;>Preserve sprite slot index
			%GraphicalBarCalculateGraphicalBarPercentage()			;>Get percentage
			%GraphicalBarRoundAwayEmptyFull()				;>Round away from empty and full for fill amount
			;$00 = HP percentage (FirstFill)
			;$1570 = Previous HP percentage (SecondFill), or the increasing fill amount to display when healing/battle starts.
			if !GiantMaskedKoopa_DisplayPreviousHP != 0
				LDX $15E9|!addr	;>Get sprite index
				LDA !1570,x
				CMP $00
				BCC ..Increase
				BEQ ..ShowCurrent

				..Decrease
					;Decrease, causes $1570 as a transperent bar to decrease towards HP percentage.
					LDA !1558,x		;\Don't decrease SecondFill until delay runs out.
					BNE ..Flicker		;/
					DEC !1570,x
					BRA ..Flicker
				..Increase
					;In this state, when $1570 less than current HP percentage,
					;do the filling animation by having $00 contain the value of $1558 every frame and the SFX.
					LDA $0100|!addr			;\Prevent playing filling SFX and filling during a black screen fade
					CMP #$14			;|
					BNE ..ShowPrev			;/
					INC !1570,x			;>Filling up animation (1 piece per frame)
					LDA $13
					AND.b #%00000001
					BNE ..ShowPrev
					LDA #$23			;\SFX of bar filling
					STA $1DFC|!addr			;/
					BRA ..ShowPrev
				..Flicker
					;In this state, every other frame displays previous HP
					;(because HP percentage gets subtracted on every damage,
					;while $1558 keeps its value temporary across frames),
					;giving a transperent look
						LDA $13
						AND.b #%00000001
						BEQ ..ShowCurrent
					
				..ShowPrev
					LDA !1570,x		;\When filling upwards or on every odd frames, $00 will contain the value
					STA $00			;/of $1570, showing a filling animation or previous HP percentage instead of current HP percentage.
					
				..ShowCurrent
			endif
			%GraphicalBarSplitFill()		;>get bar values.
			PEI ($00)	;Preserve a bunch of stuff because following subroutine uses $00-$09.
			PEI ($02)
			PEI ($04)
			PEI ($06)
			PEI ($08)
			REP #$20
			LDA.w #BarTilesLeft : STA $00
			LDA.w #BarTilesMiddle : STA $03
			LDA.w #BarTilesRight : STA $06
			SEP #$20
			LDA.b #BarTilesLeft>>16 : STA $02 : STA $05 : STA $08 ;>Resides in the same bank
			%GraphicalBarConvertBarFillAmountToTilesIndirectAddressTable() ;Convert fill amount to tiles
			REP #$20
			PLA : STA $08
			PLA : STA $06
			PLA : STA $04
			PLA : STA $02
			PLA : STA $00
			SEP #$20
			PLX								;>Restore sprite slot index
	;[GraphicalBar_For_HP] I rearrange the code and pushing values into the stack was needed because scratch RAM is going to be used:
	;Y: Oam index
	;$00: Tile X position
	;$01: Tile Y position
	;$04-$0B: Scratch RAM containing the address for indirect addressing (<opcode> ($xx),y)
	; $04-$05: 16-bit address table starting address location for X_Disp.
	; $06-$07: Same as above but for Y_Disp.
	; $08-$09: Same but for Tilemap.
	; $0A-$0B: Same but Properties.
	;The routines !DrawSpriteGraphicalBarHoriz and !DrawSpriteGraphicalBarVert already have the same type of information
	;for the Y, $00 and $01, so no concerns on those stuff. We can set up drawing the bar BEFORE we start using $04-$0B
	;for indirect addressing.
		%GetDrawInfo()		;>We need: Y: OAM index, $00 and $01: Position. It does not mess with any other data in $02-$0F. Like I said, don't push, then call this without pulling in between pushing and calling GetDrawInfo.
		;Like I said, what's first drawn in code-order is to be in front (top) of another sprite, due to after writing each tile, the OAM index gets INC by 4 (rather than DEC).
		;Therefore if the GMK's body and the bar overlaps, the bar will be on top since that is processed first.
		
		%GraphicalBarCountNumberOfTiles()	;\Get number of tiles of the graphical bar
		INX					;|This is important because FinishOamWrite needs the total tiles (the body of the sprite and the HP bar), minus 1.
		STX $04					;|
		STZ $05					;/
		
		LDX $15E9|!addr			;>Sprite index
		;[GraphicalBar_For_HP_Centering]
		;Because the sprite's origin is not always centered with the sprite's body, especially when it changes its image state and have different alignments,
		;which may have the minimum bounding box be shifted relative to its origin, we need to have a code that also shifts the bar to remain centered with
		;the sprite's body.
		;
		;The good news is that the bottom of the sprite does not shift, so the Y position of the bar can be a fixed value relative to the sprite's origin position
		;so we can place it under its feet. But the X centering position is what we need to condition the X-position offset based on its image states.
		;
		;There are 3 alignments to consider:
		;-When the koopa is in its shell form: The very start of the phase waiting for the player to get close or chasing the player like a yellow disco shell.
		;-When the koopa is standing upright, facing right
		;-When the koopa is standing upright, facing left.
		;For other sprites, there is a very high chance you'll have to trial-and-error the X and Y position offset so that it centers correctly with the sprite,
		;mainly because the origin position of each sprites may be different in SMWC's sprite section and the fact that the shape of the sprite
		;can differ.
			LDA $0F			;>We got our sprite image state previously stored in $0F, so using this can help detect how our HP bar is positioned correctly.
			CMP #$04
			BCS .StandUpright
			
			.InShell
			LDA $00								;\X position
			CLC								;|
			ADC #!Default_GiantMaskedKoopa_GraphicalBar_XPosOffset_InShell	;|
			STA $02								;/
			BRA +
			
			.StandUpright
			PHY
			LDA !157C,x			;>Sprite facing direction: $00 = right, $01 = left.
			TAY
			LDA $00
			CLC
			ADC GraphicalBarXDisp,y
			STA $02
			PLY
		+
		LDA $01							;\Y position
		CLC							;|
		ADC #!Default_GiantMaskedKoopa_GraphicalBar_YPosOffset	;|
		STA $03							;/
		
		LDA #!Default_GiantMaskedKoopa_GraphicalBar_Flipped	;\Set direction
		STA $06							;/
		
		LDA.b #!Default_GiantMaskedKoopa_GraphicalBar_Properties	;\Properties
		STA $07								;/
		PHX
		if !Default_GiantMaskedKoopa_GraphicalBar_HorizOrVert == 0
			%GraphicalBarDrawSpriteGraphicalBarHoriz()
		else
			%GraphicalBarDrawSpriteGraphicalBarVert()
		endif
		PLX
		;[GraphicalBar_For_HP]
		;Now I am done using $02-$08 after calling DrawSpriteGraphicalBarHoriz or DrawSpriteGraphicalBarVert. Therefore, those are now free to use for other things.
		;Here, this is where $04-$0B is going to be used as an indirect addressing (get a value at an address that was stored in an address; <opcode> ($xx),y)
		;First, it loads the 16-bit values representing the locations of each table (or "sub-table") into $04, $06, $08 and $0A.
		;Then later, uses them (like ADC ($04),y).
		LDA $0F			;\Save graphic state
		PHA			;/
		PHY			;>Preserve OAM index
		ASL A			;\Index the sprite's state.
		TAY			;/
		REP #$20		;\These sets up the table addresses for the following code below
		LDA X_Disp,y		;|using indirect (you load a RAM that contains an address and load that)
		STA $04			;|($xx),y
		LDA Y_Disp,y		;|
		STA $06			;|
		LDA Tilemap,y		;|
		STA $08			;|
		LDA Properties,y	;|
		STA $0A			;|
		SEP #$20		;/
		PLY			;>Restore OAM index


    LDA !sprite_smash_status,x
    STA $03                     ; | $03 = index to frame start (0 or 4)

    PLA					;\Pull state
    CMP #$04				;/
    BCS AlternateFrames		;>AlternateFrames is the state the sprite is standing upright
    LDA #$03
    STA $0F
    LDA $14                     ;\ Frame counter ..
    LSR #3                      ; | Add in frame animation rate; More LSRs for slower animation.
    AND #$03                    ; | 01 means we animate between 2 frames (00 and 01).
    ASL #2                      ; | ASL x2 (0-4) makes it switch between the first byte and fifth byte,
    STA $03                     ;/ i.e. first animation and second animation. The result is stored into $03.

ContinueGraphics: ;This is where scratch RAM is being used for indirect addressing (LDA ($xx),y)
    LDA !sprite_direction,x
    STA $02                     ; Store direction to $02 for use with property routine later.
    BNE +
        LDA $03                 ;\ 
        CLC                     ; | If sprite faces left ..
        ADC #$10                ; | Adding 16 more bytes to the table.
        STA $03                 ;/ So we can invert X_Disp to not mess up the sprite's appearance.
+   PHX                         ;\ Push sprite index ..
    LDX $0F                     ;/ And load X with number of tiles to loop through.
-   PHX                         ; Push number of tiles to loop through.
    TXA                         ;\ 
    ORA $03                     ;/ Transfer it to X and add in the "left displacement" if necessary.
    TAX                         ;\ Get it back into X for an index.

    PHY                         ; Backup the OAM index
    PHX : TYX : PLY             ; Swap X and Y. Now X is the OAM index and Y is tile loop count. This is because <opcode> ($xx),x does not exist, instead it is <opcode> ($xx,x) which we don't want.

    LDA $00                     ;\ 
    CLC : ADC ($04),y           ; | Apply X displacement of the sprite.
    STA $0300|!Base2,x          ;/

    LDA $01                     ;\ 
    CLC : ADC ($06),y           ; | Apply Y displacement of the sprite.
    STA $0301|!Base2,x          ;/

    LDA ($08),y
    STA $0302|!Base2,x
    
	;[GraphicalBar_For_HP] Manually set tile size for the main body of the sprite
	;Because most sprites are composed of 16x16 OAM tiles, and the bar being 8x8s, The GMK boss is NO exception.
	;It is really not that hard, just preserve X or Y, depending on the sprite (GMK boss uses X, in this example),
	;then take X or Y, whichever holds the OAM index (increments of 4) TXA or TYA, LSR #2, TAX or TAY to convert
	;to slot numbering (increments of 1), and SET $0460, indexed by slot numbering's bit 1 (the 2nd last bit from the right) to 1.
	PHX			;>Preserve OAM index
	TXA			;\Convert OAM index numbering to slot numbering (increments of 1)
	LSR #2			;|
	TAX			;/
	LDA $0460|!addr,x	;\Force the size bit of the OAM extra bit to be 16x16 instead of potentially be 8x8
	ORA.b #%00000010	;|
	STA $0460|!addr,x	;/
	PLX			;>Restore OAM index

    PHY
    LDY $02
    LDA ($0A),y                 ;\ Set properties based on direction.
    STA $0303|!Base2,x          ;/
    PLY

    PLY				;>This will unswap (part 1/2) XY, now Y being the OAM index and X being the tile count.
    INY #4			;>Next OAM index

    PLX                         ; Pull current tile back (unswap, part 2/2).
    DEX                         ; After drawing this tile, decrease number of tiles to go through loop. If the second tile
                                ; is drawn, then loop again to draw the first tile.

    BPL -                       ; Loop until X becomes negative (FF).

    PLX                         ; Pull back the sprite index! We pushed it at the beginning of the routine.

	;[GraphicalBar_For_HP] This is the last steps on working with OAM handling.
	;Like I said, most sprites are composed of 16x16 tiles, and having a bar made of 8x8s means we are manually setting the tile sizes.
	;When finishing OAM write, the inputs are:
	; Y = size: $00 = 8x8s, $02 = 16x16s, $FF manuel
	; A = Number of OAM slots used, minus 1.
	;So have Y=$FF, unless in 0.01% chance you are having a PURE 8x8 sprite, then leave Y=$00.
	;For A, we need the total number of tiles, which is the sprite's main body plus the bar itself, minus 1.
	;It is very easy to know how many tiles the bar is, go check out the defines in GraphicalBarDefines/SpriteOAMSettings.asm
	;and under "Giant Masked koopa tile count" contains a formula of counting the the number tiles for left end, middle, and right end.
	;Since $0F contains the number of tiles, minus 1, we don't need to decrease it by 1 again (decrease by 1 again means we subtracted
	;by 2 and that is the wrong amount). Therefore [(SpriteBodyTileCount + BarTileCount) - 1] is the same as [(SpriteBodyTileCount - 1) + BarTileCount].
		LDY #$FF                    ; Y ends with the tile size .. 02 means it's 16x16 (Edit: the bar is 8x8s, and the body of the sprite is 16x16, so a mixture was needed; Y=$FF means Manuel)
		LDA $0F                     ; A -> number of tiles drawn - 1.
		CLC
		ADC.b #!GiantMaskedKoopa_GraphicalBar_TotalTiles ; Add by the number of tiles of the graphical bar (no need to subtract by 1, else we are doing it twice).
                                ; I drew 2 tiles, so 2-1 = 1. A = 01.
    JSL $01B7B3|!BankB          ; Call the routine that draws the sprite (finish OAM write).
    RTS

AlternateFrames:
    LDA #$07
    STA $0F
    LDA $14         ;\ Frame counter ..
    LSR #3          ; | Add in frame animation rate; More LSRs for slower animation.
    AND #$01        ; | 01 means we animate between 2 frames (00 and 01).
    ASL #3          ; | ASL x2 (0-4) makes it switch between the first byte and fifth byte
    STA $03         ;/ i.e. first animation and second animation. The result is stored into $03.
    BRA ContinueGraphics

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChangeDirection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ChangeDirection:
    LDA !sprite_speed_x,x
    EOR #$FF : INC A
    STA !sprite_speed_x,x
    LDA !sprite_direction,x
    EOR #$01
    STA !sprite_direction,x
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