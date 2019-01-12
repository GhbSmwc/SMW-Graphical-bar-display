;Freeram. Note that I made SA-1 versions despite they don't need to in the case the
;user wants to port their hack into an sa-1 version by editing the latter, also to
;be consistent with my other resources.
 if !sa1 == 0
  !Freeram_TowerOfHeavenFanned = $60
 else
  !Freeram_TowerOfHeavenFanned = $60
 endif
  ;^[1 byte] Fanned bit flags: ------SI
  ; I = Inside block flag. Needed so the player reacts to the block one time per
  ;     frame instead of being executed multiple times based on the number of
  ;     collision points.
  ; S = Sound played flag. Needed to play the sound once instead of multiple times.
  ;

!Setting_HoldJump = 2
;^0 = allow the player to switch between two gravitational acceleration by
;  the jump button
; 1 = force the jump button to not be held down
; 2 = force the jump button to be held down
;

!FanAccel = $0B
;^How fast the player accelerates upwards. If !Setting_HoldJump
;is set to 0 or 1, it must be at least #$06, otherwise, #$03 instead so
;that the player doesn't completely stops or fails to shoot upwards.

;Note that if you have interface menus or other things, you might want to
;remove the semicolons around the area that effects $15