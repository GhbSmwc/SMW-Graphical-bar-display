;This is similar to StatusBarSettings.asm, however, this alone is to work with sprite tile OAM-based code.
;This works for both:
;-Pixi_CustomSprite/SpriteGraphicalBarTest.asm
;-Patch_DrawSpriteGraphicalBar/DrawOamPatch.asm

;Pixi custom sprite settings
 !Default_PixiSprite_LeftEndPieces = 3
 !Default_PixiSprite_MiddlePieces = 8
 !Default_PixiSprite_RightEndPieces = 3
 ;Length and direction (rightwards, leftwards, upwards, and downwards) handled using extra-bytes settings,
 ;see inside the sprite's ASM file for details.
;Patch sprite settings
 !Default_PatchSprite_LeftEndPieces = 3
 !Default_PatchSprite_MiddlePieces = 8
 !Default_PatchSprite_MiddleLength = 4
 !Default_PatchSprite_RightEndPieces = 3
 ;Stuff unrelated to the graphical bar
  !PatchSprite_Uninstall	= 0
   ;^0 = patch
   ; 1 = uninstall (reverts the game to a state you haven't patch this)
   
  !PatchSprite_Palette	= 0
   ;^Palette, only use 0-7.
   
  !PatchSprite_Direction	= 0
   ;^0 = fill rightwards
   ; 1 = fill leftwards (YXPPCCCT's X bit set)
   ; 2 = fill upwards
   ; 3 = fill downwards (YXPPCCCT's Y bit set)
   ; Note: The naming of left and right end tiles, are relative
   ; to where the fill starts and ends as it increases, meaning
   ; "left end" is where the fill starts and "right end" where
   ; the fill ends, regardless of the direction of the bar.
  
  ;Position of the graphical bar.
  ;Note: Origin the where the fill starts, not always the top-left corner.
   !PatchSprite_BarXPos		= $FFF0
   !PatchSprite_BarYPos		= $FFF8
   !PatchSprite_BarOnPlayer	= 1
    ;^0 = fixed on-screen position, relative to top-left of the screen.
    ; 1 = placed relative to player's on-screen position. XY pos will be the displacement from player.
  !PageNum = 1
   ;^Page number of sprite, only use 0-1.
  
  !OAMSlot = 4
   ;^Starting slot number  to use (increments of 1), not to be confused with index (which increments by 4). Use only values 0-127 ($00-$7F).