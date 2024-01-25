;This is similar to StatusBarSettings.asm, however, this alone is to work with sprite tile OAM-based code.
;This works for both:
;-Pixi_CustomSprite/SpriteGraphicalBarTest.asm
;-Patch_DrawSpriteGraphicalBar/DrawOamPatch.asm

;Pixi custom sprite settings (for "SpriteGraphicalBarTest.asm")
 !Default_PixiSprite_LeftEndPieces = 3
 !Default_PixiSprite_MiddlePieces = 8
 !Default_PixiSprite_RightEndPieces = 3
 ;Length and direction (rightwards, leftwards, upwards, and downwards) handled using extra-bytes settings,
 ;see inside the sprite's ASM file for details.
;Giant Masked Koopa settings
 ;Bar attributes
  !Default_GiantMaskedKoopa_GraphicalBar_LeftEndPieces = 3
  !Default_GiantMaskedKoopa_GraphicalBar_MiddlePieces = 8
  !Default_GiantMaskedKoopa_GraphicalBar_RightEndPieces = 3
  !Default_GiantMaskedKoopa_GraphicalBar_MiddleLength = 7		;>Number of middle tiles
 ;Bar direction
  !Default_GiantMaskedKoopa_GraphicalBar_Flipped = $00
   ;^$00 = fill left-to-right/bottom-to-top
   ;^$01 = fill right-to-left/top-to-bottom (X or Y bits on YXPPCCCT are set)
 ;Graphical bar tile displacement (from the main body origin (top-left of the 16x16 part where his feet is at)). Note: The
 ;XY origin of the bar is the end of the bar where the fill starts at when increasing.
  if !Default_GiantMaskedKoopa_GraphicalBar_Flipped == 0 ;Don't modify the if statements.
   ;Left to right displacement
    !Default_GiantMaskedKoopa_GraphicalBar_XPosOffset_InShell = $E4
    !Default_GiantMaskedKoopa_GraphicalBar_XPosOffset_StandFaceLeft = $DC
    !Default_GiantMaskedKoopa_GraphicalBar_XPosOffset_StandFaceRight = $EC
  else
   ;Right to left displacement
    !Default_GiantMaskedKoopa_GraphicalBar_XPosOffset_InShell = $25
    !Default_GiantMaskedKoopa_GraphicalBar_XPosOffset_StandFaceLeft = $1D
    !Default_GiantMaskedKoopa_GraphicalBar_XPosOffset_StandFaceRight = $2D
  endif
  !Default_GiantMaskedKoopa_GraphicalBar_YPosOffset = $10
 ;Other settings
  !Default_GiantMaskedKoopa_GraphicalBar_Properties = %00111001
   ;^Properties (YXPPCCCT).
  !Default_GiantMaskedKoopa_GraphicalBar_HorizOrVert = 0
   ;^0 = horizontal
   ; 1 = vertical
 ;DoubleBar stuff
  !GiantMaskedKoopa_DisplayPreviousHP = 1
   ;^0 = only show current HP
   ; 1 = every time damage is taken, shows a pseudo transparent rapid-flicker
   ;     bar indicating the amount of damage ($1570 acts as a second fill)
  !GiantMaskedKoopa_PrevHPDelay = 60
   ;^Amount of frames (1/60th of a second) SecondFill of the bar freezes before
   ; before decreasing towards its current HP percentage.
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
   ;^0 = fill rightwards (horizontal)
   ; 1 = fill leftwards (horizontal, YXPPCCCT's X bit set)
   ; 2 = fill upwards (vertical)
   ; 3 = fill downwards (vertical, YXPPCCCT's Y bit set)
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
  
  !GraphicalBar_OAMSlot = 4
   ;^Starting slot number to use (increments of 1) for checking, not to be confused with index (which increments by 4). Use only values 0-127 ($00-$7F).
   
   
;Defines you shouldn't touch unless you know what you're doing.
 ;Giant Masked koopa tile count
  ;Explanation: if these tiles don't exist (pieces set to 0, and/or middle length set to 0)
  ;They're not written, so they are not counted. If they do exist (pieces set to any nonzero values),
  ;that counts as a tile. After knowing if they exist or not we add all of them.
  !GiantMaskedKoopa_GraphicalBar_LeftEndExist = 0
  !GiantMaskedKoopa_GraphicalBar_MiddleExist = 0		;>How many middle tiles
  !GiantMaskedKoopa_GraphicalBar_RightEndExist = 0
  if !Default_GiantMaskedKoopa_GraphicalBar_LeftEndPieces != 0
   !GiantMaskedKoopa_GraphicalBar_LeftEndExist = 1
  endif
  if and(notequal(!Default_GiantMaskedKoopa_GraphicalBar_MiddlePieces, 0),notequal(!Default_GiantMaskedKoopa_GraphicalBar_MiddleLength, 0))	;>Number of middle tiles AND number of middle pieces MUST be nonzero to include them
   !GiantMaskedKoopa_GraphicalBar_MiddleExist = !Default_GiantMaskedKoopa_GraphicalBar_MiddleLength
  endif
  if !Default_PatchSprite_RightEndPieces != 0
   !GiantMaskedKoopa_GraphicalBar_RightEndExist = 1
  endif
  !GiantMaskedKoopa_GraphicalBar_TotalTiles = !GiantMaskedKoopa_GraphicalBar_LeftEndExist+!GiantMaskedKoopa_GraphicalBar_MiddleExist+!GiantMaskedKoopa_GraphicalBar_RightEndExist