;Status bar stuff
 !StatusBarFormat                     = $01
 ;^Number of grouped bytes per 8x8 tile:
 ; $01 = Minimalist/SMB3 [TTTTTTTT, TTTTTTTT]...[YXPCCCTT, YXPCCCTT] or SMW's default.
 ; $02 = Super status bar/Overworld border plus [TTTTTTTT YXPCCCTT, TTTTTTTT YXPCCCTT]...
 
 ;Tile positions. If you are using other status bar patches other than the Super Status Bar
 ;patch, make sure the RAMs here matches the RAM address those patch are using. By default:
 ;
 ; $0F09|!addr overwrites the word "TIME".
 ; $7FA000 and $404000 takes the top-left corner when using the super status bar patch.
 ;
  if !StatusBarFormat == $01
   if !sa1 == 0
    !GraphicalBarPos                      = $7E0f09
   else
    !GraphicalBarPos                      = $400f09
  else
   if !sa1 == 0
    !GraphicalBarPos                     = $7FA000      ;>Status bar RAM data.
   else
    !GraphicalBarPos                     = $404000      ;>Status bar RAM data.
   endif
  endif
   ;^Location of the bar when !StatusBarFormat is $01. This is under use of
   ; STA [$00], that is why it needs to be 3-bytes.
  
;Tile settings:
 !Default_MiddleLength                = 7             ;>30 = screen-wide (30 + 2 end tiles = 32, all 8x8 tile row in the screen's width)
 !Default_LeftPieces                  = 3             ;\These will by default, set the RAM for the pieces for each section
 !Default_MiddlePieces                = 8             ;|
 !Default_RightPieces                 = 3             ;/

 !Leftwards                           = 0
  ;^Have the bar fill leftwards. Note that end tiles are also
  ; mirrored. This only works properly on any status bar patches
  ; that allow editing the tile properties. Having this set to 1
  ; while using SMW's vanilla status bar causes each tiles to fill
  ; backwards while advancing to the left. If that is the case, flip
  ; the tiles in the file bin then or edit SMW's status bar table at
  ; address $008C81.

; ;Don't touch, these are used for loops to write to the status bar.
;  !GraphiBar_LeftTileExist = 0
;  !GraphiBar_MiddleTileExist = 0
;  !GraphiBar_RightTileExist = 0
;  if !Default_LeftPieces != 0
;   !GraphiBar_LeftTileExist = 1
;  endif
;  if !Default_MiddlePieces != 0 && !Default_MiddleLength != 0
;   !GraphiBar_MiddleTileExist = 1
;  endif
;  if !Default_RightPieces != 0
;   !GraphiBar_RightTileExist = 1
;  endif
;  
;  !GraphicalBar_TotalTileUsed = !GraphiBar_LeftTileExist+(!GraphiBar_MiddleTileExist*!Default_MiddleLength)+!GraphiBar_RightTileExist
;   ;^The amount of bytes the table used up.