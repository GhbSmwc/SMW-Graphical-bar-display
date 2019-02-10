;This is separate from the main define file "GraphicalBarDefines.asm" because of these reasons:
;-This is a preset setting, made for specific bars and not for all of them.
;-The main define file covers all stuff using the graphical bar routines.

;Status bar stuff
 !StatusBarFormat                     = $01
  ;^Number of grouped bytes per 8x8 tile:
  ; $01 = each 8x8 tile have two bytes each separated into "tile numbers" and "tile properties" group;
  ;       Minimalist/SMB3 [TTTTTTTT, TTTTTTTT]...[YXPCCCTT, YXPCCCTT] or SMW's default ([TTTTTTTT] only).
  ; $02 = each 8x8 tile byte have two bytes located next to each other;
  ;       Super status bar/Overworld border plus [TTTTTTTT YXPCCCTT, TTTTTTTT YXPCCCTT]...
 
 !StatusBar_UsingCustomProperties           = 0
  ;^Set this to 0 if you are using the vanilla SMW status bar or any status bar patches
  ; that doesn't enable editing the tile properties, otherwise set this to 1 (you may
  ; have to edit "!Default_GraphicalBarProperties" in order for it to work though.).
  ; This define is needed to prevent writing what it assumes tile properties into invalid
  ; RAM addresses.
 
 !Default_StatusBar_TilePropertiesSetting      = %00111000
  ;^Tile properties (if you enable editing properties in-game). Note: Bit 6 (X-flip) is
  ; forced to be set when !Default_LeftwardsBar is set to 1. If you want this to be x flipped,
  ; set that aforementioned setting.
 
 ;Tile positions. If you are using other status bar patches other than the Super Status Bar
 ;patch, make sure the RAMs here matches the RAM address those patch are using. By default:
 ;
 ; $0F09|!addr overwrites the word "TIME".
 ; $7FA000 and $404000 takes the top-left corner when using the super status bar patch.
 ;
 ;These must be 3-bytes long (6 hexadecimal digits long), as they are under a routine that
 ;uses STA [$00],y to write the tiles to the status bar.
 
 ;This covers all bars that would extend rightwards as you increase the length.
  if !StatusBarFormat == $01
   if !sa1 == 0
    !Default_GraphicalBarPosition                      = $7E0F09     ;>SMW's status bar. Replace with "$0C00|!addr" for SMB3 status bar and "$0BF6|!addr" for minimalist status bar.
   else
    !Default_GraphicalBarPosition                      = $400F09     ;>SMW's status bar (SA-1). Replace with "$0C00|!addr" for SMB3 status bar and "$0BF6|!addr" for minimalist status bar.
   endif
   ;^Location of the bar when !StatusBarFormat is $01. This is under use of
   ; STA [$00], that is why it needs to be 3-bytes.
  else
   if !sa1 == 0
    !Default_GraphicalBarPosition                     = $7FA000      ;>Status bar RAM data.
   else
    !Default_GraphicalBarPosition                     = $404000      ;>Status bar RAM data (SA-1).
   endif
   ;^Same as above, but for a format that each 8x8 tile contains 2 bytes next to each other.
  endif
  if !StatusBarFormat == $01
   if !sa1 == 0
    !Default_GraphicalBarProperties      = $0C80|!addr  ;>SMB3 status bar properties, change to "$0C36|!base" for minimalist.
   else
    !Default_GraphicalBarProperties      = $0C80|!addr  ;>SMB3 status bar properties, change to "$0C36|!base" for minimalist.
   endif
  else
   if !sa1 == 0
    !Default_GraphicalBarProperties      = $7FA001  ;>Super status bar
   else
    !Default_GraphicalBarProperties      = $404001  ;>Same as above but SA-1
   endif
  endif
   ;^Tile properties (only applies to status bar patches that lets you change the properties in-game).
   ; Remember: bit format is [YXPCCCTT].
 ;This is when you are using a bar that would extend towards the left as the length increases.
 ;!Default_GraphicalBarPositionExtendLeftwards is the position of the rightmost last tile (even when x-flipped to fill leftwards). Therefore
 ;the status bar write range is [DesiredLastTilePos-((NumberOfTiles-1)*!StatusBarFormat)] to [DesiredLastTilePos].
  if !StatusBarFormat == $01
   if !sa1 == 0
    !Default_GraphicalBarPositionExtendLeftwards       = $7E0F09     ;>SMW's status bar. Replace with "$0C00|!addr" for SMB3 status bar and "$0BF6|!addr" for minimalist status bar.
   else
    !Default_GraphicalBarPositionExtendLeftwards       = $400F09     ;>SMW's status bar (SA-1). Replace with "$0C00|!addr" for SMB3 status bar and "$0BF6|!addr" for minimalist status bar.
   endif
    ;^Location of the bar's last tile (rightmost tile, even when set to fill leftwards) when !StatusBarFormat is $01. This is under use of
    ; STA [$00], that is why it needs to be 3-bytes.
  else
   if !sa1 == 0
    !Default_GraphicalBarPositionExtendLeftwards      = $7FA03E      ;>Status bar RAM data.
   else
    !Default_GraphicalBarPositionExtendLeftwards      = $40403E      ;>Status bar RAM data (SA-1).
   endif
    ;^Same as above, but for a format that each 8x8 tile contains 2 bytes next to each other.
  endif
  if !StatusBarFormat == $01
   if !sa1 == 0
    !Default_GraphicalBarPropertiesExtendLeftwards      = $0C80|!addr  ;>SMB3 status bar properties, change to "$0C36|!base" for minimalist.
   else
    !Default_GraphicalBarPropertiesExtendLeftwards      = $0C80|!addr  ;>SMB3 status bar properties, change to "$0C36|!base" for minimalist.
   endif
  else
   if !sa1 == 0
    !Default_GraphicalBarPropertiesExtendLeftwards      = $7FA03F  ;>Super status bar
   else
    !Default_GraphicalBarPropertiesExtendLeftwards      = $7FA03F  ;>Same as above but SA-1
   endif
  endif
  !Default_GraphicalBarPositionExtendLeftwards_MaxMiddleLength = 30
  ;^The maximum length of the middle part of the leftward extending bar.
  
;Tile settings (length does not apply to [ExtendLeftwards.asm] as that is variable in-game):
 !Default_MiddleLength                = 7             ;>30 = screen-wide (30 + 2 end tiles = 32, all 8x8 tile row in the screen's width)
 !Default_LeftPieces                  = 3             ;\These will by default, set the RAM for the pieces for each section
 !Default_MiddlePieces                = 8             ;|
 !Default_RightPieces                 = 3             ;/
 
 !GraphicalBar_TotalTileUsed          = 32
  ;^The maximum number of 8x8 tile bytes you are going
  ; to use (as in, if you have multiple bars with their own
  ; lengths, the one the longest plus any of the two
  ; existing end tiles is this number). This is only
  ; needed during a code handling a double-bar. 32
  ; tiles is the full width of the screen.
  ;
  ; This positions the secondary table containing "FirstFill"
  ; !Scratchram_GraphicalBar_FillByteTbl plus this define,
  ; resulting the tables to be like this (ASCII):
  ;  SecondFill                     FirstFill
  ;     |                               |
  ;     V                               V
  ; <=======>.......................<=======>
  ;
  ; < is left end.
  ; = is middle tile.
  ; > is right end.
  ; . is unused RAM that can be garbage or used when bar is extended.

 !Default_LeftwardsBar                           = 0
  ;^0 = Fill from left to right
  ; 1 = Fill from right to left
  ; Note that end tiles are also mirrored. This only works properly
  ; on any status bar patches that allow editing the tile properties.
  ; Having this set to 1 while using SMW's vanilla status bar causes
  ; each tiles to fill backwards (rightwards as fill increases) while
  ; advancing tiles to the left. If that is the case, flip the tiles
  ; in the file bin then or edit SMW's status bar table at address $008C81.

;Double bar. Only works with the super status bar patch.
 if !sa1 == 0
  !FirstFillHexValDisplayPos           = $7FA036
 else
  !FirstFillHexValDisplayPos           = $404036
 endif
  ;Position of a hex number display of the amount of firstfill.

 if !sa1 == 0
  !SecondFillHexValDisplayPos          = $7FA03C
 else
  !SecondFillHexValDisplayPos          = $40403C
 endif
  ;Same as above, but secondfill.
 ;Same as above, but this is the "percent" fill.
  if !sa1 == 0
   !FirstFillPercentHexValDisplayPos   = $7FA076
  else
   !FirstFillPercentHexValDisplayPos   = $404076
  endif
  if !sa1 == 0
   !SecondFillPercentHexValDisplayPos  = $7FA07C
  else
   !SecondFillPercentHexValDisplayPos  = $40407C
  endif

 ;RAM testing for how much fill in bar.
  !Freeram_FirstFill = $58
   ;^[1 byte] The amount of fill for the first fill

  !Freeram_SecondFill = $5C
   ;^[1 byte] The amount of fill for the second fill
   
 ;Display type
  !DoubleBar_DisplayType = 1
   ;0 = Use alternating frames (rapid flicker). Must use
   ;    the non-double bar graphic.
   ;1 = Use overlapping graphic (differently colored fill).
   ;    This must have LG1 and LG2 use the double bar graphic.
   
  !DoubleBar_RoundAway = 1
   ;0 = allow rounding towards empty and full
   ;1 = force to round towards either 1 pieces or maximum-1.
   
  !DoubleBar_MaxQuantity = $FF
   ;^The maximum value (max quantity) controlled by D-pad.


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