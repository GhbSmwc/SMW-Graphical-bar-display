;This is separate from the main define file "GraphicalBarDefines.asm" because of these reasons:
;-This is a preset setting, made for specific bars and not for all of them.
;-The main define file covers all stuff using the graphical bar routines.
;
;This also contains many defines for example code testing.
;
; NOTE: the overworld border plus (OWB+) and the super status bar (SSB) patch uses the same tile data
; format, so make sure you set !StatusBarFormat to $02 and !StatusBar_UsingCustomProperties to 1.
; I recommend using the SSB along with the OWB+ patch if you want to test both levels and overworld
; versions all at once.

;Status bar and overworld stuff
	!OWPlusAndSSBSameFormat              = 1
		; 0 = You are using a non-Super Status Bar and
		;     Overworld border plus patch (you are using different
		;     tile data formats).
		; 1 = You are using Super Status Bar and overworld
		;     border plus patch, which they both have the
		;     same tile bytes format (excludes the redundant
		;     routines that writes to the status bar).

	!StatusBarFormat                     = $02
		;^Number of grouped bytes per 8x8 tile for the status bar (not the overworld border):
		; $01 = each 8x8 tile have two bytes each separated into "tile numbers" and "tile properties" group;
		;       Minimalist/SMB3 [TTTTTTTT, TTTTTTTT]...[YXPCCCTT, YXPCCCTT] or SMW's default ([TTTTTTTT] only).
		; $02 = each 8x8 tile byte have two bytes located next to each other;
		;       Super status bar/Overworld border plus [TTTTTTTT YXPCCCTT, TTTTTTTT YXPCCCTT]...

	!StatusBar_UsingCustomProperties           = 0
		;^Set this to 0 if you are using the vanilla SMW status bar or any status bar patches
		; that doesn't enable editing the tile properties, otherwise set this to 1 (you may
		; have to edit "!Default_GraphicalBar_Pos_Properties" in order for it to work though.).
		; This define is needed to prevent writing what it assumes tile properties into invalid
		; RAM addresses.
		;
		; NOTE: This also applies to overworld border plus write as well (both writes to status
		; bar and overworld border plus share the same "tile writer" code). But thankfully most
		; status bar patches that are layer 3-based enables you to edit tile properties,
		; therefore its likely you are going to set this to 1.

	!Default_StatusBar_TilePropertiesSetting      = %00111000
		;^Tile properties (if you enable editing properties in-game). Note: Bit 6 (X-flip) is
		; forced to be set when !Default_LeftwardsBar is set to 1. If you want this to be x flipped,
		; set that aforementioned setting. (YXPCCCTT)
		;
		;This does not apply to color-changing bars, see [Level_Simple.asm] at the table at the bottom,
		;as this define only apply to graphical bars with static tile properties.

	!Default_Overworld_TilePropertiesSetting      = %00111001
		;^Same as above, but overworld map.

	;Tile positions. If you are using other status bar patches other than the Super Status Bar
	;patch, make sure the RAMs here matches the RAM address those patch are using. By default:
	;
	; $0F09|!addr overwrites the word "TIME".
	; $7FA000 and $404000 takes the top-left corner when using the super status bar patch.
	;
	;These must be 3-bytes long (6 hexadecimal digits long), as they are under a routine that
	;uses STA [$00],y to write the tiles to the status bar.
	;
	;Additional notes:
	;
	;Minimalist status bar patch RAM address location:
	; -status_double.asm:
	; --$0B05-$0B24: Tile number top row
	; --$0B25-$0B44: Tile number bottom row
	; --$0B45-$0B64: Tile properties top row
	; --$0B65-$0B84: Tile properties bottom row
	; -status_top.asm and status_bottom.asm:
	; --$0B05-$0B24: Tile number row
	; --$0B45-$0B64: Tile properties row
	; -^To calculate position: TileLocation = StartingAddress + X (x ranges from 0-31)
	;
	;Positions I recommend, assuming you don't make other changes on the defines and positions:
	;Super status bar patch:
	;-$7FA000/$7FA001 (extend rightwards) and $7FA03E/$7fA03F (extend leftwards): Top row
	; is entirely not used by any info display.
	;Minimalist status bar (using status_double.asm):
	;-$0B2D/$0B6D (extend rightwards) and $0B3C/$0B7C (extend leftwards): The "middle" space
	; between the coin display and the score. When placed in a define, make sure you
	; add "|!addr" for sa-1.
	;Note that these information may be out of date should the patches be updated with the
	;formats and RAM address be changed.

	;This covers all bars that would extend rightwards as you increase the length.
		if !StatusBarFormat == $01
			if !sa1 == 0
				!Default_GraphicalBar_Pos_Tile                      = $7E0F09     ;>SMW's status bar.
			else
				!Default_GraphicalBar_Pos_Tile                      = $400F09     ;>SMW's status bar.
			endif
			;^Location of the bar when !StatusBarFormat is $01. This is under use of
			; STA [$00], that is why it needs to be 3-bytes.
		else
			if !sa1 == 0
				!Default_GraphicalBar_Pos_Tile                     = $7FA000      ;>Status bar RAM data.
			else
				!Default_GraphicalBar_Pos_Tile                     = $404000      ;>Status bar RAM data (SA-1).
			endif
			;^Same as above, but for a format that each 8x8 tile contains 2 bytes next to each other.
		endif
		if !StatusBarFormat == $01
			if !sa1 == 0
				!Default_GraphicalBar_Pos_Properties      = $0C80|!addr  ;>SMB3 status bar properties.
			else
				!Default_GraphicalBar_Pos_Properties      = $0C80|!addr  ;>SMB3 status bar properties.
			endif
		else
			if !sa1 == 0
				!Default_GraphicalBar_Pos_Properties      = $7FA001  ;>Super status bar
			else
				!Default_GraphicalBar_Pos_Properties      = $404001  ;>Same as above but SA-1
			endif
		endif
			;^Tile properties (only applies to status bar patches that lets you change the properties in-game).
			; Remember: bit format is [YXPCCCTT].
	;This is when you are using a bar that would extend towards the left as the length increases.
	;!Default_GraphicalBar_Pos_Tile_ExtendLeftwards is the position of the rightmost last tile (even when x-flipped to fill leftwards). Therefore
	;the status bar write range is [DesiredLastTilePos-((NumberOfTiles-1)*!StatusBarFormat)] to [DesiredLastTilePos].
		if !StatusBarFormat == $01
			if !sa1 == 0
				!Default_GraphicalBar_Pos_Tile_ExtendLeftwards       = $7E0F09     ;>SMW's status bar. Replace with "$0C00|!addr" for SMB3 status bar and "$0BF6|!addr" for minimalist status bar.
			else
				!Default_GraphicalBar_Pos_Tile_ExtendLeftwards       = $400F09     ;>SMW's status bar (SA-1). Replace with "$0C00|!addr" for SMB3 status bar and "$0BF6|!addr" for minimalist status bar.
			endif
				;^Location of the bar's last tile (rightmost tile, even when set to fill leftwards) when !StatusBarFormat is $01. This is under use of
				; STA [$00], that is why it needs to be 3-bytes.
		else
			if !sa1 == 0
				!Default_GraphicalBar_Pos_Tile_ExtendLeftwards      = $7FA03E      ;>Status bar RAM data.
			else
				!Default_GraphicalBar_Pos_Tile_ExtendLeftwards      = $40403E      ;>Status bar RAM data (SA-1).
			endif
				;^Same as above, but for a format that each 8x8 tile contains 2 bytes next to each other.
		endif
		if !StatusBarFormat == $01
			if !sa1 == 0
				!Default_GraphicalBar_Pos_Properties_ExtendLeftwards      = $0C80|!addr  ;>SMB3 status bar properties, change to "$0C36|!base" for minimalist.
			else
				!Default_GraphicalBar_Pos_Properties_ExtendLeftwards      = $0C80|!addr  ;>SMB3 status bar properties, change to "$0C36|!base" for minimalist.
			endif
		else
			if !sa1 == 0
				!Default_GraphicalBar_Pos_Properties_ExtendLeftwards      = $7FA03F  ;>Super status bar
			else
				!Default_GraphicalBar_Pos_Properties_ExtendLeftwards      = $7FA03F  ;>Same as above but SA-1
			endif
		endif
		!Default_GraphicalBar_Pos_Tile_ExtendLeftwards_MaxMiddleLength = 30
			;^The maximum length of the middle part of the leftward extending bar.
	;Overworld graphical bar:
		if !sa1 == 0
			!Default_GraphicalBar_Pos_Tile_OverworldMap = $7FEC00
		else
			!Default_GraphicalBar_Pos_Tile_OverworldMap = $41EC00
		endif
		if !sa1 == 0
			!Default_GraphicalBar_Pos_Properties_OverworldMap = $7FEC01
		else
			!Default_GraphicalBar_Pos_Properties_OverworldMap = $41EC01
		endif

;Tile settings (length does not apply to [ExtendLeftwards.asm] as that is variable in-game):
	!Default_MiddleLength                = 7             ;>30 = screen-wide (30 + 2 end tiles = 32, all 8x8 tile row in the screen's width)
	!Default_LeftPieces                  = 3             ;\These will by default, set the RAM for the pieces for each section
	!Default_MiddlePieces                = 8             ;|
	!Default_RightPieces                 = 3             ;/
		;^Don't get confused with GraphicalBarDefines.asm's Bar attributes. These are the ACTUAL number of pieces
		; that the RAM address are set to contain these values. Therefore, these are what the RAM address are set
		; by default.

	!Default_LeftwardsBar                           = 0
		;^0 = Fill from left to right (default)
		; 1 = Fill from right to left (will always SET bit X in YXPCCCTT).
		; Note that end tiles are also mirrored. This only works properly
		; on any status bar patches that allow editing the tile properties.
		; Having this set to 1 while using SMW's vanilla status bar causes
		; each tiles to fill backwards (rightwards as fill increases) while
		; advancing tiles to the left. If that is the case, flip the tiles
		; in the file bin then or edit SMW's status bar table at address $008C81.
		;
		; Note: Make sure the bar graphic tiles are fill left-to-right
		; BY DEFAULT (in the .bin files).

;Double bar. Only works with the super status bar patch.
	!Setting_DoubleBar_FillMode = 0
		;^0 = manually control how much fill for both bars.
		; 1-255 = Control only firstfill with secondfill gradually follows
		;  the firstfill, this also acts as how much delay (in frames) before
		;  secondfill increments/decrements towards firstfill.

	if !sa1 == 0
		!FirstFillHexValDisplayPos           = $7FA036
	else
		!FirstFillHexValDisplayPos           = $404036
	endif
		;^Position of a hex number display of the amount of firstfill.

	if !sa1 == 0
		!SecondFillHexValDisplayPos          = $7FA03C
	else
		!SecondFillHexValDisplayPos          = $40403C
	endif
		;^Same as above, but secondfill.
	;Same as above, but this is the "percent" fill:
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
			;^NOTE: these only display how much of the 2 fills is in the
			; bar. Meaning with !DoubleBar_DisplayIncrease set to 1,
			; when FirstQuantity is greater than SecondQuantity (when the
			; value increases), this happens:
			;
			;  SecondFill ("transparent fill") = "Current value"
			;  FirstFill ("Opaque fill") = "Previous value"
			;
			; Therefore, this is the inverse when the fill amount decreases:
			;
			;  SecondFill ("transparent fill") = "Previous value"
			;  FirstFill ("Opaque fill") = "Current value"
			;
	;Display type
		!DoubleBar_DisplayType = 1
			;^0 = Use alternating frames (rapid flicker). Must use
			;     the non-double bar graphic.
			; 1 = Use overlapping graphic (Using separate graphics).
			;     This must have LG1 and LG2 use the double bar graphic.

		!DoubleBar_RoundAway = 1
			;^0 = allow rounding towards empty and full (applies to both fills)
			; 1 = force to round towards either 1 pieces or maximum-1.
			
		!DoubleBar_DisplayIncrease = 1
			;^0 = only display the SecondFill when the bar decreases
			; 1 = display an increase as well (ideal for when HP is recovered)
	;Max amount
		!DoubleBar_MaxQuantity = $FF
			;^The maximum value (max quantity) controlled by D-pad.
	
	;RAM testing for how much fill in bar for a double bar.
		!Freeram_FirstQuantity = $58
			;^[1 byte] The amount of fill for the first fill
	
		!Freeram_SecondQuantity = $5C
			;^[1 byte] The amount of fill for the second fill.
			; Note:
			; -When Level_DoubleBar.asm is used, this acts as a literal quantity.
			; -When Level_DoubleBar2.asm is used, this acts as the percentage
			; instead (or amount filled) rather than an actual second quantity.
			
	
		!Freeram_SecondQuantityDelay = $79
			;^[1 byte or not used at all] The amount of delay
			; (in frames) the secondfill remains static
			; before incrementing/decrementing itself to firstfill. Not used
			; if !Setting_DoubleBar_FillMode is 0.

;RAM testing for Range-based bar:
	!Freeram_RangeBasedValue = $0DDB|!addr
		;^[2 bytes] The value to test what range it is in (quantity).
	!Scratchram_WhatRange = $14B0|!addr
		;^[2 bytes] Range number of what interval the quantity is in.
	if !sa1 == 0
		!Interval_Write_Pos_Tile = $7FA014
	else
		!Interval_Write_Pos_Tile = $404014
	endif
		;^Where to write a single-digit number on the status bar indicating what
		; range the quantity is in.
	if !sa1 == 0
		!Interval_Write_Pos_Properties = $7FA015
	else
		!Interval_Write_Pos_Properties = $404015
	endif
		;^Same as above but tile properties.


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
;  !Setting_GraphicalBar_SecondFillByteTableOffset = !GraphiBar_LeftTileExist+(!GraphiBar_MiddleTileExist*!Default_MiddleLength)+!GraphiBar_RightTileExist
;   ;^The amount of bytes the table used up.