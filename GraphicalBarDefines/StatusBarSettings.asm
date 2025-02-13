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

	!StatusBar_UsingCustomProperties           = 1
		;^Set this to 0 if you are using the vanilla SMW status bar or any status bar patches
		; that doesn't enable editing the tile properties, otherwise set this to 1 (you may
		; have to edit "!Default_GraphicalBar_Pos_Properties" in order for it to work though.).
		; This define is needed to prevent writing what it assumes tile properties into invalid
		; RAM addresses.
		;
		; Notes:
		; -This also applies to overworld border plus write as well (both writes to status
		;  bar and overworld border plus share the same "tile writer" code). But thankfully most
		;  status bar patches that are layer 3-based enables you to edit tile properties,
		;  therefore its likely you are going to set this to 1.
		; -When using the VRAM upload stripe image, this MUST be set to 1 because their tile
		;  properties may not be initalized and the bar could be garbage tiles or crash the game.

	!Default_StatusBar_TilePropertiesSetting      = %00111000
		;^Tile properties (if you enable editing properties in-game). Note: Bit 6 (X-flip) is
		; forced to be set when !Default_LeftwardsBar is set to 1. If you want this to be x flipped
		; on a left-to-right bar, set that here. (YXPCCCTT)
		;
		; For vertical bars on layer 3, a downward bars will be forced a Y-flip (bit 7).
		;
		;This does not apply to color-changing bars, see [Level_Simple.asm] at the table at the bottom,
		;as this define only apply to graphical bars with static tile properties.

	!Default_Overworld_TilePropertiesSetting      = %00111001
		;^Same as above, but overworld map.
		
	!PaletteChanging = 0
		;^0 = Use whatever colors specified by !Default_StatusBar_TilePropertiesSetting constantly.
		; 1 = override the colors (only modify the CCC bits in YXPCCCTT). This makes the bar
		;     bar change colors based on how much fill. To edit the threshold of at what values
		;     to change colors, open the ASM files (mentioned on next sentence) and find
		;     "PaletteThresholds" and "PaletteTable". Only supports "Level_Simple.asm",
		;     "Level_Simple_UsingStripe.asm", and "Level_Simple_VerticalBar_NonStripe".
	!UsingCustomStatusBar = 1
		;^0 = Using vanilla SMW status bar
		; 1 = Using any layer 3 custom status bar.
		; These are needed for determining what coordinate system.
	;Tile positions. If you are using other status bar patches other than the Super Status Bar
	;patch, make sure the RAMs here matches the RAM address those patch are using. By default:
	;
	; $0F09|!addr overwrites the word "TIME".
	; $7FA000 and $404000 takes the top-left corner when using the super status bar patch.
	;
	;These must be 3-bytes long (6 hexadecimal digits long), as they are under a routine that
	;uses STA [$00],y to write the tiles to the status bar.
	;
	;Defines involving XY positions refer to their positions in units of 8x8 tiles:
	; - X=0 being on the left edge (of the screen), increasing when moving to the right.,
	;   with X=31 on the right edge of the screen that is visible.
	; - Y=0 being at the top (of the screen), increasing when moving downwards.
	; As always, numbers without a prefix are decimal, a dollar sign prefix are hexadecimal.
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
	;Levels
		;Base address of status bar patches
		; - SMW's status bar :
		; -- $7E0F09 (TTTTTTTT, TTTTTTTT...) - https://smwc.me/m/smw/ram/7E0EF9
		; - Super status bar (TTTTTTTT, YXPCCCTT):
		; -- $7FA000/$404000
		; - Minimalist status bars:
		; -- $0B05 (TTTTTTTT, TTTTTTTT...)
		; -- $0B45 (YXPCCCTT, YXPCCCTT...)
		; - SMB3 status bar:
		; -- $7FB600 (TTTTTTTT)
		; -- $7FB680 (YXPCCCTT)
		
		;RAM address of the first TTTTTTTT byte.
			if !sa1 == 0
				!FreeramFromAnotherPatch_StatusBarTileStart = $7FA000
			else
				!FreeramFromAnotherPatch_StatusBarTileStart = $404000
			endif
		;RAM address of the first YXPCCCTT byte.
			if !sa1 == 0
				!FreeramFromAnotherPatch_StatusBarPropStart = $7FA001
			else
				!FreeramFromAnotherPatch_StatusBarPropStart = $404001
			endif
		;This is the maximum length if you have a bar that has variable length (including leftwards). This prevents a bar long enough
		;that it would write tiles outside the status bar data and corrupt other data.
			!Default_GraphicalBar_MaxMiddleLength = 30
		;NOTE: following does not include static end tiles, see "Tile positions for static end tiles"
				!Default_GraphicalBar_PosX_Tile = 0
				!Default_GraphicalBar_PosY_Tile = 0
		;This is when you are using a bar that would extend towards the left as the length increases.
		;The position entered here is the position of the rightmost last tile (even when x-flipped to fill leftwards). Therefore
		;the status bar write range is [DesiredLastTilePos-(NumberOfTiles-1)] to [DesiredLastTilePos]. Again, this does not
		;include static end tiles bar.
			!Default_GraphicalBar_PosX_ExtendLeftwards = 31
			!Default_GraphicalBar_PosY_ExtendLeftwards = 0
		;For vertical bars (only supports super status bars and SMB3 status bar at the time of making this,
		;since they work with a system that each row is 32-width tiles).

			!Default_VerticalBarDirection = $00 ;>Only use these values: $00 = fill and extend upwards, $02 = fill or extend downwards.
				;^Note: This makes "Level_Simple_VerticalBar_NonStripe.asm" set the Y-flip bit when set to downwards.
				; For "Level_Simple_UsingStripe.asm", use "!Default_StripeVerticalDownwardsBar" instead.
				
			;Upwards (this is where the bottom part of the bar would be at)
				!Default_GraphicalBar_PosX_VerticalUpwards = 0
				!Default_GraphicalBar_PosY_VerticalUpwards = 4
			;Downwards (this is where the top part of the bar would be at)
				!Default_GraphicalBar_PosX_VerticalDownwards = 0
				!Default_GraphicalBar_PosY_VerticalDownwards = 0
		;Layer 3 graphical bar with static end tiles that extends left or right
			;0 to not write static end tiles, otherwise set to 1 (don't set to any other number, it does addition values on it).
				!GraphicalBarExampleTest_StaticLeft = 1
				!GraphicalBarExampleTest_StaticRight = 1
			;other settings:
				!GraphicalBarExampleTest_StaticEnd_ExtendLeft = 0
					;^0 = Extend rightwards, 1 = extend leftwards, from a given position.
			;Some tile number, properties, and positioning settings:
				;Static left tile of bar
					!GraphicalBarExampleTest_LeftSideTileNum = $29
					!GraphicalBarExampleTest_LeftSideTileProps = %00111000
				;Static right tile of bar
					!GraphicalBarExampleTest_RightSideTileNum = $29
					!GraphicalBarExampleTest_RightSideTileProps = %01111000
			;Tile positions for static end tiles
				;Extend Rightwards
					!GraphicalBarExampleTest_PosX_StaticEnds_ExtendRightBar = 1
					!GraphicalBarExampleTest_PosY_StaticEnds_ExtendRightBar = 0
				;Extend leftwards
					!GraphicalBarExampleTest_PosX_StaticEnds_ExtendLeftBar = 30
					!GraphicalBarExampleTest_PosY_StaticEnds_ExtendLeftBar = 0

	;Layer 3 stripe
		;X and Y position (coordinates system works the same way as the status bar).
			!Default_StripeImage_XPos = 0			;>X position of 31 ($1F) is the rightmost tile that can be seen.
			!Default_StripeImage_YPos = 27			;>Y position of 27 ($1B) is the bottommost tile that can be seen
		;Direction. 0 = horizontal, 1 = vertical
			!Default_StripeImage_Direction = 0
		;For filling directions, see !Default_LeftwardsBar and !Default_StripeVerticalDownwardsBar Extend leftwards or upwards.
		;When set, !Default_StripeImage_XPos and !Default_StripeImage_YPos will be the bottom or right part of the bar
		;and extends leftwards or upwards. When !Default_StripeImage_Direction is 0, it would be leftwards, otherwise upwards.
			!ExtendLeftOrUpwards = 0
				;^0 = extend rightwards or downwards.
				; 1 = extend leftwards or upwards.
		;Fill Direction
			!Default_StripeVerticalDownwardsBar                            = 0
				;^0 = Fill from bottom to top
				; 1 = Fill from top to bottom (will always SET bit Y in YXPCCCTT)
				; This only applies to layer 3 vertical bars written onto the stripe
				; image. For on-status-bar or on-overworld border, it is
				; "!Default_VerticalBarDirection" on this define ASM file.

	;Overworld graphical bar:
		;Starting RAM address of OWB+'s tile number
			if !sa1 == 0
				!FreeramFromAnotherPatch_OWBorderTileStart = $7FEC00
			else
				!FreeramFromAnotherPatch_OWBorderTileStart = $41EC00
			endif
		;Starting RAM address of OWB+'s tile properties
			if !sa1 == 0
				!FreeramFromAnotherPatch_OWBorderPropStart = $7FEC01
			else
				!FreeramFromAnotherPatch_OWBorderPropStart = $41EC01
			endif
		; Again about [DisiredTile = StatusBarStartAddress + Offset]. But this time, at the time of writing this,
		; only OWB+ exists, and that uses [TTTTTTTT, YXPCCCTT] the same way as the SSB patch does, which also means
		; Offset's hex number must end in 0, 2, 4, 6, 8, A, C, E, or F. Also that you take the offset information
		; from the HTML file and subtract 1 when dealing with properties ("$7FEC01 (!TileRAM+$1)" is
		; !FreeramFromAnotherPatch_OWBorderPropStart+$00, not +$01).
		!Default_GraphicalBar_Pos_Tile_OverworldMap = !FreeramFromAnotherPatch_OWBorderTileStart+$00
		!Default_GraphicalBar_Pos_Properties_OverworldMap = !FreeramFromAnotherPatch_OWBorderPropStart+$00

;Tile settings, for all bars (length does not apply to [ExtendLeftwards.asm] as that is variable in-game):
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

;Double bar. Only works with the super status bar patch (well, at least the hex number display).
	!Setting_DoubleBar_FillMode = 0
		;^0 = manually control how much fill for both bars.
		; 1-255 = Control only firstfill with secondfill gradually follows
		;  the firstfill, this also acts as how much delay (in frames) before
		;  secondfill increments/decrements towards firstfill.
	;Number display for debugging
		!FirstFillHexValDisplayPos = !FreeramFromAnotherPatch_StatusBarTileStart+$36
			;^Position of a hex number display of the amount of firstfill.
		!SecondFillHexValDisplayPos = !FreeramFromAnotherPatch_StatusBarTileStart+$3C
			;^Same as above, but secondfill.
		;Same as above, but this is the "percent" fill:
			!FirstFillPercentHexValDisplayPos = !FreeramFromAnotherPatch_StatusBarTileStart+$76
			!SecondFillPercentHexValDisplayPos = !FreeramFromAnotherPatch_StatusBarTileStart+$7C
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
;Settings for Range-based bar ("Level_RangeBased.asm"):
	!Interval_Write_Pos_Tile = !FreeramFromAnotherPatch_StatusBarTileStart+$14
		;^Where to write a single-digit number on the status bar indicating what
		; range the quantity is in.
	!Interval_Write_Pos_Properties = !FreeramFromAnotherPatch_StatusBarPropStart+$14
		;^Same as above but tile properties.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Don't touch these below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;Patched status bar
		function PatchedStatusBarXYToAddress(x, y, StatusBarTileDataBaseAddr, format) = StatusBarTileDataBaseAddr+(x*format)+(y*32*format)
		;You don't have to do STA $7FA000+StatusBarXYToByteOffset(0, 0, $02) when you can do STA PatchedStatusBarXYToAddress(0, 0, $7FA000, $02)
		
		macro CheckValidPatchedStatusBarPos(x,y)
			assert and(greaterequal(<x>, 0), lessequal(<x>, 31)), "Invalid position on the patched status bar"
		endmacro
	
	;Vanilla SMW status bar
		function VanillaStatusBarXYToAddress(x,y, SMWStatusBar0EF9) = (select(equal(y,2), SMWStatusBar0EF9+(x-2), SMWStatusBar0EF9+$1C+(x-3)))
		
		macro CheckValidVanillaStatusBarPos(x,y)
			assert or(and(equal(<y>, 2), and(greaterequal(<x>, 2), lessequal(<x>, 29))), and(equal(<y>, 3), and(greaterequal(<x>, 3), lessequal(<x>, 29)))), "Invalid position on the vanilla status bar"
		endmacro
		
		if !sa1 == 0
			!RAM_0EF9 = $0EF9
		else
			!RAM_0EF9 = $400EF9
		endif
	;Convert XY position to address
		!Default_GraphicalBar_Pos_Tile = VanillaStatusBarXYToAddress(!Default_GraphicalBar_PosX_Tile, !Default_GraphicalBar_PosY_Tile, !RAM_0EF9)
		!Default_GraphicalBar_Pos_Tile_ExtendLeftwards = VanillaStatusBarXYToAddress(!Default_GraphicalBar_PosX_ExtendLeftwards, !Default_GraphicalBar_PosY_ExtendLeftwards, !RAM_0EF9)
		!GraphicalBarExampleTest_ExtendRightBarPos = VanillaStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendRightBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendRightBar, !RAM_0EF9)
		!GraphicalBarExampleTest_ExtendLeftBarPos = VanillaStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendLeftBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendLeftBar, !RAM_0EF9)
		
		if !UsingCustomStatusBar != 0
			%CheckValidPatchedStatusBarPos(!Default_GraphicalBar_PosX_Tile, !Default_GraphicalBar_PosY_Tile)
			%CheckValidPatchedStatusBarPos(!Default_GraphicalBar_PosX_ExtendLeftwards, !Default_GraphicalBar_PosY_ExtendLeftwards)
			%CheckValidPatchedStatusBarPos(!Default_GraphicalBar_PosX_VerticalUpwards, !Default_GraphicalBar_PosY_VerticalUpwards)
			%CheckValidPatchedStatusBarPos(!Default_GraphicalBar_PosX_VerticalDownwards, !Default_GraphicalBar_PosY_VerticalDownwards)
			%CheckValidPatchedStatusBarPos(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendRightBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendRightBar)
			%CheckValidPatchedStatusBarPos(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendLeftBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendLeftBar)
		
			!Default_GraphicalBar_Pos_Tile = PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_Tile, !Default_GraphicalBar_PosY_Tile, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!Default_GraphicalBar_Pos_Properties = PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_Tile, !Default_GraphicalBar_PosY_Tile, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)

			!Default_GraphicalBar_Pos_Tile_ExtendLeftwards = PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_ExtendLeftwards, !Default_GraphicalBar_PosY_ExtendLeftwards, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!Default_GraphicalBar_Pos_Properties_ExtendLeftwards = PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_ExtendLeftwards, !Default_GraphicalBar_PosY_ExtendLeftwards, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)

			!Default_GraphicalBar_Pos_Tile_VerticalUpwards = PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_VerticalUpwards, !Default_GraphicalBar_PosY_VerticalUpwards, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!Default_GraphicalBar_Pos_Properties_VerticalUpwards = PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_VerticalUpwards, !Default_GraphicalBar_PosY_VerticalUpwards, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)
			
			!Default_GraphicalBar_Pos_Tile_VerticalDownwards = PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_VerticalDownwards, !Default_GraphicalBar_PosY_VerticalDownwards, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!Default_GraphicalBar_Pos_Properties_VerticalDownwards = PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_VerticalDownwards, !Default_GraphicalBar_PosY_VerticalDownwards, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)
			
			!GraphicalBarExampleTest_StaticEndsExtendRightBarPos = PatchedStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendRightBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendRightBar, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!GraphicalBarExampleTest_StaticEndsExtendRightBarPropsPos = PatchedStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendRightBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendRightBar, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)
			
			!GraphicalBarExampleTest_StaticEndsExtendLeftBarPos = PatchedStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendLeftBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendLeftBar, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!GraphicalBarExampleTest_StaticEndsExtendLeftBarPropsPos = PatchedStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendLeftBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendLeftBar, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)
		endif