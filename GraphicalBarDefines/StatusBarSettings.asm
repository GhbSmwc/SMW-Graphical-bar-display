includeonce
 ;^This prevents redefinition errors when using "Patch_DrawSpriteGraphicalBar/DrawOamPatch.asm" because
 ; the text file you are reading right now uses a function, and the patch includes this file multiple
 ; times, including when incsrc another ASM file which that also include this.

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
		;^Set this to 0 if you are only using vanilla status bar, status bar patches, and/or overworld border
		; plus patch that have tile properties automatically set proper. Otherwise set this to 1 (if you need
		; to set the tile properties). 
		;
		; This define is needed as there are subroutines that either write only to tile numbers, or both tile
		; numbers and properties.
		;
		; Notes:
		; - This also applies to overworld border plus write as well (both writes to status
		;   bar and overworld border plus share the same "tile writer" code). But thankfully most
		;   status bar patches that are layer 3-based enables you to edit tile properties,
		;   therefore its likely you are going to set this to 1.
		; - When using the VRAM upload stripe image, this MUST be set to 1 because their tile
		;   properties are not be initialized and the bar could be garbage tiles or crash the game.

	;YXPCCCTT settings (split into individual parts). Does not apply if you have !StatusBar_UsingCustomProperties == 0.
	;See "GetLayer3YXPCCCTT" function on how it converts multiple numbers into a single property byte.
		!Default_StatusBar_TileProperties_Page        = 0 ;>Valid values: 0-3.
		!Default_StatusBar_TileProperties_Palette     = 6 ;>Valid values: 0-7. This does not apply to color-changing bars.
		!Default_StatusBar_TileProperties_Priority    = 1 ;>Valid values: 0-1. (you most likely always have this set to 1).
		!Default_StatusBar_TileProperties_XFlip       = 0 ;>Valid values: 0-1. NOTE: this is ignored (forcibly set) if !Default_LeftwardsBar == 1. If you want a left-to-right bar, set this to 1.
		!Default_StatusBar_TileProperties_YFlip       = 0 ;>Valid values: 0-1. NOTE: this is ignored for vertical downwards bar (forcibly set).
	;Same as above, including the XY flip override.
		!Default_Overworld_TileProperties_Page        = 1 ;>Valid values: 0-3.
		!Default_Overworld_TileProperties_Palette     = 6 ;>Valid values: 0-7.
		!Default_Overworld_TileProperties_Priority    = 1 ;>Valid values: 0-1. (you most likely always have this set to 1).
		!Default_Overworld_TileProperties_XFlip       = 0 ;>Valid values: 0-1. NOTE: this is ignored (forcibly set) if !Default_LeftwardsBar == 1. If you want a left-to-right bar, set this to 1.
		!Default_Overworld_TileProperties_YFlip       = 0 ;>Valid values: 0-1. NOTE: this is ignored for vertical downwards bar (forcibly set).
		
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
		; These are needed for determining what coordinate system as well as checking valid positionings.
		
	;Tile positions. If you are using other status bar patches other than the Super Status Bar
	;patch, make sure the RAMs here matches the RAM address those patch are using.
	;
	;Defines involving XY positions refer to their positions in units of 8x8 tiles (obviously
	;they must be integers):
	; - X=0 being on the left edge (of the area; such as the screen), increasing when
	;   moving to the right, with X=31 ($1F) on the right edge of the zone that is visible.
	; - Y=0 being at the top (of the area or screen), increasing when moving downwards.
	; As always, numbers without a prefix are decimal, a dollar sign prefix are hexadecimal.
	;
	; The range of positions that are valid depends on what type of status bar you're using:
	; - Vanilla SMW: Y can only be 2-3. And...
	; -- When Y=2, X ranges 2-29.
	; -- When Y=3, X ranges 3-29.
	; - Super status bar patch: X:0-31, Y:0-4.
	; - SMB3 status bar: X:0-31, Y:0-3
	; - Minimalist status bar top OR bottom: X:0-31. Y is *ALWAYS* 0.
	; - Minimalist status bar double: X:0-31, Y:0-1. Y=0 for top row, Y=1 for bottom row.
	;
	; Entering a position outside of the valid range may result in using a RAM address that
	; is outside the status bar tile data (which may cause glitches or crash your game).
	; This may also occur even if you use a valid position, for display elements made up of
	; multiple tiles and those tiles extend beyond the first or last byte of the tile data
	; (such as a 2-digit counter placed so the 10s digit of the counter is on the bottom-
	; rightmost of the editable tile area, causing the 1s place to be written outside the
	; tile data range).
	;
	; This define ASM file does have an assert failsafe against invalid XY positions, but
	; it is not entirely foolproof due to each status bar types having different tile range
	; and display elements spans many number of tiles to be written down.
	; 
	;
	; I use the term "area" because it is not necessarily the screen, rather a position on
	; the layer, and the fact that the SMB3 status bar's 0,0 position is the top-left of
	; the section of the bottom of the screen.
	;
	;Additional notes (this may be outdated):
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
	;
	;Levels
		;Base address of status bar patches. Note: Must be a 24-bit (3-byte) address
		;due to using "direct indirect long" ([$xx]) addressing.
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
				;Static left tile of bar (note that priority is shared with !Default_StatusBar_TileProperties_Priority.
					!GraphicalBarExampleTest_LeftSideTileNum = $29
					!GraphicalBarExampleTest_LeftSideTile_TileProperties_Page    = 0 ;>Valid values: 0-3.
					!GraphicalBarExampleTest_LeftSideTile_TileProperties_Palette = 6 ;>Valid values: 0-7.
					!GraphicalBarExampleTest_LeftSideTile_TileProperties_XFlip   = 0 ;>Valid values: 0-1.
					!GraphicalBarExampleTest_LeftSideTile_TileProperties_YFlip   = 0 ;>Valid values: 0-1.
				;Static right tile of bar
					!GraphicalBarExampleTest_RightSideTileNum = $29
					!GraphicalBarExampleTest_RightSideTile_TileProperties_Page    = 0 ;>Valid values: 0-3.
					!GraphicalBarExampleTest_RightSideTile_TileProperties_Palette = 6 ;>Valid values: 0-7.
					!GraphicalBarExampleTest_RightSideTile_TileProperties_XFlip   = 1 ;>Valid values: 0-1.
					!GraphicalBarExampleTest_RightSideTile_TileProperties_YFlip   = 0 ;>Valid values: 0-1.
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

	;Overworld graphical bar (this assumes you are using the overworld border plus patch):
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
		;Position. Works similarly to the status bar, but the Y position "skips" the intermediate rows of tiles
		;between the top and bottom. This means that going downwards on the last row of "top lines" will immediately
		;end up being on "bottom lines" on the first row. For example, with !Top_Lines set to 5 rows (Y ranges from 0-4),
		;going from Y=4 to Y=5 would now be at the first row of the bottom lines (which the true Y position would be Y=26).
		;
		;You can convert TrueYPosition (this counts all rows of the layer 3, and must be 26-27) into EditableYPosition
		;(numbering only rows the OWB+ can edit) when using the bottom lines:
		;
		; EditableYPosition = TrueYPosition - 26 + !Top_Lines
		;
		;For example (having !Top_Lines set to 5), I want a counter on the top row of bottom lines. I can literally just do this:
		;
		;!Default_GraphicalBar_PosY_OverworldMap = 26-26+5, which is row 5 (rows 0-4 are top lines, 5-6 are bottom lines)
		;
		;Conversion is not needed if you are having your stuff on the top-lines.
			!Default_GraphicalBar_PosX_OverworldMap = 0
			!Default_GraphicalBar_PosY_OverworldMap = 0

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
		;Position of a hex number display of the amount of firstfill.
			!FirstFillHexValDisplay_PosX = 27
			!FirstFillHexValDisplay_PosY = 0
			
		;Position of a hex number display of the amount of secondfill.
			!SecondFillHexValDisplay_PosX = 30
			!SecondFillHexValDisplay_PosY = 0
			
		; NOTE: these below only display how much of the 2 fills is in the
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
		;Same as above, but this is the "percent" first fill:
			!FirstFillPercentHexValDisplay_PosX = 27
			!FirstFillPercentHexValDisplay_PosY = 1
		;Percent second fill:
			!SecondFillPercentHexValDisplay_PosX = 30
			!SecondFillPercentHexValDisplay_PosY = 1
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
	;Position of a single-digit number representing how many bars is it on.
		!Interval_Write_PosX = 10
		!Interval_Write_PosY = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Don't touch these below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	if not(defined("FunctionGuard_StatusBarFunctionDefined"))
		;^This if statement prevents an issue where "includeonce" is "ignored" if two ASMs files
		; incsrcs to the same ASM file with a different path due to asar not being able to tell
		; if the incsrc'ed file is the same file: https://github.com/RPGHacker/asar/issues/287
		
		;Patched status bar. Feel free to use this.
			function PatchedStatusBarXYToAddress(x, y, StatusBarTileDataBaseAddr, format) = StatusBarTileDataBaseAddr+(x*format)+(y*32*format)
			;You don't have to do STA $7FA000+StatusBarXYToByteOffset(0, 0, $02) when you can do STA PatchedStatusBarXYToAddress(0, 0, $7FA000, $02)
			
			macro CheckValidPatchedStatusBarPos(x,y)
				assert and(greaterequal(<x>, 0), lessequal(<x>, 31)), "Invalid position on the patched status bar"
			endmacro
		
		;Vanilla SMW status bar. Again, feel free to use this.
			function VanillaStatusBarXYToAddress(x,y, SMWStatusBar0EF9) = (select(equal(y,2), SMWStatusBar0EF9+(x-2), SMWStatusBar0EF9+$1C+(x-3)))
			
			macro CheckValidVanillaStatusBarPos(x,y)
				assert or(and(equal(<y>, 2), and(greaterequal(<x>, 2), lessequal(<x>, 29))), and(equal(<y>, 3), and(greaterequal(<x>, 3), lessequal(<x>, 29)))), "Invalid position on the vanilla status bar"
			endmacro
			
			if !sa1 == 0
				!RAM_0EF9 = $0EF9
			else
				!RAM_0EF9 = $400EF9
			endif
		;Get YXPCCCTT
			function GetLayer3YXPCCCTT(Y,X,P,CCC,TT) = ((Y<<7)+(X<<6)+(P<<5)+(CCC<<2)+TT)
		;Mark that the macros and functions are now defined
			!FunctionGuard_StatusBarFunctionDefined = 1
	endif
	;Obtain tile properties
		!Default_StatusBar_TilePropertiesSetting #= GetLayer3YXPCCCTT(!Default_StatusBar_TileProperties_YFlip,!Default_StatusBar_TileProperties_XFlip,!Default_StatusBar_TileProperties_Priority,!Default_StatusBar_TileProperties_Palette,!Default_StatusBar_TileProperties_Page)
		!Default_Overworld_TilePropertiesSetting #= GetLayer3YXPCCCTT(!Default_Overworld_TileProperties_YFlip,!Default_Overworld_TileProperties_XFlip,!Default_Overworld_TileProperties_Priority,!Default_Overworld_TileProperties_Palette,!Default_Overworld_TileProperties_Page)
		!GraphicalBarExampleTest_LeftSideTileProps #= GetLayer3YXPCCCTT(!GraphicalBarExampleTest_LeftSideTile_TileProperties_YFlip,!GraphicalBarExampleTest_LeftSideTile_TileProperties_XFlip,!Default_StatusBar_TileProperties_Priority,!GraphicalBarExampleTest_LeftSideTile_TileProperties_Palette,!GraphicalBarExampleTest_LeftSideTile_TileProperties_Page)
		!GraphicalBarExampleTest_RightSideTileProps #= GetLayer3YXPCCCTT(!GraphicalBarExampleTest_RightSideTile_TileProperties_YFlip,!GraphicalBarExampleTest_RightSideTile_TileProperties_XFlip,!Default_StatusBar_TileProperties_Priority,!GraphicalBarExampleTest_RightSideTile_TileProperties_Palette,!GraphicalBarExampleTest_RightSideTile_TileProperties_Page)
	;Convert XY position to address
		!Default_GraphicalBar_Pos_Tile #= VanillaStatusBarXYToAddress(!Default_GraphicalBar_PosX_Tile, !Default_GraphicalBar_PosY_Tile, !RAM_0EF9)
		!Default_GraphicalBar_Pos_Tile_ExtendLeftwards #= VanillaStatusBarXYToAddress(!Default_GraphicalBar_PosX_ExtendLeftwards, !Default_GraphicalBar_PosY_ExtendLeftwards, !RAM_0EF9)
		!GraphicalBarExampleTest_ExtendRightBarPos #= VanillaStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendRightBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendRightBar, !RAM_0EF9)
		!GraphicalBarExampleTest_ExtendLeftBarPos #= VanillaStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendLeftBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendLeftBar, !RAM_0EF9)
		
		if !UsingCustomStatusBar == 0
			%CheckValidVanillaStatusBarPos(!Default_GraphicalBar_PosX_Tile, !Default_GraphicalBar_PosY_Tile)
			%CheckValidVanillaStatusBarPos(!Default_GraphicalBar_PosX_ExtendLeftwards, !Default_GraphicalBar_PosY_ExtendLeftwards)
			%CheckValidVanillaStatusBarPos(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendRightBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendRightBar)
			%CheckValidVanillaStatusBarPos(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendLeftBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendLeftBar)
			
		else
		
			%CheckValidPatchedStatusBarPos(!Default_GraphicalBar_PosX_Tile, !Default_GraphicalBar_PosY_Tile)
			%CheckValidPatchedStatusBarPos(!Default_GraphicalBar_PosX_ExtendLeftwards, !Default_GraphicalBar_PosY_ExtendLeftwards)
			%CheckValidPatchedStatusBarPos(!Default_GraphicalBar_PosX_VerticalUpwards, !Default_GraphicalBar_PosY_VerticalUpwards)
			%CheckValidPatchedStatusBarPos(!Default_GraphicalBar_PosX_VerticalDownwards, !Default_GraphicalBar_PosY_VerticalDownwards)
			%CheckValidPatchedStatusBarPos(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendRightBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendRightBar)
			%CheckValidPatchedStatusBarPos(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendLeftBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendLeftBar)
			%CheckValidPatchedStatusBarPos(!Default_GraphicalBar_PosX_OverworldMap, !Default_GraphicalBar_PosY_OverworldMap)
			%CheckValidPatchedStatusBarPos(!FirstFillHexValDisplay_PosX, !FirstFillHexValDisplay_PosY)
			%CheckValidPatchedStatusBarPos(!SecondFillHexValDisplay_PosX, !SecondFillHexValDisplay_PosY)
			%CheckValidPatchedStatusBarPos(!FirstFillPercentHexValDisplay_PosX, !FirstFillPercentHexValDisplay_PosY)
			%CheckValidPatchedStatusBarPos(!SecondFillPercentHexValDisplay_PosX, !SecondFillPercentHexValDisplay_PosY)
			%CheckValidPatchedStatusBarPos(!Interval_Write_PosX, !Interval_Write_PosY)
		
			!Default_GraphicalBar_Pos_Tile #= PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_Tile, !Default_GraphicalBar_PosY_Tile, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!Default_GraphicalBar_Pos_Properties #= PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_Tile, !Default_GraphicalBar_PosY_Tile, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)

			!Default_GraphicalBar_Pos_Tile_ExtendLeftwards #= PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_ExtendLeftwards, !Default_GraphicalBar_PosY_ExtendLeftwards, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!Default_GraphicalBar_Pos_Properties_ExtendLeftwards #= PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_ExtendLeftwards, !Default_GraphicalBar_PosY_ExtendLeftwards, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)

			!Default_GraphicalBar_Pos_Tile_VerticalUpwards #= PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_VerticalUpwards, !Default_GraphicalBar_PosY_VerticalUpwards, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!Default_GraphicalBar_Pos_Properties_VerticalUpwards #= PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_VerticalUpwards, !Default_GraphicalBar_PosY_VerticalUpwards, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)
			
			!Default_GraphicalBar_Pos_Tile_VerticalDownwards #= PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_VerticalDownwards, !Default_GraphicalBar_PosY_VerticalDownwards, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!Default_GraphicalBar_Pos_Properties_VerticalDownwards #= PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_VerticalDownwards, !Default_GraphicalBar_PosY_VerticalDownwards, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)
			
			!GraphicalBarExampleTest_StaticEndsExtendRightBarPos #= PatchedStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendRightBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendRightBar, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!GraphicalBarExampleTest_StaticEndsExtendRightBarPropsPos #= PatchedStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendRightBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendRightBar, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)
			
			!GraphicalBarExampleTest_StaticEndsExtendLeftBarPos #= PatchedStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendLeftBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendLeftBar, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!GraphicalBarExampleTest_StaticEndsExtendLeftBarPropsPos #= PatchedStatusBarXYToAddress(!GraphicalBarExampleTest_PosX_StaticEnds_ExtendLeftBar, !GraphicalBarExampleTest_PosY_StaticEnds_ExtendLeftBar, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)
			
			!Default_GraphicalBar_Pos_Tile_OverworldMap #= PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_OverworldMap, !Default_GraphicalBar_PosY_OverworldMap, !FreeramFromAnotherPatch_OWBorderTileStart, $02)
			!Default_GraphicalBar_Pos_Properties_OverworldMap #= PatchedStatusBarXYToAddress(!Default_GraphicalBar_PosX_OverworldMap, !Default_GraphicalBar_PosY_OverworldMap, !FreeramFromAnotherPatch_OWBorderPropStart, $02)
			
			!FirstFillHexValDisplayPos #= PatchedStatusBarXYToAddress(!FirstFillHexValDisplay_PosX, !FirstFillHexValDisplay_PosY, !FreeramFromAnotherPatch_StatusBarTileStart, $02)
			!SecondFillHexValDisplayPos #= PatchedStatusBarXYToAddress(!SecondFillHexValDisplay_PosX, !SecondFillHexValDisplay_PosY, !FreeramFromAnotherPatch_StatusBarTileStart, $02)
			
			!FirstFillPercentHexValDisplayPos #= PatchedStatusBarXYToAddress(!FirstFillPercentHexValDisplay_PosX, !FirstFillPercentHexValDisplay_PosY, !FreeramFromAnotherPatch_StatusBarTileStart, $02)
			!SecondFillPercentHexValDisplayPos #= PatchedStatusBarXYToAddress(!SecondFillPercentHexValDisplay_PosX, !SecondFillPercentHexValDisplay_PosY, !FreeramFromAnotherPatch_StatusBarTileStart, $02)
			
			!Interval_Write_Pos_Tile #= PatchedStatusBarXYToAddress(!Interval_Write_PosX, !Interval_Write_PosY, !FreeramFromAnotherPatch_StatusBarTileStart, !StatusBarFormat)
			!Interval_Write_Pos_Properties #= PatchedStatusBarXYToAddress(!Interval_Write_PosX, !Interval_Write_PosY, !FreeramFromAnotherPatch_StatusBarPropStart, !StatusBarFormat)
		endif