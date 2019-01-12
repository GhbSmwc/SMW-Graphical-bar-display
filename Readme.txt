Tower of Heaven fan block, by GreenHammerBro

Github: [https://github.com/GhbSmwc/SMW_TowerOfHeavenFan]

Should be 7 items in main folder.

This fan block, when touch by the player, causes Mario to accelerate upwards.
The longer or lower you are in the wind area while touching, the higher the
player is flung upwards. This was inspired by askiisoft's Tower of Heaven.

How to insert:

-Lunar Magic:
--Graphics
  1) Copy "ExGFX80.bin" and paste it in the "ExGraphics" folder. Now press
     the golden mushroom in Lunar magic to insert Exgraphic to ROM. If you
     haven't created an Exgraphics file, extract the regular graphics (red
     mushroom) and re-insert (green mushroom), then extract ExGraphic from
     rom (blue mushroom). The reason why regular graphics is involved is
     because to avoid angry lunar magic saying you need to insert graphics
     due to a 4bpp format and garbled graphics.*

  2) Press the red poison mushroom and on the last Drop-down list (AN2),
     set it to use the Exgraphic file that you inserted.*

--Map16 and exanimation (.mwl for quick insert):
  3) Go to [File -> Open Level from File...] then select Exanimation.mwl.
     It should have the level (not global) exanimation data stored in that
     file.**
---Exanimation note:
     The 8 8x8 tiles to animate couldn't be a 32x8 dimension, therefore, all
     tiles are arranged in a row on the 8x8 map editor. If you wanted a
     different graphic(s), you have to arrange each 8x8 in a row and
     re-arrange them on the map16 editor so that it displays properly. 

  4) Open your map16 tile editor, import the 16 tile from the file (? block
     with a red left arrow) and select Tiles.map16 to insert the tiles.

-ASM:
1) Paste "TOHFanDefines" folder in GPS's in the same directory and not inside
   the "blocks" folder. Do the same thing with uberasm tool and yes, the same
   directory as the exe itself as well. If you make any changes to the define,
   the existing copies must match to use up-to-date RAM address and settings.

2) Paste "FanBlock.asm" into GPS's "blocks" folder and insert this block as:
   [500:025 FanBlock.asm] in the list folder.**

3) Open UberasmTool_library/TOHFan.asm and paste that ASM file in uberasm
   tool's library folder, in any level file that you are going to use the fan,
   add this code: [JSL TOHFan_TowerOfHeavenFan] as "main". This must be
   executed every frame.

   Due to how SMW runs the code in a certain sequence, the holding the jump
   button when you get caought in the fand wind does not work properly when
   executed within a Gamemode 14 due to this taking place AFTER the player
   gravity handling at $00D948 and before the controller update at $0021CA.

* = It inserts at slot #$80 by default.
** = Inserts at page 5. Feel free to move them around.
------------------------------------------------------------------------------
Note:
 -Make sure that the screen scrolls up when the player uses the fan blocks,
  this can be done by:

 --changing $00F878 from #$D0 to #$80,
 --Setting $7E1404 to a non zero every frame; or
 --using my center scroll patch with a define being set to allow it.
 -During testing of LM 3.01, I did notice a strange exanimation quirk that
  when frames update, their graphics disappear for 1 frame before displaying
  the next frame. This is not caused by me, it might be FuSoYa deliberately
  did that to prevent possible V-blank overflow.
------------------------------------------------------------------------------
Version history (M/D/Y)
5/23/2016
 -First Release.

1/11/2019
 -Merged the two-stated RAM address into a single byte, suggested by
  JackTheSpades [https://www.smwcentral.net/?p=profile&id=7869].
 -Defines are now moved to a universal folder called via [incsrc <filepath>],
  making updates to freeram more manageable.