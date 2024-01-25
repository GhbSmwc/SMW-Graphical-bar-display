There should be 11 items in the current folder.

Bug fixes by GreenHammerBro, please give credit, it took more than a week to complete.

GHB's note: due to the sprite's origin being the "foot" on the sprite centered, its impossible to have an accurate object collision hitbox, espically when using $01802A. I tried tweaking the hitbox, but you only get set dimensions of hitboxes. I recommend “Custom Object Clipping” by S.L and have ASM knowledge if you have a serious problem with it.

Another note is that I recommend the “No More Sprite Tile Limits", as sprites graphics can fail to show up.

9/2/2017
- Converted the sprite to PIXI and added full SA-1 compatibility.
- The code has been massively cleaned up and simplified to make the sprite faster and smaller.
- The sprite has been made quite a lot easier to customize.

12/31/2015

Man, I really hate that the author uses a space characters as an indention rather than a tab (used before opcodes and comments each line). Anyway, here are the changes:

- Converted all the asm files from TRASM to use xkas (rules reason).
- Fix several codes not using defines, thus you don't have to scroll down just to replace a sprite table with a different RAM, now all you have to do is change the numbers being defined.
- As a result, I have discovered that the sprite state and origional Y position are being shared, causing glitches with the wall climb (blue mask) koopa.
- Replace the old "BEQ list" for the sprite state to use a pointer table, to save space.
- I finally fix a bug that if you damage the boss while in midair and lure it into a wall before touching the ground, and if it turns into a wall climb mode (blue masked), it will leave the arena and leaving the player stuck. It now uses a 16-bit Y position and that if it goes above the position in define "!ClimbingPeak".
- Added lots of comments on the giant masked koopa in case if you want to modify it.
- Fix the countdown (after defeating the boss, it waits before lunging) bug that it uses SMW's level time limit, causing inconsistency on how long until it lunges itself.
- Added palmask (rules reason).
- Fix the hitbox of the boss.

1/12/2016
- Fix a "stomp detection" error on the sprite's hitbox: if you are going downwards (fast enough, higher than the value of #$10) and touch ANY SIDE of the sprite, the boss registers a hit rather than hurting mairo.
 
1/18/2016
- Fix another problem with the giant masked koopa: The projectile sprites like the rocks and fireballs's cfg file is broken.
- Fix a bug that the koopa doesn't face/lunge towards the player when dying, it uses its last facing direction before retreating into its shell.
 
1/19/2016
- Fix the earthquake problem: the earthquake should happen when the walking koopa LANDS on the ground, not JUMPING away from it.
- Fix a terrable aiming routine on the fireball, should now aim directly at the player at a consistent speed.
 
1/22/2016
- Fix sprite spawning routines not utilizing all slot indexes, they now use 12 (or 22 if sa-1, see defines) slots.
- Fixed a fireball bug that it were to be aimed leftwards, the fireball would be facing backwards.
 
1/23/2016
- The fireball are using an inappropriate size for a smoke (also, there is a graphical glitch that if $9D is set the first frame it appears, it displays a garbage tile). It should now use a 16x16 smoke sprite.
- Fix the spawning routine for Koopas being launched in the direction the boss is facing, now shoots towards Mario. I also make them spawn in different Y positions becuase they touch each other and one of them turns around.
