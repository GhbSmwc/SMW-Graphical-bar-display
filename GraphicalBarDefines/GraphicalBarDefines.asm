;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SA-1 handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Only include this if there is no SA-1 detection, such as including this
;in a (seperate) patch.
;	!dp = $0000
;	!addr = $0000
;	!sa1 = 0
;	!gsu = 0

;	if read1($00FFD6) == $15
;		sfxrom
;		!dp = $6000
;		!addr = !dp
;		!gsu = 1
;	elseif read1($00FFD5) == $23
;		sa1rom
;		!dp = $3000
;		!addr = $6000
;		!sa1 = 1
;	endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Graphical bar defines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ram stuff (note that ram banks $7E/$7F cannot be accessed when
;sa-1 mode is running, so use banks $40/$41).

 if !sa1 == 0
  !Scratchram_GraphicalBar_LeftEndPiece   = $60		;>normal ROM
 else
  !Scratchram_GraphicalBar_LeftEndPiece   = $400195	;>SA-1 ROM
 endif
  ;^[1 byte] number of pieces on the left end byte/8x8 tile.


 if !sa1 == 0
  !Scratchram_GraphicalBar_MiddlePiece    = $61
 else
  !Scratchram_GraphicalBar_MiddlePiece    = $400196
 endif
  ;^[1 byte] number of pieces on each middle byte/8x8 tile.


 if !sa1 == 0
  !Scratchram_GraphicalBar_RightEndPiece  = $62
 else
  !Scratchram_GraphicalBar_RightEndPiece  = $400197
 endif
  ;^[1 byte] number of pieces on the right end byte/8x8 tile.

 if !sa1 == 0
  !Scratchram_GraphicalBar_FillByteTbl = $7F844A
 else
  !Scratchram_GraphicalBar_FillByteTbl = $400198
 endif
  ;^[>= 4 bytes] Used to hold the fill amount for each
  ; byte to be converted into tile numbers to be used for display.
  ; The amount of bytes used is:
  ;
  ; BytesUsed = LeftExist + (MiddleExist*Length) + RightExist
  ;
  ; where any variable with "exist" in name is either 0
  ; (pieces is 0) or 1 (pieces is nonzero).
  ;
  ; Also used for calculating the percentage:
  ;  +$00 to +$01 = quantity
  ;  +$02 to +$03 = max quantity


 if !sa1 == 0
  !Scratchram_GraphicalBar_TempLength  = $7F8449
 else
  !Scratchram_GraphicalBar_TempLength  = $4001B8
 endif
  ;^[1 byte] how many middle bytes/8x8 to be written on the bar. This is
  ;basically the length of the bar.

;Graphical bar Settings
 !Setting_GraphicalBar_IndexSize = 0
  ;^0 = 8-bit indexing for byte table, 1 = 16-bit. Only set this to 1
  ; if you, somehow wanted to have a middle length of 255 middle bytes/8x8
  ; (which is extremely unlikely), and have any bar ends enabled
  ; (this would have the index value being at $0100 or $0101).
  ;
  ; each byte/tile consumes an index, so if this is 0, and you have both ends
  ; enabled, your actual middle's maximum is 253 (because 253 middle bytes
  ; plus 2 ends = 255 total bytes used up).
 
 !Setting_GraphicalBar_SNESMathOnly = 0
  ;^Info follows:
  ;-Set this to 0 if any of your code AT LEAST calls the graphical bar routine under the SA-1 processor;
  ; should you ever have one code calls this using SA-1 and the other calls using SNES, or ALL calls
  ; using SA-1.
  ;-Set this to 1 if *all* codes that call graphical bar routine are not using SA-1 processor.
  ;
  ;The reason for this is because if the user only uses the graphical bar routine on a SA-1
  ;ROM, but never processed the routine by SA-1, using SA-1's math registers is useless as
  ;the SNES's 8-bit math registers ($4202, $4203, $4216-$4217) become available for 8-bit*8-bit = 16-bit.
  ;
  ;Things to note:
  ;
  ;-SNES' math handles 8bit*8bit = 16bit numbers, all unsigned. This will be unavailable to
  ; be used if processing SA-1.
  ;-SA-1's math are 16bit*16bit = 32bit, all *signed*. The register is always available
  ; to use regardless if SNES or SA-1 being used.
 
 !Setting_Beta32bitMultiplication = 0
  ;^In case if you are editing this code to handle 32 bit for quantity, set this to 1.
  ; This is a beta code.