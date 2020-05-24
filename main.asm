; example 6 SNES code

.p816
.smart


.segment "ZEROPAGE"

temp1: .res 2
pad1: .res 2
pad1_new: .res 2
pad2: .res 2
pad2_new: .res 2
in_nmi: .res 2


.segment "BSS"

PAL_BUFFER: .res 512

OAM_BUFFER: .res 512 ;low table
OAM_BUFFER2: .res 32 ;high table



.include "defines.asm"
.include "macros.asm"
.include "init.asm"







.segment "CODE"

; enters here in forced blank
main:
.a16 ; just a standardized setting from init code
.i16
	phk
	plb
	
	jsr clear_sp_buffer
	
	
; COPY PALETTES to PAL_BUFFER	
;	BLOCK_MOVE  length, src_addr, dst_addr
	BLOCK_MOVE  288, BG_Palette, PAL_BUFFER
	
	
; COPY sprites to sprite buffer
	BLOCK_MOVE  16, Sprites, OAM_BUFFER
	
; COPY just 1 high table number	
	A8
	lda #$AA
	sta OAM_BUFFER2
	
	
; DMA from PAL_BUFFER to CGRAM
	A8
	stz pal_addr ; $2121 cg address = zero

	stz $4300 ; transfer mode 0 = 1 register write once
	lda #$22  ; $2122
	sta $4301 ; destination, pal data
	ldx #.loword(PAL_BUFFER)
	stx $4302 ; source
	lda #^PAL_BUFFER
	sta $4304 ; bank
	ldx #512 ; full palette size
	stx $4305 ; length
	lda #1
	sta $420b ; start dma, channel 0
	
	
; DMA from OAM_BUFFER to the OAM RAM
	jsr dma_oam
	
	
; DMA from Spr_Tiles to VRAM	
	lda #V_INC_1 ; the value $80
	sta vram_inc  ; $2115 = set the increment mode +1
	ldx #$4000
	stx vram_addr ; set an address in the vram of $4000
	
	lda #1
	sta $4300 ; transfer mode, 2 registers 1 write
			  ; $2118 and $2119 are a pair Low/High
	lda #$18  ; $2118
	sta $4301 ; destination, vram data
	ldx #.loword(Spr_Tiles)
	stx $4302 ; source
	lda #^Spr_Tiles
	sta $4304 ; bank
	ldx #(End_Spr_Tiles-Spr_Tiles) ;let the assembler figure out
							   ;the size of the tiles for us
	stx $4305 ; length
	lda #1
	sta $420b ; start dma, channel 0	
	
;$2101 sssnn-bb
;sss = sprite sizes, 000 = 8x8 and 16x16 sprites
;nn = displacement for the 2nd set of sprite tiles, 00 = normal
;-bb = where are the sprite tiles, in steps of $2000
;that upper bit is useless, as usual, so I marked it with a dash -
	lda #2 ;sprite tiles at $4000
	sta spr_addr_size ;= $2101

;allow sprites on the main screen	
	lda #SPR_ON ; $10, only show sprites
	sta main_screen ; $212c
	
	;turn on NMI interrupts and auto-controller reads
	lda #NMI_ON|AUTO_JOY_ON
	sta $4200
	
	lda #FULL_BRIGHT ; $0f = turn the screen on, full brighness
	sta fb_bright ; $2100


InfiniteLoop:	
;game loop
	jsr wait_nmi ;wait for the beginning of v-blank
	jsr dma_oam  ;copy the OAM_BUFFER to the OAM
	
	jsr pad_poll ;read controllers
	AXY16
	
	lda pad1
	and #KEY_LEFT
	beq @not_left
@left:
	A8
	dec OAM_BUFFER ;decrease the X values
	dec OAM_BUFFER+4
	dec OAM_BUFFER+8
	dec OAM_BUFFER+12
	A16
@not_left:

	lda pad1
	and #KEY_RIGHT
	beq @not_right
@right:
	A8
	inc OAM_BUFFER ;increase the X values
	inc OAM_BUFFER+4
	inc OAM_BUFFER+8
	inc OAM_BUFFER+12
	A16
@not_right:

	lda pad1
	and #KEY_UP
	beq @not_up
@up:
	A8
	dec OAM_BUFFER+1 ;decrease the Y values
	dec OAM_BUFFER+5
	dec OAM_BUFFER+9
	dec OAM_BUFFER+13
	A16
@not_up:

	lda pad1
	and #KEY_DOWN
	beq @not_down
@down:
	A8
	inc OAM_BUFFER+1 ;increase the Y values
	inc OAM_BUFFER+5
	inc OAM_BUFFER+9
	inc OAM_BUFFER+13
	A16
@not_down:
	A8
	
	jmp InfiniteLoop
	
	
	
	
	
	
	
	
	
clear_sp_buffer:
.a8
.i16
	php
	A8
	XY16
	lda #224 ;put all y values just below the screen
	ldx #$0000
	ldy #128 ;number of sprites
@loop:
	sta OAM_BUFFER+1, x
	inx
	inx
	inx
	inx ;add 4 to x
	dey
	bne @loop
	plp
	rts
	
	
wait_nmi:
.a8
.i16
;should work fine regardless of size of A
	lda in_nmi ;load A register with previous in_nmi
@check_again:	
	WAI ;wait for an interrupt
	cmp in_nmi	;compare A to current in_nmi
				;wait for it to change
				;make sure it was an nmi interrupt
	beq @check_again
	rts

	
dma_oam:
.a8
.i16
	php
	A8
	XY16
	ldx #$0000
	stx oam_addr_L ;$2102 (and 2103)
	
	stz $4300 ; transfer mode 0 = 1 register write once
	lda #4 ;$2104 oam data
	sta $4301 ; destination, oam data
	ldx #.loword(OAM_BUFFER)
	stx $4302 ; source
	lda #^OAM_BUFFER
	sta $4304 ; bank
	ldx #544
	stx $4305 ; length
	lda #1
	sta $420b ; start dma, channel 0
	plp
	rts
	
	
pad_poll:
.a8
.i16
; reads both controllers to pad1, pad1_new, pad2, pad2_new
; auto controller reads done, call this once per main loop
; copies the current controller reads to these variables
; pad1, pad1_new, pad2, pad2_new (all 16 bit)
	php
	A8
@wait:
; wait till auto-controller reads are done
	lda $4212
	lsr a
	bcs @wait
	
	A16
	lda pad1
	sta temp1 ; save last frame
	lda $4218 ; controller 1
	sta pad1
	eor temp1
	and pad1
	sta pad1_new
	
	lda pad2
	sta temp1 ; save last frame
	lda $421a ; controller 2
	sta pad2
	eor temp1
	and pad2
	sta pad2_new
	plp
	rts
	

Sprites:
;4 bytes per sprite = x, y, tile #, attribute
.byte $80, $80, $00, SPR_PRIOR_2	
.byte $90, $80, $02, SPR_PRIOR_2	
.byte $80, $90, $20, SPR_PRIOR_2	
.byte $90, $90, $22, SPR_PRIOR_2

;the attribute bits are
;vhoo pppN
;N=1st or 2nd set of sprite tiles
;ppp = palette
;oo = sprite priority
;vh = vertical and horizontal flip

;high table attributes, 2 bits
;sx
;x = 9th X bit (set is like off screen to the left)
;s = size, small or large (based on 2101 settings)


;note, high table bits (2 per sprite)
;will always be X=0 and size =1
;1010 1010 = $AA	
	
	
	
	

.include "header.asm"	


.segment "RODATA1"

BG_Palette:
.incbin "default.pal"
.incbin "sprite.pal" ;is 32 bytes, 256+32=288

Spr_Tiles:
.incbin "sprite.chr"
End_Spr_Tiles:


