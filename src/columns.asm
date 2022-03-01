; This disassembly was created using Emulicious (https://www.emulicious.net)
.MEMORYMAP
SLOTSIZE $7FF0
SLOT 0 $0000
SLOTSIZE $10
SLOT 1 $7FF0
SLOTSIZE $4000
SLOT 2 $8000
DEFAULTSLOT 2
.ENDME
.ROMBANKMAP
BANKSTOTAL 8
BANKSIZE $7FF0
BANKS 1
BANKSIZE $10
BANKS 1
BANKSIZE $4000
BANKS 6
.ENDRO

.EMPTYFILL $FF

.INCLUDE "constants/sms.asm"
.INCLUDE "constants/audio.asm"
.INCLUDE "variables.asm"

.BANK 0 SLOT 0
.ORG $0000

start:
	jp init

.ORG $8
setVdpAddress:
	di
	ld a, e
	out (Port_VDPAddress), a
	ld a, $40
	or d
	out (Port_VDPAddress), a
	ei
	ret

.ORG $18
; Data from 18 to 1A (3 bytes)
_DATA_18_:
.db $F3 $7B $D3

; Data from 1B to 27 (13 bytes)
_DATA_1B_:
.db $BF $7A $D3 $BF $FB $C9 $FF $FF $FF $FF $FF $FF $FF

_LABEL_28_:
	ld a, $E0
	jp +


.ORG $30
_LABEL_30_:
	ld a, $A0
	jp +

.ORG $38
handleInterruptEntrypoint:
	jp handleInterrupt

+:
	out (Port_VDPAddress), a
	ld a, $81
	out (Port_VDPAddress), a
	ret

.ORG $66
handlePauseInterruptEntrypoint:
	push af
	ld a, (var.previousState)
	cp $10
	jr c, +
	cp $16
	jr nc, +
	ld a, (_RAM_C002_)
	cpl
	ld (_RAM_C002_), a
	pop af
	retn

+:
	xor a
	ld (_RAM_C002_), a
	pop af
	retn

init:
	di

	ld sp, $DFF0

	im 1

	; Configure memory mapper
	ld hl, _RAM_FFFC_
	ld (hl), $00
	inc hl
	ld (hl), $00
	inc hl
	ld (hl), $01
	inc hl
	ld (hl), $02

	; Clear some RAM
	; TODO: Why those places.
	ld hl, _RAM_C001_
	ld de, _RAM_C001_ + 1
	ld bc, $00FE
	ld (hl), $00
	ldir

	; TODO: Clear entities?
	ld hl, v_entities
	ld de, v_entities + 1
	ld bc, $0EFF
	ld (hl), $00
	ldir

	ld hl, _RAM_DD00_
	ld de, _RAM_DD00_ + 1
	ld bc, $02EF
	ld (hl), $00
	ldir

	; Wait for scan line 176
	-:
		in a, (Port_VCounter)
		cp $B0
	jr nz, -

	xor a
	out (Port_VDPAddress), a
	ld a, $C0
	out (Port_VDPAddress), a

	; Clear palette
	xor a
	ld b, $20

	; TODO: ?
	ex (sp), hl
	ex (sp), hl

	-:
		out (Port_VDPData), a
		nop
	djnz -

	call io_LABEL_126_

	; TODO
	ld (_RAM_C001_), a

	ld a, $01
	ld (_RAM_C6A8_), a
	ld (_RAM_C6AD_), a

	ld a, $03
	ld (_RAM_C6AB_), a
	ld (_RAM_C6B0_), a

; TODO: Reset?
reset:
	di

	ld sp, $DFF0

	; Reset some values
	; TODO
	xor a
	ld (var.previousState), a
	ld (var.state), a
	ld (_RAM_C005_), a
	ld (_RAM_C002_), a
	ld (_RAM_C006_), a
	ld (_RAM_C6C2_), a
	ld (_RAM_C008_), a
	ld (_RAM_C009_), a

	; Init Vdp Registers
	in a, (Port_VDPStatus)
	ld b, $16
	ld c, Port_VDPAddress
	ld hl, initialVdpRegisters
	otir

	; TODO
	rst $30	; _LABEL_30_

	call clearFilteredPalette

	; TODO: Clear VRAM and more RAM
	call _LABEL_5C6_

	; Unnecessary reset
	ld a, $00
	ld (var.previousState), a
	ld (var.state), a

	ei
	jp mainLoop

io_LABEL_126_:
	ld a, $F5
	out (Port_IOPortControl), a
	in a, (Port_IOPort2)
	and $C0
	cp $C0
	jr nz, +
	ld a, $55
	out (Port_IOPortControl), a
	in a, (Port_IOPort2)
	and $C0
	or a
	jr nz, +
	ld a, $FF
	out (Port_IOPortControl), a
	ret

+:
	xor a
	ret

; Data from 144 to 144 (1 bytes)
.db $C9

; Data from 145 to 15A (22 bytes)
initialVdpRegisters:
.db $06
.db $80
.db $A0
.db $81
.db $FF
.db $82
.db $FF
.db $83
.db $FF
.db $84
.db $FF
.db $85
.db $FF
.db $86
.db $00
.db $87
.db $00
.db $88
.db $00
.db $89
.db $00
.db $8A

mainLoop:
	call input.update
	call fade.update

	; Loop by overriding return address
	ld hl, mainLoop
	push hl

	; Update var.previousState on state change
	ld hl, var.state
	ld a, (var.previousState)
	xor (hl)
	and $7F
	ld a, (hl)
	jr z, @endif
		ld (var.previousState), a
	@endif:

	ld hl, updatersPointers

jumpToAthPointer:
	ld e, a
	ld d, $00
	add hl, de
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	jp (hl)

waitInterrupt_LABEL_181_:
	ld a, $01
	ld (var.interrupt.handlerIndex), a
-:
	ld a, (var.interrupt.handlerIndex)
	or a
	jr nz, -
	ret

waitInterrupt_LABEL_18D_:
	ld a, $02
	ld (var.interrupt.handlerIndex), a
-:
	ld a, (var.interrupt.handlerIndex)
	or a
	jr nz, -
	ret

waitInterrupt_LABEL_199_:
	ld a, $03
	ld (var.interrupt.handlerIndex), a
-:
	ld a, (var.interrupt.handlerIndex)
	or a
	jr nz, -
	ret

; Unused?
waitInterrupt_LABEL_1A5_:
	ld a, $04
	ld (var.interrupt.handlerIndex), a
-:
	ld a, (var.interrupt.handlerIndex)
	or a
	jr nz, -
	ret

tickTimer:
	ld hl, (var.timer)
	dec hl
	ld (var.timer), hl
	ld a, l
	or h
	ret

; Jump Table from 1BB to 1F6 (30 entries, indexed by var.state)
updatersPointers:
.dw _LABEL_1641_
.dw _LABEL_1641_
.dw _LABEL_1649_
.dw updateMainMenuState
.dw _LABEL_175C_
.dw updateDemoState
.dw _LABEL_1996_
.dw updateModeMenuState
.dw _LABEL_19F6_
.dw updateOptionsMenuState
.dw _LABEL_1BA6_
.dw _LABEL_1CBD_
.dw _LABEL_1E79_
.dw _LABEL_209C_
.dw _LABEL_2346_
.dw _LABEL_2469_
.dw updateGameplayState
.dw _LABEL_1D64_
.dw _LABEL_1F15_
.dw _LABEL_2168_
.dw _LABEL_23F9_
.dw _LABEL_2510_
.dw _LABEL_1E20_
.dw _LABEL_1E59_
.dw _LABEL_1C98_
.dw _LABEL_1DEF_
.dw _LABEL_2027_
.dw _LABEL_22AE_
.dw _LABEL_2444_
.dw _LABEL_25A1_

handleInterrupt:
	push af
	in a, (Port_VDPStatus)
	ld a, (_RAM_FFFF_)
	push af
	push bc
	push de
	push hl
	exx
	push af
	push bc
	push de
	push hl
	push ix
	push iy

	call _LABEL_2DB0_

	ld a, (var.interrupt.handlerIndex)
	ld hl, interruptHandlersPointers
	jp jumpToAthPointer

interruptHandlersPointers:
.dw _LABEL_220_
.dw _LABEL_241_
.dw _LABEL_282_
.dw _LABEL_2FE_
.dw _LABEL_241_

; 1st entry of Jump Table from 216 (indexed by var.interrupt.handlerIndex)
_LABEL_220_:
	ld hl, +	; Overriding return address
	push hl
	ld a, (_RAM_C002_)
	or a
	jp nz, _LABEL_498F_
	jp _LABEL_45CA_

+:
	pop iy
	pop ix
	pop hl
	pop de
	pop bc
	pop af
	exx
	pop hl
	pop de
	pop bc
	pop af
	ld (_RAM_FFFF_), a
	pop af
	ei
	ret

; 2nd entry of Jump Table from 216 (indexed by var.interrupt.handlerIndex)
_LABEL_241_:
	call _LABEL_41D_
	call _LABEL_42C_
	call _LABEL_438_
	ld b, $00
-:
	djnz -
	ld a, (var.pallete.shouldUpdate)
	and $01
	call nz, writePalette
	xor a
	ld (var.pallete.shouldUpdate), a
	ld (var.palette._RAM_C022_), a
	xor a
	ld (var.interrupt.handlerIndex), a
_LABEL_261_:
	ld hl, +	; Overriding return address
	push hl
	ld a, (_RAM_C002_)
	or a
	jp nz, _LABEL_498F_
	jp _LABEL_45CA_

+:
	pop iy
	pop ix
	pop hl
	pop de
	pop bc
	pop af
	exx
	pop hl
	pop de
	pop bc
	pop af
	ld (_RAM_FFFF_), a
	pop af
	ei
	ret

; 3rd entry of Jump Table from 216 (indexed by var.interrupt.handlerIndex)
_LABEL_282_:
	call _LABEL_41D_
	call _LABEL_42C_
	call _LABEL_438_
	ld a, (_RAM_C002_)
	or a
	jr nz, _LABEL_2D6_
	ld hl, _RAM_C419_
	ld c, Port_VDPData
	exx
	ld hl, $38DA
	ld de, $0040
	ld bc, $12BE
-:
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	exx
	ld a, (_RAM_C005_)
	bit 0, a
	ld a, $19
	jr z, +
	ld a, $09
+:
	call _LABEL_1114_
	inc hl
	inc hl
	exx
	add hl, de
	djnz -
--:
	ld a, (var.pallete.shouldUpdate)
	and $01
	call nz, writePalette
	call _LABEL_2B86_
	xor a
	ld (var.pallete.shouldUpdate), a
	ld (var.palette._RAM_C022_), a
	xor a
	ld (var.interrupt.handlerIndex), a
	jp _LABEL_261_

_LABEL_2D6_:
	ld c, Port_VDPData
	exx
	ld hl, $38DA
	ld de, $0040
	ld b, $12
-:
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	exx
	ld hl, _DATA_2F8_
	ld a, $09
	call _LABEL_1114_
	exx
	add hl, de
	djnz -
	jr --

; Data from 2F8 to 2FD (6 bytes)
_DATA_2F8_:
.db $00 $00 $00 $00 $00 $00

; 4th entry of Jump Table from 216 (indexed by var.interrupt.handlerIndex)
_LABEL_2FE_:
	call _LABEL_41D_
	call _LABEL_42C_
	call _LABEL_438_
	ld a, (_RAM_C002_)
	or a
	jp nz, _LABEL_3CD_
	ld hl, (_RAM_C69A_)
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	ld de, _RAM_C419_
	add hl, de
	exx
	ld a, (_RAM_C69A_)
	neg
	add a, $12
	ld b, a
	ld hl, $38C6
	ld de, $0040
	ld c, Port_VDPData
	ld a, $09
	ex af, af'
-:
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	exx
	ex af, af'
	call _LABEL_1114_
	ex af, af'
	inc hl
	inc hl
	exx
	add hl, de
	djnz -
	ld a, (_RAM_C69A_)
	or a
	jr z, +
	ld b, a
-:
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	exx
	ld a, $09
	ld hl, _DATA_3C7_
	call _LABEL_1114_
	exx
	add hl, de
	djnz -
+:
	ld a, (var.pallete.shouldUpdate)
	and $01
	call nz, writePalette
	ld hl, (_RAM_C6A6_)
	ld h, $00
	add hl, hl
	add hl, hl
	add hl, hl
	ld de, _RAM_C561_
	add hl, de
	exx
	ld a, (_RAM_C6A6_)
	neg
	add a, $12
	ld b, a
	ld hl, $38EE
	ld de, $0040
	ld c, $BE
	ld a, $09
	ex af, af'
-:
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	exx
	ex af, af'
	call _LABEL_1114_
	ex af, af'
	inc hl
	inc hl
	exx
	add hl, de
	djnz -
	ld a, (_RAM_C6A6_)
	or a
	jr z, _LABEL_3B6_
	ld b, a
-:
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	exx
	ld a, $09
	ld hl, _DATA_3C7_
	call _LABEL_1114_
	exx
	add hl, de
	djnz -
_LABEL_3B6_:
	call _LABEL_2B86_
	xor a
	ld (var.pallete.shouldUpdate), a
	ld (var.palette._RAM_C022_), a
	xor a
	ld (var.interrupt.handlerIndex), a
	jp _LABEL_261_

; Data from 3C7 to 3CC (6 bytes)
_DATA_3C7_:
.db $0F $0F $0F $0F $0F $0F

_LABEL_3CD_:
	ld c, Port_VDPData
	exx
	ld hl, $38C6
	ld de, $0040
	ld bc, $12BE
-:
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	exx
	ld hl, _DATA_2F8_
	ld a, $09
	call _LABEL_1114_
	inc hl
	inc hl
	exx
	add hl, de
	djnz -
	ld a, (var.pallete.shouldUpdate)
	and $01
	call nz, writePalette
	ld c, Port_VDPData
	exx
	ld hl, $38EE
	ld de, $0040
	ld bc, $12BE
-:
	ld a, l
	out (Port_VDPAddress), a
	ld a, h
	or $40
	out (Port_VDPAddress), a
	exx
	ld hl, _DATA_2F8_
	ld a, $09
	call _LABEL_1114_
	inc hl
	inc hl
	exx
	add hl, de
	djnz -
	jr _LABEL_3B6_

_LABEL_41D_:
	ld hl, _RAM_C004_
	in a, (Port_IOPort2)
	and $10
	ld c, (hl)
	ld (hl), a
	xor c
	and c
	ret z
	jp reset

_LABEL_42C_:
	ld a, (_RAM_C018_)
	or a
	ret z
	dec a
	jr z, +
	rst $28	; _LABEL_28_
	ret

+:
	rst $30	; _LABEL_30_
	ret

_LABEL_438_:
	ld c, Port_VDPData
	ld a, $00
	out (Port_VDPAddress), a
	ld a, $7F
	out (Port_VDPAddress), a
	ld hl, _RAM_C300_
	call outi64
	ld a, $80
	out (Port_VDPAddress), a
	ld a, $7F
	out (Port_VDPAddress), a
	ld hl, _RAM_C340_
	call outi128
	ret

input.update:
	ld a, (var.previousState)
	cp $10 ; < gameplay
	jr c, @_LABEL_49C_

	cp $16 ; >= ?
	jr nc, @_LABEL_49C_

	; TODO
	ld a, (var.input.player1.data)
	and $0C
	ld a, $08
	jr z, +
	ld a, (var.input.player1.timer)
	dec a
	jr nz, +
	ld a, (var.input.player1.data)
	and $F3
	ld (var.input.player1.data), a
	ld a, $01
+:
	ld (var.input.player1.timer), a
	ld a, (var.input.player2.data)
	and $0C
	ld a, $08
	jr z, +
	ld a, (var.input.player2.timer)
	dec a
	jr nz, +
	ld a, (var.input.player2.data)
	and $F3
	ld (var.input.player2.data), a
	ld a, $01
+:
	ld (var.input.player2.timer), a

	jr ++

@_LABEL_49C_:
	ld a, (var.input.player1.data)
	and $0F
	ld a, $10
	jr z, +
	ld a, (var.input.player1.timer)
	dec a
	jr nz, +
	ld a, (var.input.player1.data)
	and $F0
	ld (var.input.player1.data), a
	ld a, $0A
+:
	ld (var.input.player1.timer), a
	ld a, (var.input.player2.data)
	and $0F
	ld a, $10
	jr z, +
	ld a, (var.input.player2.timer)
	dec a
	jr nz, +
	ld a, (var.input.player2.data)
	and $F0
	ld (var.input.player2.data), a
	ld a, $0A
+:
	ld (var.input.player2.timer), a

++:
	ld a, (var.input.player1.data)
	cpl
	ld d, a
	call @getDataB
	ld (var.input.player1.data), a
	and d
	ld (var.input.player1.debounced), a

	ld a, (var.input.player2.data)
	cpl
	ld d, a
	call @getDataA
	ld (var.input.player2.data), a
	and d
	ld (var.input.player2.debounced), a

	ret

@getDataA:
	in a, (Port_IOPort1)
	ld c, a
	in a, (Port_IOPort2)
	rl c
	rla
	rl c
	rla
	cpl
	and $3F
	ret

@getDataB:
	in a, (Port_IOPort1)
	cpl
	and $3F
	ret

updateEntities_LABEL_508_:
	ld ix, v_entities

	ld b, $10
	-:
		push bc
		ld hl, +	; Overriding return address
		push hl
		ld l, (ix+0)
		ld h, (ix+1)
		ld a, l
		or h
		ret z
		jp (hl)

		+:
		ld de, $0020
		add ix, de
		pop bc
	djnz -

	ret

_LABEL_526_:
	call _LABEL_68F_
	ld a, $01
	ld (var.fade.state), a
	
	; Clear filtered pallete
	ld hl, v_filteredPalette
	ld de, v_filteredPalette + 1
	ld (hl), $00
	call ldi31
	ld hl, var.pallete.shouldUpdate
	set 0, (hl)
	ld a, $04
	ld (var.fade.timer), a
	ret

_LABEL_544_:
	ld a, $02
	ld (var.fade.state), a
	ld hl, v_filteredPalette
	ld de, v_palette
	ld bc, $0020
	call ldi32
	ld a, $04
	ld (var.fade.timer), a
	ret

fade.update:
	; Fade effect is updated every 4th
	; frame, we use a timer to handle this.
	ld hl, var.fade.timer
	dec (hl)
	ret nz

	; Update frame reached, reset timer
	ld a, $04
	ld (var.fade.timer), a

	; If state isn't set, do nothing
	ld a, (var.fade.state)
	or a
	ret z

	call @doUpdate

	; Now we request a palette update
	ld hl, var.pallete.shouldUpdate
	set 0, (hl)

	; Increment progress and return if fading hasn't ended yet
	ld hl, var.fade.progress
	inc (hl)
	ld a, (hl)
	cp $04
	ret nz

	; If we reach here, the fade in our fade out
	; transition is over and we can reset our variables.
	xor a
	ld (var.fade.progress), a
	ld (var.fade.state), a
	ret

@doUpdate:
	ld a, (var.fade.state)
	dec a

	; If fade in
	jr nz, @else
		ld a, (var.fade.progress)
		ld d, a
		ld a, $03
		sub d
		jr @endif
	@else:
		ld a, (var.fade.progress)
	@endif:

	ld c, a
	ld de, v_palette
	ld hl, v_filteredPalette

	; Number of colors
	ld b, $20

@colorLoop:
	push bc

	; Fade red component
	ld a, (de)
	and 0b00000011
	sub c
	jr nc, @endif2
		xor a
	@endif2:
	ld (hl), a

	; Fade green component
	rlc c
	rlc c
	ld a, (de)
	and 0b00001100
	sub c
	jr nc, @endif3
		xor a
	@endif3:
	or (hl)
	ld (hl), a

	; Fade blue component
	rlc c
	rlc c
	ld a, (de)
	and 0b00110000
	sub c
	jr nc, @endif4
		xor a
	@endif4:
	or (hl)
	ld (hl), a

	inc hl
	inc de
	pop bc
	djnz @colorLoop

	ret

_LABEL_5C6_:
	; TODO: Clear VRAM from $00 to $1F
	ld de, $0000
	ld hl, $0000
	ld bc, $0010
	call _LABEL_62E_

	; TODO: Clear VRAM from $2000 to $201F
	ld de, $2000
	ld hl, $0000
	ld bc, $0010
	call _LABEL_62E_

	; TODO: Clear VRAM from $3800 to $3EFF
	ld de, $3800
	ld hl, $0000
	ld bc, $0380
	call _LABEL_62E_

_LABEL_5EA_:
	; TODO: Clear RAM from $C100 to $C300 (512 bytes)
	ld hl, v_entities
	ld de, v_entities + 1
	ld (hl), $00
	call ldi128
	call ldi128
	call ldi128
	call ldi127

	; TODO: ?
	ld hl, _RAM_C300_
	ld (hl), $D0

	ret

_LABEL_604_:
	rst $08	; setVdpAddress
	inc b
--:
	ld a, b
	ld b, c
	ld c, Port_VDPData
-:
	outi
	jr nz, -
	ld b, a
	ld c, $00
	djnz --
	ret

; Data from 614 to 62D (26 bytes)
.db $08 $CF $08 $04 $C5 $41 $18 $03 $C5 $06 $00 $D3 $BE $00 $10 $FC
.db $C1 $10 $F5 $C9 $D3 $BE $00 $10 $FB $C9

_LABEL_62E_:
	rst $08	; setVdpAddress
	inc b
	ld d, c
	ld c, Port_VDPData
	push bc
	ld b, d
	jr _LABEL_63A_

-:
	push bc
	ld b, $00
_LABEL_63A_:
	ld a, l
	out (c), a
	ld a, h
	jr +

+:
	out (c), a
	djnz _LABEL_63A_
	pop bc
	djnz -
	ret

_LABEL_648_:
	rst $08	; setVdpAddress
	push bc
	ld b, c
	ld c, Port_VDPData
-:
	outi
	outi
	jr nz, -
	ex de, hl
	ld bc, $0040
	add hl, bc
	ex de, hl
	pop bc
	djnz _LABEL_648_
	ret

_LABEL_65D_:
	rst $08	; setVdpAddress
	push bc
	ld b, c
	ld c, Port_VDPData
-:
	outi
	jr +

+:
	outi
	jr nz, -
	ex de, hl
	ld bc, $0040
	add hl, bc
	ex de, hl
	pop bc
	djnz _LABEL_65D_
	ret

; Data from 674 to 68E (27 bytes)
.db $C5 $08 $CF $7E $D3 $BE $08 $18 $00 $D3 $BE $08 $23 $0D $20 $F3
.db $08 $EB $01 $40 $00 $09 $EB $C1 $10 $E6 $C9

_LABEL_68F_:
	ld de, v_palette
	jr +

; Data from 694 to 69E (11 bytes)
.db $3A $21 $C0 $F6 $01 $32 $21 $C0 $11 $23 $C0

+:
	ld a, (hl)
	inc hl
	push hl
	ld l, a
	ld h, $00
	add hl, de
	ex de, hl
	pop hl
	ld a, (hl)
	ld c, a
	ld b, $00
	inc hl
	ldir
	ret

clearFilteredPalette:
	ld hl, v_filteredPalette
	ld de, v_filteredPalette + 1
	ld (hl), $00
	call ldi63
	ld hl, var.pallete.shouldUpdate
	set 0, (hl)
	ret

writePalette:
	xor a
	out (Port_VDPAddress), a
	ld a, $C0
	out (Port_VDPAddress), a
	ld hl, v_filteredPalette
	ld c, Port_VDPData
	jp outi32

_LABEL_6D0_:
	ld a, (hl)
	inc hl
	exx
	ld e, a
	ld d, $00
	exx
	ld b, a
-:
	push bc
	push de
	exx
	pop hl
	push hl
	exx
	call _LABEL_6E7_
	pop de
	inc de
	pop bc
	djnz -
	ret

_LABEL_6E7_:
	ld a, (hl)
	inc hl
	or a
	ret z
	ld b, a
	and $80
	ld c, a
	ld a, b
	and $7F
	ld b, a
-:
	rst $08	; setVdpAddress
	ld a, (hl)
	out (Port_VDPData), a
	xor a
	or c
	jr z, +
	inc hl
+:
	exx
	add hl, de
	push hl
	exx
	pop de
	djnz -
	xor a
	or c
	jp nz, _LABEL_6E7_
	inc hl
	jp _LABEL_6E7_

_LABEL_70C_:
	ld a, (hl)
	inc hl
	exx
	ld e, a
	ld d, $00
	exx
	ld b, a
-:
	push bc
	push de
	exx
	pop hl
	push hl
	exx
	call _LABEL_723_
	pop de
	inc de
	pop bc
	djnz -
	ret

_LABEL_723_:
	ld a, (hl)
	inc hl
	or a
	ret z
	ld b, a
	and $80
	ld c, a
	ld a, b
	and $7F
	ld b, a
-:
	ld a, (hl)
	ld (de), a
	bit 7, c
	jr z, +
	inc hl
+:
	exx
	add hl, de
	push hl
	exx
	pop de
	djnz -
	bit 7, c
	jp nz, _LABEL_723_
	inc hl
	jp _LABEL_723_

_LABEL_746_:
	ld (_RAM_C0F8_), a
	rst $08	; setVdpAddress
	ld c, (hl)
	inc hl
	ld b, (hl)
	inc hl
--:
	ld a, (hl)
	exx
	ld c, Port_VDPData
	ld b, $04
	ld h, a
	ld a, (_RAM_C0F8_)
-:
	rra
	ld d, h
	jr c, +
	ld d, $00
+:
	out (c), d
	djnz -
	exx
	inc hl
	dec bc
	ld a, b
	or c
	jp nz, --
	ret

; Data from 76B to 1012 (2216 bytes)
.incbin "columns_DATA_76B_.inc"

outi128:
.repeat 64
	outi
.endr

outi64:
.repeat 32
	outi
.endr

outi32:
.repeat 32
	outi
.endr
	ret

_LABEL_1114_:
	outi
	out (Port_VDPData), a
	outi
	out (Port_VDPData), a
	outi
	out (Port_VDPData), a
	outi
	out (Port_VDPData), a
	outi
	out (Port_VDPData), a
	outi
	out (Port_VDPData), a
	ret

; Data from 112D to 116A (62 bytes)
.db $D3 $BE $D3 $BE $D3 $BE $D3 $BE $D3 $BE $D3 $BE $D3 $BE $D3 $BE
.db $D3 $BE $D3 $BE $D3 $BE $D3 $BE $C9 $D3 $BE $08 $D3 $BE $08 $D3
.db $BE $08 $D3 $BE $08 $D3 $BE $08 $D3 $BE $08 $D3 $BE $08 $D3 $BE
.db $08 $D3 $BE $08 $D3 $BE $08 $D3 $BE $08 $D3 $BE $08 $C9

.INCLUDE "ldi.asm"

; Data from 126C to 12A8 (61 bytes)
.db $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8
.db $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8
.db $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8
.db $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $C9

_LABEL_12A9_:
	push hl
	ld hl, (_RAM_C01A_)
	ld a, h
	rrca
	rrca
	xor h
	rrca
	xor l
	rrca
	rrca
	rrca
	rrca
	xor l
	rrca
	adc hl, hl
	jr nz, +
	ld hl, $733C
+:
	ld a, r
	xor l
	ld (_RAM_C01A_), hl
	pop hl
	ret

_LABEL_12C8_:
	ld a, (var.fade.state)
	or a
	ret z
	pop hl
	jp waitInterrupt_LABEL_181_

_LABEL_12D1_:
	call clearFilteredPalette
	ld a, $01
	ld (_RAM_C018_), a
	call waitInterrupt_LABEL_181_
	di
	jp _LABEL_5C6_

_LABEL_12E0_:
	ld a, $0F
_LABEL_12E2_:
	ld de, $1AC0
	ld hl, _DATA_8034_
	jp _LABEL_746_

_LABEL_12EB_:
	ld de, $3580
	ld hl, _DATA_8320_
	jp _LABEL_746_

; Data from 12F4 to 12FC (9 bytes)
.db $11 $00 $2F $21 $7E $81 $C3 $46 $07

_LABEL_12FD_:
	ld hl, _RAM_CD00_
	ld de, _RAM_CD01_
	ld (hl), $00
	call ldi128
	call ldi128
	call ldi128
	call ldi128
	call ldi128
	jp ldi128

_LABEL_1317_:
	ld a, (_RAM_C6AC_)
	ld (_RAM_C699_), a
	ld a, (_RAM_C6B1_)
	ld (_RAM_C6A5_), a
	xor a
	ld (_RAM_C6BE_), a
	ld (_RAM_C6C0_), a
	ld (_RAM_C690_), a
	ld (_RAM_C691_), a
	ld (_RAM_C692_), a
	ld (_RAM_C694_), a
	ld (_RAM_C695_), a
	ld (_RAM_C696_), a
	ld (_RAM_C697_), a
	ld (_RAM_C698_), a
	ld (_RAM_C693_), a
	ld (_RAM_C69A_), a
	ld (_RAM_C69B_), a
	ld (_RAM_C69C_), a
	ld (_RAM_C69D_), a
	ld (_RAM_C69E_), a
	ld (_RAM_C6A0_), a
	ld (_RAM_C6A1_), a
	ld (_RAM_C6A2_), a
	ld (_RAM_C6A3_), a
	ld (_RAM_C6A4_), a
	ld (_RAM_C69F_), a
	ld (_RAM_C6A6_), a
	ld (_RAM_C6A7_), a
	ret

_LABEL_136D_:
	ld hl, _RAM_C400_
	ld b, $06
	ld (hl), $FF
	inc hl
-:
	ld (hl), $00
	inc hl
	djnz -
	ld (hl), $FF
	inc hl
	ex de, hl
	ld hl, _RAM_C400_
	call ldi128
	call ldi32
	ld hl, _RAM_C4A7_
	call ldi8
	ld hl, _RAM_C400_
	ld de, _RAM_C548_
	call ldi128
	jp ldi48

_LABEL_1399_:
	ld hl, $13C5
	push hl
	call _LABEL_12A9_
	and $38
	ld e, a
	ld d, $00
	ld hl, $11EB
	add hl, de
	add hl, de
	push hl
	ld hl, $C428
	add hl, de
	ex de, hl
	ld a, (_RAM_C6A8_)
	add a, a
	ld c, a
	ld b, $00
	ld hl, _DATA_1457_
	add hl, bc
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	push hl
	call ldi64
	pop hl
	ret

; Data from 13C5 to 13F7 (51 bytes)
.db $21 $EE $13 $E5 $3A $AB $C6 $87 $87 $87 $87 $5F $16 $00 $21 $7B
.db $11 $19 $E5 $21 $18 $C4 $36 $FF $06 $06 $23 $36 $00 $10 $FB $23
.db $36 $FF $21 $18 $C4 $11 $20 $C4 $C9 $21 $A7 $C4 $11 $A8 $C4 $CD
.db $5B $12 $C9

_LABEL_13F8_:
	ld hl, $1424
	push hl
	call _LABEL_12A9_
	and $38
	ld e, a
	ld d, $00
	ld hl, $11EB
	add hl, de
	add hl, de
	push hl
	ld hl, $C570
	add hl, de
	ex de, hl
	ld a, (_RAM_C6AD_)
	add a, a
	ld c, a
	ld b, $00
	ld hl, _DATA_145D_
	add hl, bc
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	push hl
	call ldi64
	pop hl
	ret

; Data from 1424 to 1456 (51 bytes)
.db $21 $4D $14 $E5 $3A $B0 $C6 $87 $87 $87 $87 $5F $16 $00 $21 $7B
.db $11 $19 $E5 $21 $60 $C5 $36 $FF $06 $06 $23 $36 $00 $10 $FB $23
.db $36 $FF $21 $60 $C5 $11 $68 $C5 $C9 $21 $EF $C5 $11 $F0 $C5 $CD
.db $5B $12 $C9

; Pointer Table from 1457 to 145C (3 entries, indexed by _RAM_C6A8_)
_DATA_1457_:
.dw _DATA_1463_ _DATA_14A3_ _DATA_14E3_

; Pointer Table from 145D to 1462 (3 entries, indexed by _RAM_C6AD_)
_DATA_145D_:
.dw _DATA_1523_ _DATA_1563_ _DATA_15A3_

; 1st entry of Pointer Table from 1457 (indexed by _RAM_C6A8_)
; Data from 1463 to 14A2 (64 bytes)
_DATA_1463_:
.db $FF $01 $02 $03 $04 $01 $02 $FF $FF $04 $03 $01 $02 $04 $03 $FF
.db $FF $02 $01 $03 $04 $02 $01 $FF $FF $03 $04 $01 $02 $03 $04 $FF
.db $FF $01 $02 $03 $04 $01 $02 $FF $FF $01 $04 $01 $02 $03 $02 $FF
.db $FF $03 $02 $03 $04 $01 $04 $FF $FF $03 $04 $01 $02 $03 $04 $FF

; 2nd entry of Pointer Table from 1457 (indexed by _RAM_C6A8_)
; Data from 14A3 to 14E2 (64 bytes)
_DATA_14A3_:
.db $FF $01 $02 $03 $04 $01 $05 $FF $FF $02 $04 $05 $02 $03 $03 $FF
.db $FF $04 $02 $05 $04 $01 $01 $FF $FF $05 $04 $01 $03 $03 $04 $FF
.db $FF $01 $02 $03 $05 $01 $02 $FF $FF $03 $05 $01 $01 $03 $04 $FF
.db $FF $01 $02 $03 $04 $05 $02 $FF $FF $05 $04 $01 $02 $03 $04 $FF

; 3rd entry of Pointer Table from 1457 (indexed by _RAM_C6A8_)
; Data from 14E3 to 1522 (64 bytes)
_DATA_14E3_:
.db $FF $01 $02 $03 $04 $05 $06 $FF $FF $04 $03 $05 $06 $04 $03 $FF
.db $FF $06 $05 $01 $02 $02 $01 $FF $FF $01 $02 $03 $04 $05 $06 $FF
.db $FF $05 $06 $01 $02 $03 $04 $FF $FF $03 $02 $03 $06 $05 $02 $FF
.db $FF $01 $05 $04 $01 $03 $06 $FF $FF $03 $04 $05 $06 $01 $02 $FF

; 1st entry of Pointer Table from 145D (indexed by _RAM_C6AD_)
; Data from 1523 to 1562 (64 bytes)
_DATA_1523_:
.db $FF $07 $08 $09 $0A $07 $08 $FF $FF $0A $09 $07 $08 $0A $09 $FF
.db $FF $08 $07 $09 $0A $08 $07 $FF $FF $09 $0A $07 $08 $09 $0A $FF
.db $FF $07 $08 $09 $0A $07 $08 $FF $FF $07 $0A $07 $08 $09 $08 $FF
.db $FF $09 $08 $09 $0A $07 $0A $FF $FF $09 $0A $07 $08 $09 $0A $FF

; 2nd entry of Pointer Table from 145D (indexed by _RAM_C6AD_)
; Data from 1563 to 15A2 (64 bytes)
_DATA_1563_:
.db $FF $07 $08 $09 $0A $07 $0B $FF $FF $08 $0A $0B $08 $09 $09 $FF
.db $FF $0A $08 $0B $0A $07 $07 $FF $FF $0B $0A $07 $09 $09 $0A $FF
.db $FF $07 $08 $09 $0B $07 $08 $FF $FF $09 $0B $07 $07 $09 $0A $FF
.db $FF $07 $08 $09 $0A $0B $08 $FF $FF $0B $0A $07 $08 $09 $0A $FF

; 3rd entry of Pointer Table from 145D (indexed by _RAM_C6AD_)
; Data from 15A3 to 15E2 (64 bytes)
_DATA_15A3_:
.db $FF $07 $08 $09 $0A $0B $0C $FF $FF $0A $09 $0B $0C $0A $09 $FF
.db $FF $0C $0B $07 $08 $08 $07 $FF $FF $07 $08 $09 $0A $0B $0C $FF
.db $FF $0B $0C $07 $08 $09 $0A $FF $FF $09 $08 $09 $0C $0B $08 $FF
.db $FF $07 $0B $0A $07 $09 $0C $FF $FF $09 $0A $0B $0C $07 $08 $FF

_LABEL_15E3_:
	ld hl, v_entities
	ld de, v_entities + 1
	ld (hl), $00
	call ldi128
	call ldi128
	call ldi128
	jp ldi127

_LABEL_15F7_:
	ld de, $3A54
	rst $08	; setVdpAddress
	ld a, $10
	ex af, af'
	ld a, (_RAM_C6CC_)
	add a, a
	add a, a
	add a, $AE
	call +
	ex af, af'
	ld de, $3A94
	rst $08	; setVdpAddress
	ld a, $10
	ex af, af'
	call +
	ld de, $3A68
	rst $08	; setVdpAddress
	ld a, $10
	ex af, af'
	ld a, (_RAM_C6CD_)
	add a, a
	add a, a
	add a, $AE
	call +
	ex af, af'
	ld de, $3AA8
	rst $08	; setVdpAddress
	ld a, $10
	ex af, af'
+:
	out (Port_VDPData), a
	inc a
	ex af, af'
	jr +

+:
	out (Port_VDPData), a
	ex af, af'
	jr +

+:
	out (Port_VDPData), a
	inc a
	ex af, af'
	jr +

+:
	out (Port_VDPData), a
	ex af, af'
	ret

; 1st entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1641_:
	ld a, $02
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

; 3rd entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1649_:
	call _LABEL_12C8_
	call _LABEL_15E3_
	call _LABEL_12D1_
	ld a, $02
	ld (_RAM_FFFF_), a
	ld a, $01
	call _LABEL_12E2_
	ld hl, _DATA_8000_
	ld de, $37C0
	ld a, $0F
	call _LABEL_746_
	ld de, $0000
	ld hl, _DATA_8899_
	call _LABEL_6D0_
	ld de, _RAM_CD00_
	ld hl, _DATA_86B3_
	call _LABEL_70C_
	ld hl, _RAM_CD00_
	ld de, $3844
	ld bc, $0A38
	call _LABEL_648_
	ld de, _RAM_CD00_
	ld hl, _DATA_87FD_
	call _LABEL_70C_
	ld hl, _RAM_CD00_
	ld de, $3AC2
	ld bc, $0C08
	call _LABEL_648_
	ld de, _RAM_CD00_
	ld hl, _DATA_8846_
	call _LABEL_70C_
	ld hl, _RAM_CD00_
	ld de, $3AF6
	ld bc, $0C08
	call _LABEL_648_
	xor a
	ld de, $3B4E
	call _LABEL_2885_
	ld a, $01
	ld de, $3BD8
	call _LABEL_2885_
	ld a, $03
	ld de, $3C58
	call _LABEL_2885_
	ld a, $02
	ld de, $3CDA
	call _LABEL_2885_
	ld a, $04
	ld de, $3DE6
	call _LABEL_2885_
	ld hl, _DATA_8012_
	call _LABEL_68F_
	ld hl, _DATA_86A1_
	call _LABEL_68F_
	ld hl, palette_DATA_9A15_
	call _LABEL_526_
	xor a
	ld (_RAM_D000_), a
	ld (_RAM_C00A_), a

	; Spawn arrow
	ld hl, initArrow
	ld (v_entities), hl

	ld a, $02
	ld (_RAM_C018_), a
	ei

	; Request main menu song
	ld a, $81
	ld (_RAM_DD04_), a

	ld hl, $0258
	ld (var.timer), hl
	ld a, $03
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

; 4th entry of Jump Table from 1BB (indexed by var.state)
updateMainMenuState:
	ld hl, (var.timer)
	ld a, l
	or h
	jr z, @timerEnded

	ld a, (var.input.player1.debounced)
	and JOY_FIREA | JOY_FIREB
	jr nz, @actionPressed

	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	
	; TODO: Tick and check timer
	call tickTimer
	jp nz, waitInterrupt_LABEL_181_

	; Audio fade out
	ld a, $0C
	ld (audioFadeOutTimer_RAM_DD0E_), a
	ld a, $04
	ld (audio_RAM_DD0F_), a

@timerEnded:
	ld a, (audioFadeOutTimer_RAM_DD0E_)
	or a
	jp nz, waitInterrupt_LABEL_181_

	call _LABEL_544_
	ld a, $04
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

@actionPressed:
	ld hl, (_RAM_C10E_)
	ld h, $00
	ld de, _DATA_1759_
	add hl, de
	ld a, (hl)
	ld (_RAM_C005_), a
	ld a, $06
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

; Data from 1759 to 175B (3 bytes)
_DATA_1759_:
.db $00 $04 $02

; 5th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_175C_:
	call _LABEL_12C8_
	ld a, (_RAM_C007_)
	bit 0, a
	jr nz, _LABEL_17AD_
	xor a
	ld (_RAM_C005_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_9A27_
	ld de, $2020
	call _LABEL_6D0_
	call _LABEL_1BA6_
	ld hl, _DATA_18CE_
	ld de, _RAM_C460_
	call ldi72
	xor a
	ld (_RAM_C699_), a
	ld (_RAM_C6A5_), a
	ld a, $02
	ld (var.pallete.shouldUpdate), a
	ld hl, $03C0
	ld (var.timer), hl
	ld a, $01
	ld (_RAM_C006_), a
	ld hl, _DATA_194E_
	ld (_RAM_C6D0_), hl
	xor a
	ld (_RAM_C6CE_), a
	ld a, $05
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

_LABEL_17AD_:
	ld a, $01
	ld (_RAM_C005_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_9AE9_
	ld de, $2020
	call _LABEL_6D0_
	call _LABEL_1CBD_
	call _LABEL_136D_
	ld hl, _DATA_1916_
	ld de, _RAM_C470_
	call ldi56
	xor a
	ld (_RAM_C699_), a
	ld (_RAM_C6A5_), a
	ld a, $02
	ld (var.pallete.shouldUpdate), a
	ld hl, $0708
	ld (var.timer), hl
	ld a, $04
	ld (_RAM_C006_), a
	ld hl, _DATA_1966_
	ld (_RAM_C6D0_), hl
	xor a
	ld (_RAM_C6CE_), a
	ld a, $05
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 6th entry of Jump Table from 1BB (indexed by var.state)
updateDemoState:
	in a, (Port_IOPort1)
	cpl
	and $30
	jr nz, _LABEL_1845_
	call tickTimer
	jp z, +
	ld a, (_RAM_C009_)
	or a
	jr nz, _LABEL_1864_
	ld a, (_RAM_C008_)
	or a
	jr nz, +
	call _LABEL_189C_
	call _LABEL_2E54_
	call _LABEL_2EF0_
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	call _LABEL_2DFE_
	jp waitInterrupt_LABEL_18D_

+:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C009_), a
	ld (_RAM_C6C2_), a
	ld (_RAM_C0A8_), a
	ld (_RAM_C006_), a
	ld hl, _RAM_C007_
	inc (hl)
	call _LABEL_544_
	ld a, $00
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

_LABEL_1845_:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C009_), a
	ld (_RAM_C6C2_), a
	ld (_RAM_C0A8_), a
	ld (_RAM_C006_), a
	ld hl, _RAM_C007_
	inc (hl)
	call _LABEL_544_
	ld a, $02
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

_LABEL_1864_:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C009_), a
	ld (_RAM_C6C2_), a
	ld (_RAM_C0A8_), a
	ld (_RAM_C006_), a
	ld a, $82
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A206_
	ld de, $2400
	call _LABEL_6D0_
	ld hl, _LABEL_2FB7_
	ld (_RAM_C120_), hl
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3B1B_
	ld (v_entities), hl
	jp waitInterrupt_LABEL_18D_

_LABEL_189C_:
	ld a, (_RAM_C6CE_)
	or a
	jr z, +
	dec a
	ld (_RAM_C6CE_), a
	ld a, (_RAM_C6CF_)
	ld (var.input.player1.data), a
	xor a
	ld (var.input.player1.debounced), a
	ret

+:
	ld hl, (_RAM_C6D0_)
	ld a, (hl)
	ld (_RAM_C6CE_), a
	inc hl
	ld a, (_RAM_C6CF_)
	cpl
	ld c, a
	ld a, (hl)
	ld (_RAM_C6CF_), a
	inc hl
	ld (_RAM_C6D0_), hl
	ld (var.input.player1.data), a
	and c
	ld (var.input.player1.debounced), a
	ret

; Data from 18CE to 1915 (72 bytes)
_DATA_18CE_:
.db $FF $06 $01 $02 $06 $03 $01 $FF $FF $02 $06 $03 $03 $06 $06 $FF
.db $FF $03 $03 $04 $02 $06 $01 $FF $FF $04 $02 $06 $02 $01 $02 $FF
.db $FF $04 $06 $06 $04 $04 $06 $FF $FF $01 $05 $01 $05 $03 $02 $FF
.db $FF $06 $01 $02 $01 $06 $04 $FF $FF $02 $01 $03 $02 $04 $01 $FF
.db $FF $01 $03 $05 $03 $06 $06 $FF

; Data from 1916 to 194D (56 bytes)
_DATA_1916_:
.db $FF $00 $04 $06 $01 $02 $00 $FF $FF $04 $01 $01 $05 $01 $05 $FF
.db $FF $03 $04 $04 $02 $02 $06 $FF $FF $04 $02 $02 $06 $05 $04 $FF
.db $FF $06 $04 $01 $04 $05 $05 $FF $FF $04 $01 $05 $05 $04 $05 $FF
.db $FF $02 $04 $02 $03 $05 $02 $FF

; Data from 194E to 1965 (24 bytes)
_DATA_194E_:
.db $80 $00 $03 $20 $30 $00 $30 $02 $F0 $00 $03 $04 $30 $00 $30 $02
.db $F0 $00 $F0 $00 $F0 $00 $F0 $00

; Data from 1966 to 1995 (48 bytes)
_DATA_1966_:
.db $80 $00 $03 $20 $20 $00 $03 $20 $20 $00 $30 $02 $F0 $00 $80 $00
.db $03 $20 $20 $00 $03 $08 $20 $00 $30 $02 $A0 $00 $03 $08 $20 $00
.db $03 $20 $20 $00 $30 $02 $F0 $00 $F0 $00 $F0 $00 $F0 $00 $F0 $00

; 7th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1996_:
	call _LABEL_5EA_
	call _LABEL_12FD_
	ld a, $05
	ld de, _RAM_CD62_
	call _LABEL_289D_
	ld a, $06
	ld de, _RAM_CDEA_
	call _LABEL_289D_
	ld a, $07
	ld de, _RAM_CE44_
	call _LABEL_289D_
	call waitInterrupt_LABEL_181_
	ld de, $3B0A
	ld hl, _RAM_CD00_
	ld bc, $092C
	call _LABEL_65D_
	ld hl, _LABEL_2FF9_
	ld (v_entities), hl
	ld a, $07
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

; 8th entry of Jump Table from 1BB (indexed by var.state)
updateModeMenuState:
	ld a, (var.input.player1.debounced)
	and $30
	jr nz, +
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	jp waitInterrupt_LABEL_181_

+:
	ld a, (_RAM_C10E_)
	ld d, a
	ld a, (_RAM_C005_)
	and $06
	add a, d
	ld (_RAM_C005_), a
	ld a, $08
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

; 9th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_19F6_:
	call _LABEL_5EA_
	call _LABEL_12FD_
	ld hl, (_RAM_C005_)
	ld h, $00
	add hl, hl
	ld de, _DATA_1A0B_
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	jp (hl)

; Jump Table from 1A0B to 1A16 (6 entries, indexed by _RAM_C005_)
_DATA_1A0B_:
.dw _LABEL_1A17_ _LABEL_1A5F_ _LABEL_1A90_ _LABEL_1ACF_ _LABEL_1B0E_ _LABEL_1B3F_

; 1st entry of Jump Table from 1A0B (indexed by _RAM_C005_)
_LABEL_1A17_:
	ld a, $08
	ld de, _RAM_CD58_
	call _LABEL_289D_
	ld a, $09
	ld de, _RAM_CDD0_
	call _LABEL_289D_
	ld a, $0A
	ld de, _RAM_CE54_
	call _LABEL_289D_
	ld a, $0B
	ld de, $CED6
	call _LABEL_289D_
	ld a, $0C
	ld de, _RAM_CF56_
	call _LABEL_289D_
	ld hl, _LABEL_3030_
	ld (v_entities), hl
_LABEL_1A45_:
	call waitInterrupt_LABEL_181_
	ld de, $3AC0
	ld hl, _RAM_CD00_
	ld bc, $0C40
	call _LABEL_65D_
	call updateEntities_LABEL_508_
	ld a, $09
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

; 2nd entry of Jump Table from 1A0B (indexed by _RAM_C005_)
_LABEL_1A5F_:
	ld a, $08
	ld de, $CD58
	call _LABEL_289D_
	ld a, $09
	ld de, $CDD0
	call _LABEL_289D_
	ld a, $0A
	ld de, $CE54
	call _LABEL_289D_
	ld a, $12
	ld de, $CED6
	call _LABEL_289D_
	ld a, $0C
	ld de, $CF56
	call _LABEL_289D_
	ld hl, _LABEL_3030_
	ld (v_entities), hl
	jp _LABEL_1A45_

; 3rd entry of Jump Table from 1A0B (indexed by _RAM_C005_)
_LABEL_1A90_:
	ld a, $08
	ld de, $CD58
	call _LABEL_289D_
	ld a, $09
	ld de, $CDD6
	call _LABEL_289D_
	ld a, $0A
	ld de, $CE1A
	call _LABEL_289D_
	ld a, $0B
	ld de, $CE5C
	call _LABEL_289D_
	ld a, $10
	ld de, $CEDC
	call _LABEL_289D_
	ld a, $0C
	ld de, $CF5C
	call _LABEL_289D_
	ld hl, _LABEL_312A_
	ld (v_entities), hl
	ld hl, _LABEL_31F5_
	ld (_RAM_C120_), hl
	jp _LABEL_1A45_

; 4th entry of Jump Table from 1A0B (indexed by _RAM_C005_)
_LABEL_1ACF_:
	ld a, $08
	ld de, $CD58
	call _LABEL_289D_
	ld a, $09
	ld de, $CDD6
	call _LABEL_289D_
	ld a, $0A
	ld de, $CE1A
	call _LABEL_289D_
	ld a, $12
	ld de, $CE5C
	call _LABEL_289D_
	ld a, $11
	ld de, _RAM_CED6_
	call _LABEL_289D_
	ld a, $0C
	ld de, $CF5C
	call _LABEL_289D_
	ld hl, _LABEL_312A_
	ld (v_entities), hl
	ld hl, _LABEL_31F5_
	ld (_RAM_C120_), hl
	jp _LABEL_1A45_

; 5th entry of Jump Table from 1A0B (indexed by _RAM_C005_)
_LABEL_1B0E_:
	ld a, $08
	ld de, $CD58
	call _LABEL_289D_
	ld a, $09
	ld de, $CDD0
	call _LABEL_289D_
	ld a, $0A
	ld de, $CE54
	call _LABEL_289D_
	ld a, $0B
	ld de, $CED6
	call _LABEL_289D_
	ld a, $0C
	ld de, $CF56
	call _LABEL_289D_
	ld hl, _LABEL_3030_
	ld (v_entities), hl
	jp _LABEL_1A45_

; 6th entry of Jump Table from 1A0B (indexed by _RAM_C005_)
_LABEL_1B3F_:
	ld a, $08
	ld de, $CD58
	call _LABEL_289D_
	ld a, $09
	ld de, $CDD0
	call _LABEL_289D_
	ld a, $0A
	ld de, $CE54
	call _LABEL_289D_
	ld a, $12
	ld de, $CED6
	call _LABEL_289D_
	ld a, $0C
	ld de, $CF56
	call _LABEL_289D_
	ld hl, _LABEL_3030_
	ld (v_entities), hl
	jp _LABEL_1A45_

; 10th entry of Jump Table from 1BB (indexed by var.state)
updateOptionsMenuState:
	ld hl, (v_entities)
	ld de, (_RAM_C120_)
	ld a, l
	or h
	or e
	or d
	jr z, +
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	jp waitInterrupt_LABEL_181_

+:
	ld a, $0C
	ld (audioFadeOutTimer_RAM_DD0E_), a
	ld a, $04
	ld (audio_RAM_DD0F_), a
	call _LABEL_544_
	ld a, (_RAM_C005_)
	add a, $0A
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

; Data from 1B9E to 1BA5 (8 bytes)
.db $E8 $00 $ED $00 $E8 $00 $F3 $00

; 11th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1BA6_:
	call _LABEL_12C8_
	call _LABEL_12D1_
	ld a, $02
	ld (_RAM_FFFF_), a
	ld a, $01
	call _LABEL_12E2_
	ld a, $0F
	call _LABEL_12EB_
	call _LABEL_1317_
	call _LABEL_136D_
	ld hl, $0000
	ld (_RAM_C6BE_), hl
	ld hl, _DATA_9F05_
	ld de, $21E0
	call _LABEL_6D0_
	ld hl, _DATA_A020_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_C1A7_
	ld de, $0000
	call _LABEL_6D0_
	ld hl, _DATA_C435_
	ld de, $2800
	call _LABEL_6D0_
	ld hl, _DATA_C012_
	ld de, $3800
	call _LABEL_6D0_
	ld hl, _LABEL_3599_
	ld (v_entities), hl
	ld hl, $32F7
	ld (_RAM_C2A0_), hl
	ld hl, $332D
	ld (_RAM_C2C0_), hl
	ld hl, $3363
	ld (_RAM_C2E0_), hl
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_8012_
	call _LABEL_68F_
	ld hl, palette_DATA_9A15_
	call _LABEL_68F_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_C000_
	call _LABEL_526_
	ld a, $02
	ld (_RAM_C018_), a
	ei
	ld a, $83
	ld (_RAM_DD04_), a
	ld a, $0E
	ld (var.pallete.shouldUpdate), a
	ld hl, $003C
	ld (var.timer), hl
	ld a, $10
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 17th entry of Jump Table from 1BB (indexed by var.state)
updateGameplayState:
	ld a, (_RAM_C008_)
	or a
	jr nz, ++
	ld a, (_RAM_C002_)
	or a
	jr nz, +
	call _LABEL_2E54_
+:
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	jp waitInterrupt_LABEL_18D_

++:
	xor a
	ld (_RAM_C008_), a
	ld a, $85
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_9F8E_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, _LABEL_2FB7_
	ld (_RAM_C120_), hl
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3AC4_
	ld (v_entities), hl
	ld a, $18
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 25th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1C98_:
	xor a
	ld (_RAM_C002_), a
	ld a, (_RAM_C008_)
	or a
	jr nz, +
	call _LABEL_2E54_
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	jp waitInterrupt_LABEL_18D_

+:
	xor a
	ld (_RAM_C008_), a
	call _LABEL_544_
	ld a, $00
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 12th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1CBD_:
	call _LABEL_12C8_
	call _LABEL_12D1_
	ld a, $02
	ld (_RAM_FFFF_), a
	ld a, $01
	call _LABEL_12E2_
	ld a, $0F
	call _LABEL_12EB_
	call _LABEL_1317_
	call _LABEL_1399_
	ld hl, _DATA_83C2_
	ld de, $15C0
	ld a, $01
	call _LABEL_746_
	ld hl, _DATA_9F05_
	ld de, $21E0
	call _LABEL_6D0_
	ld hl, _DATA_A020_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_C612_
	ld de, $0000
	call _LABEL_6D0_
	ld hl, _DATA_C463_
	ld de, $3800
	call _LABEL_6D0_
	ld hl, _LABEL_3599_
	ld (v_entities), hl
	ld hl, $404D
	ld (_RAM_C140_), hl
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_8012_
	call _LABEL_68F_
	ld hl, palette_DATA_9A15_
	call _LABEL_68F_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_C451_
	call _LABEL_526_
	ld a, $02
	ld (_RAM_C018_), a
	ei
	ld a, $88
	ld (_RAM_DD04_), a
	xor a
	ld (_RAM_C699_), a
	ld (_RAM_C6A5_), a
	ld (_RAM_C6C3_), a
	ld (_RAM_C6C4_), a
	ld (_RAM_C6C5_), a
	ld a, $12
	ld (var.pallete.shouldUpdate), a
	ld hl, $003C
	ld (var.timer), hl
	ld a, $11
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 18th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1D64_:
	ld a, (_RAM_C008_)
	or a
	jr nz, +
	ld a, (_RAM_C009_)
	or a
	jr nz, _LABEL_1DB8_
	call _LABEL_2EF0_
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	call _LABEL_2DFE_
	jp waitInterrupt_LABEL_18D_

+:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C6C2_), a
	ld (_RAM_C0A8_), a
	ld a, $85
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_9F8E_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, _LABEL_2FB7_
	ld (_RAM_C120_), hl
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3AC4_
	ld (v_entities), hl
	ld a, $19
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

_LABEL_1DB8_:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C6C2_), a
	ld (_RAM_C0A8_), a
	ld a, $82
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A206_
	ld de, $2400
	call _LABEL_6D0_
	ld hl, _LABEL_2FB7_
	ld (_RAM_C120_), hl
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3B1B_
	ld (v_entities), hl
	ld a, $19
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 26th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1DEF_:
	xor a
	ld (_RAM_C002_), a
	ld a, (_RAM_C008_)
	or a
	jr nz, ++
	ld a, (_RAM_C002_)
	or a
	jr nz, +
	call _LABEL_2EF0_
+:
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	call _LABEL_2DFE_
	jp waitInterrupt_LABEL_18D_

++:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C009_), a
	call _LABEL_544_
	ld a, $00
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 23rd entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1E20_:
	call _LABEL_12C8_
	call _LABEL_12D1_
	ld a, $02
	ld (_RAM_FFFF_), a
	call _LABEL_12E0_
	ld de, $38DE
	ld hl, _DATA_1E71_
	ld bc, $0008
	call _LABEL_604_
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_8012_
	call _LABEL_526_
	ld a, $02
	ld (_RAM_C018_), a
	ei
	ld hl, $003C
	ld (var.timer), hl
	ld a, $17
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

; 24th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1E59_:
	ld a, (var.input.player1.debounced)
	and $30
	jr nz, +
	call tickTimer
	jp nz, waitInterrupt_LABEL_181_
+:
	call _LABEL_544_
	ld a, $00
	ld (var.state), a
	jp waitInterrupt_LABEL_181_

; Data from 1E71 to 1E78 (8 bytes)
_DATA_1E71_:
.db $EE $00 $F5 $00 $E4 $00 $F1 $00

; 13th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1E79_:
	call _LABEL_12C8_
	call _LABEL_12D1_
	ld a, $02
	ld (_RAM_FFFF_), a
	ld a, $01
	call _LABEL_12E2_
	call _LABEL_1317_
	call _LABEL_136D_
	xor a
	ld (_RAM_C6CC_), a
	ld (_RAM_C6CD_), a
	ld hl, _DATA_83C2_
	ld de, $15C0
	ld a, $01
	call _LABEL_746_
	ld hl, _DATA_9F05_
	ld de, $21E0
	call _LABEL_6D0_
	ld hl, _DATA_A020_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_CFED_
	ld de, $0000
	call _LABEL_6D0_
	ld hl, _DATA_CD35_
	ld de, $3800
	call _LABEL_6D0_
	call _LABEL_15F7_
	ld hl, _LABEL_3599_
	ld (v_entities), hl
	ld hl, _LABEL_3B74_
	ld (_RAM_C120_), hl
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_8012_
	call _LABEL_68F_
	ld hl, palette_DATA_9A15_
	call _LABEL_68F_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_CD23_
	call _LABEL_526_
	ld a, $02
	ld (_RAM_C018_), a
	ei
	ld a, $86
	ld (_RAM_DD04_), a
	ld a, $03
	ld (var.palette._RAM_C022_), a
	ld hl, $003C
	ld (var.timer), hl
	ld a, $12
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

; 19th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1F15_:
	ld a, (_RAM_C008_)
	or a
	jr nz, +
	ld a, (_RAM_C009_)
	or a
	jp nz, _LABEL_1FA9_
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	jp waitInterrupt_LABEL_199_

+:
	xor a
	ld (_RAM_C008_), a
	ld a, (_RAM_C6B2_)
	add a, $02
	ld d, a
	ld a, (_RAM_C6CD_)
	inc a
	ld (_RAM_C6CD_), a
	cp d
	jr nc, +
	call _LABEL_15F7_
	ld a, $85
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A0A2_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, $2FB7
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3B41_
	ld (v_entities), hl
	ld hl, _LABEL_4007_
	ld (_RAM_C120_), hl
	ld a, $1A
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

+:
	call _LABEL_15F7_
	ld a, $85
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A164_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, $2FB7
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_4023_
	ld (v_entities), hl
	ld hl, _LABEL_3B5E_
	ld (_RAM_C120_), hl
	ld a, $1A
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

_LABEL_1FA9_:
	xor a
	ld (_RAM_C009_), a
	ld a, (_RAM_C6B2_)
	add a, $02
	ld d, a
	ld a, (_RAM_C6CC_)
	inc a
	ld (_RAM_C6CC_), a
	cp d
	jr nc, +
	call _LABEL_15F7_
	ld a, $85
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A0A2_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, $2FB7
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3B33_
	ld (v_entities), hl
	ld hl, _LABEL_4015_
	ld (_RAM_C120_), hl
	ld a, $1A
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

+:
	call _LABEL_15F7_
	ld a, $85
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A164_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, $2FB7
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3B4F_
	ld (v_entities), hl
	ld hl, _LABEL_4032_
	ld (_RAM_C120_), hl
	ld a, $1A
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

; 27th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_2027_:
	xor a
	ld (_RAM_C002_), a
	ld a, (_RAM_C008_)
	or a
	jr nz, +
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	jp waitInterrupt_LABEL_199_

+:
	xor a
	ld (_RAM_C008_), a
	ld a, (_RAM_C6B2_)
	add a, $02
	ld d, a
	ld a, (_RAM_C6CC_)
	cp d
	jr nc, _LABEL_2091_
	ld a, (_RAM_C6CD_)
	cp d
	jr nc, _LABEL_2091_
	xor a
	ld (_RAM_C009_), a
	call _LABEL_5EA_
	call _LABEL_1317_
	call _LABEL_136D_
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A020_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, _LABEL_3599_
	ld (v_entities), hl
	ld hl, _LABEL_3B74_
	ld (_RAM_C120_), hl
	ld a, $86
	ld (_RAM_DD04_), a
	ld a, $03
	ld (var.palette._RAM_C022_), a
	ld hl, $003C
	ld (var.timer), hl
	ld a, $12
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

_LABEL_2091_:
	call _LABEL_544_
	ld a, $00
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

; 14th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_209C_:
	call _LABEL_12C8_
	call _LABEL_12D1_
	ld a, $02
	ld (_RAM_FFFF_), a
	ld a, $01
	call _LABEL_12E2_
	ld a, $0F
	call _LABEL_12EB_
	call _LABEL_1317_
	call _LABEL_1399_
	call _LABEL_13F8_
	xor a
	ld (_RAM_C6CC_), a
	ld (_RAM_C6CD_), a
	ld hl, _DATA_83C2_
	ld de, $15C0
	ld a, $01
	call _LABEL_746_
	ld hl, _DATA_9F05_
	ld de, $21E0
	call _LABEL_6D0_
	ld hl, _DATA_A020_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_D9A0_
	ld de, $0000
	call _LABEL_6D0_
	ld hl, _DATA_D6F2_
	ld de, $3800
	call _LABEL_6D0_
	call _LABEL_15F7_
	ld hl, _LABEL_3599_
	ld (v_entities), hl
	ld hl, _LABEL_3B74_
	ld (_RAM_C120_), hl
	ld hl, $40CF
	ld (_RAM_C140_), hl
	ld hl, $4146
	ld (_RAM_C160_), hl
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_8012_
	call _LABEL_68F_
	ld hl, palette_DATA_9A15_
	call _LABEL_68F_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_D6E0_
	call _LABEL_526_
	ld a, $02
	ld (_RAM_C018_), a
	ei
	ld a, $88
	ld (_RAM_DD04_), a
	xor a
	ld (_RAM_C699_), a
	ld (_RAM_C6A5_), a
	ld (_RAM_C6C4_), a
	ld a, $60
	ld (_RAM_C6C3_), a
	ld a, (_RAM_C6B3_)
	add a, $03
	ld (_RAM_C6C5_), a
	ld a, $20
	ld (var.pallete.shouldUpdate), a
	ld a, $03
	ld (var.palette._RAM_C022_), a
	ld hl, $003C
	ld (var.timer), hl
	ld a, $13
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

; 20th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_2168_:
	ld hl, (_RAM_C6C4_)
	ld a, l
	or h
	jp z, _LABEL_221C_
	ld a, (_RAM_C008_)
	or a
	jr nz, ++
	ld a, (_RAM_C009_)
	or a
	jr nz, _LABEL_21D5_
	ld a, (_RAM_C002_)
	or a
	jr nz, +
	call _LABEL_2EF0_
+:
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	jp waitInterrupt_LABEL_199_

++:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C6C2_), a
	ld a, (_RAM_C6CD_)
	inc a
	ld (_RAM_C6CD_), a
	cp $09
	jr nc, _LABEL_221C_
	call _LABEL_15F7_
	ld a, $85
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A0A2_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, $2FB7
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3B41_
	ld (v_entities), hl
	ld hl, _LABEL_4007_
	ld (_RAM_C120_), hl
	ld a, $1B
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

_LABEL_21D5_:
	xor a
	ld (_RAM_C009_), a
	ld (_RAM_C6C2_), a
	ld a, (_RAM_C6CC_)
	inc a
	ld (_RAM_C6CC_), a
	cp $09
	jr nc, _LABEL_221C_
	call _LABEL_15F7_
	ld a, $85
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A0A2_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, $2FB7
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3B33_
	ld (v_entities), hl
	ld hl, _LABEL_4015_
	ld (_RAM_C120_), hl
	ld a, $1B
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

_LABEL_221C_:
	xor a
	ld (_RAM_C009_), a
	ld (_RAM_C6C2_), a
	call _LABEL_15F7_
	ld a, $85
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A164_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld a, (_RAM_C6CC_)
	ld d, a
	ld a, (_RAM_C6CD_)
	cp d
	jr z, ++
	jr nc, +
	ld hl, $2FB7
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3B4F_
	ld (v_entities), hl
	ld hl, _LABEL_4032_
	ld (_RAM_C120_), hl
	ld a, $1B
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

+:
	ld hl, $2FB7
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3B5E_
	ld (v_entities), hl
	ld hl, _LABEL_4023_
	ld (_RAM_C120_), hl
	ld a, $1B
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

++:
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A0A2_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, $2FB7
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3B65_
	ld (v_entities), hl
	ld hl, _LABEL_4039_
	ld (_RAM_C120_), hl
	ld a, $1B
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

; 28th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_22AE_:
	xor a
	ld (_RAM_C002_), a
	ld a, (_RAM_C008_)
	or a
	jr nz, +
	call _LABEL_2EF0_
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	jp waitInterrupt_LABEL_199_

+:
	ld hl, (_RAM_C6C4_)
	ld a, l
	or h
	jp z, _LABEL_2334_
	ld a, (_RAM_C6CC_)
	cp $09
	jr nc, _LABEL_2334_
	ld a, (_RAM_C6CD_)
	cp $09
	jr nc, _LABEL_2334_
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C009_), a
	call _LABEL_5EA_
	call _LABEL_1317_
	call _LABEL_1399_
	call _LABEL_13F8_
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A020_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, _LABEL_3599_
	ld (v_entities), hl
	ld hl, _LABEL_3B74_
	ld (_RAM_C120_), hl
	ld hl, $40CF
	ld (_RAM_C140_), hl
	ld hl, $4146
	ld (_RAM_C160_), hl
	ld a, $88
	ld (_RAM_DD04_), a
	xor a
	ld (_RAM_C699_), a
	ld (_RAM_C6A5_), a
	ld a, $03
	ld (var.palette._RAM_C022_), a
	ld hl, $003C
	ld (var.timer), hl
	ld a, $13
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

_LABEL_2334_:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C009_), a
	call _LABEL_544_
	ld a, $00
	ld (var.state), a
	jp waitInterrupt_LABEL_199_

; 15th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_2346_:
	call _LABEL_12C8_
	call _LABEL_12D1_
	ld a, $02
	ld (_RAM_FFFF_), a
	ld a, $01
	call _LABEL_12E2_
	ld a, $0F
	call _LABEL_12EB_
	call _LABEL_1317_
	ld a, (_RAM_C6AC_)
	ld (_RAM_C6A5_), a
	call _LABEL_136D_
	ld hl, _DATA_9F05_
	ld de, $21E0
	call _LABEL_6D0_
	ld hl, _DATA_A020_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_E4AD_
	ld de, $0000
	call _LABEL_6D0_
	ld hl, _DATA_C435_
	ld de, $2800
	call _LABEL_6D0_
	ld hl, _DATA_E2AE_
	ld de, $3800
	call _LABEL_6D0_
	ld hl, _LABEL_3599_
	ld (v_entities), hl
	ld hl, $32F7
	ld (_RAM_C2A0_), hl
	ld hl, $332D
	ld (_RAM_C2C0_), hl
	ld hl, $3363
	ld (_RAM_C2E0_), hl
	xor a
	ld (_RAM_C69A_), a
	ld (_RAM_C6A6_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_8012_
	call _LABEL_68F_
	ld hl, palette_DATA_9A15_
	call _LABEL_68F_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_E29C_
	call _LABEL_526_
	ld a, $02
	ld (_RAM_C018_), a
	ei
	ld a, $86
	ld (_RAM_DD04_), a
	ld a, $0E
	ld (var.pallete.shouldUpdate), a
	ld a, $70
	ld (var.palette._RAM_C022_), a
	ld hl, $003C
	ld (var.timer), hl
	ld a, $14
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 21st entry of Jump Table from 1BB (indexed by var.state)
_LABEL_23F9_:
	ld a, (_RAM_C008_)
	or a
	jr nz, +
	ld a, (_RAM_C009_)
	or a
	jr nz, +
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	jp waitInterrupt_LABEL_18D_

+:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C009_), a
	ld a, $85
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_9F8E_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, _LABEL_2FB7_
	ld (_RAM_C120_), hl
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3AC4_
	ld (v_entities), hl
	ld a, $1C
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 29th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_2444_:
	xor a
	ld (_RAM_C002_), a
	ld a, (_RAM_C008_)
	or a
	jr nz, +
	call _LABEL_2E54_
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	jp waitInterrupt_LABEL_18D_

+:
	xor a
	ld (_RAM_C008_), a
	call _LABEL_544_
	ld a, $00
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 16th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_2469_:
	call _LABEL_12C8_
	call _LABEL_12D1_
	ld a, $02
	ld (_RAM_FFFF_), a
	ld a, $01
	call _LABEL_12E2_
	call _LABEL_1317_
	call _LABEL_1399_
	ld hl, _DATA_83C2_
	ld de, $15C0
	ld a, $01
	call _LABEL_746_
	ld hl, _DATA_9F05_
	ld de, $21E0
	call _LABEL_6D0_
	ld hl, _DATA_A020_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_EAF9_
	ld de, $0000
	call _LABEL_6D0_
	ld hl, _DATA_E8E0_
	ld de, $3800
	call _LABEL_6D0_
	ld hl, _LABEL_3599_
	ld (v_entities), hl
	ld hl, $404D
	ld (_RAM_C140_), hl
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_8012_
	call _LABEL_68F_
	ld hl, palette_DATA_9A15_
	call _LABEL_68F_
	ld a, $03
	ld (_RAM_FFFF_), a
	ld hl, _DATA_E8CE_
	call _LABEL_526_
	ld a, $02
	ld (_RAM_C018_), a
	ei
	ld a, $88
	ld (_RAM_DD04_), a
	xor a
	ld (_RAM_C699_), a
	ld (_RAM_C6A5_), a
	ld (_RAM_C6C3_), a
	ld (_RAM_C6C4_), a
	ld (_RAM_C6C5_), a
	ld a, $52
	ld (var.pallete.shouldUpdate), a
	ld a, $10
	ld (var.palette._RAM_C022_), a
	ld hl, $003C
	ld (var.timer), hl
	ld a, $15
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 22nd entry of Jump Table from 1BB (indexed by var.state)
_LABEL_2510_:
	ld a, (_RAM_C008_)
	or a
	jr nz, ++
	ld a, (_RAM_C009_)
	or a
	jr nz, _LABEL_256A_
	ld a, (_RAM_C002_)
	or a
	jr nz, +
	call _LABEL_2EF0_
+:
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	call _LABEL_2DFE_
	jp waitInterrupt_LABEL_18D_

++:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C6C2_), a
	ld (_RAM_C0A8_), a
	ld a, $85
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_9F8E_
	ld de, $2400
	ld a, $0F
	call _LABEL_746_
	ld hl, _LABEL_2FB7_
	ld (_RAM_C120_), hl
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3AC4_
	ld (v_entities), hl
	ld a, $1D
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

_LABEL_256A_:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C6C2_), a
	ld (_RAM_C0A8_), a
	ld a, $82
	ld (_RAM_DD04_), a
	ld a, $02
	ld (_RAM_FFFF_), a
	ld hl, _DATA_A206_
	ld de, $2400
	call _LABEL_6D0_
	ld hl, _LABEL_2FB7_
	ld (_RAM_C120_), hl
	ld (_RAM_C140_), hl
	ld (_RAM_C160_), hl
	ld hl, _LABEL_3B1B_
	ld (v_entities), hl
	ld a, $1D
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

; 30th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_25A1_:
	xor a
	ld (_RAM_C002_), a
	ld a, (_RAM_C008_)
	or a
	jr nz, +
	call _LABEL_2EF0_
	call updateEntities_LABEL_508_
	call drawEntities_LABEL_25CC_
	call _LABEL_2DFE_
	jp waitInterrupt_LABEL_18D_

+:
	xor a
	ld (_RAM_C008_), a
	ld (_RAM_C009_), a
	call _LABEL_544_
	ld a, $00
	ld (var.state), a
	jp waitInterrupt_LABEL_18D_

drawEntities_LABEL_25CC_:
	xor a
	ld (_RAM_C3C0_), a
	ld ix, v_entities
	ld de, $0020
	ld b, $10
-:
	push bc
	push de
	call +
	pop de
	add ix, de
	pop bc
	djnz -
	ld a, (_RAM_C3C0_)
	ld e, a
	ld d, $00
	ld hl, $C300
	add hl, de
	ld (hl), $D0
	ret

+:
	ld a, (ix+2)
	or a
	ret z
	ld a, (ix+3)
	or a
	ret z
	ld e, a
	ld d, $00
	ld hl, _DATA_265A_ - 2
	add hl, de
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld c, (hl)
	ld b, $00
	inc hl
	ld a, (_RAM_C3C0_)
	add a, c
	cp $41
	ret nc
	push hl
	ld a, (_RAM_C3C0_)
	ld e, a
	ld d, $00
	ld hl, $C300
	add hl, de
	ex de, hl
	pop hl
	ld b, c
-:
	ld a, (hl)
	add a, (ix+6)
	cp $D0
	jr nz, +
	inc a
+:
	ld (de), a
	inc hl
	inc de
	djnz -
	ld a, (_RAM_C3C0_)
	add a, c
	cp $40
	jr z, +
	ld a, $D0
	ld (de), a
+:
	push hl
	ld a, (_RAM_C3C0_)
	ld e, a
	ld d, $00
	ld hl, $C340
	add hl, de
	add hl, de
	ex de, hl
	pop hl
	add a, c
	ld (_RAM_C3C0_), a
	ld b, c
-:
	ld a, (hl)
	add a, (ix+9)
	ld (de), a
	inc hl
	inc de
	ldi
	djnz -
	ret

; Data from 2658 to 2659 (2 bytes)
.db $00 $00

; Pointer Table from 265A to 2699 (32 entries, indexed by _RAM_C103_)
_DATA_265A_:
.dw _DATA_269A_ _DATA_269E_ _DATA_26B7_ _DATA_26D0_ _DATA_26E9_ _DATA_26F0_ _DATA_26F7_ _DATA_26FE_
.dw _DATA_2705_ _DATA_270C_ _DATA_2713_ _DATA_271A_ _DATA_2721_ _DATA_2728_ _DATA_272F_ _DATA_2733_
.dw _DATA_2737_ _DATA_2744_ _DATA_2751_ _DATA_2755_ _DATA_276E_ _DATA_2787_ _DATA_27C4_ _DATA_27FB_
.dw _DATA_2868_ _DATA_2881_ _DATA_2881_ _DATA_2881_ _DATA_2881_ _DATA_2881_ _DATA_2881_ _DATA_2881_

; 1st entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 269A to 269D (4 bytes)
_DATA_269A_:
.db $01 $FF $00 $BF

; 2nd entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 269E to 26B6 (25 bytes)
_DATA_269E_:
.db $08 $00 $23 $41 $67 $83 $A2 $C9 $E1 $E0 $40 $20 $43 $00 $41 $50
.db $40 $32 $42 $97 $44 $C5 $41 $1B $43

; 3rd entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26B7 to 26CF (25 bytes)
_DATA_26B7_:
.db $08 $18 $33 $50 $79 $9D $B2 $D1 $EE $33 $43 $E6 $40 $53 $41 $28
.db $42 $CD $41 $A3 $44 $6A $40 $10 $42

; 4th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26D0 to 26E8 (25 bytes)
_DATA_26D0_:
.db $08 $02 $28 $3D $63 $7B $A7 $BA $E4 $74 $44 $37 $40 $DD $42 $02
.db $41 $B9 $43 $FB $42 $51 $44 $A6 $40

; 5th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26E9 to 26EF (7 bytes)
_DATA_26E9_:
.db $02 $00 $08 $00 $AC $00 $AD

; 6th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26F0 to 26F6 (7 bytes)
_DATA_26F0_:
.db $02 $00 $08 $00 $AE $00 $AF

; 7th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26F7 to 26FD (7 bytes)
_DATA_26F7_:
.db $02 $00 $08 $00 $B0 $00 $B1

; 8th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26FE to 2704 (7 bytes)
_DATA_26FE_:
.db $02 $00 $08 $00 $B2 $00 $B3

; 9th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2705 to 270B (7 bytes)
_DATA_2705_:
.db $02 $00 $08 $00 $B4 $00 $B5

; 10th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 270C to 2712 (7 bytes)
_DATA_270C_:
.db $02 $00 $08 $00 $B6 $00 $B7

; 11th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2713 to 2719 (7 bytes)
_DATA_2713_:
.db $02 $00 $08 $00 $B8 $00 $B9

; 12th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 271A to 2720 (7 bytes)
_DATA_271A_:
.db $02 $00 $08 $00 $BA $00 $BB

; 13th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2721 to 2727 (7 bytes)
_DATA_2721_:
.db $02 $00 $08 $00 $BC $00 $BD

; 14th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2728 to 272E (7 bytes)
_DATA_2728_:
.db $02 $00 $08 $00 $BE $00 $BF

; 15th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 272F to 2732 (4 bytes)
_DATA_272F_:
.db $01 $00 $00 $0E

; 16th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2733 to 2736 (4 bytes)
_DATA_2733_:
.db $01 $00 $00 $14

; 17th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2737 to 2743 (13 bytes)
_DATA_2737_:
.db $04 $FC $FC $04 $04 $FC $15 $04 $16 $FC $17 $04 $18

; 18th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2744 to 2750 (13 bytes)
_DATA_2744_:
.db $04 $FC $FC $04 $04 $FC $19 $04 $1A $FC $1B $04 $1C

; 19th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2751 to 2754 (4 bytes)
_DATA_2751_:
.db $01 $FF $00 $BE

; 20th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2755 to 276D (25 bytes)
_DATA_2755_:
.db $08 $00 $00 $00 $00 $08 $08 $08 $08 $00 $20 $08 $21 $10 $22 $18
.db $23 $00 $24 $08 $25 $10 $26 $18 $27

; 21st entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 276E to 2786 (25 bytes)
_DATA_276E_:
.db $08 $00 $00 $00 $00 $08 $08 $08 $08 $00 $28 $08 $29 $10 $2A $18
.db $2B $00 $2C $08 $2D $10 $2E $18 $2F

; 22nd entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2787 to 27C3 (61 bytes)
_DATA_2787_:
.db $14 $00 $00 $00 $00 $00 $08 $08 $08 $08 $08 $10 $10 $10 $10 $10
.db $18 $18 $18 $18 $18 $00 $20 $08 $21 $10 $22 $18 $23 $20 $24 $00
.db $25 $08 $26 $10 $27 $18 $28 $20 $29 $00 $2A $08 $2B $10 $2C $18
.db $2D $20 $2E $00 $2F $08 $30 $10 $31 $18 $32 $20 $33

; 23rd entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 27C4 to 27FA (55 bytes)
_DATA_27C4_:
.db $12 $00 $00 $00 $00 $00 $00 $08 $08 $08 $08 $08 $08 $10 $10 $10
.db $10 $10 $10 $00 $20 $08 $21 $10 $22 $18 $23 $20 $24 $28 $25 $00
.db $26 $08 $27 $10 $28 $18 $29 $20 $2A $28 $2B $00 $2C $08 $2D $10
.db $2E $18 $2F $20 $30 $28 $31

; 24th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 27FB to 2867 (109 bytes)
_DATA_27FB_:
.db $24 $00 $00 $00 $00 $00 $00 $08 $08 $08 $08 $08 $08 $10 $10 $10
.db $10 $10 $10 $18 $18 $18 $18 $18 $18 $20 $20 $20 $20 $20 $20 $28
.db $28 $28 $28 $28 $28 $00 $20 $08 $20 $10 $20 $18 $20 $20 $20 $28
.db $21 $00 $20 $08 $22 $10 $23 $18 $24 $20 $25 $28 $26 $00 $20 $08
.db $27 $10 $28 $18 $29 $20 $2A $28 $26 $00 $2C $08 $2D $10 $2E $18
.db $2F $20 $30 $28 $31 $00 $32 $08 $33 $10 $34 $18 $35 $20 $36 $28
.db $37 $00 $26 $08 $26 $10 $26 $18 $26 $20 $26 $28 $26

; 25th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2868 to 2880 (25 bytes)
_DATA_2868_:
.db $08 $00 $00 $00 $00 $08 $08 $08 $08 $00 $30 $08 $31 $10 $32 $18
.db $33 $00 $34 $08 $35 $10 $36 $18 $37

; 26th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2881 to 2884 (4 bytes)
_DATA_2881_:
.db $01 $00 $00 $00

_LABEL_2885_:
	ld c, a
	rst $08	; setVdpAddress
	ld a, c
	add a, a
	ld e, a
	ld d, $00
	ld hl, _DATA_28BF_
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld b, (hl)
	inc hl
	ld c, Port_VDPData
-:
	outi
	jr nz, -
	ret

_LABEL_289D_:
	ld c, a
	ex af, af'
	ld b, $00
	ld hl, $28BF
	add hl, bc
	add hl, bc
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld c, (hl)
	ld b, $00
	inc hl
	push hl
	ld hl, $126B
	xor a
	sbc hl, bc
	sbc hl, bc
	push hl
	pop iy
	pop hl
	ld c, $BE
	jp (iy)

; Pointer Table from 28BF to 28E6 (20 entries, indexed by _RAM_C6A8_)
_DATA_28BF_:
.dw _DATA_28E7_ _DATA_290C_ _DATA_291D_ _DATA_292A_ _DATA_293B_ _DATA_2952_ _DATA_2969_ _DATA_297A_
.dw _DATA_2985_ _DATA_2994_ _DATA_29A9_ _DATA_29B6_ _DATA_29C1_ _DATA_29CA_ _DATA_29D7_ _DATA_29E4_
.dw _DATA_29F1_ _DATA_2A00_ _DATA_2A1B_ _DATA_2A24_

; 1st entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 28E7 to 290B (37 bytes)
_DATA_28E7_:
.db $24 $EF $00 $F1 $00 $E4 $00 $F2 $00 $F2 $00 $00 $00 $F2 $00 $F3
.db $00 $E0 $00 $F1 $00 $F3 $00 $00 $00 $E1 $00 $F4 $00 $F3 $00 $F3
.db $00 $EE $00 $ED $00

; 2nd entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 290C to 291C (17 bytes)
_DATA_290C_:
.db $10 $D7 $00 $00 $00 $EF $00 $EB $00 $E0 $00 $F8 $00 $E4 $00 $F1
.db $00

; 3rd entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 291D to 2929 (13 bytes)
_DATA_291D_:
.db $0C $F5 $00 $E4 $00 $F1 $00 $F2 $00 $F4 $00 $F2 $00

; 4th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 292A to 293A (17 bytes)
_DATA_292A_:
.db $10 $D8 $00 $00 $00 $EF $00 $EB $00 $E0 $00 $F8 $00 $E4 $00 $F1
.db $00

; 5th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 293B to 2951 (23 bytes)
_DATA_293B_:
.db $16 $FE $00 $00 $00 $F2 $00 $E4 $00 $E6 $00 $E0 $00 $00 $00 $D7
.db $00 $DF $00 $DF $00 $D6 $00

; 6th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2952 to 2968 (23 bytes)
_DATA_2952_:
.db $16 $F2 $00 $E4 $00 $EB $00 $E4 $00 $E2 $00 $F3 $00 $00 $00 $E6
.db $00 $E0 $00 $EC $00 $E4 $00

; 7th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2969 to 2979 (17 bytes)
_DATA_2969_:
.db $10 $EE $00 $F1 $00 $E8 $00 $E6 $00 $E8 $00 $ED $00 $E0 $00 $EB
.db $00

; 8th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 297A to 2984 (11 bytes)
_DATA_297A_:
.db $0A $E5 $00 $EB $00 $E0 $00 $F2 $00 $E7 $00

; 9th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2985 to 2993 (15 bytes)
_DATA_2985_:
.db $0E $EE $00 $EF $00 $F3 $00 $E8 $00 $EE $00 $ED $00 $F2 $00

; 10th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2994 to 29A8 (21 bytes)
_DATA_2994_:
.db $14 $E3 $00 $E8 $00 $E5 $00 $E5 $00 $E8 $00 $E2 $00 $F4 $00 $EB
.db $00 $F3 $00 $F8 $00

; 11th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29A9 to 29B5 (13 bytes)
_DATA_29A9_:
.db $0C $E1 $00 $EB $00 $EE $00 $E2 $00 $EA $00 $F2 $00

; 12th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29B6 to 29C0 (11 bytes)
_DATA_29B6_:
.db $0A $EB $00 $E4 $00 $F5 $00 $E4 $00 $EB $00

; 13th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29C1 to 29C9 (9 bytes)
_DATA_29C1_:
.db $08 $E4 $00 $F7 $00 $E8 $00 $F3 $00

; 14th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29CA to 29D6 (13 bytes)
_DATA_29CA_:
.db $0C $00 $00 $E4 $00 $E0 $00 $F2 $00 $F8 $00 $00 $00

; 15th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29D7 to 29E3 (13 bytes)
_DATA_29D7_:
.db $0C $ED $00 $EE $00 $F1 $00 $EC $00 $E0 $00 $EB $00

; 16th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29E4 to 29F0 (13 bytes)
_DATA_29E4_:
.db $0C $00 $00 $E7 $00 $E0 $00 $F1 $00 $E3 $00 $00 $00

; 17th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29F1 to 29FF (15 bytes)
_DATA_29F1_:
.db $0E $EC $00 $E0 $00 $F3 $00 $E2 $00 $E7 $00 $E4 $00 $F2 $00

; 18th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2A00 to 2A1A (27 bytes)
_DATA_2A00_:
.db $1A $EC $00 $E8 $00 $ED $00 $F4 $00 $F3 $00 $E4 $00 $F2 $00 $00
.db $00 $EC $00 $E0 $00 $F3 $00 $E2 $00 $E7 $00

; 19th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2A1B to 2A23 (9 bytes)
_DATA_2A1B_:
.db $08 $E7 $00 $E8 $00 $E6 $00 $E7 $00

; Data from 2A24 to 2A26 (3 bytes)
_DATA_2A24_:
.db $02 $00 $00

_LABEL_2A27_:
	xor a
	ld (_RAM_C6BB_), a
_LABEL_2A2B_:
	ld a, (hl)
	cp $FF
	ret z
	push bc
	push hl
--:
	ld (_RAM_C6B9_), hl
	ld b, $00
	ld a, (hl)
	cp $FF
	jr z, ++
	or a
	jr nz, _LABEL_2A41_
	add hl, de
	jr --

_LABEL_2A41_:
	add hl, de
	cp (hl)
	jr nz, +
	inc b
	jr _LABEL_2A41_

+:
	ld c, a
	ld a, b
	cp $02
	jr c, --
	dec b
	ld a, (_RAM_C6BB_)
	add a, b
	ld (_RAM_C6BB_), a
	inc b
	inc b
	push hl
	push de
	ld hl, (_RAM_C6B9_)
	ld de, $0098
	add hl, de
	pop de
-:
	ld (hl), c
	add hl, de
	djnz -
	pop hl
	jr --

++:
	pop hl
	pop bc
	add hl, bc
	jr _LABEL_2A2B_

_LABEL_2A6E_:
	ld l, (ix+16)
	ld h, (ix+17)
	ld de, _DATA_18_
	add hl, de
	ld a, (hl)
	or a
	ret z
	cp $FF
	ret z
	ld hl, $C418
	ld de, _RAM_C4B0_
	ld b, $90
	ld c, $00
-:
	cp (hl)
	jr nz, +
	ld (de), a
+:
	inc hl
	inc de
	djnz -
	ret

_LABEL_2A91_:
	ld a, (_RAM_C699_)
	bit 0, (ix+31)
	jr z, +
	ld a, (_RAM_C6A5_)
+:
	inc a
	ld d, a
	add a, a
	add a, d
	ld e, a
	ld d, $00
	ld hl, $0000
	ld b, $07
	ld a, (ix+28)
-:
	rrca
	jr nc, +
	add hl, de
+:
	ex de, hl
	add hl, hl
	ex de, hl
	djnz -
	ex de, hl
	ld hl, $0000
	ld a, (ix+27)
	inc a
	ld b, a
-:
	add hl, de
	djnz -
	ret

_LABEL_2AC2_:
	ld c, $10
	jr +

_LABEL_2AC6_:
	ld c, $11
	jr +

_LABEL_2ACA_:
	ld c, $12
	jr +

_LABEL_2ACE_:
	ld c, $13
	jr +

_LABEL_2AD2_:
	ld c, $00
+:
	ld hl, $0098
	add hl, de
	ex de, hl
	ld b, $90
-:
	ld a, (de)
	or a
	jr z, +
	ld (hl), c
+:
	inc hl
	inc de
	djnz -
	ret

_LABEL_2AE5_:
	ld hl, $0098
	add hl, de
	ex de, hl
	ld b, $90
-:
	ld a, (de)
	or a
	jr z, +
	ld (hl), a
+:
	inc hl
	inc de
	djnz -
	ret

_LABEL_2AF6_:
	ld b, $06
--:
	push bc
	push hl
	ld b, $11
	ld de, $FFF8
-:
	xor a
	cp (hl)
	jr z, +
	add hl, de
	djnz -
	jr +++

+:
	ex de, hl
	ld hl, $FFF8
	add hl, de
-:
	ld a, (hl)
	or a
	jr z, ++
	ld (de), a
	ld (hl), $00
	ld a, e
	sub $08
	jr nc, +
	dec d
+:
	ld e, a
++:
	ld a, l
	sub $08
	jr nc, +
	dec h
+:
	ld l, a
	djnz -
+++:
	pop hl
	inc hl
	pop bc
	djnz --
	ret

_LABEL_2B2A_:
	ld hl, _RAM_C4B0_
	jr +

; Data from 2B2F to 2B31 (3 bytes)
.db $21 $F8 $C5

+:
	ld bc, $9000
-:
	ld a, (hl)
	or a
	jr z, +
	inc c
+:
	inc hl
	djnz -
	ret

_LABEL_2B3E_:
	ex af, af'
	ld a, (_RAM_C005_)
	bit 1, a
	ret z
	ex af, af'
	ld e, a
	ld d, $00
	sub $03
	jr c, +
	and $07
	inc a
	ld d, a
+:
	ld a, e
	ld e, $00
	sub $02
	jr c, +
	and $07
	inc a
	ld e, a
+:
	bit 0, (ix+31)
	jr nz, ++
	ld a, (_RAM_C69A_)
	sub e
	jr nc, +
	xor a
+:
	ld (_RAM_C69A_), a
	ld a, (_RAM_C6A7_)
	add a, e
	ld (_RAM_C6A7_), a
	ret

++:
	ld a, (_RAM_C6A6_)
	sub e
	jr nc, +
	xor a
+:
	ld (_RAM_C6A6_), a
	ld a, (_RAM_C69B_)
	add a, e
	ld (_RAM_C69B_), a
	ret

_LABEL_2B86_:
	ld a, (var.pallete.shouldUpdate)
	and $FE
	jr z, +
	rrca
	rrca
	call c, ++
	rrca
	call c, _LABEL_2BDE_
	rrca
	call c, _LABEL_2BFC_
	rrca
	call c, _LABEL_2C21_
	rrca
	call c, _LABEL_2C15_
	rrca
	call c, _LABEL_2C1B_
	rrca
+:
	ld a, (var.palette._RAM_C022_)
	or a
	ret z
	rrca
	call c, _LABEL_2C9C_
	rrca
	call c, _LABEL_2CB5_
	rrca
	rrca
	rrca
	call c, _LABEL_2CCE_
	rrca
	call c, _LABEL_2CE8_
	rrca
	call c, _LABEL_2D06_
	rrca
	ret

++:
	ex af, af'
	ld a, $52
	out (Port_VDPAddress), a
	ld a, $7D
	out (Port_VDPAddress), a
	ld a, (_RAM_C699_)
	add a, $D6
	jr +

+:
	out (Port_VDPData), a
	ld a, $10
	jr +

+:
	out (Port_VDPData), a
	ex af, af'
	ret

_LABEL_2BDE_:
	push af
	xor a
	ld hl, (_RAM_C697_)
	ld de, $3C88
	call _LABEL_2D1F_
	ld de, $3C92
	rst $08	; setVdpAddress
	ld a, (_RAM_C6B9_)
	add a, $D6
	out (Port_VDPData), a
	ld a, $10
	jr +

+:
	out (Port_VDPData), a
	pop af
	ret

_LABEL_2BFC_:
	push af
	ld a, (_RAM_C696_)
	ld hl, (_RAM_C694_)
	ld de, $3B46
	call _LABEL_2D1F_
	ld a, $D6
	out (Port_VDPData), a
	ld a, $00
	jr +

+:
	out (Port_VDPData), a
	pop af
	ret

_LABEL_2C15_:
	ld de, $3BD6
	jp +

_LABEL_2C1B_:
	ld de, $3B2A
	jp +

_LABEL_2C21_:
	ld de, $3B02
+:
	push af
	ld a, $10
	ex af, af'
	rst $08	; setVdpAddress
	exx
	ld a, (_RAM_C6C5_)
	and $F0
	rrca
	rrca
	add a, $AE
	call _LABEL_2C85_
	ld b, a
	ld a, (_RAM_C6C5_)
	and $0F
	add a, a
	add a, a
	add a, $AE
	call _LABEL_2C85_
	ld c, a
	in a, (Port_VDPData)
	in a, (Port_VDPData)
	ld a, (_RAM_C6C4_)
	and $F0
	rrca
	rrca
	add a, $AE
	call _LABEL_2C85_
	ld d, a
	ld a, (_RAM_C6C4_)
	and $0F
	add a, a
	add a, a
	add a, $AE
	call _LABEL_2C85_
	ld e, a
	exx
	ld hl, $0040
	add hl, de
	ex de, hl
	rst $08	; setVdpAddress
	exx
	ld a, b
	call _LABEL_2C85_
	ld a, c
	call _LABEL_2C85_
	jr +

+:
	in a, (Port_VDPData)
	nop
	jr +

+:
	in a, (Port_VDPData)
	ld a, d
	call _LABEL_2C85_
	ld a, e
	call _LABEL_2C85_
	pop af
	ret

_LABEL_2C85_:
	out (Port_VDPData), a
	inc a
	ex af, af'
	jr +

+:
	out (Port_VDPData), a
	ex af, af'
	jr +

+:
	jr +

+:
	out (Port_VDPData), a
	inc a
	ex af, af'
	jr +

+:
	out (Port_VDPData), a
	ex af, af'
	ret

_LABEL_2C9C_:
	ex af, af'
	ld a, $16
	out (Port_VDPAddress), a
	ld a, $7D
	out (Port_VDPAddress), a
	jr +

+:
	ld a, (_RAM_C699_)
	add a, $D6
	out (Port_VDPData), a
	jr +

+:
	xor a
	out (Port_VDPData), a
	ex af, af'
	ret

_LABEL_2CB5_:
	ex af, af'
	ld a, $28
	out (Port_VDPAddress), a
	ld a, $7D
	out (Port_VDPAddress), a
	jr +

+:
	ld a, (_RAM_C6A5_)
	add a, $D6
	out (Port_VDPData), a
	jr +

+:
	xor a
	out (Port_VDPData), a
	ex af, af'
	ret

_LABEL_2CCE_:
	ex af, af'
	ld a, $6C
	out (Port_VDPAddress), a
	ld a, $7D
	out (Port_VDPAddress), a
	ld a, (_RAM_C6A5_)
	add a, $D6
	jr +

+:
	out (Port_VDPData), a
	ld a, $10
	jr +

+:
	out (Port_VDPData), a
	ex af, af'
	ret

_LABEL_2CE8_:
	push af
	xor a
	ld hl, (_RAM_C6A3_)
	ld de, $3CB0
	call _LABEL_2D1F_
	ld de, $3CBA
	rst $08	; setVdpAddress
	ld a, (_RAM_C6B9_)
	add a, $D6
	out (Port_VDPData), a
	ld a, $10
	jr +

+:
	out (Port_VDPData), a
	pop af
	ret

_LABEL_2D06_:
	push af
	ld a, (_RAM_C6A2_)
	ld hl, (_RAM_C6A0_)
	ld de, $3B6E
	call _LABEL_2D1F_
	ld a, $D6
	out (Port_VDPData), a
	ld a, $00
	jr +

+:
	out (Port_VDPData), a
	pop af
	ret

_LABEL_2D1F_:
	push af
	rst $08	; setVdpAddress
	pop af
	call _LABEL_2D4D_
	ld hl, _RAM_C6B4_
	ld b, $06
	ld a, $10
	ex af, af'
	xor a
--:
	or (hl)
	jr z, ++
	ld a, $D6
	add a, (hl)
	out (Port_VDPData), a
	ex af, af'
	nop
	jp +

+:
	out (Port_VDPData), a
	ex af, af'
-:
	inc hl
	djnz --
	ret

++:
	in a, (Port_VDPData)
	nop
	jp +

+:
	in a, (Port_VDPData)
	xor a
	jr -

_LABEL_2D4D_:
	ld b, $04
-:
	add hl, hl
	rla
	djnz -
	push hl
	ld l, h
	ld h, a
	ld de, $186A
	call _LABEL_2DA5_
	cp $0A
	jr nc, +
	ld (_RAM_C6B4_), a
	ld de, $0271
	call _LABEL_2DA5_
	ld (_RAM_C6B5_), a
	add hl, hl
	add hl, hl
	add hl, hl
	add hl, hl
	pop de
	ld a, e
	rrca
	rrca
	rrca
	rrca
	or l
	ld l, a
	ld de, $03E8
	call _LABEL_2DA5_
	ld (_RAM_C6B6_), a
	ld de, $0064
	call _LABEL_2DA5_
	ld (_RAM_C6B7_), a
	ld de, $000A
	call _LABEL_2DA5_
	ld (_RAM_C6B8_), a
	ld a, l
	ld (_RAM_C6B9_), a
	ret

+:
	ld hl, $0909
	ld (_RAM_C6B4_), hl
	ld (_RAM_C6B6_), hl
	ld (_RAM_C6B8_), hl
	ret

_LABEL_2DA5_:
	xor a
-:
	or a
	sbc hl, de
	jr c, +
	inc a
	jr -

+:
	add hl, de
	ret

_LABEL_2DB0_:
	ld a, (_RAM_C6C2_)
	or a
	ret z
	ld a, (_RAM_C002_)
	or a
	ret nz
	ld a, (_RAM_C005_)
	bit 1, a
	jr nz, ++
	ld hl, _RAM_C6C3_
-:
	ld a, (hl)
	inc a
	daa
	ld (hl), a
	cp $60
	jp c, +
	ld (hl), $00
	inc hl
	jr -

+:
	ld a, (_RAM_C005_)
	bit 2, a
	jr nz, +
	ld hl, var.pallete.shouldUpdate
	set 4, (hl)
	ret

+:
	ld hl, var.pallete.shouldUpdate
	set 4, (hl)
	set 6, (hl)
	ret

++:
	ld hl, _RAM_C6C3_
-:
	ld a, (hl)
	dec a
	daa
	ld (hl), a
	cp $60
	jp c, +
	ld (hl), $59
	inc hl
	jr -

+:
	ld hl, var.pallete.shouldUpdate
	set 5, (hl)
	ret

_LABEL_2DFE_:
	ld a, (_RAM_C0A8_)
	ld hl, _DATA_2E07_
	jp jumpToAthPointer

; Jump Table from 2E07 to 2E0E (4 entries, indexed by _RAM_C0A8_)
_DATA_2E07_:
.dw _LABEL_2E0F_ _LABEL_2E10_ _LABEL_2E10_ _LABEL_2E0F_

; 1st entry of Jump Table from 2E07 (indexed by _RAM_C0A8_)
_LABEL_2E0F_:
	ret

; 2nd entry of Jump Table from 2E07 (indexed by _RAM_C0A8_)
_LABEL_2E10_:
	ld a, (var.fade.state)
	or a
	ret nz
	ld a, (_RAM_C0AA_)
	or a
	jr z, +
	dec a
	ld (_RAM_C0AA_), a
	ret

+:
	ld a, (_RAM_C699_)
	ld d, a
	ld a, (_RAM_C6A5_)
	cp d
	jr nc, +
	ld a, d
+:
	neg
	add a, $09
	ld (_RAM_C0AA_), a
	ld hl, _RAM_C0A9_
	inc (hl)
	ld a, (hl)
	and $03
	ld e, a
	ld d, $00
	ld hl, _DATA_2E4C_
	add hl, de
	ld de, _RAM_C02F_
	call ldi4
	ld hl, var.pallete.shouldUpdate
	set 0, (hl)
	ret

; Data from 2E4C to 2E53 (8 bytes)
_DATA_2E4C_:
.db $3F $3F $00 $00 $3F $3F $00 $00

_LABEL_2E54_:
	ld a, (_RAM_C6BD_)
	or a
	jr z, +
	dec a
	ld (_RAM_C6BD_), a
	ret

+:
	ld a, $08
	ld (_RAM_C6BD_), a
	ld de, $21A0
	rst $08	; setVdpAddress
	ld a, (_RAM_C6BC_)
	ld l, $00
	or a
	rra
	rr l
	rra
	rr l
	rra
	rr l
	ld h, a
	ld de, _DATA_2E90_
	add hl, de
	ld bc,  $2000 | Port_VDPData
-:
	outi
	jr nz, -
	ld a, (_RAM_C6BC_)
	inc a
	cp $03
	jr c, +
	xor a
+:
	ld (_RAM_C6BC_), a
	ret

; Data from 2E90 to 2EEF (96 bytes)
_DATA_2E90_:
.db $7E $7E $7E $7E $00 $81 $81 $81 $18 $81 $99 $81 $24 $99 $BD $81
.db $24 $99 $BD $81 $18 $81 $99 $81 $00 $81 $81 $81 $7E $00 $7E $7E
.db $7E $7E $7E $7E $18 $81 $99 $81 $24 $99 $BD $81 $5A $BD $FF $99
.db $5A $BD $FF $99 $24 $99 $BD $81 $18 $81 $99 $81 $7E $00 $7E $7E
.db $7E $7E $7E $7E $66 $99 $FF $81 $5A $BD $FF $99 $3C $FF $FF $BD
.db $3C $FF $FF $BD $5A $BD $FF $99 $66 $99 $FF $81 $7F $00 $7F $7F

_LABEL_2EF0_:
	ld a, (_RAM_C6CA_)
	or a
	jr z, +
	dec a
	ld (_RAM_C6CA_), a
	ret

+:
	ld a, $08
	ld (_RAM_C6CA_), a
	ld a, (_RAM_C6CB_)
	add a, a
	ld e, a
	ld d, $00
	ld hl, _DATA_2F24_
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld a, (hl)
	inc hl
	ld de, $21C0
	call _LABEL_746_
	ld a, (_RAM_C6CB_)
	inc a
	cp $18
	jr c, +
	xor a
+:
	ld (_RAM_C6CB_), a
	ret

; Pointer Table from 2F24 to 2F53 (24 entries, indexed by _RAM_C6CB_)
_DATA_2F24_:
.dw _DATA_2F54_ _DATA_2F5F_ _DATA_2F6A_ _DATA_2F75_ _DATA_2F80_ _DATA_2F75_ _DATA_2F6A_ _DATA_2F5F_
.dw _DATA_2F54_ _DATA_2F8B_ _DATA_2F96_ _DATA_2FA1_ _DATA_2FAC_ _DATA_2FA1_ _DATA_2F96_ _DATA_2F8B_
.dw _DATA_2F54_ _DATA_2F54_ _DATA_2F54_ _DATA_2F54_ _DATA_2F54_ _DATA_2F54_ _DATA_2F54_ _DATA_2F54_

; 1st entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F54 to 2F5E (11 bytes)
_DATA_2F54_:
.db $07 $08
.dsb 9, $00

; 2nd entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F5F to 2F69 (11 bytes)
_DATA_2F5F_:
.db $07 $08 $00 $18 $18 $18 $18 $18 $18 $18 $18

; 3rd entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F6A to 2F74 (11 bytes)
_DATA_2F6A_:
.db $07 $08 $00 $3C $3C $3C $3C $3C $3C $3C $3C

; 4th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F75 to 2F7F (11 bytes)
_DATA_2F75_:
.db $07 $08 $00 $7E $7E $7E $7E $7E $7E $7E $7E

; 5th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F80 to 2F8A (11 bytes)
_DATA_2F80_:
.db $07 $08 $00 $FF $FF $FF $FF $FF $FF $FF $FF

; 10th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F8B to 2F95 (11 bytes)
_DATA_2F8B_:
.db $0F $08 $00 $18 $18 $18 $18 $18 $18 $18 $18

; 11th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F96 to 2FA0 (11 bytes)
_DATA_2F96_:
.db $0F $08 $00 $3C $3C $3C $3C $3C $3C $3C $3C

; 12th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2FA1 to 2FAB (11 bytes)
_DATA_2FA1_:
.db $0F $08 $00 $7E $7E $7E $7E $7E $7E $7E $7E

; 13th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2FAC to 2FB6 (11 bytes)
_DATA_2FAC_:
.db $0F $08 $00 $FF $FF $FF $FF $FF $FF $FF $FF

_LABEL_2FB7_:
	push ix
	pop hl
	ld e, l
	ld d, h
	inc de
	ld (hl), $00
	jp ldi31

; init Main menu Arrow Entity
initArrow:
	ld (ix+2), $01
	ld (ix+3), $01

	; Option
	ld (ix+14), $00

	; Options count
	ld (ix+15), $03

	; Y position
	ld (ix+6), $78

	; X position
	ld (ix+9), $50

	; Set next updater
	ld (ix+0), <updateArrow
	ld (ix+1), >updateArrow

	ret

; Update Menu Arrow Entity
updateArrow:
	ld a, (var.input.player1.debounced)
	and JOY_UP | JOY_DOWN
	ret z

	; If skipped: Arrow doesnt move
	call _LABEL_341A_

	; y = $78 + option * 16
	ld a, (ix+14)
	add a, a
	add a, a
	add a, a
	add a, a
	add a, $78
	ld (ix+6), a
	ret

_LABEL_2FF9_:
	ld (ix+2), $01
	ld (ix+3), $01
	ld (ix+14), $00
	ld (ix+15), $02
	ld (ix+6), $88
	ld (ix+9), $50
	ld (ix+0), <_LABEL_301A_
	ld (ix+1), >_LABEL_301A_
	ret

_LABEL_301A_:
	ld a, (var.input.player1.debounced)
	and $03
	ret z
	call _LABEL_341A_
	ld a, (ix+14)
	add a, a
	add a, a
	add a, a
	add a, a
	add a, $88
	ld (ix+6), a
	ret

_LABEL_3030_:
	ld (ix+2), $01
	ld (ix+3), $01
	ld (ix+14), $03
	ld (ix+15), $04
	ld (ix+6), $A0
	ld (ix+9), $30
	ld (ix+0), <_LABEL_3064_
	ld (ix+1), >_LABEL_3064_
	call _LABEL_3444_
	call _LABEL_3459_
	call _LABEL_349D_
	ld a, (_RAM_C005_)
	bit 0, a
	jp z, _LABEL_34BB_
	jp _LABEL_34D6_

_LABEL_3064_:
	ld a, (var.input.player1.debounced)
	and $30
	jr z, +
	ld a, (ix+14)
	cp $03
	jp z, _LABEL_2FB7_
+:
	ld a, (var.input.player1.debounced)
	and $03
	jr z, +
	call _LABEL_341A_
	ld a, (ix+14)
	add a, a
	add a, a
	add a, a
	add a, a
	add a, $70
	ld (ix+6), a
+:
	ld a, (var.input.player1.debounced)
	and $0C
	ret z
	ld a, $95
	ld (var.audio.request), a
	ld a, (ix+14)
	or a
	jr z, _LABEL_30A1_
	dec a
	jr z, _LABEL_30C6_
	dec a
	jr z, _LABEL_30E5_
	ret

_LABEL_30A1_:
	ld hl, _RAM_C6A8_
	ld a, (var.input.player1.debounced)
	bit 2, a
	jr z, ++
	dec (hl)
	jp p, +
	ld (hl), $02
+:
	call _LABEL_3444_
	jp _LABEL_3459_

++:
	inc (hl)
	ld a, (hl)
	cp $03
	jp c, +
	ld (hl), $00
+:
	call _LABEL_3444_
	jp _LABEL_3459_

_LABEL_30C6_:
	ld hl, _RAM_C6A9_
	ld a, (var.input.player1.debounced)
	bit 2, a
	jr z, +
	dec (hl)
	jp p, _LABEL_349D_
	ld (hl), $04
	jp _LABEL_349D_

+:
	inc (hl)
	ld a, (hl)
	cp $05
	jp c, _LABEL_349D_
	ld (hl), $00
	jp _LABEL_349D_

_LABEL_30E5_:
	ld a, (_RAM_C005_)
	bit 0, a
	jr nz, ++
	ld hl, _RAM_C6AC_
	ld a, (var.input.player1.debounced)
	bit 2, a
	jr z, +
	dec (hl)
	jp p, _LABEL_34BB_
	ld (hl), $09
	jp _LABEL_34BB_

+:
	inc (hl)
	ld a, (hl)
	cp $0A
	jp c, _LABEL_34BB_
	ld (hl), $00
	jp _LABEL_34BB_

++:
	ld hl, _RAM_C6AB_
	ld a, (var.input.player1.debounced)
	bit 2, a
	jr z, +
	dec (hl)
	jp p, _LABEL_34D6_
	ld (hl), $07
	jp _LABEL_34D6_

+:
	inc (hl)
	ld a, (hl)
	cp $08
	jp c, _LABEL_34D6_
	ld (hl), $00
	jp _LABEL_34D6_

_LABEL_312A_:
	ld (ix+2), $01
	ld (ix+3), $01
	ld (ix+14), $04
	ld (ix+15), $05
	ld (ix+6), $A0
	ld (ix+9), $18
	ld (ix+0), <_LABEL_3166_
	ld (ix+1), >_LABEL_3166_
	call _LABEL_3444_
	call _LABEL_3459_
	call _LABEL_349D_
	ld a, (_RAM_C005_)
	bit 0, a
	jr nz, +
	call _LABEL_34BB_
	jp _LABEL_34F1_

+:
	call _LABEL_34D6_
	jp _LABEL_3503_

_LABEL_3166_:	
	ld a, (var.input.player1.debounced)	; var.input.player1.debounced = $C00C
	and $30
	jr z, +
	ld a, (ix+14)
	cp $04
	jp z, _LABEL_2FB7_
+:	
	ld a, (var.input.player1.debounced)	; var.input.player1.debounced = $C00C
	and $03
	jr z, +
	call _LABEL_341A_
	ld e, (ix+14)
	ld d, $00
	ld hl, _DATA_31AB_
	add hl, de
	ld a, (hl)
	ld (ix+6), a
+:	
	ld a, (var.input.player1.debounced)	; var.input.player1.debounced = $C00C
	and $0C
	ret z
	ld a, $95
	ld (var.audio.request), a	; var.audio.request = $DD05
	ld a, (ix+14)
	or a
	jp z, _LABEL_30A1_
	dec a
	jp z, _LABEL_30C6_
	dec a
	jp z, _LABEL_30E5_
	dec a
	jp z, +
	ret
	
; Data from 31AB to 31AF (5 bytes)	
_DATA_31AB_:	
	.db $70 $78 $80 $90 $A0
	
+:	
	ld a, (_RAM_C005_)	; _RAM_C005_ = $C005
	bit 0, a
	jr nz, ++
	ld hl, _RAM_C6B2_	; _RAM_C6B2_ = $C6B2
	ld a, (var.input.player1.debounced)	; var.input.player1.debounced = $C00C
	bit 2, a
	jr z, +
	dec (hl)
	jp p, _LABEL_34F1_
	ld (hl), $03
	jp _LABEL_34F1_
	
+:	
	inc (hl)
	ld a, (hl)
	cp $04
	jp c, _LABEL_34F1_
	ld (hl), $00
	jp _LABEL_34F1_
	
++:	
	ld hl, _RAM_C6B3_	; _RAM_C6B3_ = $C6B3
	ld a, (var.input.player1.debounced)	; var.input.player1.debounced = $C00C
	bit 2, a
	jr z, +
	dec (hl)
	jp p, _LABEL_3503_
	ld (hl), $06
	jp _LABEL_3503_
	
+:	
	inc (hl)
	ld a, (hl)
	cp $07
	jp c, _LABEL_3503_
	ld (hl), $00
	jp _LABEL_3503_

_LABEL_31F5_:
	ld (ix+2), $01
	ld (ix+3), $13
	ld (ix+14), $03
	ld (ix+15), $04
	ld (ix+6), $A0
	ld (ix+9), $E0
	ld (ix+0), $29
	ld (ix+1), $32
	call _LABEL_3514_
	call _LABEL_351F_
	call _LABEL_3559_
	ld a, (_RAM_C005_)
	bit 0, a
	jp z, _LABEL_3577_
	jp _LABEL_3588_

; Data from 3229 to 32F6 (206 bytes)
.db $3A $11 $C0 $E6 $30 $28 $08 $DD $7E $0E $FE $03 $CA $B7 $2F $3A
.db $11 $C0 $E6 $03 $28 $10 $CD $1A $34 $DD $5E $0E $16 $00 $21 $6A
.db $32 $19 $7E $DD $77 $06 $3A $11 $C0 $E6 $0C $C8 $3E $95 $32 $05
.db $DD $DD $7E $0E $B7 $CA $6E $32 $3D $CA $93 $32 $3D $CA $B2 $32
.db $C9 $70 $78 $80 $A0 $21 $AD $C6 $3A $11 $C0 $CB $57 $28 $0C $35
.db $F2 $7E $32 $36 $02 $CD $14 $35 $C3 $1F $35 $34 $7E $FE $03 $DA
.db $8D $32 $36 $00 $CD $14 $35 $C3 $1F $35 $21 $AE $C6 $3A $11 $C0
.db $CB $57 $28 $09 $35 $F2 $59 $35 $36 $04 $C3 $59 $35 $34 $7E $FE
.db $05 $DA $59 $35 $36 $00 $C3 $59 $35 $3A $05 $C0 $CB $47 $20 $1F
.db $21 $B1 $C6 $3A $11 $C0 $CB $57 $28 $09 $35 $F2 $77 $35 $36 $09
.db $C3 $77 $35 $34 $7E $FE $0A $DA $77 $35 $36 $00 $C3 $77 $35 $21
.db $B0 $C6 $3A $11 $C0 $CB $57 $28 $09 $35 $F2 $88 $35 $36 $07 $C3
.db $88 $35 $34 $7E $FE $08 $DA $88 $35 $36 $00 $C3 $88 $35

_LABEL_32F7_:
	ld (ix+2), $01
	ld (ix+3), $02
	ld (ix+0), $07
	ld (ix+1), $33
	ld a, (_RAM_C002_)
	or a
	ret nz
	ld l, (ix+5)
	ld h, (ix+6)
	ld de, $0080
	add hl, de
	ld (ix+5), l
	ld (ix+6), h
	ld l, (ix+8)
	ld h, (ix+9)
	ld de, $0020
	add hl, de
	ld (ix+8), l
	ld (ix+9), h
	ret

_LABEL_332D_:
	ld (ix+2), $01
	ld (ix+3), $03
	ld (ix+0), $3D
	ld (ix+1), $33
	ld a, (_RAM_C002_)
	or a
	ret nz
	ld l, (ix+5)
	ld h, (ix+6)
	ld de, $0100
	add hl, de
	ld (ix+5), l
	ld (ix+6), h
	ld l, (ix+8)
	ld h, (ix+9)
	ld de, $0040
	add hl, de
	ld (ix+8), l
	ld (ix+9), h
	ret

_LABEL_3363_:
	ld (ix+2), $01
	ld (ix+3), $04
	ld (ix+0), $73
	ld (ix+1), $33
	ld a, (_RAM_C002_)
	or a
	ret nz
	ld l, (ix+5)
	ld h, (ix+6)
	ld de, $0180
	add hl, de
	ld (ix+5), l
	ld (ix+6), h
	ld l, (ix+8)
	ld h, (ix+9)
	ld de, $0060
	add hl, de
	ld (ix+8), l
	ld (ix+9), h
	ret

_LABEL_3399_:
	ld (ix+18), $00
	ld (ix+3), $05
	jr +

_LABEL_33A3_:
	ld (ix+18), $02
	jr +

_LABEL_33A9_:
	ld (ix+18), $04
	jr +

_LABEL_33AF_:
	ld (ix+18), $06
	jr +

_LABEL_33B5_:
	ld (ix+18), $08
+:
	ld (ix+14), $04
	ld (ix+6), $3F
	ld (ix+2), $01
	ld (ix+0), $CE
	ld (ix+1), $33
	ret

_LABEL_33CE_:
	ld a, (_RAM_C002_)
	or a
	ret nz
	xor a
	ld a, (ix+9)
	sbc a, (ix+18)
	ld (ix+9), a
	dec (ix+14)
	ret nz
	ld (ix+14), $28
	ld (ix+0), $EE
	ld (ix+1), $33
	ret

_LABEL_33EE_:
	dec (ix+14)
	ret nz
	ld (ix+14), $04
	ld (ix+0), $FF
	ld (ix+1), $33
	ret

_LABEL_33FF_:
	ld a, (_RAM_C002_)
	or a
	ret nz
	ld a, (ix+9)
	add a, (ix+18)
	ld (ix+9), a
	dec (ix+14)
	ret nz
	ld (ix+0), $B7
	ld (ix+1), $2F
	ret

_LABEL_341A_:
	rrca
	
	; If 
	jr nc, @endif3
		; Request arrow sound
		ld a, SOUND_ARROW
		ld (var.audio.request), a

		; Update option index, handling loop
		ld a, (ix+14)
		sub $01
		jr nc, @endif
			ld a, (ix+15)
			dec a
		@endif:
		ld (ix+14), a

		ret

	@endif3:
		; Request arrow sound
		ld a, SOUND_ARROW
		ld (var.audio.request), a

		; Update option index, handling loop
		ld a, (ix+14)
		inc a
		cp (ix+15)
		jr c, @endif2
			xor a
		@endif2:
		ld (ix+14), a

		ret

_LABEL_3444_:
	ld de, $3BA6
	ld a, (_RAM_C005_)
	and $02
	jr z, +
	ld de, $3B88
+:
	ld a, (_RAM_C6A8_)
	add a, $0D
	jp _LABEL_2885_

_LABEL_3459_:
	ld de, $3C26
	ld a, (_RAM_C005_)
	bit 1, a
	jr z, +
	ld de, $3BC8
+:
	rst $08	; setVdpAddress
	ld a, (_RAM_C6A8_)
	add a, a
	add a, a
	add a, a
	ld e, a
	ld d, $00
	ld hl, _DATA_3485_
	add hl, de
	ld a, $09
	ld bc,  $0600 | Port_VDPData
-:
	outi
	nop
	jr +

+:
	out (c), a
	nop
	nop
	jr nz, -
	ret

; Data from 3485 to 349C (24 bytes)
_DATA_3485_:
.db $01 $02 $03 $04 $00 $00 $00 $00 $01 $02 $03 $04 $05 $00 $00 $00
.db $01 $02 $03 $04 $05 $06 $00 $00

_LABEL_349D_:
	ld hl, (_RAM_C6A9_)
	ld h, $00
	add hl, hl
	ld de, _DATA_34B1_
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld de, $2020
	jp _LABEL_6D0_

; Pointer Table from 34B1 to 34BA (5 entries, indexed by _RAM_C6A9_)
_DATA_34B1_:
.dw _DATA_9A27_ _DATA_9AE9_ _DATA_9B95_ _DATA_9D97_ _DATA_9E3F_

_LABEL_34BB_:
	ld de, $3CAA
	ld a, (_RAM_C005_)
	and $02
	jr z, +
	ld de, $3C0E
+:
	rst $08	; setVdpAddress
	ld a, (_RAM_C6AC_)
	add a, $D6
	out (Port_VDPData), a
	xor a
	jr +

+:
	out (Port_VDPData), a
	ret

_LABEL_34D6_:
	ld de, $3CAA
	ld a, (_RAM_C005_)
	and $02
	jr z, +
	ld de, $3C0E
+:
	rst $08	; setVdpAddress
	ld a, (_RAM_C6AB_)
	add a, $D8
	out (Port_VDPData), a
	xor a
	jr +

+:
	out (Port_VDPData), a
	ret

_LABEL_34F1_:
	ld de, $3C98
	rst $08	; setVdpAddress
	ld a, (_RAM_C6B2_)
	add a, a
	add a, $D9
	out (Port_VDPData), a
	xor a
	jr +

+:
	out (Port_VDPData), a
	ret

_LABEL_3503_:
	ld de, $3C92
	rst $08	; setVdpAddress
	ld a, (_RAM_C6B3_)
	add a, $D9
	out (Port_VDPData), a
	xor a
	jr +

+:
	out (Port_VDPData), a
	ret

_LABEL_3514_:
	ld de, $3BAC
	ld a, (_RAM_C6AD_)
	add a, $0D
	jp _LABEL_2885_

_LABEL_351F_:
	ld de, $3BEC
	rst $08	; setVdpAddress
	ld a, (_RAM_C6AD_)
	add a, a
	add a, a
	add a, a
	ld e, a
	ld d, $00
	ld hl, _DATA_3541_
	add hl, de
	ld a, $09
	ld bc,  $0600 | Port_VDPData
-:
	outi
	nop
	jr +

+:
	out (c), a
	nop
	nop
	jr nz, -
	ret

; Data from 3541 to 3558 (24 bytes)
_DATA_3541_:
.db $07 $08 $09 $0A $00 $00 $00 $00 $07 $08 $09 $0A $0B $00 $00 $00
.db $07 $08 $09 $0A $0B $0C $00 $00

_LABEL_3559_:
	ld hl, (_RAM_C6AE_)
	ld h, $00
	add hl, hl
	ld de, _DATA_356D_
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld de, $20E0
	jp _LABEL_6D0_

; Pointer Table from 356D to 3576 (5 entries, indexed by _RAM_C6AE_)
_DATA_356D_:
.dw _DATA_9A27_ _DATA_9AE9_ _DATA_9B95_ _DATA_9D97_ _DATA_9E3F_

_LABEL_3577_:
	ld de, $3C30
	rst $08	; setVdpAddress
	ld a, (_RAM_C6B1_)
	add a, $D6
	out (Port_VDPData), a
	xor a
	jr +

+:
	out (Port_VDPData), a
	ret

_LABEL_3588_:
	ld de, $3C30
	rst $08	; setVdpAddress
	ld a, (_RAM_C6B0_)
	add a, $D8
	out (Port_VDPData), a
	xor a
	jr +

+:
	out (Port_VDPData), a
	ret

_LABEL_3599_:
	ld (ix+22), $00
	ld (ix+23), $00
	ld (ix+24), $00
	call _LABEL_448C_
	ld hl, (_RAM_C699_)
	ld h, $00
	ld de, _DATA_362C_
	add hl, de
	ld a, (hl)
	ld (_RAM_DD01_), a
	ld (_RAM_DD02_), a
	xor a
	ld (_RAM_DD08_), a
	ld (ix+2), $01
	ld (ix+3), $14
	ld (ix+6), $20
	ld (ix+25), $3C
	ld (ix+31), $00
	ld (ix+0), $E7
	ld (ix+1), $35
	ld a, (_RAM_C005_)
	bit 1, a
	ld a, $70
	jr z, +
	ld a, $20
+:
	ld (ix+9), a
	ret

_LABEL_35E7_:
	dec (ix+25)
	ret p
	ld (ix+2), $00
	call _LABEL_43D3_
	ld (ix+0), $36
	ld (ix+1), $36
	ld a, (_RAM_C005_)
	bit 0, a
	ret z
	ld a, $01
	ld (_RAM_C6C2_), a
	ld a, (_RAM_C005_)
	bit 1, a
	ret nz
	ld a, $01
	ld (_RAM_C0A8_), a
	ret

_LABEL_3611_:
	ld a, (_RAM_C005_)
	bit 1, a
	ld a, $70
	jr z, +
	ld a, $20
+:
	ld (ix+9), a
	ld (ix+6), $20
	ld (ix+3), $15
	ld (ix+2), $01
	ret

; Data from 362C to 3635 (10 bytes)
_DATA_362C_:
.db $04 $05 $06 $07 $09 $0C $10 $20 $60 $FF

_LABEL_3636_:
	ld hl, $C41B
	bit 0, (ix+31)
	jp nz, _LABEL_3C04_
	ld a, (_RAM_C69A_)
	add a, a
	add a, a
	add a, a
	ld e, a
	ld d, $00
	add hl, de
	ld (ix+16), l
	ld (ix+17), h
	ld de, $01C2
	ld b, $09
-:
	or a
	ld hl, (_RAM_C697_)
	sbc hl, de
	jr nc, +
	ld hl, $FFCE
	add hl, de
	ex de, hl
	djnz -
+:
	ld a, (_RAM_C699_)
	cp b
	jr nc, ++
	ld a, b
	ld (_RAM_C699_), a
	ld a, $93
	ld (var.audio.request), a
	ld a, (_RAM_C005_)
	bit 1, a
	jr nz, +
	ld hl, var.pallete.shouldUpdate
	set 1, (hl)
	jr ++

+:
	ld hl, var.palette._RAM_C022_
	set 0, (hl)
++:
	ld a, $01
	ld (_RAM_C693_), a
	ld a, (ix+22)
	ld (ix+19), a
	ld a, (ix+23)
	ld (ix+20), a
	ld a, (ix+24)
	ld (ix+21), a
	call _LABEL_43D3_
	call _LABEL_448C_
	ld l, (ix+16)
	ld h, (ix+17)
	ld de, $0010
	add hl, de
	ld a, (hl)
	or a
	jp nz, _LABEL_36F0_
	ld a, (_RAM_C699_)
	call _LABEL_4514_
	ld (ix+0), $F9
	ld (ix+1), $36
	ld hl, (_RAM_C699_)
	ld h, $00
	ld de, _DATA_362C_
	add hl, de
	ld a, (hl)
	ld (_RAM_DD01_), a
	ld (_RAM_DD02_), a
	ld a, (_RAM_C005_)
	or a
	jp z, _LABEL_4464_
	ld hl, _RAM_C441_
	ld b, $06
	xor a
-:
	or (hl)
	jr nz, +
	inc hl
	djnz -
+:
	ld a, b
	or a
	ld a, $80
	jr nz, +
	xor a
+:
	ld (_RAM_DD08_), a
	jp _LABEL_4464_

_LABEL_36F0_:
	call _LABEL_4464_
	ld a, $01
	ld (_RAM_C008_), a
	ret

_LABEL_36F9_:
	ld a, (_RAM_C002_)
	or a
	jp nz, _LABEL_3611_
	ld (ix+2), $00
	call _LABEL_447C_
	ld a, (_RAM_C69B_)
	or a
	jp nz, _LABEL_378F_
	ld a, (_RAM_C693_)
	or a
	jr z, +
	ld a, (var.input.player1.data)
	and $02
	jr nz, ++
	xor a
	ld (_RAM_C693_), a
+:
	ld a, (var.input.player1.data)
	bit 1, a
	jr z, ++
	and $0C
	jr z, +++
	xor a
	ld (var.input.player1.data), a
	ld (var.input.player1.debounced), a
++:
	dec (ix+25)
	jp p, ++++
+++:
	ld a, (_RAM_C699_)
	call _LABEL_4514_
	ld e, (ix+16)
	ld d, (ix+17)
	ld hl, _DATA_18_
	add hl, de
	ld a, (hl)
	or a
	jr nz, +++++
	ld hl, $0008
	add hl, de
	ld (ix+16), l
	ld (ix+17), h
	ld de, $0018
	add hl, de
	ld a, (hl)
	or a
	jr z, ++++
	ld a, $92
	ld (var.audio.request), a
++++:
	ld a, (var.input.player1.debounced)
	and $30
	call nz, _LABEL_4384_
	ld hl, _LABEL_4464_	; Overriding return address
	push hl
	ld a, (var.input.player1.debounced)
	and $0C
	ret z
	ld a, (var.input.player1.debounced)
	bit 2, a
	jp z, _LABEL_436F_
	jp _LABEL_435A_

+++++:
	call _LABEL_4464_
	ld (ix+25), $14
	ld (ix+0), $A0
	ld (ix+1), $37
	ret

_LABEL_378F_:
	ld (ix+14), $00
	ld (ix+25), $10
	ld (ix+0), $3C
	ld (ix+1), $3A
	ret

_LABEL_37A0_:
	ld a, (_RAM_C002_)
	or a
	jp nz, _LABEL_3611_
	ld (ix+2), $00
	ld a, (var.input.player1.data)
	and $0E
	cp $02
	jr z, _LABEL_37F7_
	dec (ix+25)
	jp m, _LABEL_37F7_
	call _LABEL_447C_
	ld hl, +	; Overriding return address
	push hl
	ld a, (var.input.player1.debounced)
	and $30
	call nz, _LABEL_4384_
	ld a, (var.input.player1.debounced)
	and $0C
	ret z
	ld a, (var.input.player1.debounced)
	bit 2, a
	jp z, _LABEL_436F_
	jp _LABEL_435A_

+:
	call _LABEL_4464_
	ld e, (ix+16)
	ld d, (ix+17)
	ld hl, _DATA_18_
	add hl, de
	ld a, (hl)
	or a
	ret nz
	ld (ix+25), $00
	ld (ix+0), $F9
	ld (ix+1), $36
	ret

_LABEL_37F7_:
	ld (ix+27), $00
	ld (ix+30), $00
	ld hl, _RAM_C4B0_
	ld de, _RAM_C4B1_
	ld (hl), $00
	call ldi128
	call _LABEL_123D_
	ld (ix+0), $16
	ld (ix+1), $38
	ret

_LABEL_3816_:
	ld hl, _RAM_C419_
	ld de, $0008
	ld bc, $0001
	call _LABEL_2A27_
	ld a, (_RAM_C6BB_)
	ld (ix+28), a
	ld (ix+0), $31
	ld (ix+1), $38
	ret

_LABEL_3831_:
	ld hl, _RAM_C419_
	ld de, $0001
	ld bc, $0008
	call _LABEL_2A27_
	ld a, (_RAM_C6BB_)
	add a, (ix+28)
	ld (ix+28), a
	ld (ix+0), $4F
	ld (ix+1), $38
	ret

_LABEL_384F_:
	ld hl, _RAM_C406_
	ld de, $0007
	ld bc, $0008
	call _LABEL_2A27_
	ld a, (_RAM_C6BB_)
	add a, (ix+28)
	ld (ix+28), a
	ld (ix+0), $6D
	ld (ix+1), $38
	ret

_LABEL_386D_:
	ld hl, _RAM_C401_
	ld de, $0009
	ld bc, $0008
	call _LABEL_2A27_
	ld a, (_RAM_C6BB_)
	add a, (ix+28)
	ld (ix+28), a
	ld (ix+0), $8B
	ld (ix+1), $38
	ret

_LABEL_388B_:
	ld a, (ix+21)
	cp $0D
	jr z, _LABEL_38D3_
	ld a, (ix+28)
	or a
	jr z, +
	add a, (ix+30)
	ld (ix+30), a
	ld a, (ix+28)
	call _LABEL_2A91_
	ld (ix+28), l
	ld (ix+29), h
	ld a, (_RAM_C005_)
	and $03
	call z, _LABEL_452A_
	ld (ix+25), $2F
	ld (ix+0), $2B
	ld (ix+1), $39
	ret

+:
	ld a, (ix+30)
	or a
	call nz, _LABEL_2B3E_
	ld (ix+25), $05
	ld (ix+0), $EF
	ld (ix+1), $38
	ret

_LABEL_38D3_:
	ld (ix+21), $00
	call _LABEL_2A6E_
	ld (ix+28), $00
	ld (ix+29), $00
	ld (ix+25), $2F
	ld (ix+0), $2B
	ld (ix+1), $39
	ret

_LABEL_38EF_:
	dec (ix+25)
	ret p
	ld (ix+0), $36
	ld (ix+1), $36
	ld a, (_RAM_C005_)
	and $04
	ret z
	ld a, (ix+31)
	cpl
	ld (ix+31), a
	and $01
	ld e, a
	ld d, $00
	ld hl, _DATA_3928_
	add hl, de
	ld de, _RAM_C026_
	ld a, (_RAM_C005_)
	bit 0, a
	jr z, +
	ld de, _RAM_C02D_
+:
	ldi
	ldi
	ld hl, var.pallete.shouldUpdate
	set 0, (hl)
	ret

; Data from 3928 to 392A (3 bytes)
_DATA_3928_:
.db $0F $05 $0F

_LABEL_392B_:
	ld a, (_RAM_C002_)
	or a
	jp nz, _LABEL_3611_
	ld (ix+2), $00
	ld hl, +	; Overriding return address
	push hl
	bit 3, (ix+25)
	ld de, _RAM_C418_
	jp z, _LABEL_2AD2_
	jp _LABEL_2AE5_

+:
	dec (ix+25)
	ret p
	ld (ix+0), $54
	ld (ix+1), $39
	ret

_LABEL_3954_:
	call _LABEL_2B2A_
	bit 0, (ix+31)
	jr nz, +
	ld hl, (_RAM_C697_)
	add hl, bc
	ld (_RAM_C697_), hl
	ld a, (_RAM_C005_)
	and $03
	jr nz, ++
	ld hl, var.pallete.shouldUpdate
	set 2, (hl)
	jr ++

+:
	ld hl, (_RAM_C6A3_)
	add hl, bc
	ld (_RAM_C6A3_), hl
	ld a, (_RAM_C005_)
	and $03
	jr nz, ++
	ld hl, var.palette._RAM_C022_
	set 5, (hl)
++:
	ld a, (_DATA_1B_)
	add a, a
	cp $0F
	jr c, +
	ld a, $0F
+:
	add a, $F0
	ld (_RAM_DD09_), a
	ld a, $90
	ld (var.audio.request), a
	ld (ix+25), $0B
	ld (ix+0), $A6
	ld (ix+1), $39
	ret

_LABEL_39A6_:
	ld a, (_RAM_C002_)
	or a
	jp nz, _LABEL_3611_
	ld (ix+2), $00
	dec (ix+25)
	jr z, +
	ld a, (ix+25)
	and $01
	ret nz
	ld a, (ix+25)
	and $FE
	ld e, a
	ld d, $00
	ld hl, _DATA_39D0_
	add hl, de
	ld a, (hl)
	inc hl
	ld h, (hl)
	ld l, a
	ld de, _RAM_C418_
	jp (hl)

; Jump Table from 39D0 to 39DB (6 entries, indexed by unknown)
_DATA_39D0_:
.dw $0000 _LABEL_2AD2_ _LABEL_2ACE_ _LABEL_2ACA_ _LABEL_2AC6_ _LABEL_2AC2_

+:
	ld (ix+25), $10
	ld (ix+0), $E9
	ld (ix+1), $39
	ret

_LABEL_39E9_:
	dec (ix+25)
	ret p
	ld a, (_RAM_C005_)
	and $03
	jr nz, ++
	ld e, (ix+28)
	ld d, (ix+29)
	bit 0, (ix+31)
	jr nz, +
	ld hl, (_RAM_C694_)
	ld a, (_RAM_C696_)
	add hl, de
	adc a, $00
	ld (_RAM_C694_), hl
	ld (_RAM_C696_), a
	ld hl, var.pallete.shouldUpdate
	set 3, (hl)
	jr ++

+:
	ld hl, (_RAM_C6A0_)
	ld a, (_RAM_C6A2_)
	add hl, de
	adc a, $00
	ld (_RAM_C6A0_), hl
	ld (_RAM_C6A2_), a
	ld hl, var.palette._RAM_C022_
	set 6, (hl)
++:
	ld hl, _RAM_C4A1_
	call _LABEL_2AF6_
	inc (ix+27)
	ld (ix+0), $FF
	ld (ix+1), $37
	ret

; Data from 3A3C to 3AC3 (136 bytes)
.db $3A $02 $C0 $B7 $C2 $11 $36 $DD $36 $02 $00 $DD $7E $0E $E6 $FE
.db $0F $5F $16 $00 $21 $73 $3A $19 $7E $DD $77 $13 $DD $77 $14 $DD
.db $77 $15 $CD $64 $44 $DD $34 $0E $DD $7E $0E $FE $0A $D8 $DD $36
.db $00 $78 $DD $36 $01 $3A $C9 $10 $11 $12 $13 $00 $3A $02 $C0 $B7
.db $C2 $11 $36 $DD $36 $02 $00 $DD $35 $19 $F0 $3A $9B $C6 $B7 $CA
.db $9B $3A $3D $32 $9B $C6 $21 $9A $C6 $34 $DD $36 $19 $08 $C9 $3A
.db $9A $C6 $87 $87 $87 $5F $16 $00 $21 $11 $C4 $19 $06 $06 $AF $B6
.db $23 $10 $FC $B7 $C2 $F0 $36 $3A $9A $C6 $FE $12 $D2 $F0 $36 $DD
.db $36 $00 $36 $DD $36 $01 $36 $C9

_LABEL_3AC4_:
	ld (ix+3), $17
	ld (ix+6), $54
	ld a, (_RAM_C005_)
	bit 1, a
	ld a, $68
	jr z, +
	ld a, $18
+:
	ld (ix+9), a
_LABEL_3ADA_:
	ld (ix+2), $01
	ld (ix+14), $00
	ld (ix+25), $16
	ld (ix+16), $A1
	ld (ix+17), $C4
	ld (ix+0), $F7
	ld (ix+1), $3A
	ret

; Data from 3AF7 to 3B1A (36 bytes)
.db $DD $7E $0E $B7 $28 $04 $DD $35 $0E $C9 $CD $81 $45 $F0 $DD $36
.db $19 $C0 $DD $36 $00 $11 $DD $36 $01 $3B $DD $35 $19 $C0 $3E $01
.db $32 $08 $C0 $C9

_LABEL_3B1B_:
	ld (ix+3), $18
	ld (ix+6), $48
	ld a, (_RAM_C005_)
	bit 1, a
	ld a, $68
	jr z, +
	ld a, $18
+:
	ld (ix+9), a
	jr _LABEL_3ADA_

_LABEL_3B33_:
	ld (ix+3), $14
	ld (ix+6), $48
	ld (ix+9), $20
	jr _LABEL_3ADA_

_LABEL_3B41_:
	ld (ix+3), $15
	ld (ix+6), $48
	ld (ix+9), $20
	jr _LABEL_3ADA_

_LABEL_3B4F_:
	ld (ix+3), $16
	ld (ix+6), $48
	ld (ix+9), $1C
	jp _LABEL_3ADA_

_LABEL_3B5E_:
	ld (ix+3), $00
	jp _LABEL_3ADA_

_LABEL_3B65_:
	ld (ix+3), $19
	ld (ix+6), $48
	ld (ix+9), $20
	jp _LABEL_3ADA_

_LABEL_3B74_:
	ld (ix+22), $00
	ld (ix+23), $00
	ld (ix+24), $00
	call _LABEL_44E9_
	ld (ix+2), $01
	ld (ix+3), $14
	ld (ix+6), $20
	ld (ix+25), $3C
	ld (ix+31), $01
	ld (ix+0), $AE
	ld (ix+1), $3B
	ld a, (_RAM_C005_)
	bit 1, a
	ld a, $70
	jr z, +
	ld a, $C0
+:
	ld (ix+9), a
	ret

; Data from 3BAE to 3C03 (86 bytes)
.db $DD $35 $19 $F0 $DD $36 $02 $00 $CD $D3 $43 $DD $36 $00 $01 $DD
.db $36 $01 $3C $3A $05 $C0 $CB $47 $C8 $3A $05 $C0 $CB $4F $C0 $C9
.db $3A $05 $C0 $CB $4F $28 $11 $DD $36 $09 $C0 $DD $36 $06 $20 $DD
.db $36 $03 $15 $DD $36 $02 $01 $C9 $DD $36 $09 $70 $DD $36 $06 $20
.db $DD $36 $03 $15 $DD $36 $02 $01 $C9 $04 $05 $06 $07 $09 $0C $10
.db $20 $60 $FF $21 $63 $C5

_LABEL_3C04_:
	inc hl
	ld a, (_RAM_C6A6_)
	add a, a
	add a, a
	add a, a
	ld e, a
	ld d, $00
	add hl, de
	ld (ix+16), l
	ld (ix+17), h
	ld de, $01C2
	ld b, $09
-:
	or a
	ld hl, (_RAM_C6A3_)
	sbc hl, de
	jr nc, +
	ld hl, $FFCE
	add hl, de
	ex de, hl
	djnz -
+:
	ld a, (_RAM_C6A5_)
	cp b
	jr nc, ++
	ld a, b
	ld (_RAM_C6A5_), a
	ld a, $93
	ld (var.audio.request), a
	ld a, (_RAM_C005_)
	bit 1, a
	jr nz, +
	ld hl, var.palette._RAM_C022_
	set 4, (hl)
	jr ++

+:
	ld hl, var.palette._RAM_C022_
	set 1, (hl)
++:
	ld a, $01
	ld (_RAM_C69F_), a
	ld a, (ix+22)
	ld (ix+19), a
	ld a, (ix+23)
	ld (ix+20), a
	ld a, (ix+24)
	ld (ix+21), a
	call _LABEL_43D3_
	call _LABEL_44E9_
	ld l, (ix+16)
	ld h, (ix+17)
	ld de, $0010
	add hl, de
	ld a, (hl)
	or a
	jp nz, +
	ld a, (_RAM_C6A5_)
	call _LABEL_4514_
	ld (ix+0), $9E
	ld (ix+1), $3C
	jp _LABEL_4464_

+:
	call _LABEL_4464_
	ld a, (_RAM_C005_)
	bit 2, a
	jr nz, +
	ld a, $01
	ld (_RAM_C009_), a
	ret

+:
	ld a, $01
	ld (_RAM_C008_), a
	ret

; Data from 3C9E to 3FAD (784 bytes)
.db $3A $02 $C0 $B7 $C2 $CE $3B $DD $36 $02 $00 $CD $7C $44 $3A $A7
.db $C6 $B7 $C2 $34 $3D $3A $9F $C6 $B7 $28 $0B $3A $10 $C0 $E6 $02
.db $20 $16 $AF $32 $9F $C6 $3A $10 $C0 $CB $4F $28 $0B $E6 $0C $28
.db $0D $AF $32 $10 $C0 $32 $11 $C0 $DD $35 $19 $F2 $07 $3D $3A $A5
.db $C6 $CD $14 $45 $DD $5E $10 $DD $56 $11 $21 $18 $00 $19 $7E $B7
.db $20 $34 $21 $08 $00 $19 $DD $75 $10 $DD $74 $11 $11 $18 $00 $19
.db $7E $B7 $28 $05 $3E $92 $32 $05 $DD $3A $11 $C0 $E6 $30 $C4 $84
.db $43 $21 $64 $44 $E5 $3A $11 $C0 $E6 $0C $C8 $3A $11 $C0 $CB $57
.db $CA $6F $43 $C3 $5A $43 $CD $64 $44 $DD $36 $19 $14 $DD $36 $00
.db $45 $DD $36 $01 $3D $C9 $DD $36 $0E $00 $DD $36 $19 $10 $DD $36
.db $00 $10 $DD $36 $01 $3F $C9 $3A $02 $C0 $B7 $C2 $CE $3B $DD $36
.db $02 $00 $3A $10 $C0 $E6 $0E $FE $02 $28 $43 $DD $35 $19 $FA $9C
.db $3D $CD $7C $44 $21 $7F $3D $E5 $3A $11 $C0 $E6 $30 $C4 $84 $43
.db $3A $11 $C0 $E6 $0C $C8 $3A $11 $C0 $CB $57 $CA $6F $43 $C3 $5A
.db $43 $CD $64 $44 $DD $5E $10 $DD $56 $11 $21 $18 $00 $19 $7E $B7
.db $C0 $DD $36 $19 $00 $DD $36 $00 $9E $DD $36 $01 $3C $C9 $3A $05
.db $C0 $E6 $04 $C2 $F7 $37 $DD $36 $1B $00 $DD $36 $1E $00 $21 $F8
.db $C5 $11 $F9 $C5 $36 $00 $CD $6B $11 $CD $3D $12 $DD $36 $00 $C3
.db $DD $36 $01 $3D $C9 $21 $61 $C5 $11 $08 $00 $01 $01 $00 $CD $27
.db $2A $3A $BB $C6 $DD $77 $1C $DD $36 $00 $DE $DD $36 $01 $3D $C9
.db $21 $61 $C5 $11 $01 $00 $01 $08 $00 $CD $27 $2A $3A $BB $C6 $DD
.db $86 $1C $DD $77 $1C $DD $36 $00 $FC $DD $36 $01 $3D $C9 $21 $4E
.db $C5 $11 $07 $00 $01 $08 $00 $CD $27 $2A $3A $BB $C6 $DD $86 $1C
.db $DD $77 $1C $DD $36 $00 $1A $DD $36 $01 $3E $C9 $21 $49 $C5 $11
.db $09 $00 $01 $08 $00 $CD $27 $2A $3A $BB $C6 $DD $86 $1C $DD $77
.db $1C $DD $36 $00 $38 $DD $36 $01 $3E $C9 $DD $7E $1C $B7 $28 $1F
.db $DD $86 $1E $DD $77 $1E $DD $7E $1C $CD $91 $2A $DD $75 $1C $DD
.db $74 $1D $DD $36 $19 $2F $DD $36 $00 $7E $DD $36 $01 $3E $C9 $DD
.db $7E $1E $B7 $C4 $3E $2B $DD $36 $19 $05 $DD $36 $00 $71 $DD $36
.db $01 $3E $C9 $DD $35 $19 $F0 $DD $36 $00 $01 $DD $36 $01 $3C $C9
.db $3A $02 $C0 $B7 $C2 $CE $3B $DD $36 $02 $00 $21 $9A $3E $E5 $DD
.db $CB $19 $5E $11 $60 $C5 $CA $D2 $2A $C3 $E5 $2A $DD $35 $19 $F0
.db $DD $36 $00 $A7 $DD $36 $01 $3E $C9 $CD $2F $2B $2A $A3 $C6 $09
.db $22 $A3 $C6 $3E $90 $32 $05 $DD $DD $36 $19 $0B $DD $36 $00 $C3
.db $DD $36 $01 $3E $C9 $3A $02 $C0 $B7 $C2 $CE $3B $DD $36 $02 $00
.db $DD $35 $19 $28 $1A $DD $7E $19 $E6 $01 $C0 $DD $7E $19 $E6 $FE
.db $5F $16 $00 $21 $D0 $39 $19 $7E $23 $66 $6F $11 $60 $C5 $E9 $DD
.db $36 $19 $10 $DD $36 $00 $FA $DD $36 $01 $3E $C9 $DD $35 $19 $F0
.db $21 $E9 $C5 $CD $F6 $2A $DD $34 $1B $DD $36 $00 $AC $DD $36 $01
.db $3D $C9 $3A $02 $C0 $B7 $C2 $CE $3B $DD $36 $02 $00 $DD $7E $0E
.db $E6 $FE $0F $5F $16 $00 $21 $47 $3F $19 $7E $DD $77 $13 $DD $77
.db $14 $DD $77 $15 $CD $64 $44 $DD $34 $0E $DD $7E $0E $FE $0A $D8
.db $DD $36 $00 $4C $DD $36 $01 $3F $C9 $10 $11 $12 $13 $00 $3A $02
.db $C0 $B7 $C2 $CE $3B $DD $36 $02 $00 $DD $35 $19 $F0 $3A $A7 $C6
.db $B7 $CA $6F $3F $3D $32 $A7 $C6 $21 $A6 $C6 $34 $DD $36 $19 $08
.db $C9 $3A $A6 $C6 $87 $87 $87 $5F $16 $00 $21 $59 $C5 $19 $06 $06
.db $AF $B6 $23 $10 $FC $B7 $C2 $88 $3C $3A $A6 $C6 $FE $12 $D2 $88
.db $3C $DD $36 $00 $01 $DD $36 $01 $3C $C9 $DD $36 $03 $17 $DD $36
.db $06 $54 $3A $05 $C0 $CB $4F $3E $68 $28 $02 $3E $B8 $DD $77 $09

_LABEL_3FAE_:
	ld (ix+2), $01
	ld (ix+14), $00
	ld (ix+25), $16
	ld (ix+16), $E9
	ld (ix+17), $C5
	ld (ix+0), $CB
	ld (ix+1), $3F
	ret

; Data from 3FCB to 4006 (60 bytes)
.db $DD $7E $0E $B7 $28 $04 $DD $35 $0E $C9 $CD $81 $45 $F0 $DD $36
.db $19 $C0 $DD $36 $00 $E5 $DD $36 $01 $3F $DD $35 $19 $C0 $3E $01
.db $32 $08 $C0 $C9 $DD $36 $03 $18 $DD $36 $06 $48 $3A $05 $C0 $CB
.db $4F $3E $68 $28 $02 $3E $B8 $DD $77 $09 $18 $A7

_LABEL_4007_:
	ld (ix+3), $14
	ld (ix+6), $48
	ld (ix+9), $C0
	jr _LABEL_3FAE_

_LABEL_4015_:
	ld (ix+3), $15
	ld (ix+6), $48
	ld (ix+9), $C0
	jr _LABEL_3FAE_

_LABEL_4023_:
	ld (ix+3), $16
	ld (ix+6), $48
	ld (ix+9), $BC
	jp _LABEL_3FAE_

_LABEL_4032_:
	ld (ix+3), $00
	jp _LABEL_3FAE_

_LABEL_4039_:
	ld (ix+3), $19
	ld (ix+6), $48
	ld (ix+9), $C0
	jp _LABEL_3FAE_

; Data from 4048 to 4359 (786 bytes)
.db $04 $03 $10 $11 $12 $DD $36 $02 $01 $DD $36 $03 $0F $DD $36 $09
.db $80 $DD $36 $06 $9F $DD $36 $00 $66 $DD $36 $01 $40 $C9 $3A $02
.db $C0 $B7 $28 $05 $DD $36 $02 $00 $C9 $DD $36 $02 $01 $3A $A4 $C4
.db $B7 $C0 $AF $32 $C2 $C6 $21 $01 $C1 $11 $21 $00 $D9 $21 $AD $40
.db $06 $10 $7E $23 $08 $7E $23 $D9 $77 $2B $08 $77 $19 $D9 $10 $F2
.db $3E $9F $21 $06 $C1 $11 $1D $00 $06 $10 $0E $80 $77 $23 $23 $23
.db $71 $19 $10 $F8 $C9 $C3 $41 $45 $42 $50 $42 $5B $42 $66 $42 $71
.db $42 $7C $42 $86 $42 $90 $42 $9A $42 $A4 $42 $AE $42 $B8 $42 $C2
.db $42 $CC $42 $D6 $42 $E0 $42 $DD $36 $02 $01 $DD $36 $03 $0F $DD
.db $36 $09 $30 $DD $36 $06 $C0 $DD $36 $00 $E8 $DD $36 $01 $40 $C9
.db $3A $02 $C0 $B7 $28 $05 $DD $36 $02 $00 $C9 $DD $36 $02 $01 $3A
.db $A4 $C4 $B7 $28 $0E $3A $9A $C6 $87 $87 $87 $ED $44 $C6 $9F $DD
.db $77 $06 $C9 $AF $32 $C2 $C6 $21 $01 $C1 $11 $21 $00 $D9 $21 $AD
.db $40 $06 $10 $7E $23 $08 $7E $23 $D9 $77 $2B $08 $77 $19 $D9 $10
.db $F2 $3A $9A $C6 $87 $87 $87 $ED $44 $C6 $9F $21 $06 $C1 $11 $1D
.db $00 $06 $10 $0E $30 $77 $23 $23 $23 $71 $19 $10 $F8 $C9 $DD $36
.db $02 $01 $DD $36 $03 $0F $DD $36 $09 $C8 $DD $36 $06 $C0 $DD $36
.db $00 $5F $DD $36 $01 $41 $C9 $3A $02 $C0 $B7 $28 $05 $DD $36 $02
.db $00 $C9 $DD $36 $02 $01 $3A $EB $C5 $B7 $28 $0E $3A $A6 $C6 $87
.db $87 $87 $ED $44 $C6 $9F $DD $77 $06 $C9 $AF $32 $C2 $C6 $21 $01
.db $C1 $11 $21 $00 $D9 $21 $AD $40 $06 $10 $7E $23 $08 $7E $23 $D9
.db $77 $2B $08 $77 $19 $D9 $10 $F2 $21 $04 $42 $22 $00 $C1 $3A $A6
.db $C6 $87 $87 $87 $ED $44 $C6 $9F $21 $06 $C1 $11 $1D $00 $06 $10
.db $0E $C8 $77 $23 $23 $23 $71 $19 $10 $F8 $C9 $DD $36 $1C $81 $DD
.db $36 $1D $00 $DD $36 $02 $01 $DD $36 $03 $10 $DD $36 $0E $3C $DD
.db $36 $00 $E0 $DD $36 $01 $41 $C9 $CD $FD $42 $DD $35 $0E $F0 $AF
.db $32 $C2 $C6 $CD $EA $05 $DD $36 $0E $08 $DD $36 $00 $FA $DD $36
.db $01 $41 $DD $35 $0E $F0 $3E $01 $32 $09 $C0 $C9 $DD $36 $1C $81
.db $DD $36 $1D $00 $DD $36 $02 $01 $DD $36 $03 $10 $DD $36 $0E $3C
.db $DD $36 $00 $21 $DD $36 $01 $42 $C9 $CD $FD $42 $DD $35 $0E $F0
.db $AF $32 $C2 $C6 $CD $EA $05 $DD $36 $0E $08 $DD $36 $00 $3B $DD
.db $36 $01 $42 $DD $35 $0E $F0 $3E $01 $32 $08 $C0 $C9 $DD $36 $1C
.db $00 $DD $36 $1D $81 $C3 $E8 $42 $DD $36 $1C $00 $DD $36 $1D $7F
.db $C3 $E8 $42 $DD $36 $1C $A7 $DD $36 $1D $59 $C3 $E8 $42 $DD $36
.db $1C $A7 $DD $36 $1D $A7 $C3 $E8 $42 $DD $36 $1C $D0 $DD $36 $1D
.db $75 $C3 $E8 $42 $DD $36 $1C $D0 $DD $36 $1D $8B $18 $62 $DD $36
.db $1C $8B $DD $36 $1D $30 $18 $58 $DD $36 $1C $8B $DD $36 $1D $D0
.db $18 $4E $DD $36 $1C $F1 $DD $36 $1D $4E $18 $44 $DD $36 $1C $F1
.db $DD $36 $1D $B2 $18 $3A $DD $36 $1C $D4 $DD $36 $1D $42 $18 $30
.db $DD $36 $1C $D4 $DD $36 $1D $BE $18 $26 $DD $36 $1C $BE $DD $36
.db $1D $2C $18 $1C $DD $36 $1C $BE $DD $36 $1D $D4 $18 $12 $DD $36
.db $1C $B2 $DD $36 $1D $0F $18 $08 $DD $36 $1C $B2 $DD $36 $1D $F1
.db $DD $36 $02 $01 $DD $36 $03 $10 $DD $36 $0E $3C $DD $36 $00 $FD
.db $DD $36 $01 $42 $C9 $DD $6E $05 $DD $66 $06 $DD $5E $1C $16 $00
.db $CB $7B $28 $02 $16 $FF $19 $DD $75 $05 $DD $74 $06 $DD $6E $08
.db $DD $66 $09 $DD $5E $1D $16 $00 $CB $7B $28 $02 $16 $FF $19 $DD
.db $75 $08 $DD $74 $09 $C9 $DD $7E $0C $B7 $28 $04 $DD $35 $0C $C9
.db $DD $6E $0D $DD $66 $0E $7E $DD $77 $0C $23 $DD $7E $0B $3C $BE
.db $30 $0E $DD $77 $0B $23 $5F $16 $00 $19 $7E $DD $77 $03 $B7 $C9
.db $37 $C9

_LABEL_435A_:
	ld e, (ix+16)
	ld d, (ix+17)
	ld hl, $000F
	add hl, de
	ld a, (hl)
	or a
	ret nz
	dec de
	ld (ix+16), e
	ld (ix+17), d
	ret

_LABEL_436F_:
	ld e, (ix+16)
	ld d, (ix+17)
	ld hl, $0011
	add hl, de
	ld a, (hl)
	or a
	ret nz
	inc de
	ld (ix+16), e
	ld (ix+17), d
	ret

_LABEL_4384_:
	bit 4, a
	jr nz, +
	ld d, (ix+21)
	ld a, (ix+20)
	ld (ix+21), a
	ld a, (ix+19)
	ld (ix+20), a
	ld (ix+19), d
	jr ++

+:
	ld d, (ix+19)
	ld a, (ix+20)
	ld (ix+19), a
	ld a, (ix+21)
	ld (ix+20), a
	ld (ix+21), d
++:
	ld a, $95
	ld (var.audio.request), a
	ret

_LABEL_43B4_:
	call _LABEL_12A9_
	and $07
	cp c
	jr nc, _LABEL_43B4_
	inc a
	ex af, af'
	ld a, (_RAM_C005_)
	bit 1, a
	jr nz, +
	ex af, af'
	ret

+:
	bit 0, (ix+31)
	jr nz, +
	ex af, af'
	ret

+:
	ex af, af'
	add a, $06
	ret

_LABEL_43D3_:
	ld a, (_RAM_C006_)
	or a
	jr nz, _LABEL_4426_
	ld a, (_RAM_C005_)
	and $07
	jr z, ++
-:
	ld a, (_RAM_C6A8_)
	bit 0, (ix+31)
	jr z, +
	ld a, (_RAM_C6AD_)
+:
	add a, $04
	ld c, a
	call _LABEL_43B4_
	ld (ix+22), a
	call _LABEL_43B4_
	ld (ix+23), a
	call _LABEL_43B4_
	ld (ix+24), a
	ret

++:
	ld hl, (_RAM_C697_)
	ld de, (_RAM_C6BE_)
	xor a
	sbc hl, de
	ld de, $012C
	sbc hl, de
	jr c, -
	ld hl, (_RAM_C6BE_)
	add hl, de
	ld (_RAM_C6BE_), hl
	ld a, $0D
	ld (ix+22), a
	ld (ix+23), a
	ld (ix+24), a
	ret

_LABEL_4426_:
	ld d, a
	add a, a
	add a, d
	ld e, a
	ld d, $00
	ld hl, $4440
	add hl, de
	ld a, (hl)
	ld (ix+22), a
	inc hl
	ld a, (hl)
	ld (ix+23), a
	inc hl
	ld a, (hl)
	ld (ix+24), a
	ld hl, _RAM_C006_
	inc (hl)
	ret

; Data from 4443 to 4463 (33 bytes)
.db $01 $06 $04 $0D $0D $0D $02 $05 $03 $01 $05 $03 $06 $06 $03 $02
.db $03 $01 $03 $05 $03 $01 $01 $06 $02 $03 $04 $06 $05 $01 $04 $04
.db $02

_LABEL_4464_:
	ld l, (ix+16)
	ld h, (ix+17)
	ld de, $0008
	ld a, (ix+19)
	ld (hl), a
	add hl, de
	ld a, (ix+20)
	ld (hl), a
	add hl, de
	ld a, (ix+21)
	ld (hl), a
	ret

_LABEL_447C_:
	ld l, (ix+16)
	ld h, (ix+17)
	ld de, $0008
	xor a
	ld (hl), a
	add hl, de
	ld (hl), a
	add hl, de
	ld (hl), a
	ret

_LABEL_448C_:
	ld hl, $0040
	ld de, $38D6
	ld a, (_RAM_C005_)
	bit 1, a
	jr nz, _LABEL_44B4_
	ld de, $38D2
	ld a, (_RAM_C005_)
	and $04
	jr z, _LABEL_44B4_
	exx
	ld de, $0000
	ld bc, $0000
	exx
	call _LABEL_44BF_
	ld hl, $0040
	ld de, $38EC
_LABEL_44B4_:
	exx
	ld e, (ix+22)
	ld d, (ix+23)
	ld c, (ix+24)
	exx
_LABEL_44BF_:
	rst $08	; setVdpAddress
	exx
	ld a, e
	out (Port_VDPData), a
	ld a, $19
	jr +

+:
	out (Port_VDPData), a
	exx
	ex de, hl
	add hl, de
	ex de, hl
	rst $08	; setVdpAddress
	exx
	ld a, d
	out (Port_VDPData), a
	ld a, $19
	jr +

+:
	out (Port_VDPData), a
	exx
	ex de, hl
	add hl, de
	ex de, hl
	rst $08	; setVdpAddress
	exx
	ld a, c
	out (Port_VDPData), a
	ld a, $19
	jr +

+:
	out (Port_VDPData), a
	ret

_LABEL_44E9_:
	ld hl, $0040
	ld de, $38E8
	ld a, (_RAM_C005_)
	bit 1, a
	jr nz, _LABEL_44B4_
	ld de, $38EC
	ld a, (_RAM_C005_)
	and $04
	jr z, _LABEL_44B4_
	exx
	ld de, $0000
	ld bc, $0000
	exx
	call _LABEL_44BF_
	ld hl, $0040
	ld de, $38D2
	jp _LABEL_44B4_

_LABEL_4514_:
	ld e, a
	ld d, $00
	ld hl, _DATA_4520_
	add hl, de
	ld a, (hl)
	ld (ix+25), a
	ret

; Data from 4520 to 4529 (10 bytes)
_DATA_4520_:
.db $20 $18 $14 $10 $0C $09 $06 $03 $01 $00

_LABEL_452A_:
	xor a
	call _LABEL_2D4D_
	ld a, $48
	bit 0, (ix+31)
	jr z, +
	ld a, $E8
+:
	ld (_RAM_C149_), a
	ld (_RAM_C169_), a
	ld (_RAM_C189_), a
	ld (_RAM_C1A9_), a
	ld (_RAM_C1C9_), a
	ld hl, _RAM_C1C3_
	ld de, $0020
	exx
	ld hl, _RAM_C6B6_
	ld b, $04
	xor a
-:
	or (hl)
	jr z, +
	ld a, (hl)
	add a, $05
+:
	exx
	ld (hl), a
	sbc hl, de
	exx
	inc hl
	djnz -
	ld hl, $3399
	ld (_RAM_C140_), hl
	ld hl, $33A3
	ld (_RAM_C160_), hl
	ld hl, $33A9
	ld (_RAM_C180_), hl
	ld hl, $33AF
	ld (_RAM_C1A0_), hl
	ld hl, $33B5
	ld (_RAM_C1C0_), hl
	ret

; Data from 4581 to 45C9 (73 bytes)
.db $DD $36 $0E $02 $DD $6E $10 $DD $66 $11 $DD $7E $19 $B7 $28 $11
.db $FE $04 $38 $02 $3E $04 $4F $3E $14 $91 $CD $C1 $45 $3C $0D $20
.db $F9 $AF $CD $C1 $45 $DD $7E $19 $FE $05 $38 $10 $DD $6E $10 $DD
.db $66 $11 $11 $F8 $FF $19 $DD $75 $10 $DD $74 $11 $DD $35 $19 $C9
.db $06 $06 $77 $23 $10 $FC $23 $23 $C9

_LABEL_45CA_:
	ld hl, _RAM_DD10_
	ld a, (hl)
	or a
	jr z, +
	ret p
	dec (hl)
	jp _LABEL_498F_

+:
	call _LABEL_4649_
	call _LABEL_4630_
	call _LABEL_4670_
	call _LABEL_46D6_
	ld ix, _RAM_DD40_
	bit 7, (ix+0)
	call nz, _LABEL_4A5B_
	ld ix, _RAM_DD70_
	bit 7, (ix+0)
	call nz, _LABEL_4A5B_
	ld ix, _RAM_DDA0_
	bit 7, (ix+0)
	call nz, _LABEL_4A5B_
	ld ix, _RAM_DDD0_
	bit 7, (ix+0)
	call nz, _LABEL_4AEA_
	ld ix, _RAM_DE00_
	bit 7, (ix+0)
	call nz, _LABEL_4A5B_
	ld ix, _RAM_DE30_
	bit 7, (ix+0)
	call nz, _LABEL_4A5B_
	ld ix, _RAM_DE60_
	bit 7, (ix+0)
	call nz, _LABEL_4A5B_
	ret

_LABEL_4630_:
	ld hl, _RAM_DD01_
	ld a, (hl)
	or a
	ret z
	dec (hl)
	ret nz
	ld a, (_RAM_DD02_)
	ld (hl), a
	ld hl, _RAM_DD4A_
	ld de, $0030
	ld b, $04
-:
	inc (hl)
	add hl, de
	djnz -
	ret

_LABEL_4649_:
	ld de, _RAM_DD04_
	call +
	call +
+:
	ld a, (de)
	and $7F
	jr z, ++
	dec a
	ld hl, $4C99
	ld c, a
	call _LABEL_49C0_
	ld hl, _RAM_DD0B_
	cp (hl)
	jr c, +
	and $7F
	ld (hl), a
	ld a, (de)
	ld (_RAM_DD03_), a
+:
	xor a
	ld (de), a
++:
	inc de
	ret

_LABEL_4670_:
	ld a, (audioFadeOutTimer_RAM_DD0E_)
	or a
	ret z
	ld a, (audio_RAM_DD0F_)
	dec a
	jr z, +
	ld (audio_RAM_DD0F_), a
	ret

+:
	ld a, $12
	ld (audio_RAM_DD0F_), a
	ld a, (audioFadeOutTimer_RAM_DD0E_)
	dec a
	ld (audioFadeOutTimer_RAM_DD0E_), a
	jp z, _LABEL_497C_
	ld hl, _RAM_DD48_
	ld de, $0030
	ld b, $04
-:
	ld a, (hl)
	inc a
	cp $10
	jr nc, +
	ld (hl), a
+:
	add hl, de
	djnz -
	ret

; 1st entry of Jump Table from 46F4 (indexed by _RAM_DD03_)
_LABEL_46A1_:
	ld a, $0C
	ld (audioFadeOutTimer_RAM_DD0E_), a
	ld a, $12
	ld (audio_RAM_DD0F_), a
	jp _LABEL_471F_

; Data from 46AE to 46D5 (40 bytes)
.db $AF $32 $0B $DD $32 $00 $DE $32 $30 $DE $32 $60 $DE $21 $70 $DD
.db $CB $96 $21 $A0 $DD $CB $96 $21 $D0 $DD $CB $96 $21 $AB $49 $0E
.db $7F $06 $03 $ED $B3 $C3 $1F $47

_LABEL_46D6_:
	ld a, (_RAM_DD03_)
	bit 7, a
	jp z, _LABEL_497C_
	cp $90
	jr c, +
	cp $A0
	jr c, _LABEL_4725_
	cp $A8
	jp nc, _LABEL_497C_
	sub $A0
	ld hl, _DATA_46F4_
	call _LABEL_49B6_
	jp (hl)

; Jump Table from 46F4 to 46F7 (2 entries, indexed by _RAM_DD03_)
_DATA_46F4_:
.dw _LABEL_46A1_ _LABEL_497C_

+:
	sub $81
	ret m
	ex af, af'
	call _LABEL_497C_
	ex af, af'
	ld hl, $4CC1
	ld c, a
	ex af, af'
	call _LABEL_49C0_
	ld (_RAM_DD01_), a
	ld (_RAM_DD02_), a
	ex af, af'
	ld hl, $4CCA
	call _LABEL_49B6_
	ld b, (hl)
	inc hl
	ld de, _RAM_DD40_
-:
	call _LABEL_4767_
	djnz -
_LABEL_471F_:
	ld a, $80
	ld (_RAM_DD03_), a
	ret

_LABEL_4725_:
	sub $90
	ld hl, _DATA_4CE8_
	call _LABEL_49B6_
	ld b, (hl)
	inc hl
-:
	inc hl
	ld a, (hl)
	dec hl
	cp $A0
	jr z, ++
	cp $C0
	jr z, +
	ld de, _RAM_DE60_
	ld iy, _RAM_DDD0_
	jr +++

+:
	ld de, _RAM_DE30_
	ld iy, _RAM_DDA0_
	jr +++

++:
	ld de, _RAM_DE00_
	ld iy, _RAM_DD70_
+++:
	push de
	call +
	pop ix
	xor a
	exx
	call _LABEL_4B44_
	exx
	djnz -
	jr _LABEL_471F_

+:
	set 2, (iy+0)
_LABEL_4767_:
	ld c, $39
	push de
	pop ix
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ldi
	ld a, c
	ld (de), a
	inc de
	xor a
	ld (ix+39), a
	ld (ix+40), a
	ld (ix+41), a
	inc a
	ld (de), a
	push hl
	ld hl, $0026
	add hl, de
	ex de, hl
	pop hl
	ret

_LABEL_4795_:
	bit 7, (ix+6)
	ret z
	bit 1, (ix+0)
	ret nz
	ld e, (ix+16)
	ld d, (ix+17)
	push ix
	pop hl
	ld b, $00
	ld c, $14
	add hl, bc
	ex de, hl
	ldi
	ldi
	ldi
	ld a, (hl)
	srl a
	ld (de), a
	xor a
	ld (ix+18), a
	ld (ix+19), a
	ret

_LABEL_47C0_:
	bit 7, (ix+7)
	ret z
	bit 1, (ix+0)
	ret nz
	bit 7, (ix+29)
	ret nz
	ld a, $FF
	ld (ix+31), a
	and $10
	or (ix+30)
	ld (ix+29), a
	ret

_LABEL_47DD_:
	ld l, (ix+11)
	ld h, (ix+12)
	ld a, (ix+6)
	or a
	ret z
	dec (ix+20)
	ret nz
	inc (ix+20)
	push hl
	ld l, (ix+18)
	ld h, (ix+19)
	dec (ix+21)
	jr nz, +
	ld e, (ix+16)
	ld d, (ix+17)
	push de
	pop iy
	ld a, (iy+1)
	ld (ix+21), a
	ld a, (ix+22)
	ld c, a
	and $80
	rlca
	neg
	ld b, a
	add hl, bc
	ld (ix+18), l
	ld (ix+19), h
+:
	pop bc
	add hl, bc
	dec (ix+23)
	ret nz
	ld a, (iy+3)
	ld (ix+23), a
	ld a, (ix+22)
	neg
	ld (ix+22), a
	ret

_LABEL_4830_:
	ld a, (ix+7)
	or a
	ret z
	bit 4, (ix+29)
	jr z, ++
	ld d, (ix+32)
	ld a, (ix+31)
	sub d
	jr nc, +
	xor a
+:
	or a
	ld (ix+31), a
	jr nz, _LABEL_48C3_
	ld a, (ix+29)
	xor $30
	ld (ix+29), a
	jr _LABEL_48C3_

++:
	bit 5, (ix+29)
	jr z, +++
	ld a, (ix+31)
	ld d, (ix+33)
	ld e, (ix+34)
	add a, d
	jr c, +
	cp e
	jr c, ++
+:
	ld a, e
++:
	cp e
	ld (ix+31), a
	jr nz, _LABEL_48C3_
	ld a, (ix+29)
	bit 3, (ix+29)
	jr z, +
	xor $30
	jr ++

+:
	xor $60
++:
	ld (ix+29), a
	jr _LABEL_48C3_

+++:
	bit 6, (ix+29)
	jr z, ++
	ld a, (ix+31)
	ld d, (ix+35)
	add a, d
	jr nc, +
	ld a, $FF
+:
	cp $FF
	ld (ix+31), a
	jr nz, _LABEL_48C3_
	ld a, (ix+29)
	and $8F
	ld (ix+29), a
	jr _LABEL_48C3_

++:
	ld a, (ix+31)
	ld d, (ix+36)
	add a, d
	jr nc, +
	ld a, (ix+29)
	and $0F
	ld (ix+29), a
	ld a, $FF
	ld (ix+31), a
	jp _LABEL_4AD9_

+:
	ld (ix+31), a
_LABEL_48C3_:
	ld a, (ix+31)
	rrca
	rrca
	rrca
	rrca
	and $0F
	ret

_LABEL_48CD_:
	bit 1, (ix+0)
	ret nz
	bit 7, (ix+7)
	jp z, _LABEL_4AD9_
	ld a, (ix+29)
	and $0F
	or $80
	ld (ix+29), a
	ret

_LABEL_48E4_:
	res 1, (ix+0)
	res 4, (ix+0)
	ld e, (ix+3)
	ld d, (ix+4)
_LABEL_48F2_:
	ld a, (de)
	inc de
	cp $E0
	jp nc, _LABEL_4AEB_
	bit 3, (ix+0)
	jp nz, _LABEL_495E_
	cp $80
	jr c, _LABEL_4928_
	jr z, _LABEL_4959_
	ex af, af'
	ld a, (ix+29)
	and $7F
	ld (ix+29), a
	ex af, af'
	call _LABEL_49AE_
	ld (ix+11), l
	ld (ix+12), h
_LABEL_4919_:
	ld a, (de)
	inc de
	or a
	jp p, _LABEL_4928_
	ld a, (ix+13)
	ld (ix+10), a
	dec de
	jr +

_LABEL_4928_:
	call _LABEL_49C5_
	ld (ix+10), a
	ld (ix+13), a
+:
	ld (ix+3), e
	ld (ix+4), d
	bit 1, (ix+0)
	ret nz
	bit 6, (ix+0)
	jr nz, +
	res 5, (ix+0)
+:
	ld a, (ix+15)
	ld (ix+14), a
	xor a
	ld (ix+21), a
	bit 7, (ix+7)
	ret nz
	ld (ix+31), a
	ret

_LABEL_4959_:
	call _LABEL_48CD_
	jr _LABEL_4919_

_LABEL_495E_:
	ld h, a
	ld a, (de)
	inc de
	ld l, a
	or h
	jr z, ++
	ld b, $00
	ld a, (ix+5)
	or a
	ld c, a
	jp p, +
	dec b
+:
	add hl, bc
++:
	ld (ix+11), l
	ld (ix+12), h
	ld a, (de)
	inc de
	jp _LABEL_4928_

; 2nd entry of Jump Table from 46F4 (indexed by _RAM_DD03_)
_LABEL_497C_:
	push hl
	push bc
	push de
	ld hl, _RAM_DD03_
	ld de, _RAM_DD03_ + 1
	ld bc, $018C
	ld (hl), $00
	ldir
	pop de
	pop bc
	pop hl
_LABEL_498F_:
	push hl
	push bc
	ld hl, _DATA_49A4_
	ld b, $0A
	ld c, Port_PSG
	otir
	ld a, $FF
	ld (_RAM_DD0A_), a
	pop bc
	pop hl
	jp _LABEL_471F_

; Data from 49A4 to 49AD (10 bytes)
_DATA_49A4_:
.db $80 $00 $A0 $00 $C0 $00 $9F $BF $DF $FF

_LABEL_49AE_:
	and $7F
	add a, (ix+5)
	ld hl, _DATA_49CF_
_LABEL_49B6_:
	ld c, a
	ld b, $00
	add hl, bc
	add hl, bc
	ld c, (hl)
	inc hl
	ld h, (hl)
	ld l, c
	ret

_LABEL_49C0_:
	ld b, $00
	add hl, bc
	ld a, (hl)
	ret

_LABEL_49C5_:
	ld b, (ix+2)
	dec b
	ret z
	ld c, a
-:
	add a, c
	djnz -
	ret

; Data from 49CF to 4A5A (140 bytes)
_DATA_49CF_:
.db $56 $03 $26 $03 $F9 $02 $CE $02 $A5 $02 $80 $02 $5C $02 $3A $02
.db $1A $02 $FB $01 $DF $01 $C4 $01 $AB $01 $93 $01 $7D $01 $67 $01
.db $53 $01 $40 $01 $2E $01 $1D $01 $0D $01 $FE $00 $EF $00 $E2 $00
.db $D6 $00 $C9 $00 $BE $00 $B4 $00 $A9 $00 $A0 $00 $97 $00 $8F $00
.db $87 $00 $7F $00 $78 $00 $71 $00 $6B $00 $65 $00 $5F $00 $5A $00
.db $55 $00 $50 $00 $4B $00 $47 $00 $43 $00 $40 $00 $3C $00 $39 $00
.db $36 $00 $33 $00 $30 $00 $2D $00 $2B $00 $28 $00 $26 $00 $24 $00
.db $22 $00 $20 $00 $1F $00 $1D $00 $1B $00 $1A $00 $18 $00 $17 $00
.db $16 $00 $15 $00 $13 $00 $12 $00 $11 $00 $00 $00

_LABEL_4A5B_:
	dec (ix+10)
	jr nz, +
	call _LABEL_48E4_
	bit 4, (ix+0)
	ret nz
	call _LABEL_4795_
	call _LABEL_47C0_
	jr ++

+:
	ld a, (ix+14)
	or a
	jr z, +
	dec (ix+14)
	call z, _LABEL_48CD_
+:
	ld a, (ix+6)
	or a
	jr z, +++
++:
	call _LABEL_47DD_
	bit 2, (ix+0)
	ret nz
	bit 6, (ix+0)
	ret nz
	bit 5, (ix+0)
	jr nz, +++
	ld d, $00
	ld a, (ix+37)
	or a
	jp p, +
	dec d
+:
	ld e, a
	add hl, de
	ld a, (ix+1)
	cp $E0
	jr nz, +
	ld a, $C0
+:
	ld c, a
	ld a, l
	and $0F
	or c
	out (Port_PSG), a
	ld a, l
	and $F0
	or h
	rrca
	rrca
	rrca
	rrca
	out (Port_PSG), a
+++:
	call _LABEL_4830_
	bit 2, (ix+0)
	ret nz
	bit 4, (ix+0)
	ret nz
	add a, (ix+8)
	bit 4, a
	jr z, +
	ld a, $0F
+:
	or (ix+1)
	add a, $10
	out (Port_PSG), a
	ret

_LABEL_4AD9_:
	set 4, (ix+0)
	bit 2, (ix+0)
	ret nz
	ld a, $1F
	add a, (ix+1)
	out (Port_PSG), a
	ret

_LABEL_4AEA_:
	ret

_LABEL_4AEB_:
	ld hl, +	; Overriding return address
	push hl
	sub $E0
	ld hl, _DATA_4B03_
	add a, a
	ld c, a
	ld b, $00
	add hl, bc
	ld c, (hl)
	inc hl
	ld h, (hl)
	ld l, c
	ld a, (de)
	jp (hl)

+:
	inc de
	jp _LABEL_48F2_

; Jump Table from 4B03 to 4B42 (32 entries, indexed by unknown)
_DATA_4B03_:
.dw _LABEL_4C43_ _LABEL_4C3E_ _LABEL_4B45_ _LABEL_4B4D_ _LABEL_4C42_ _LABEL_4B43_ _LABEL_4B5D_ _LABEL_4C38_
.dw _LABEL_4C20_ _LABEL_4C43_ _LABEL_4C42_ _LABEL_4C42_ _LABEL_4C42_ _LABEL_4C42_ _LABEL_4C42_ _LABEL_4C6F_
.dw _LABEL_4C2A_ _LABEL_4C6F_ _LABEL_4B9E_ _LABEL_4B78_ _LABEL_4B94_ _LABEL_4B90_ _LABEL_4B98_ _LABEL_4C09_
.dw _LABEL_4BDC_ _LABEL_4BF6_ _LABEL_4B78_ _LABEL_4B71_ _LABEL_4C6E_ _LABEL_4B9E_ _LABEL_4B53_ _LABEL_4B44_

; 6th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B43_:
	ret

; 32nd entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B44_:
	ret

; 3rd entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B45_:
	ld a, (_RAM_DD09_)
	ld (ix+5), a
	dec de
	ret

; 4th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B4D_:
	ld a, (_RAM_DD08_)
	ld (_RAM_DD07_), a
; 31st entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B53_:
	ld a, (_RAM_DD08_)
	or a
	jr z, _LABEL_4B98_
	inc de
	inc de
	jr _LABEL_4B98_

; 7th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B5D_:
	ex af, af'
	ld a, (audioFadeOutTimer_RAM_DD0E_)
	or a
	ret nz
	ex af, af'
	res 4, (ix+0)
	add a, (ix+8)
	and $0F
	ld (ix+8), a
	ret

; 28th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B71_:
	add a, (ix+5)
	ld (ix+5), a
	ret

; 20th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B78_:
	out (Port_PSG), a
	or $FC
	inc a
	jr nz, +
	ld hl, _RAM_DDA0_
	set 6, (hl)
	ld a, $DF
	out (Port_PSG), a
	set 2, (hl)
	ret

+:
	set 6, (ix+0)
	ret

; 22nd entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B90_:
	ld (ix+7), a
	ret

; 21st entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B94_:
	ld (ix+6), a
	ret

; 23rd entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B98_:
	ex de, hl
	ld e, (hl)
	inc hl
	ld d, (hl)
	dec de
	ret

; 19th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4B9E_:
	ld a, (ix+1)
	cp $A0
	jr z, +++
	cp $C0
	jr z, ++
	bit 6, (ix+0)
	jr nz, +
	ld hl, _RAM_DDA0_
	res 2, (hl)
	res 6, (hl)
	set 4, (hl)
	ld hl, _RAM_DE30_
	ld (hl), $00
+:
	ld hl, _RAM_DDD0_
	jr ++++

++:
	ld hl, _RAM_DDA0_
	jr ++++

+++:
	ld hl, _RAM_DD70_
++++:
	res 2, (hl)
	set 4, (hl)
	or $1F
	out (Port_PSG), a
	xor a
	ld (_RAM_DD0B_), a
	ld (ix+0), a
	pop bc
	pop bc
	ret

; 25th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4BDC_:
	ld c, a
	inc de
	ld a, (de)
	ld b, a
	push bc
	push ix
	pop hl
	dec (ix+9)
	ld c, (ix+9)
	dec (ix+9)
	ld b, $00
	add hl, bc
	ld (hl), d
	dec hl
	ld (hl), e
	pop de
	dec de
	ret

; 26th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4BF6_:
	push ix
	pop hl
	ld c, (ix+9)
	ld b, $00
	add hl, bc
	ld e, (hl)
	inc hl
	ld d, (hl)
	inc (ix+9)
	inc (ix+9)
	ret

; 24th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4C09_:
	inc de
	add a, $27
	ld c, a
	ld b, $00
	push ix
	pop hl
	add hl, bc
	ld a, (hl)
	or a
	jr nz, +
	ld a, (de)
	ld (hl), a
+:
	inc de
	dec (hl)
	jp nz, _LABEL_4B98_
	inc de
	ret

; 9th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4C20_:
	call _LABEL_49C5_
	ld (ix+14), a
	ld (ix+15), a
	ret

; 17th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4C2A_:
	ld (ix+16), e
	ld (ix+17), d
	ld (ix+6), $80
	inc de
	inc de
	inc de
	ret

; 8th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4C38_:
	set 1, (ix+0)
	dec de
	ret

; 2nd entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4C3E_:
	ld (ix+37), a
	ret

; 5th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4C42_:
	ret

; 1st entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4C43_:
	ld (ix+7), $80
	ld (ix+31), $FF
	push de
	push ix
	pop hl
	ld b, $00
	ld c, $20
	add hl, bc
	ex de, hl
	ldi
	ldi
	ldi
	ldi
	ldi
	pop de
	inc de
	inc de
	inc de
	inc de
	ret

; Data from 4C65 to 4C6D (9 bytes)
.db $B7 $28 $02 $3E $08 $DD $77 $1E $C9

; 29th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4C6E_:
	ret

; 16th entry of Jump Table from 4B03 (indexed by unknown)
_LABEL_4C6F_:
	ld (bc), a
	ld bc, $0100
	ld (bc), a
	ld (bc), a
	inc bc
	inc bc
	inc b
	inc b
	add a, c
	dec b
	ld (bc), a
	nop
	nop
	ld bc, $0201
	ld (bc), a
	ld (bc), a
	ld (bc), a
	inc bc
	inc bc
	inc bc
	inc bc
	inc b
	inc b
	inc b
	inc b
	dec b
	dec b
	dec b
	dec b
	ld b, $06
	ld b, $06
	rlca
	rlca
	rlca
	ex af, af'
	add a, c
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	ld c, b
	ld d, b
	ld b, b
	ld (hl), b
	ld h, b
	jr nc, _LABEL_4D0F_
	ld h, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	ld (hl), b
	add a, b
	add a, b
	add a, b
	add a, b
	add a, b
	ld (hl), b
	ld (hl), b
	ld (hl), b
	nop
	nop
	inc b
	inc b
	nop
	inc b
	inc b
	add hl, bc
	ld b, $FA
	ld c, h
	ld hl, _DATA_764E_
	ld c, (hl)
	add hl, de
	ld d, e
	ld ($9E54), hl
	ld d, h
	add a, b
	ld e, c
	ld d, $61
	sub a
	ld h, d
	jp m, $FA4C	; Possibly invalid
; Data from 4CDF to 4CDF (1 bytes)
.db $4C

; Pointer Table from 4CE0 to 4CE7 (4 entries, indexed by _RAM_DD03_)
_DATA_4CE0_:
.dw _DATA_4CFA_ _DATA_4CFA_ _DATA_4CFA_ _DATA_4CFA_

; Pointer Table from 4CE8 to 4CF9 (9 entries, indexed by _RAM_DD03_)
_DATA_4CE8_:
.dw _DATA_6395_ _DATA_63D9_ _DATA_63FD_ _DATA_6425_ _DATA_6453_ _DATA_647B_ _DATA_648B_ _DATA_64AD_
.dw _DATA_6395_

; 1st entry of Pointer Table from 4CE0 (indexed by _RAM_DD03_)
; Data from 4CFA to 4D0E (21 bytes)
_DATA_4CFA_:
.db $03 $80 $80 $03 $16 $4D $E8 $00 $06 $02 $80 $A0 $03 $6F $4D $E8
.db $00 $06 $04 $80 $C0

_LABEL_4D0F_:
	inc bc
	jp z, $E84D	; Possibly invalid
; Data from 4D13 to 6394 (5762 bytes)
.incbin "columns_DATA_4D13_.inc"

; 1st entry of Pointer Table from 4CE8 (indexed by _RAM_DD03_)
; Data from 6395 to 6398 (4 bytes)
_DATA_6395_:
.db $02 $80 $A0 $01

; Pointer Table from 6399 to 639A (1 entries, indexed by unknown)
.dw _DATA_63AF_

; Data from 639B to 63A1 (7 bytes)
.db $00 $00 $01 $01 $80 $C0 $01

; Pointer Table from 63A2 to 63A3 (1 entries, indexed by unknown)
.dw _DATA_63A8_

; Data from 63A4 to 63A7 (4 bytes)
.db $00 $00 $01 $01

; 1st entry of Pointer Table from 63A2 (indexed by unknown)
; Data from 63A8 to 63AE (7 bytes)
_DATA_63A8_:
.db $FF $10 $E5 $01 $F6 $B5 $63

; 1st entry of Pointer Table from 6399 (indexed by unknown)
; Data from 63AF to 63D8 (42 bytes)
_DATA_63AF_:
.db $E1 $0A $FF $01 $E5 $00 $E2 $E0 $FF $01 $FF $00 $00 $C1 $03 $C3
.db $C5 $E6 $02 $FB $FF $F7 $00 $04 $BC $63 $C1 $03 $C3 $03 $C5 $03
.db $E6 $02 $FB $01 $F7 $00 $02 $C9 $63 $F2

; 2nd entry of Pointer Table from 4CE8 (indexed by _RAM_DD03_)
; Data from 63D9 to 63DC (4 bytes)
_DATA_63D9_:
.db $01 $80 $E0 $01

; Pointer Table from 63DD to 63DE (1 entries, indexed by unknown)
.dw _DATA_63E3_

; Data from 63DF to 63E2 (4 bytes)
.db $00 $00 $07 $01

; 1st entry of Pointer Table from 63DD (indexed by unknown)
; Data from 63E3 to 63FC (26 bytes)
_DATA_63E3_:
.db $F3 $E7 $F0 $01 $01 $FD $6F $E0 $FF $10 $20 $06 $14 $C3 $03 $B4
.db $03 $E6 $02 $E7 $F7 $00 $05 $F2 $63 $F2

; 3rd entry of Pointer Table from 4CE8 (indexed by _RAM_DD03_)
; Data from 63FD to 6400 (4 bytes)
_DATA_63FD_:
.db $01 $88 $E0 $01

; Pointer Table from 6401 to 6402 (1 entries, indexed by unknown)
.dw _DATA_6407_

; Data from 6403 to 6406 (4 bytes)
.db $00 $00 $00 $00

; 1st entry of Pointer Table from 6401 (indexed by unknown)
; Data from 6407 to 6424 (30 bytes)
_DATA_6407_:
.db $F3 $E3 $F0 $01 $01 $10 $05 $FF $01 $00 $30 $02 $FF $10 $00 $5D
.db $02 $FF $00 $00 $E3 $03 $E6 $02 $F7 $00 $05 $1A $64 $F2

; 4th entry of Pointer Table from 4CE8 (indexed by _RAM_DD03_)
; Data from 6425 to 6428 (4 bytes)
_DATA_6425_:
.db $02 $80 $A0 $01

; Pointer Table from 6429 to 642A (1 entries, indexed by unknown)
.dw _DATA_6438_

; Data from 642B to 6431 (7 bytes)
.db $00 $00 $00 $00 $80 $C0 $01

; Pointer Table from 6432 to 6433 (1 entries, indexed by unknown)
.dw _DATA_6438_

; Data from 6434 to 6437 (4 bytes)
.db $00 $00 $00 $00

; 1st entry of Pointer Table from 6429 (indexed by unknown)
; Data from 6438 to 6452 (27 bytes)
_DATA_6438_:
.db $E1 $FF $F0 $01 $02 $09 $06 $A5 $04 $A7 $B1 $A9 $AA $B2 $AC $AE
.db $E7 $B1 $02 $E6 $01 $F7 $00 $08 $48 $64 $F2

; 5th entry of Pointer Table from 4CE8 (indexed by _RAM_DD03_)
; Data from 6453 to 6456 (4 bytes)
_DATA_6453_:
.db $02 $80 $A0 $01

; Pointer Table from 6457 to 6458 (1 entries, indexed by unknown)
.dw _DATA_6466_

; Data from 6459 to 645F (7 bytes)
.db $00 $00 $00 $01 $80 $C0 $01

; Pointer Table from 6460 to 6461 (1 entries, indexed by unknown)
.dw _DATA_6468_

; Data from 6462 to 6465 (4 bytes)
.db $00 $00 $00 $01

; 1st entry of Pointer Table from 6457 (indexed by unknown)
; Data from 6466 to 6467 (2 bytes)
_DATA_6466_:
.db $E1 $03

; 1st entry of Pointer Table from 6460 (indexed by unknown)
; Data from 6468 to 647A (19 bytes)
_DATA_6468_:
.db $F0 $09 $03 $C0 $04 $A0 $03 $A2 $05 $A7 $05 $E6 $02 $F7 $00 $08
.db $6F $64 $F2

; 6th entry of Pointer Table from 4CE8 (indexed by _RAM_DD03_)
; Data from 647B to 647E (4 bytes)
_DATA_647B_:
.db $01 $88 $E0 $01

; Pointer Table from 647F to 6480 (1 entries, indexed by unknown)
.dw _DATA_6485_

; Data from 6481 to 6484 (4 bytes)
.db $00 $00 $00 $05

; 1st entry of Pointer Table from 647F (indexed by unknown)
; Data from 6485 to 648A (6 bytes)
_DATA_6485_:
.db $F3 $E4 $00 $10 $02 $F2

; 7th entry of Pointer Table from 4CE8 (indexed by _RAM_DD03_)
; Data from 648B to 648E (4 bytes)
_DATA_648B_:
.db $01 $88 $E0 $01

; Pointer Table from 648F to 6490 (1 entries, indexed by unknown)
.dw _DATA_6495_

; Data from 6491 to 6494 (4 bytes)
.db $00 $00 $00 $00

; 1st entry of Pointer Table from 648F (indexed by unknown)
; Data from 6495 to 64AC (24 bytes)
_DATA_6495_:
.db $F3 $E7 $F0 $01 $01 $26 $40 $00 $80 $03 $00 $30 $06 $00 $30 $0C
.db $E6 $02 $F7 $00 $06 $A2 $64 $F2

; 8th entry of Pointer Table from 4CE8 (indexed by _RAM_DD03_)
; Data from 64AD to 64B0 (4 bytes)
_DATA_64AD_:
.db $02 $80 $A0 $01

; Pointer Table from 64B1 to 64B2 (1 entries, indexed by unknown)
.dw _DATA_64C0_

; Data from 64B3 to 64B9 (7 bytes)
.db $00 $00 $00 $00 $80 $C0 $01

; Pointer Table from 64BA to 64BB (1 entries, indexed by unknown)
.dw _DATA_64C2_

; Data from 64BC to 64BF (4 bytes)
.db $00 $00 $00 $00

; 1st entry of Pointer Table from 64B1 (indexed by unknown)
; Data from 64C0 to 64C1 (2 bytes)
_DATA_64C0_:
.db $E1 $01

; 1st entry of Pointer Table from 64BA (indexed by unknown)
; Data from 64C2 to 764D (4492 bytes)
_DATA_64C2_:
.db $F0 $06 $01 $02 $08 $E0 $B4 $06 $14 $0A $1E $C0 $0A $FF $01 $C4
.db $02 $E7 $FF $10 $02 $E7 $FF $00 $04 $E6 $02 $F7 $00 $05 $CF $64
.db $F2
.dsb 4459, $FF

; Data from 764E to 7FEF (2466 bytes)
_DATA_764E_:
.dsb 2466, $FF

.BANK 1 SLOT 1
.ORG $0000

; Data from 7FF0 to 7FFF (16 bytes)
.db $54 $4D $52 $20 $53 $45 $47 $41 $54 $59 $6F $12 $20 $51 $00 $4F

.BANK 2
.ORG $0000

; Data from 8000 to 8011 (18 bytes)
_DATA_8000_:
.db $10 $00 $00 $02 $1C $F8 $1C $02 $00 $00 $00 $40 $38 $1F $38 $40
.db $00 $00

; Data from 8012 to 8033 (34 bytes)
_DATA_8012_:
.db $00 $20
.dsb 15, $00
.db $3F
.dsb 15, $00
.db $3F

; Data from 8034 to 831F (748 bytes)
_DATA_8034_:
.db $48 $01 $7E $66 $6E $76 $66 $66 $7E $00 $38 $38 $18 $18 $18 $18
.db $3C $00 $7E $66 $66 $0C $18 $30 $7E $00 $7E $66 $06 $1C $06 $66
.db $7E $00 $6C $6C $6C $6C $7E $0C $0C $00 $7E $60 $60 $7E $06 $66
.db $7E $00 $7E $66 $60 $7E $66 $66 $7E $00 $7E $66 $66 $0C $18 $18
.db $18 $00 $7E $66 $66 $3C $66 $66 $7E $00 $7E $66 $66 $7E $06 $66
.db $7E $00 $18 $3C $24 $24 $7E $66 $66 $00 $7C $66 $66 $7C $66 $66
.db $7C $00 $3C $66 $66 $60 $66 $66 $3C $00 $7C $66 $66 $66 $66 $66
.db $7C $00 $7E $60 $60 $7C $60 $60 $7E $00 $7E $60 $60 $7C $60 $60
.db $60 $00 $3C $66 $60 $6E $66 $66 $3C $00 $66 $66 $66 $7E $66 $66
.db $66 $00 $3C $18 $18 $18 $18 $18 $3C $00 $7C $18 $18 $18 $58 $78
.db $30 $00 $62 $66 $6C $78 $6C $66 $62 $00 $60 $60 $60 $60 $60 $60
.db $7E $00 $82 $C6 $EE $FE $D6 $C6 $C6 $00 $46 $66 $76 $7E $6E $66
.db $62 $00 $3C $66 $66 $66 $66 $66 $3C $00 $7C $66 $66 $66 $7C $60
.db $60 $00 $3C $66 $66 $76 $6E $66 $3E $00 $7C $66 $66 $7C $6C $66
.db $66 $00 $3C $66 $60 $3C $06 $66 $3C $00 $7E $18 $18 $18 $18 $18
.db $18 $00 $66 $66 $66 $66 $66 $66 $3C $00 $66 $66 $66 $66 $24 $3C
.db $18 $00 $D6 $D6 $D6 $D6 $D6 $FE $6C $00 $42 $66 $3C $18 $3C $66
.db $42 $00 $66 $66 $66 $3C $18 $18 $18 $00 $7E $06 $3C $18 $3C $60
.db $7E $00 $00 $18 $18 $00 $18 $18 $00 $00 $18 $18 $18 $18 $00 $18
.db $18 $00 $3C $66 $66 $0C $18 $00 $18 $00 $00 $00 $00 $00 $00 $18
.db $18 $00 $3C $42 $5A $52 $5A $42 $3C $00 $A0 $01 $00 $38 $7C $EE
.db $C6 $C6 $C6 $C6 $C6 $FE $FE $C6 $C6 $C6 $C6 $00 $00 $FC $FE $C6
.db $C6 $C6 $C6 $FC $FC $C6 $C6 $C6 $C6 $FE $FC $00 $00 $7C $FE $C6
.db $C6 $C6 $C0 $C0 $C0 $C0 $C6 $C6 $C6 $FE $7C $00 $00 $FC $FE
.dsb 10, $C6
.db $FE $FC $00 $00 $FE $FE $C0 $C0 $C0 $C0 $FC $FC $C0 $C0 $C0 $C0
.db $FE $FE $00 $00 $FE $FE $C0 $C0 $C0 $C0 $FC $FC $C0 $C0 $C0 $C0
.db $C0 $C0 $00 $00 $7C $FE $C6 $C6 $C6 $C0 $C0 $DE $DE $C6 $C6 $C6
.db $FE $7C $00 $00 $C6 $C6 $C6 $C6 $C6 $C6 $FE $FE $C6 $C6 $C6 $C6
.db $C6 $C6 $00 $00 $FE $FE
.dsb 10, $38
.db $FE $FE $00 $00 $7E $7E $18 $18 $18 $18 $18 $18 $18 $18 $D8 $D8
.db $F8 $70 $00 $00 $C6 $C6 $CC $CC $D8 $D8 $F0 $F0 $D8 $D8 $CC $CC
.db $C6 $C6 $00 $00
.dsb 12, $C0
.db $FE $FE $00 $00 $C6 $C6 $EE $EE $FE $FE $D6 $D6 $C6 $C6 $C6 $C6
.db $C6 $C6 $00 $00 $C6 $C6 $C6 $E6 $E6 $F6 $F6 $DE $DE $CE $CE $C6
.db $C6 $C6 $00 $00 $7C $FE
.dsb 10, $C6
.db $FE $7C $00 $00 $FC $FE $C6 $C6 $C6 $C6 $FE $FC $C0 $C0 $C0 $C0
.db $C0 $C0 $00 $00 $7C $FE $C6 $C6 $C6 $C6 $C6 $C6 $C6 $C6 $DE $DE
.db $FE $7C $00 $00 $FC $FE $C6 $C6 $C6 $C6 $FE $FC $D8 $DC $CC $CE
.db $C6 $C6 $00 $00 $7C $FE $C6 $C6 $C0 $E0 $70 $1C $0E $06 $C6 $C6
.db $FE $7C $00 $00 $FE $FE
.dsb 12, $38
.db $00 $00
.dsb 12, $C6
.db $FE $7C $00 $00 $C6 $C6 $C6 $C6 $C6 $EE $6C $6C $6C $6C $7C $38
.db $38 $38 $00 $00 $C6 $C6 $D6 $D6 $D6 $D6 $D6 $D6 $D6 $D6 $FE $7C
.db $6C $6C $00 $00 $C6 $C6 $C6 $6C $6C $6C $38 $38 $6C $6C $6C $C6
.db $C6 $C6 $00 $00 $C6 $C6 $C6 $EE $6C $6C $7C $38 $38 $38 $38 $38
.db $38 $38 $00 $00 $FE $FE $06 $0C $0C $18 $18 $30 $30 $60 $60 $C0
.db $FE $FE $00

; Data from 8320 to 83C1 (162 bytes)
_DATA_8320_:
.db $A0 $00 $00 $7C $FE $C6 $C6 $C6 $E6 $F6 $DE $CE $C6 $C6 $C6 $FE
.db $7C $00 $00 $18 $38 $38
.dsb 9, $18
.db $7E $7E $00 $00 $7C $FE $C6 $C6 $C6 $0E $1C $38 $70 $E0 $C0 $C0
.db $FE $FE $00 $00 $7C $FE $C6 $C6 $C6 $06 $3C $3C $06 $C6 $C6 $C6
.db $FE $7C $00 $00 $0C $0C $1C $1C $3C $2C $6C $4C $CC $FE $FE $0C
.db $0C $0C $00 $00 $FE $FE $C0 $C0 $C0 $C0 $FC $FE $06 $06 $C6 $C6
.db $FE $7C $00 $00 $7C $FE $C6 $C6 $C0 $C0 $FC $FE $C6 $C6 $C6 $C6
.db $FE $7C $00 $00 $FE $FE $C6 $C6 $06 $0E $0C $1C $18 $18 $18 $18
.db $18 $18 $00 $00 $7C $FE $C6 $C6 $C6 $C6 $7C $FE $C6 $C6 $C6 $C6
.db $FE $7C $00 $00 $7C $FE $C6 $C6 $C6 $C6 $FE $7E $06 $06 $C6 $C6
.db $FE $7C $00

; Data from 83C2 to 86A0 (735 bytes)
_DATA_83C2_:
.db $40 $01 $00 $0F $1F $3F $78 $78 $78 $78 $00 $FE $FE $FE $1E $1E
.db $1E $1E $78 $78 $78 $7F $7F $7F $00 $00 $1E $1E $1E $FC $F8 $F0
.db $00 $00 $00 $03 $07 $07 $07 $03 $03 $03 $00 $C0 $C0 $C0 $C0 $C0
.db $C0 $C0 $03 $03 $03 $03 $03 $03 $00 $00 $C0 $C0 $C0 $C0 $C0 $C0
.db $00 $00 $00 $7F $7F $7F $00 $00 $03 $07 $00 $F8 $FC $FE $1E $1E
.db $FE $FC $0F $1E $3C $7F $7F $7F $00 $00 $F8 $00 $00 $FE $FE $FE
.db $00 $00 $00 $7F $7F $7F $00 $00 $03 $07 $00 $FE $FC $F8 $00 $00
.db $F8 $FC $0F $00 $00 $7F $7F $7F $00 $00 $FE $06 $06 $FE $FC $F8
.db $00 $00 $00 $01 $03 $07 $0F $1F $3F $7F $00 $FE $FC $F8 $F0 $E0
.db $FE $FE $7F $00 $01 $01 $01 $01 $00 $00 $FE $00 $FC $FC $FC $FC
.db $00 $00 $00 $7F $7F $7F $78 $78 $7F $7F $00 $FE $FE $FE $00 $00
.db $F8 $FC $7F $00 $00 $7F $7F $7F $00 $00 $FE $1E $1E $FE $FC $F8
.db $00 $00 $00 $0F $1F $3F $78 $78 $7F $7F $00 $FE $FE $FE $00 $00
.db $FC $FE $7F $78 $78 $7F $7F $7F $00 $00 $FE $1E $1E $FC $F8 $F0
.db $00 $00 $00 $7F $7F $7F $00 $00 $00 $01 $00 $FE $FE $FE $00 $7E
.db $FC $F8 $03 $07 $0F $1F $3F $7E $00 $00 $F0 $E0 $C0 $80 $00 $00
.db $00 $00 $00 $0F $1F $3F $78 $78 $7F $3F $00 $FE $FE $FE $1E $1E
.db $FC $FE $7F $78 $78 $7F $7F $7F $00 $00 $FE $1E $1E $FC $F8 $F0
.db $00 $00 $00 $0F $1F $3F $78 $78 $7F $7F $00 $FE $FE $FE $1E $1E
.db $FE $FE $3F $00 $00 $7F $7F $7F $00 $00 $FE $1E $1E $FC $F8 $F0
.db $00 $00 $01 $04 $07 $0A $0D $10 $13 $16 $02 $05 $08 $0B $0E $11
.db $14 $17 $03 $06 $09 $0C $0F $12 $15 $18 $00 $03 $00 $30 $3F $00
.db $0F $0F $30 $3F $40 $3F $40 $70 $8F $67 $98 $6F $90 $6F $90 $6F
.db $90 $6F $90 $67 $98 $70 $8F $3E $41 $3F $40 $0F $30 $00 $FF $7F
.db $80 $7F $80 $7F $80 $00 $FF $7F $80 $7F $80 $7F $80 $00 $FF $00
.db $FF $FE $01 $FE $01 $FE $01 $00 $FF $FE $01 $FE $01 $FE $01 $00
.db $FF $F0 $0F $FC $03 $7C $83 $0E $F1 $E6 $19 $F6 $09 $F6 $09 $F6
.db $09 $F6 $09 $E6 $19 $0E $F1 $FC $02 $FC $02 $F0 $0C $00 $F0 $00
.db $1F $1F $60 $7F $80 $7F $80 $E0 $1F $CF $30 $DF $20 $DF $20 $DC
.db $23 $DF $20 $DF $20 $C0 $3F $C0 $3F $DF $20 $DF $20 $DC $23 $DF
.db $20 $DF $20 $CF $30 $E0 $1F $7F $80 $7F $80 $1F $60 $00 $1F $00
.db $FE $FC $02 $FC $03 $FC $03 $01 $FE $FD $02 $FD $02 $FD $02 $01
.db $FE $F9 $06 $F9 $06 $01 $FE $01 $FE $F9 $06 $F9 $06 $01 $FE $FD
.db $02 $FD $02 $FD $02 $01 $FE $FC $03 $FC $03 $FC $02 $00 $FE $00
.db $3F $3F $C0 $FF $00 $FF $00 $C0 $3F $9F $60 $BF $40 $BF $40 $B0
.db $4F $B7 $48 $B7 $48 $B0 $4F $B0 $4F $B7 $48 $B7 $48 $B1 $4E $BF
.db $40 $BF $40 $9F $60 $C0 $3F $FF $00 $FF $00 $3F $C0 $00 $3F $00
.db $FC $F8 $04 $F8 $04 $F8 $04 $00 $FC $F8 $04 $F8 $04 $F8 $04 $00
.db $FC $F8 $04 $F8 $04 $18 $E5 $18 $E5 $D8 $25 $D9 $26 $D9 $26 $D9
.db $26 $DB $24 $DB $24 $1B $E4 $FE $01 $FE $01 $FE $01 $00 $FF $00
.db $0F $06 $19 $0F $30 $19 $26 $19 $26 $36 $49 $36 $49 $36 $49 $6F
.db $90 $6F $90 $6F $90 $D9 $26 $D9 $26 $D9 $26 $B0 $4F $B0 $4F $BF
.db $40 $7F $80 $7F $80 $00 $FF $7F $80 $7F $80 $7F $80 $00 $FF $00
.db $1C $1C $A2 $22 $DD $AA $55 $A6 $59 $CA $35 $DC $22 $C0 $3C $60
.db $90 $60 $90 $60 $90 $B0 $48 $B0 $48 $B0 $48 $D8 $24 $D8 $24 $D8
.db $24 $EC $12 $EC $12 $0C $F2 $FE $01 $FE $01 $FE $01 $00 $FF

; Data from 86A1 to 86B2 (18 bytes)
_DATA_86A1_:
.db $00 $10 $00 $3F $00 $00 $00 $00 $0F $03 $00 $10 $02 $20 $1B $30
.db $2F $34

; Data from 86B3 to 87FC (330 bytes)
_DATA_86B3_:
.db $02 $02 $00 $87 $01 $02 $03 $04 $05 $06 $07 $0A $00 $87 $07 $06
.db $05 $04 $08 $09 $01 $04 $00 $98 $0A $0B $0C $0D $0E $0F $10 $11
.db $12 $13 $14 $15 $15 $14 $13 $12 $11 $10 $16 $17 $18 $19 $1A $1B
.db $03 $00 $FF $1C $1D $1E $1F $20 $21 $22 $23 $24 $25 $26 $27 $26
.db $27 $28 $29 $26 $2A $2B $2C $2D $2E $2F $30 $31 $32 $00 $00 $33
.db $34 $35 $36 $37 $38 $39 $3A $3B $3C $36 $3D $36 $3D $3E $3F $40
.db $41 $3B $42 $43 $3C $44 $45 $46 $47 $00 $48 $49 $4A $4B $4B $4C
.db $4D $4B $4E $4F $50 $4B $51 $4B $51 $52 $53 $54 $55 $4F $56 $57
.db $50 $58 $59 $5A $4B $5B $5C $5D $5E $4B $4B $5F $60 $4B $61 $4F
.db $50 $4B $51 $4B $51 $52 $62 $63 $64 $4F $65 $66 $50 $4B $67 $68
.db $69 $5B $00 $6A $6B $6C $6D $6E $6F $70 $71 $72 $73 $74 $75 $76
.db $77 $78 $A7 $79 $7A $7B $72 $78 $7C $7D $6D $7E $7F $80 $00 $00
.db $81 $82 $83 $84 $26 $85 $86 $87 $88 $89 $8A $8B $8C $8D $8E $8F
.db $90 $91 $92 $93 $26 $94 $95 $96 $97 $98 $03 $00 $98 $99 $9A $9B
.db $9C $9D $9E $9F $A0 $A1 $A2 $A3 $A4 $A4 $A3 $A2 $A1 $A5 $A6 $A7
.db $16 $A8 $A9 $AA $AB $04 $00 $87 $AC $AD $AE $AF $B0 $B1 $B2 $0A
.db $00 $87 $B2 $B1 $B0 $AF $B3 $B4 $AC $02 $00 $00 $13 $00 $04 $02
.db $02 $00 $81 $02 $10 $00 $06 $02 $5F $00 $02 $04 $02 $00 $82 $04
.db $00 $07 $04 $03 $00 $85 $04 $00 $00 $04 $04 $03 $00 $81 $04 $14
.db $00 $81 $02 $0C $00 $81 $04 $0F $00 $81 $04 $14 $00 $04 $02 $1D
.db $00 $04 $02 $02 $00 $81 $02 $02 $00 $00

; Data from 87FD to 8845 (73 bytes)
_DATA_87FD_:
.db $02 $02 $00 $AE $B5 $B6 $B7 $B8 $B9 $BA $BB $BC $BD $00 $BE $BF
.db $C0 $C1 $C2 $C3 $C4 $C5 $C6 $C7 $C8 $C9 $CA $CB $CC $CD $CE $CF
.db $CF $CE $D0 $D1 $D1 $D0 $00 $D2 $D2 $00 $00 $D3 $D3 $00 $00 $D4
.db $D4 $00 $00 $1E $00 $02 $02 $02 $00 $02 $02 $02 $00 $81 $02 $03
.db $00 $81 $02 $03 $00 $82 $02 $00 $00

; Data from 8846 to 8898 (83 bytes)
_DATA_8846_:
.db $02 $B0 $B6 $B5 $00 $00 $BA $B9 $B8 $B7 $00 $BD $BC $BB $C1 $C0
.db $BF $BE $C5 $C4 $C3 $C2 $C9 $C8 $C7 $C6 $CD $CC $CB $CA $CE $CF
.db $CF $CE $D0 $D1 $D1 $D0 $00 $D2 $D2 $00 $00 $D3 $D3 $00 $00 $D4
.db $D4 $00 $00 $02 $02 $02 $00 $04 $02 $81 $00 $13 $02 $02 $00 $02
.db $02 $02 $00 $02 $02 $02 $00 $81 $02 $03 $00 $81 $02 $03 $00 $82
.db $02 $00 $00

; Data from 8899 to 9A14 (4476 bytes)
_DATA_8899_:
.incbin "columns_DATA_8899_.inc"

; Data from 9A15 to 9A26 (18 bytes)
palette_DATA_9A15_:
.db $10 $10 $00 $03 $38 $04 $33 $0B $0F $00 $0A $30 $02 $0C $22 $15
.db $2A $3F

; 1st entry of Pointer Table from 34B1 (indexed by _RAM_C6A9_)
; Data from 9A27 to 9AE8 (194 bytes)
_DATA_9A27_:
.db $04 $9C $18 $3C $3C $34 $0C $0C $3C $18 $24 $5A $7E $3C $24 $5A
.db $66 $5A $38 $18 $00 $00 $80 $80 $10 $00 $08 $42 $42 $7E $03 $40
.db $82 $00 $7E $06 $FF $89 $7E $18 $38 $78 $FC $FF $4E $2C $18 $00
.db $8A $24 $72 $72 $CB $F3 $72 $4A $24 $3C $66 $03 $7E $94 $24 $18
.db $3C $38 $18 $00 $00 $80 $80 $10 $00 $18 $66 $6E $7E $62 $62 $7E
.db $00 $7E $06 $FF $89 $7E $10 $34 $76 $EB $0C $30 $10 $00 $00 $83
.db $00 $30 $30 $03 $00 $8C $08 $00 $24 $42 $7E $3C $24 $00 $00 $18
.db $3C $7E $04 $FF $8D $7E $3C $18 $66 $6E $7E $62 $62 $7E $00 $40
.db $FC $60 $03 $40 $02 $00 $88 $18 $38 $78 $FC $FF $4E $2C $18 $00
.db $B0 $24 $72 $72 $CB $F3 $72 $4A $24 $24 $5A $7E $3C $24 $5A $66
.db $5A $38 $18 $3C $66 $86 $86 $76 $3C $08 $5A $D3 $FF $DD $DD $C1
.db $3C $7E $FD $F9 $E5 $E5 $C1 $81 $00 $10 $34 $76 $EB $0C $30 $10
.db $00 $00

; 2nd entry of Pointer Table from 34B1 (indexed by _RAM_C6A9_)
; Data from 9AE9 to 9B94 (172 bytes)
_DATA_9AE9_:
.db $04 $90 $FF $81 $BD $A5 $A5 $BD $81 $FF $FE $80 $B8 $A0 $A0 $80
.db $80 $00 $08 $FF $02 $FE $03 $FA $83 $C2 $FE $00 $08 $FF $88 $FE
.db $80 $B8 $A0 $A0 $80 $80 $00 $00 $02 $FE $03 $FA $85 $C2 $FE $00
.db $FE $FE $03 $FA $8D $C2 $FE $00 $FF $81 $BD $A5 $A5 $BD $81 $FF
.db $01 $01 $03 $05 $82 $3D $01 $09 $FF $88 $FE $80 $B8 $A0 $A0 $80
.db $80 $00 $00 $8A $FE $80 $B8 $A0 $A0 $80 $80 $00 $FE $FE $03 $FA
.db $83 $C2 $FE $00 $08 $FF $90 $FE $80 $B8 $A0 $A0 $80 $80 $00 $FE
.db $80 $B8 $A0 $A0 $80 $80 $00 $08 $FF $00 $92 $FF $81 $BD $A5 $A5
.db $BD $81 $FF $FF $81 $BD $A5 $A5 $BD $81 $FF $FE $FE $03 $FA $85
.db $C2 $FE $00 $01 $01 $03 $05 $85 $3D $01 $FF $FE $FE $03 $FA $8B
.db $C2 $FE $00 $FF $81 $BD $A5 $A5 $BD $81 $FF $00

; 3rd entry of Pointer Table from 34B1 (indexed by _RAM_C6A9_)
; Data from 9B95 to 9D96 (514 bytes)
_DATA_9B95_:
.db $04 $87 $00 $44 $EE $FE $7C $38 $10 $11 $00 $98 $10 $38 $38 $D6
.db $FE $10 $10 $38 $10 $10 $38 $FE $7C $7C $6C $82 $10 $6C $44 $82
.db $82 $44 $6C $10 $00 $87 $44 $AA $10 $00 $82 $44 $28 $03 $10 $86
.db $38 $7C $7C $38 $10 $10 $0B $00 $95 $28 $00 $C6 $00 $44 $10 $10
.db $38 $FE $7C $7C $6C $82 $00 $10 $38 $7C $7C $38 $10 $00 $00 $08
.db $00 $02 $10 $84 $38 $7C $7C $38 $03 $10 $8F $38 $7C $FE $FE $7C
.db $10 $7C $10 $38 $38 $D6 $FE $10 $10 $38 $10 $00 $00 $98 $44 $AA
.db $10 $00 $82 $44 $28 $10 $00 $28 $44 $82 $82 $44 $28 $00 $00 $28
.db $44 $82 $82 $6C $00 $44 $03 $00 $95 $28 $00 $C6 $00 $44 $00 $10
.db $10 $FE $38 $28 $44 $00 $10 $6C $44 $82 $82 $44 $6C $10 $00 $04
.db $A1 $7E $81 $99 $BD $BD $99 $81 $7E $7E $E1 $E1 $81 $81 $87 $87
.db $7E $7E $87 $87 $99 $99 $E1 $E1 $7E $7E $E7 $E7 $81 $81 $E7 $E7
.db $7E $7E $06 $81 $02 $7E $87 $E7 $81 $E7 $E7 $81 $E7 $7E $00 $02
.db $7E $96 $66 $42 $42 $66 $7E $00 $7E $1E $1E $7E $7E $78 $78 $00
.db $7E $78 $78 $66 $66 $1E $1E $00 $07 $7E $89 $00 $7E $18 $18 $66
.db $66 $18 $18 $00 $07 $7E $81 $00 $00 $A1 $7E $FF $E7 $C3 $C3 $E7
.db $FF $7E $7E $9F $9F $FF $FF $F9 $F9 $7E $7E $F9 $F9 $E7 $E7 $9F
.db $9F $7E $7E $99 $99 $FF $FF $99 $99 $7E $7E $06 $FF $02 $7E $06
.db $FF $81 $7E $00 $89 $7E $FF $E7 $C3 $C3 $E7 $FF $7E $7E $06 $FF
.db $02 $7E $06 $FF $02 $7E $02 $99 $02 $FF $02 $99 $02 $7E $02 $99
.db $02 $E7 $02 $99 $02 $7E $87 $99 $FF $99 $99 $FF $99 $7E $00 $04
.db $81 $1C $07 $00 $02 $3F $84 $03 $3F $3F $30 $04 $3F $87 $03 $1F
.db $1F $03 $3F $3F $FF $04 $CC $93 $C0 $0C $0C $FF $FF $F0 $FF $FF
.db $0F $FF $FF $3F $00 $00 $0F $00 $00 $0C $00 $00 $02 $1C $06 $0C
.db $90 $FF $C0 $0C $FC $C0 $C0 $CF $C0 $FF $C0 $0C $7C $60 $0C $FC
.db $C0 $04 $33 $02 $3F $02 $03 $02 $FF $8E $F0 $FF $FF $0F $FF $FF
.db $3F $00 $00 $0F $00 $00 $0C $00 $00 $02 $1C $06 $0C $91 $3F $00
.db $00 $3C $00 $00 $0F $00 $3F $3F $03 $1F $1F $03 $3F $3F $33 $03
.db $00 $81 $0C $03 $00 $8B $3F $00 $00 $0F $00 $00 $3C $00 $FF $FF
.db $F0 $05 $FF $00 $82 $7C $60 $06 $30 $91 $FF $C0 $0C $FC $C0 $C0
.db $CF $C0 $FF $C0 $0C $7C $60 $0C $FC $C0 $FF $04 $CC $93 $C0 $0C
.db $0C $3F $3F $30 $3F $3F $03 $3F $3F $FF $C0 $C0 $CF $C0 $CC $CC
.db $C0 $00

; 4th entry of Pointer Table from 34B1 (indexed by _RAM_C6A9_)
; Data from 9D97 to 9E3E (168 bytes)
_DATA_9D97_:
.db $04 $A9 $7E $81 $99 $BD $BD $99 $81 $7E $7E $E1 $E1 $81 $81 $87
.db $87 $7E $7E $87 $87 $99 $99 $E1 $E1 $7E $7E $E7 $E7 $81 $81 $E7
.db $E7 $7E $7E $E7 $E7 $99 $99 $E7 $E7 $7E $99 $06 $81 $81 $99 $00
.db $02 $7E $8E $66 $42 $42 $66 $7E $00 $7E $1E $1E $7E $7E $78 $78
.db $00 $07 $7E $89 $00 $7E $18 $18 $7E $7E $18 $18 $00 $07 $7E $89
.db $00 $18 $18 $7E $18 $18 $7E $18 $00 $00 $91 $7E $FF $E7 $C3 $C3
.db $E7 $FF $7E $7E $9F $9F $FF $FF $F9 $F9 $7E $7E $06 $FF $02 $7E
.db $02 $99 $02 $FF $02 $99 $02 $7E $02 $99 $02 $E7 $02 $99 $81 $7E
.db $08 $FF $00 $89 $7E $FF $E7 $C3 $C3 $E7 $FF $7E $7E $06 $FF $02
.db $7E $02 $F9 $02 $E7 $02 $9F $02 $7E $06 $FF $02 $7E $02 $99 $02
.db $E7 $02 $99 $81 $7E $08 $FF $00

; 5th entry of Pointer Table from 34B1 (indexed by _RAM_C6A9_)
; Data from 9E3F to 9F04 (198 bytes)
_DATA_9E3F_:
.db $04 $97 $30 $10 $02 $10 $00 $04 $20 $00 $06 $0C $54 $A4 $48 $14
.db $08 $00 $00 $10 $1E $30 $2C $22 $1C $05 $00 $8F $02 $04 $08 $00
.db $30 $FC $18 $31 $39 $3F $1E $00 $34 $18 $7E $03 $FF $82 $7E $18
.db $00 $AB $30 $16 $37 $3A $D4 $AE $74 $20 $06 $0C $14 $44 $A0 $48
.db $14 $08 $10 $6C $E0 $C0 $CC $C0 $60 $38 $00 $01 $01 $03 $07 $0E
.db $3E $F8 $30 $F4 $66 $CE $C6 $C0 $60 $18 $3C $18 $7E $03 $FF $82
.db $7E $18 $00 $02 $00 $85 $02 $10 $00 $04 $20 $04 $00 $85 $40 $00
.db $08 $00 $00 $03 $10 $83 $0E $1E $1C $03 $00 $02 $01 $88 $03 $07
.db $0E $3E $F8 $00 $08 $1E $03 $3F $8A $1E $00 $04 $00 $02 $2B $18
.db $26 $14 $00 $00 $A3 $30 $10 $02 $10 $00 $04 $20 $00 $06 $0C $14
.db $04 $A0 $40 $14 $08 $10 $7C $F0 $C0 $CC $C0 $60 $38 $03 $02 $02
.db $04 $0A $34 $C8 $00 $30 $40 $60 $03 $C0 $85 $60 $18 $14 $00 $1E
.db $03 $3F $82 $1E $00 $00

; Data from 9F05 to 9F8D (137 bytes)
_DATA_9F05_:
.db $01 $8E $7E $00 $7E $7E $81 $7E $FF $FF $C3 $3C $FF $FF $E7 $18
.db $03 $FF $8F $18 $FF $E7 $FF $3C $FF $C3 $FF $7E $FF $81 $7E $00
.db $7E $7E $08 $00 $04 $28 $04 $10 $04 $28 $10 $00 $04 $28 $04 $44
.db $81 $00 $03 $10 $04 $44 $04 $28 $09 $00 $03 $44 $81 $00 $03 $82
.db $8D $28 $00 $28 $28 $10 $00 $10 $10 $28 $00 $28 $28 $00 $03 $82
.db $81 $00 $03 $44 $04 $00 $84 $92 $00 $92 $92 $08 $00 $84 $82 $00
.db $82 $82 $08 $00 $84 $92 $00 $92 $92 $04 $00 $82 $10 $00 $06 $10
.db $84 $38 $10 $38 $38 $04 $FE $82 $7C $38 $03 $7C $8B $6C $7C $7C
.db $EE $44 $EE $EE $C6 $82 $C6 $C6 $00

; Data from 9F8E to A01F (146 bytes)
_DATA_9F8E_:
.db $90 $00 $00 $3F $7F $7F $70 $70 $70 $71 $00 $C7 $E7 $E7 $07 $07
.db $07 $E7 $00 $E0 $F0 $78 $3C $1E $0E $FE $00 $70 $79 $7F $7F $7F
.db $7F $7F $00 $E7 $E7 $E7 $E7 $E7 $E7 $E7 $00 $FE $FE $FE $00 $00
.db $F0 $00 $71 $7F $7F $3F $00 $3F $7F $7F $E7 $E7 $E7 $E7 $00 $C7
.db $E7 $E7 $FE $0E $0E $0E $00 $0E $0E $0E $76 $70 $70 $70 $00 $7F
.db $7F $7F $E7 $E7 $E7 $E7 $00 $E7 $E7 $E7 $00 $FE $FE $FE $00 $FC
.db $FE $FE $70 $70 $70 $70 $70 $7F $7F $3F $E7 $E7 $E7 $E7 $E7 $E7
.db $E7 $C7 $0E $0E $1E $3E $7C $F8 $F0 $E0 $70 $70 $7F $70 $70 $7F
.db $7F $7F $07 $07 $07 $07 $07 $E7 $E7 $E7 $0E $0E $0E $FC $FC $0E
.db $0E $0E

; Data from A020 to A0A1 (130 bytes)
_DATA_A020_:
.db $80 $00 $00 $79 $7D $6D $6D $6D $6D $7D $00 $F1 $F3 $83 $87 $86
.db $86 $F6 $00 $1E $9F $9B $DB $DB $DB $DB $00 $66 $66 $66 $66 $66
.db $3C $3C $79 $7D $6D $6D $6D $6D $6D $00 $F7 $87 $86 $86 $86 $F6
.db $F6 $00 $DB $DB $DB $DB $DB $DF $DE $00 $18 $18 $18 $18 $18 $18
.db $18 $00 $00 $78 $6C $6C $6D $6D $6D $7D $00 $46 $E6 $E6 $F6 $B6
.db $B6 $B6 $00 $CE $DF $DB $DB $D8 $D8 $DE $00 $7C $7C $60 $60 $60
.db $60 $78 $79 $61 $61 $61 $61 $61 $61 $00 $F6 $F6 $B6 $B6 $B6 $B6
.db $B3 $00 $CF $C3 $C3 $DB $DB $DF $8E $00 $78 $60 $60 $60 $60 $7C
.db $7C $00

; Data from A0A2 to A163 (194 bytes)
_DATA_A0A2_:
.db $C0 $00 $00 $66 $66 $66 $66 $66 $66 $66 $00 $63 $67 $66 $66 $66
.db $66 $66 $00 $E3 $F3 $33 $33 $33 $33 $33 $00 $0C $0C $8C $8C $CC
.db $CC $6C $66 $66 $66 $66 $7F $7F $39 $00 $66 $66 $66 $66 $E6 $E7
.db $C3 $00 $33 $33 $33 $33 $33 $F3 $E3 $00 $6C $3C $3C $1C $1C $0C
.db $0C $00 $00 $60 $60 $60 $60 $60 $60 $60 $00 $3C $7E $66 $66 $66
.db $66 $66 $00 $3C $7E $66 $66 $60 $60 $7C $00 $7E $7E $18 $18 $18
.db $18 $18 $60 $60 $60 $60 $60 $7E $7E $00 $66 $66 $66 $66 $66 $7E
.db $3C $00 $3E $06 $06 $66 $66 $7E $3C $00 $18 $18 $18 $18 $18 $18
.db $18 $00 $00 $7C $7E $66 $66 $66 $66 $66 $00 $F8 $FC $CC $CD $CD
.db $CD $FD $00 $63 $F3 $F3 $FB $9B $9B $9B $00 $6C $6C $6C $6C $6C
.db $6C $6C $66 $66 $66 $66 $66 $7E $7C $00 $F9 $F1 $D9 $DD $CD $CD
.db $CD $00 $FB $FB $9B $9B $9B $99 $99 $00 $6C $6C $6C $6C $FC $F8
.db $98 $00

; Data from A164 to A205 (162 bytes)
_DATA_A164_:
.db $A0 $00 $00 $3C $7E $66 $66 $60 $60 $60 $00 $7C $7E $66 $66 $66
.db $66 $7E $00 $18 $3C $3C $7E $66 $66 $66 $00 $66 $66 $76 $76 $76
.db $7E $7E $00 $7C $7E $66 $66 $66 $66 $66 $6E $6E $66 $66 $66 $7E
.db $3C $00 $7C $78 $6C $6C $66 $66 $66 $00 $7E $7E $66 $66 $66 $66
.db $66 $00 $7E $7E $6E $6E $6E $66 $66 $00 $66 $66 $66 $66 $66 $7E
.db $7C $00 $00 $3C $7E $66 $66 $66 $60 $60 $00 $66 $66 $66 $66 $66
.db $66 $7E $00 $18 $3C $3C $7E $66 $66 $66 $00 $C6 $C6 $C6 $EE $EE
.db $FE $D6 $00 $7C $7E $66 $66 $66 $66 $7E $60 $60 $66 $66 $66 $7E
.db $3C $00 $7E $66 $66 $66 $66 $66 $66 $00 $7E $7E $66 $66 $66 $66
.db $66 $00 $D6 $C6 $C6 $C6 $C6 $C6 $C6 $00 $7C $60 $60 $60 $60 $60
.db $60 $00

; Data from A206 to BFFF (7674 bytes)
_DATA_A206_:
.db $04 $09 $FF $87 $FE $FC $F8 $F0 $E0 $C0 $80 $21 $FF $87 $FE $FC
.db $F8 $F0 $E0 $C0 $80 $7F $FF $09 $FF $00 $90 $01 $03 $07 $0F $1F
.db $3F $7F $FF $01 $02 $04 $08 $10 $20 $40 $80 $21 $FF $87 $FE $FC
.db $F8 $F0 $E0 $C0 $80 $7F $FF $09 $FF $00 $91 $FE $FC $F8 $F0 $E0
.db $C0 $80 $00 $FE $FD $FB $F7 $EF $DF $BF $7F $00 $06 $1C $83 $0C
.db $00 $C7 $05 $CE $83 $8E $00 $C7 $06 $67 $81 $00 $07 $30 $88 $00
.db $01 $03 $07 $0F $1F $3F $7F $07 $07 $81 $00 $06 $0E $82 $07 $00
.db $06 $67 $82 $C3 $00 $06 $30 $81 $E0 $0A $00 $81 $3F $06 $39 $82
.db $00 $1C $06 $9C $82 $00 $FC $06 $E6 $81 $00 $07 $03 $82 $00 $9F
.db $06 $87 $82 $00 $C4 $06 $0E $06 $39 $82 $3F $00 $06 $9C $82 $1C
.db $00 $06 $E6 $82 $FC $00 $07 $03 $81 $00 $07 $87 $89 $00 $0E $0E
.db $04 $00 $0E $0E $04 $00 $00 $11 $00 $06 $1C $83 $0C $00 $C7 $05
.db $CE $83 $8E $00 $C7 $06 $67 $81 $00 $07 $30 $08 $00 $07 $07 $81
.db $00 $06 $0E $82 $07 $00 $06 $67 $82 $C3 $00 $06 $30 $81 $E0 $0A
.db $00 $81 $3F $06 $39 $82 $00 $1C $06 $9C $82 $00 $FC $06 $E6 $81
.db $00 $07 $03 $82 $00 $9F $06 $87 $82 $00 $C4 $06 $0E $06 $39 $82
.db $3F $00 $06 $9C $82 $1C $00 $06 $E6 $82 $FC $00 $07 $03 $81 $00
.db $07 $87 $89 $00 $0E $0E $04 $00 $0E $0E $04 $00 $00
.dsb 7389, $FF

.BANK 3
.ORG $0000

; Data from C000 to C011 (18 bytes)
_DATA_C000_:
.db $00 $10 $00 $3F $00 $0F
.dsb 12, $00

; Data from C012 to C1A6 (405 bytes)
_DATA_C012_:
.db $02 $4C $00 $81 $01 $06 $02 $81 $01 $0E $00 $81 $03 $03 $04 $82
.db $05 $06 $04 $00 $81 $07 $06 $00 $81 $07 $0E $00 $86 $08 $09 $0A
.db $0B $0C $0D $04 $00 $81 $07 $06 $00 $81 $07 $0E $00 $81 $0E $03
.db $0F $82 $10 $11 $04 $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07
.db $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07
.db $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07 $0E $00 $82 $06
.db $12 $04 $04 $85 $05 $06 $00 $00 $07 $06 $00 $81 $07 $0E $00 $8B
.db $0D $13 $14 $15 $16 $17 $18 $0D $00 $00 $07 $06 $00 $81 $07 $0E
.db $00 $82 $11 $19 $04 $0F $85 $10 $11 $00 $00 $07 $06 $00 $81 $07
.db $18 $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07
.db $0E $00 $82 $06 $12 $06 $04 $83 $1A $00 $07 $06 $00 $81 $07 $0E
.db $00 $8B $0D $1B $1C $1D $1E $1F $20 $21 $22 $00 $07 $06 $00 $81
.db $07 $0E $00 $82 $11 $19 $06 $0F $83 $23 $00 $07 $06 $00 $81 $07
.db $18 $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07
.db $0E $00 $81 $03 $04 $04 $82 $05 $06 $03 $00 $81 $07 $06 $00 $81
.db $07 $0E $00 $87 $24 $25 $26 $27 $28 $29 $0D $03 $00 $81 $01 $06
.db $02 $8B $01 $00 $00 $2A $2B $2C $2D $2E $2F $30 $31 $04 $00 $81
.db $0E $04 $0F $82 $10 $11 $0D $00 $88 $2A $2B $32 $33 $34 $35 $36
.db $37 $22 $00 $00 $53 $10 $81 $12 $1F $10 $81 $12 $1F $10 $81 $12
.db $1F $10 $81 $12 $1F $10 $81 $12 $1F $10 $81 $12 $1F $10 $81 $12
.db $1F $10 $81 $12 $0E $10 $81 $12 $10 $10 $81 $12 $0E $10 $81 $12
.db $10 $10 $81 $12 $0E $10 $81 $12 $10 $10 $81 $12 $1F $10 $81 $12
.db $1F $10 $81 $12 $0E $10 $81 $12 $10 $10 $81 $12 $0E $10 $81 $12
.db $10 $10 $81 $12 $0E $10 $81 $12 $10 $10 $81 $12 $1F $10 $81 $12
.db $1F $10 $81 $12 $1F $10 $81 $12 $18 $10 $07 $14 $81 $16 $22 $10
.db $02 $14 $28 $10 $00

; Data from C1A7 to C434 (654 bytes)
_DATA_C1A7_:
.db $04 $08 $00 $81 $FF $05 $80 $83 $83 $82 $FF $05 $00 $81 $FF $05
.db $00 $02 $FF $02 $C0 $04 $00 $02 $FF $06 $00 $84 $FE $FF $01 $01
.db $06 $00 $82 $80 $C0 $08 $82 $02 $CE $02 $CD $04 $CC $02 $67 $98
.db $66 $67 $E7 $E6 $67 $67 $E6 $E7 $07 $E3 $E3 $07 $E7 $E6 $33 $73
.db $F0 $E0 $E0 $F0 $70 $30 $F9 $F9 $06 $E1 $8B $E0 $F0 $F8 $FC $F8
.db $F0 $E0 $C0 $C0 $FF $FF $06 $00 $02 $FF $05 $00 $83 $01 $FF $FE
.db $05 $00 $81 $80 $0B $00 $9E $7F $FF $C0 $C0 $CF $DF $D8 $DF $CF
.db $C0 $DF $CF $87 $CF $0C $8C $CC $CC $CF $87 $C3 $E7 $66 $06 $06
.db $66 $E7 $C3 $E3 $F3 $04 $33 $95 $F3 $E3 $F1 $F9 $19 $19 $F1 $F9
.db $19 $19 $F9 $F9 $81 $F9 $F9 $81 $F9 $F9 $C0 $FF $7F $09 $00 $96
.db $80 $C0 $E0 $F0 $DF $DF $D8 $DF $DF $D8 $DF $DF $8E $CE $CE $8E
.db $CE $CE $CF $8F $07 $0F $04 $0C $84 $CF $C7 $C3 $E7 $04 $66 $A5
.db $E7 $C3 $E3 $F3 $33 $03 $03 $33 $F3 $E3 $18 $39 $71 $E1 $E0 $F0
.db $39 $18 $F8 $FC $80 $F8 $FC $0C $FC $F8 $F8 $FC $FE $FF $FE $FC
.db $F8 $F0 $E0 $C0 $80 $05 $00 $06 $CE $02 $CF $02 $0F $9E $0C $0F
.db $0F $0C $CF $CF $E6 $E6 $07 $E3 $E3 $01 $E1 $E0 $19 $19 $39 $31
.db $F1 $E1 $E1 $C1 $FC $FC $80 $FC $FC $80 $FC $FC $06 $E1 $02 $FD
.db $9B $01 $03 $07 $0F $1F $3F $7E $FC $FE $F0 $E3 $C7 $8F $1E $3C
.db $38 $00 $00 $83 $C3 $23 $13 $0B $0B $00 $00 $80 $05 $86 $03 $00
.db $82 $34 $36 $03 $37 $03 $00 $A2 $16 $36 $77 $F7 $F7 $00 $00 $31
.db $33 $37 $33 $B1 $F0 $7E $F8 $F0 $E0 $C0 $E0 $F0 $F8 $0B $0B $13
.db $23 $C3 $80 $00 $00 $86 $86 $F6 $F7 $F7 $03 $00 $85 $37 $36 $36
.db $F6 $F6 $03 $00 $82 $F7 $B6 $03 $36 $93 $06 $00 $00 $F0 $F0 $70
.db $30 $31 $33 $07 $FF $7C $3E $7E $FC $F8 $F0 $E0 $C0 $00 $1C $00
.db $04 $FF $04 $00 $04 $FF $04 $00 $81 $FE $03 $FF $06 $00 $82 $80
.db $C0 $08 $00 $02 $F1 $02 $F2 $04 $F3 $02 $98 $98 $99 $98 $18 $19
.db $98 $98 $19 $18 $F8 $1C $1C $F8 $18 $19 $CC $8C $0F $1F $1F $0F
.db $8F $CF $07 $07 $06 $1F $88 $E0 $F0 $F8 $FC $F8 $F0 $E0 $C0 $03
.db $FF $05 $00 $03 $FF $05 $00 $02 $FF $81 $FE $05 $00 $81 $80 $0B
.db $00 $81 $7F $03 $FF $9A $F0 $E0 $E7 $E0 $F0 $FF $E0 $F0 $78 $30
.db $F3 $73 $33 $33 $30 $78 $3C $18 $99 $F9 $F9 $99 $18 $3C $1C $0C
.db $04 $CC $95 $0C $1C $0E $06 $E6 $E6 $0E $06 $E6 $E6 $07 $07 $7F
.db $07 $07 $7F $07 $07 $FF $FF $7F $09 $00 $96 $80 $C0 $E0 $F0 $E0
.db $E0 $E7 $E0 $E0 $E7 $E0 $E0 $71 $31 $31 $71 $31 $31 $30 $70 $F8
.db $F0 $04 $F3 $84 $30 $38 $3C $18 $04 $99 $A5 $18 $3C $1C $0C $CC
.db $FC $FC $CC $0C $1C $E7 $C6 $8E $1E $1F $0F $C6 $E7 $07 $03 $7F
.db $07 $03 $F3 $03 $07 $F8 $FC $FE $FF $FE $FC $F8 $F0 $E0 $C0 $80
.db $05 $00 $06 $F1 $04 $F0 $9E $F3 $F0 $F0 $F3 $30 $30 $19 $19 $F8
.db $1C $1C $FE $1E $1F $E6 $E6 $C6 $CE $0E $1E $1E $3E $03 $03 $7F
.db $03 $03 $7F $03 $03 $06 $1F $02 $03 $70 $00 $00 $7F $00 $7F $00
.db $7F $00 $43 $00 $00 $7F $00 $7F $00 $7F $00 $43 $00 $00

; Data from C435 to C450 (28 bytes)
_DATA_C435_:
.db $01 $04 $80 $1C $00 $81 $80 $20 $00 $02 $80 $1D $00 $84 $80 $00
.db $00 $80 $1C $00 $02 $80 $82 $00 $80 $1C $00 $00

; Data from C451 to C462 (18 bytes)
_DATA_C451_:
.db $00 $10 $00 $3F $00 $2E $2A $00 $1A $38 $34 $30 $20 $13 $3F $3F
.db $00 $00

; Data from C463 to C611 (431 bytes)
_DATA_C463_:
.db $02 $20 $01 $20 $02 $08 $03 $85 $04 $05 $04 $03 $06 $06 $07 $81
.db $06 $0C $03 $02 $08 $81 $09 $03 $0A $87 $0B $0C $0D $00 $0D $08
.db $0E $06 $0F $81 $0E $0C $08 $02 $10 $8B $11 $12 $13 $14 $15 $16
.db $17 $00 $17 $10 $0E $06 $0F $81 $0E $0C $10 $02 $18 $81 $19 $03
.db $1A $87 $1B $1C $1D $00 $1D $18 $0E $06 $0F $81 $0E $0C $18 $08
.db $1E $85 $1F $20 $1F $1E $0E $06 $0F $81 $0E $18 $1E $81 $0E $06
.db $0F $81 $0E $18 $1E $81 $0E $06 $0F $81 $0E $18 $1E $81 $0E $06
.db $0F $81 $0E $0D $1E $8C $21 $22 $23 $24 $25 $26 $27 $28 $22 $21
.db $1E $0E $06 $0F $81 $0E $0C $1E $8D $29 $2A $2B $2C $2D $2E $2F
.db $30 $31 $2B $2A $29 $0E $06 $0F $81 $0E $0C $1E $81 $32 $0A $0F
.db $82 $32 $0E $06 $0F $81 $0E $0C $1E $81 $32 $04 $0F $81 $33 $04
.db $0F $83 $34 $32 $0E $06 $0F $81 $0E $0C $1E $81 $35 $0A $36 $82
.db $35 $0E $06 $0F $81 $0E $18 $1E $81 $0E $06 $0F $81 $0E $18 $1E
.db $81 $0E $06 $0F $81 $0E $0C $1E $0B $37 $82 $38 $0E $06 $0F $82
.db $0E $38 $13 $37 $85 $39 $3A $3B $3C $0E $06 $0F $85 $0E $3C $3B
.db $3A $39 $0D $37 $88 $3D $3E $3F $40 $41 $42 $42 $0E $06 $0F $88
.db $0E $42 $42 $41 $40 $3F $3E $3D $07 $37 $8B $43 $44 $45 $46 $47
.db $48 $49 $4A $4B $42 $0E $06 $0F $81 $0E $04 $42 $95 $4C $4D $4E
.db $4F $50 $51 $37 $37 $52 $53 $54 $55 $56 $57 $58 $59 $5A $0F $5B
.db $42 $06 $06 $07 $91 $06 $42 $42 $5C $5D $5E $5F $60 $61 $62 $63
.db $53 $52 $64 $64 $65 $66 $03 $67 $84 $68 $69 $67 $6A $0B $42 $8C
.db $5C $5D $6B $6C $6D $6E $6F $70 $64 $64 $71 $72 $1C $42 $82 $72
.db $71 $00 $4A $00 $81 $02 $08 $00 $81 $02 $16 $00 $81 $02 $1F $00
.db $81 $02 $1F $00 $81 $02 $1F $00 $81 $02 $7F $00 $81 $02 $1F $00
.db $02 $02 $1F $00 $81 $02 $1F $00 $81 $02 $1F $00 $81 $02 $68 $00
.db $81 $02 $1F $00 $04 $02 $1E $00 $05 $02 $31 $00 $81 $04 $06 $00
.db $81 $06 $0A $00 $02 $02 $16 $00 $02 $04 $26 $00 $02 $02 $00

; Data from C612 to CD22 (1809 bytes)
_DATA_C612_:
.incbin "columns_DATA_C612_.inc"

; Data from CD23 to CD34 (18 bytes)
_DATA_CD23_:
.db $00 $10 $00 $3F $0F $02 $00 $00 $1A $38 $14 $29 $3E $01 $0F $0E
.db $09 $04

; Data from CD35 to CFEC (696 bytes)
_DATA_CD35_:
.db $02 $C3 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $02 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $01 $02 $05 $06 $06 $8E $05 $07 $08 $09 $02 $01 $02 $01
.db $02 $01 $0A $0B $0C $05 $06 $06 $86 $05 $01 $02 $03 $04 $0D $06
.db $0E $8E $0D $0F $10 $11 $12 $13 $14 $15 $16 $17 $18 $10 $19 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $1B $10 $1C
.db $1D $1E $1F $20 $21 $22 $23 $10 $24 $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $0D $06 $0E $8E $0D $0F $10 $26 $27 $28 $29 $28 $29 $2A
.db $2B $10 $2C $0D $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E
.db $0D $2D $2E $2F $02 $01 $02 $01 $02 $01 $30 $31 $32 $0D $06 $0E
.db $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $1A $04 $03 $04 $03
.db $04 $03 $04 $03 $04 $03 $04 $0D $06 $0E $86 $0D $1A $04 $01 $02
.db $0D $06 $0E $8E $0D $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01
.db $02 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $10
.db $10 $33 $34 $35 $36 $37 $38 $39 $33 $10 $10 $0D $06 $0E $86 $0D
.db $1A $04 $01 $02 $0D $06 $0E $8E $0D $10 $10 $3A $3B $3C $3D $3E
.db $3F $40 $3A $10 $10 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06
.db $0E $8E $0D $41 $42 $43 $44 $45 $46 $45 $46 $45 $47 $43 $42 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $25 $02 $01
.db $02 $01 $02 $01 $02 $01 $02 $01 $02 $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $0D $06 $0E $8E $0D $1A $04 $03 $04 $03 $04 $03 $04 $03
.db $04 $03 $04 $0D $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E
.db $0D $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $0D $06 $0E
.db $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $1A $04 $03 $04 $03
.db $04 $03 $04 $03 $04 $03 $04 $0D $06 $0E $86 $0D $1A $04 $01 $02
.db $0D $06 $0E $8E $0D $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01
.db $02 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $1A
.db $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $0D $06 $0E $86 $0D
.db $1A $04 $01 $02 $0D $06 $0E $8E $0D $25 $02 $01 $02 $01 $02 $01
.db $02 $01 $02 $01 $02 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06
.db $0E $8E $0D $48 $49 $4A $4B $4C $4D $4C $4D $4E $4F $50 $51 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $52 $10 $53
.db $54 $55 $56 $57 $58 $59 $53 $10 $5A $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $05 $06 $06 $8E $05 $5B $5C $5D $5E $5F $60 $5F $60 $61
.db $62 $63 $64 $05 $06 $06 $C3 $05 $1A $04 $01 $02 $01 $65 $66 $65
.db $66 $65 $66 $65 $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $65 $66 $65 $66 $65 $66 $65 $25 $02 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $00 $49 $00 $81 $02 $13
.db $00 $81 $02 $7F $00 $56 $00 $81 $02 $1F $00 $81 $02 $7F $00 $7F
.db $00 $41 $00 $81 $02 $0E $00 $81 $04 $06 $00 $81 $06 $0C $00 $81
.db $04 $06 $00 $81 $06 $42 $00 $00

; Data from CFED to D6DF (1779 bytes)
_DATA_CFED_:
.incbin "columns_DATA_CFED_.inc"

; Data from D6E0 to D6F1 (18 bytes)
_DATA_D6E0_:
.db $00 $10 $00 $3F $0F $02 $00 $25 $2A $2F $14 $29 $3E $01 $0F $0E
.db $09 $04

; Data from D6F2 to D99F (686 bytes)
_DATA_D6F2_:
.db $02 $C3 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $02 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $01 $02 $05 $06 $06 $8E $05 $07 $08 $09 $02 $01 $02 $01
.db $02 $01 $0A $0B $0C $05 $06 $06 $86 $05 $01 $02 $03 $04 $0D $06
.db $0E $8E $0D $0F $10 $11 $12 $13 $14 $15 $16 $17 $18 $10 $19 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $1B $10 $1C
.db $1D $1E $1F $20 $21 $22 $23 $10 $24 $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $0D $06 $0E $8E $0D $0F $10 $26 $27 $28 $29 $28 $29 $2A
.db $2B $10 $2C $0D $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E
.db $0D $2D $2E $2F $02 $01 $02 $01 $02 $01 $30 $31 $32 $0D $06 $0E
.db $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $1A $04 $03 $04 $03
.db $04 $03 $04 $03 $04 $03 $04 $0D $06 $0E $86 $0D $1A $04 $01 $02
.db $0D $06 $0E $8E $0D $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01
.db $02 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $10
.db $10 $33 $34 $35 $36 $37 $38 $39 $33 $10 $10 $0D $06 $0E $86 $0D
.db $1A $04 $01 $02 $0D $06 $0E $8E $0D $10 $10 $3A $3B $3C $3D $3E
.db $3F $40 $3A $10 $10 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06
.db $0E $8E $0D $41 $42 $43 $44 $45 $46 $45 $46 $45 $47 $43 $42 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $25 $02 $01
.db $02 $01 $02 $01 $02 $01 $02 $01 $02 $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $0D $06 $0E $8E $0D $48 $49 $4A $4B $4C $4D $4E $4F $50
.db $4A $49 $51 $0D $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E
.db $0D $52 $53 $54 $55 $56 $57 $58 $59 $5A $54 $53 $5B $0D $06 $0E
.db $86 $0D $25 $02 $03 $04 $0D $06 $0E $82 $0D $5C $0A $10 $82 $5D
.db $0D $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $82 $0D $5E $04
.db $10 $81 $5F $04 $10 $83 $60 $61 $0D $06 $0E $86 $0D $25 $02 $03
.db $04 $0D $06 $0E $82 $0D $62 $0A $63 $82 $64 $0D $06 $0E $86 $0D
.db $1A $04 $01 $02 $0D $06 $0E $8E $0D $65 $66 $67 $66 $67 $66 $67
.db $66 $67 $66 $67 $66 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06
.db $0E $8E $0D $68 $69 $6A $6B $6C $6D $6C $6D $6E $6F $70 $71 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $72 $10 $73
.db $74 $75 $76 $77 $78 $79 $73 $10 $7A $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $05 $06 $06 $8E $05 $7B $7C $7D $7E $7F $80 $7F $80 $81
.db $82 $83 $84 $05 $06 $06 $C3 $05 $1A $04 $01 $02 $01 $85 $86 $85
.db $86 $85 $86 $85 $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $85 $86 $85 $86 $85 $86 $85 $25 $02 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $00 $49 $00 $81 $02 $13
.db $00 $81 $02 $7F $00 $56 $00 $81 $02 $1F $00 $81 $02 $60 $00 $81
.db $02 $1F $00 $81 $02 $7F $00 $3F $00 $81 $02 $0E $00 $81 $04 $06
.db $00 $81 $06 $0C $00 $81 $04 $06 $00 $81 $06 $42 $00 $00

; Data from D9A0 to E29B (2300 bytes)
_DATA_D9A0_:
.incbin "columns_DATA_D9A0_.inc"

; Data from E29C to E2AD (18 bytes)
_DATA_E29C_:
.db $00 $10 $00 $3F $00 $0F $05
.dsb 11, $00

; Data from E2AE to E4AC (511 bytes)
_DATA_E2AE_:
.db $02 $4C $00 $81 $01 $06 $02 $81 $01 $0E $00 $81 $03 $03 $04 $82
.db $05 $06 $04 $00 $81 $07 $06 $00 $81 $07 $04 $00 $82 $08 $09 $03
.db $0A $81 $0B $04 $00 $86 $0C $0D $0E $0F $10 $11 $04 $00 $81 $07
.db $06 $00 $81 $07 $04 $00 $86 $12 $13 $14 $15 $16 $17 $04 $00 $81
.db $18 $03 $19 $82 $1A $1B $04 $00 $81 $07 $06 $00 $81 $07 $04 $00
.db $82 $1C $1D $03 $1E $81 $1F $0E $00 $81 $07 $06 $00 $81 $07 $18
.db $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07 $18
.db $00 $81 $07 $06 $00 $81 $07 $0E $00 $82 $06 $20 $04 $04 $85 $05
.db $06 $00 $00 $07 $06 $00 $85 $07 $00 $00 $08 $09 $04 $0A $82 $21
.db $08 $04 $00 $8B $11 $22 $23 $24 $25 $26 $27 $11 $00 $00 $07 $06
.db $00 $8B $07 $00 $00 $12 $28 $29 $2A $2B $2C $2D $12 $04 $00 $82
.db $1B $2E $04 $19 $85 $1A $1B $00 $00 $07 $06 $00 $85 $07 $00 $00
.db $1C $1D $04 $1E $82 $2F $1C $0E $00 $81 $07 $06 $00 $81 $07 $18
.db $00 $81 $07 $06 $00 $81 $07 $0E $00 $82 $06 $20 $06 $04 $83 $30
.db $00 $07 $06 $00 $83 $07 $00 $31 $06 $0A $82 $21 $08 $04 $00 $8B
.db $11 $32 $33 $34 $35 $36 $37 $38 $39 $00 $07 $06 $00 $8B $07 $00
.db $3A $3B $3C $3D $3E $3F $40 $41 $12 $04 $00 $82 $1B $2E $06 $19
.db $83 $42 $00 $07 $06 $00 $83 $07 $00 $43 $06 $1E $82 $2F $1C $0E
.db $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07 $0E
.db $00 $81 $03 $04 $04 $82 $05 $06 $03 $00 $81 $07 $06 $00 $81 $07
.db $03 $00 $82 $08 $09 $04 $0A $81 $0B $04 $00 $87 $44 $45 $46 $47
.db $48 $49 $11 $03 $00 $81 $01 $06 $02 $81 $01 $03 $00 $87 $12 $4A
.db $4B $4C $4D $4E $4F $04 $00 $81 $18 $04 $19 $82 $1A $1B $0E $00
.db $82 $1C $1D $04 $1E $81 $1F $22 $00 $00 $53 $10 $81 $12 $1F $10
.db $81 $12 $1F $10 $81 $12 $1F $10 $81 $12 $1F $10 $81 $12 $1F $10
.db $81 $12 $1F $10 $81 $12 $1F $10 $81 $12 $0E $10 $81 $12 $10 $10
.db $81 $12 $09 $10 $81 $12 $04 $10 $81 $12 $10 $10 $81 $12 $09 $10
.db $81 $12 $04 $10 $81 $12 $10 $10 $81 $12 $09 $10 $81 $12 $15 $10
.db $81 $12 $1F $10 $81 $12 $0E $10 $81 $12 $10 $10 $81 $12 $09 $10
.db $81 $12 $04 $10 $81 $12 $10 $10 $81 $12 $09 $10 $81 $12 $04 $10
.db $81 $12 $10 $10 $81 $12 $09 $10 $81 $12 $15 $10 $81 $12 $1F $10
.db $81 $12 $1F $10 $81 $12 $18 $10 $07 $14 $81 $16 $4C $10 $00

; Data from E4AD to E8CD (1057 bytes)
_DATA_E4AD_:
.incbin "columns_DATA_E4AD_.inc"

; Data from E8CE to E8DF (18 bytes)
_DATA_E8CE_:
.db $00 $10 $00 $3F $00 $2A $00 $1A $38 $34 $30 $20 $0F $05 $3F $3F
.db $00 $00

; Data from E8E0 to EAF8 (537 bytes)
_DATA_E8E0_:
.db $02 $20 $01 $20 $02 $08 $03 $85 $04 $05 $04 $03 $06 $06 $07 $85
.db $06 $03 $08 $09 $08 $08 $03 $02 $0A $81 $0B $03 $0C $87 $0D $0E
.db $0F $00 $0F $0A $10 $06 $11 $87 $10 $0A $12 $00 $12 $13 $14 $03
.db $15 $90 $16 $0A $0A $17 $17 $18 $19 $1A $1B $1C $1D $1E $00 $1E
.db $17 $10 $06 $11 $90 $10 $17 $1F $00 $1F $20 $21 $22 $23 $24 $25
.db $17 $17 $26 $26 $27 $03 $28 $87 $29 $2A $2B $00 $2B $26 $10 $06
.db $11 $87 $10 $26 $2C $00 $2C $2D $2E $03 $2F $83 $30 $26 $26 $08
.db $31 $85 $32 $33 $32 $31 $10 $06 $11 $85 $10 $31 $34 $35 $34 $14
.db $31 $81 $10 $06 $11 $81 $10 $18 $31 $81 $10 $06 $11 $81 $10 $18
.db $31 $81 $10 $06 $11 $81 $10 $0D $31 $8C $36 $37 $38 $39 $3A $3B
.db $3C $3D $37 $36 $31 $10 $06 $11 $9A $10 $31 $3E $3F $40 $41 $42
.db $43 $44 $45 $3F $3E $31 $46 $47 $48 $49 $4A $4B $4C $4D $4E $48
.db $47 $46 $10 $06 $11 $8E $10 $4F $50 $51 $52 $53 $54 $55 $56 $57
.db $51 $50 $4F $58 $0A $11 $82 $58 $10 $06 $11 $82 $10 $59 $0A $11
.db $82 $59 $58 $04 $11 $81 $5A $04 $11 $83 $5B $58 $10 $06 $11 $82
.db $10 $59 $04 $11 $81 $5A $04 $11 $83 $5B $59 $5C $0A $5D $82 $5C
.db $10 $06 $11 $82 $10 $5E $0A $5F $81 $5E $0C $31 $81 $10 $06 $11
.db $81 $10 $18 $31 $81 $10 $06 $11 $81 $10 $0C $31 $0B $60 $82 $61
.db $10 $06 $11 $82 $10 $61 $13 $60 $85 $62 $63 $64 $65 $10 $06 $11
.db $85 $10 $65 $64 $63 $62 $0D $60 $88 $66 $67 $68 $69 $6A $6B $6B
.db $10 $06 $11 $88 $10 $6B $6B $6A $69 $68 $67 $66 $07 $60 $8B $6C
.db $6D $6E $6F $70 $71 $72 $73 $74 $6B $10 $06 $11 $9A $10 $6B $75
.db $76 $77 $78 $79 $7A $7B $7C $7D $60 $60 $7E $7F $80 $81 $82 $83
.db $84 $85 $86 $11 $87 $6B $06 $06 $07 $91 $06 $6B $88 $11 $89 $8A
.db $8B $8C $8D $8E $8F $7F $7E $90 $90 $91 $92 $03 $93 $84 $94 $95
.db $93 $96 $0A $6B $84 $97 $98 $99 $9A $03 $98 $86 $9B $9C $90 $90
.db $9D $9E $1C $6B $82 $9E $9D $00 $4A $00 $81 $02 $08 $00 $81 $02
.db $03 $00 $81 $02 $12 $00 $81 $02 $0C $00 $81 $02 $12 $00 $81 $02
.db $0C $00 $81 $02 $12 $00 $81 $02 $0C $00 $81 $02 $12 $00 $81 $02
.db $0C $00 $81 $02 $72 $00 $81 $02 $13 $00 $81 $02 $0B $00 $02 $02
.db $12 $00 $02 $02 $0B $00 $81 $02 $13 $00 $81 $02 $0B $00 $81 $02
.db $13 $00 $81 $02 $0B $00 $81 $02 $13 $00 $81 $02 $54 $00 $81 $02
.db $1F $00 $04 $02 $1E $00 $05 $02 $31 $00 $81 $04 $06 $00 $81 $06
.db $0A $00 $02 $02 $3E $00 $02 $02 $00

; Data from EAF9 to FFFF (5383 bytes)
_DATA_EAF9_:
.incbin "columns_DATA_EAF9_.inc"

.BANK 4
.ORG $0000

; Data from 10000 to 13FFF (16384 bytes)
.dsb 16384, $FF

.BANK 5
.ORG $0000

; Data from 14000 to 17FFF (16384 bytes)
.dsb 16384, $FF

.BANK 6
.ORG $0000

; Data from 18000 to 1BFFF (16384 bytes)
.dsb 16384, $FF

.BANK 7
.ORG $0000

; Data from 1C000 to 1FFFF (16384 bytes)
.dsb 16384, $FF

