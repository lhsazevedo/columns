state.demo.init:
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
state.demo.update:
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
    call updateEntities
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
    call fade.out
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
    call fade.out
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
