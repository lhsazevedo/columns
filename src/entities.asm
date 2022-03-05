; init Main menu Arrow Entity
entities.arrow.init:
    ld (ix+2), $01
    ld (ix+3), $01

    ; Initial option
    ld (ix+14), $00

    ; Options count
    ld (ix+15), $03

    ; Y position
    ld (ix+6), $78

    ; X position
    ld (ix+9), $50

    ; Set updater
    ld (ix+0), <entities.arrow.update
    ld (ix+1), >entities.arrow.update

    ret

; Update Menu Arrow Entity
entities.arrow.update:
    ld a, (var.input.player1.debounced)
    and JOY_UP | JOY_DOWN
    ret z

    ; TODO: If skipped: Arrow doesnt move
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

entities.optionsArrow.init:
    ld (ix+2), $01
    ld (ix+3), $01

    ; Initial option
    ld (ix+14), $00

    ; Options count
    ld (ix+15), $02

    ; Y position
    ld (ix+6), $88

    ; X position
    ld (ix+9), $50

    ; Set updater
    ld (ix+0), <entities.optionsArrow.update
    ld (ix+1), >entities.optionsArrow.update
    ret

entities.optionsArrow.update:
    ld a, (var.input.player1.debounced)
    and JOY_UP | JOY_DOWN
    ret z

    ; TODO: If skipped: Arrow doesnt move
    call _LABEL_341A_

    ; y = $88 + option * 16
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
    ld a, (mode_RAM_C005_)
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
    ld hl, optDifficulty_RAM_C6A8_
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
    ld hl, optBlockType_RAM_C6A9_
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
    ld a, (mode_RAM_C005_)
    bit 0, a
    jr nz, ++
    ld hl, optLevel_RAM_C6AC_
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
    ld hl, optHigh_RAM_C6AB_
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
    ld a, (mode_RAM_C005_)
    bit 0, a
    jr nz, +
    call _LABEL_34BB_
    jp _LABEL_34F1_

+:
    call _LABEL_34D6_
    jp _LABEL_3503_

_LABEL_3166_:
    ld a, (var.input.player1.debounced) ; var.input.player1.debounced = $C00C
    and $30
    jr z, +
    ld a, (ix+14)
    cp $04
    jp z, _LABEL_2FB7_
+:
    ld a, (var.input.player1.debounced) ; var.input.player1.debounced = $C00C
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
    ld a, (var.input.player1.debounced) ; var.input.player1.debounced = $C00C
    and $0C
    ret z
    ld a, $95
    ld (var.audio.request), a ; var.audio.request = $DD05
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
    ld a, (mode_RAM_C005_) ; mode_RAM_C005_ = $C005
    bit 0, a
    jr nz, ++
    ld hl, optMatches_RAM_C6B2_ ; optMatches_RAM_C6B2_ = $C6B2
    ld a, (var.input.player1.debounced) ; var.input.player1.debounced = $C00C
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
    ld hl, _RAM_C6B3_ ; _RAM_C6B3_ = $C6B3
    ld a, (var.input.player1.debounced) ; var.input.player1.debounced = $C00C
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
    ; TODO: Entity label 3229
    ld (ix+0), $29
    ld (ix+1), $32
    call _LABEL_3514_
    call _LABEL_351F_
    call _LABEL_3559_
    ld a, (mode_RAM_C005_)
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
    ; TODO: Entity label 3307
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
    ; TODO: Entity label 333D
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
    ; TODO: Entity label 3373
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
    ; TODO: Entity label 33CE
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
    ; TODO: Entity label 33EE
    ld (ix+0), $EE
    ld (ix+1), $33
    ret

_LABEL_33EE_:
    dec (ix+14)
    ret nz
    ld (ix+14), $04
    ; TODO: Entity label 33FF
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
    ; TODO: Entity label 2FB7
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
    ld a, (mode_RAM_C005_)
    and $02
    jr z, +
    ld de, $3B88
+:
    ld a, (optDifficulty_RAM_C6A8_)
    add a, $0D
    jp _LABEL_2885_

_LABEL_3459_:
    ld de, $3C26
    ld a, (mode_RAM_C005_)
    bit 1, a
    jr z, +
    ld de, $3BC8
+:
    rst $08 ; setVdpAddress
    ld a, (optDifficulty_RAM_C6A8_)
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
    ld hl, (optBlockType_RAM_C6A9_)
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

; Pointer Table from 34B1 to 34BA (5 entries, indexed by optBlockType_RAM_C6A9_)
_DATA_34B1_:
.dw _DATA_9A27_ _DATA_9AE9_ _DATA_9B95_ _DATA_9D97_ _DATA_9E3F_

_LABEL_34BB_:
    ld de, $3CAA
    ld a, (mode_RAM_C005_)
    and $02
    jr z, +
    ld de, $3C0E
+:
    rst $08 ; setVdpAddress
    ld a, (optLevel_RAM_C6AC_)
    add a, $D6
    out (Port_VDPData), a
    xor a
    jr +

+:
    out (Port_VDPData), a
    ret

_LABEL_34D6_:
    ld de, $3CAA
    ld a, (mode_RAM_C005_)
    and $02
    jr z, +
    ld de, $3C0E
+:
    rst $08 ; setVdpAddress
    ld a, (optHigh_RAM_C6AB_)
    add a, $D8
    out (Port_VDPData), a
    xor a
    jr +

+:
    out (Port_VDPData), a
    ret

_LABEL_34F1_:
    ld de, $3C98
    rst $08 ; setVdpAddress
    ld a, (optMatches_RAM_C6B2_)
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
    rst $08 ; setVdpAddress
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
    rst $08 ; setVdpAddress
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
    rst $08 ; setVdpAddress
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
    rst $08 ; setVdpAddress
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
    ; TODO: Entity label 35E7
    ld (ix+0), $E7
    ld (ix+1), $35
    ld a, (mode_RAM_C005_)
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
    ld (ix+0), <initFallingBlock_LABEL_3636_
    ld (ix+1), >initFallingBlock_LABEL_3636_
    ld a, (mode_RAM_C005_)
    bit 0, a
    ret z
    ld a, $01
    ld (_RAM_C6C2_), a
    ld a, (mode_RAM_C005_)
    bit 1, a
    ret nz
    ld a, $01
    ld (_RAM_C0A8_), a
    ret

_LABEL_3611_:
    ld a, (mode_RAM_C005_)
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

initFallingBlock_LABEL_3636_:
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
    ld a, (mode_RAM_C005_)
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
    ld (ix+0), <updateFallingBlock_LABEL_36F9_
    ld (ix+1), >updateFallingBlock_LABEL_36F9_
    ld hl, (_RAM_C699_)
    ld h, $00
    ld de, _DATA_362C_
    add hl, de
    ld a, (hl)
    ld (_RAM_DD01_), a
    ld (_RAM_DD02_), a
    ld a, (mode_RAM_C005_)
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

updateFallingBlock_LABEL_36F9_:
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
    ld hl, _LABEL_4464_ ; Overriding return address
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
    ; TODO: Entity label 37A0
    ld (ix+0), $A0
    ld (ix+1), $37
    ret

_LABEL_378F_:
    ld (ix+14), $00
    ld (ix+25), $10
    ; TODO: Entity label 3A3C
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
    ld hl, + ; Overriding return address
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
    ld (ix+0), <updateFallingBlock_LABEL_36F9_
    ld (ix+1), >updateFallingBlock_LABEL_36F9_
    ret

_LABEL_37F7_:
    ld (ix+27), $00
    ld (ix+30), $00
    ld hl, _RAM_C4B0_
    ld de, _RAM_C4B1_
    ld (hl), $00
    call ldi128
    call _LABEL_123D_
    ; TODO: Entity label 3816
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
    ; TODO: Entity label 3831
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
    ; TODO: Entity label 384F
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
    ; TODO: Entity label 386D
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
    ; TODO: Entity label 388B
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
    ld a, (mode_RAM_C005_)
    and $03
    call z, _LABEL_452A_
    ld (ix+25), $2F
    ; TODO: Entity label 392B
    ld (ix+0), $2B
    ld (ix+1), $39
    ret

+:
    ld a, (ix+30)
    or a
    call nz, _LABEL_2B3E_
    ld (ix+25), $05
    ; TODO: Entity label 38EF
    ld (ix+0), $EF
    ld (ix+1), $38
    ret

_LABEL_38D3_:
    ld (ix+21), $00
    call _LABEL_2A6E_
    ld (ix+28), $00
    ld (ix+29), $00
    ld (ix+25), $2F
    ; TODO: Entity label 392B
    ld (ix+0), $2B
    ld (ix+1), $39
    ret

_LABEL_38EF_:
    dec (ix+25)
    ret p
    ld (ix+0), <initFallingBlock_LABEL_3636_
    ld (ix+1), >initFallingBlock_LABEL_3636_
    ld a, (mode_RAM_C005_)
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
    ld a, (mode_RAM_C005_)
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
    ld hl, + ; Overriding return address
    push hl
    bit 3, (ix+25)
    ld de, _RAM_C418_
    jp z, _LABEL_2AD2_
    jp _LABEL_2AE5_

+:
    dec (ix+25)
    ret p
    ; TODO: Entity label 3954
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
    ld a, (mode_RAM_C005_)
    and $03
    jr nz, ++
    ld hl, var.pallete.shouldUpdate
    set 2, (hl)
    jr ++

+:
    ld hl, (_RAM_C6A3_)
    add hl, bc
    ld (_RAM_C6A3_), hl
    ld a, (mode_RAM_C005_)
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
    ; TODO: Entity label 39A6
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
    ; TODO: Entity label 39E9
    ld (ix+0), $E9
    ld (ix+1), $39
    ret

_LABEL_39E9_:
    dec (ix+25)
    ret p
    ld a, (mode_RAM_C005_)
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
    ; TODO: Entity label 37FF
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
    ld a, (mode_RAM_C005_)
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
    ; TODO: Entity label 3AF7
    ld (ix+0), $F7
    ld (ix+1), $3A
    ret

; Data from 3AF7 to 3B1A (36 bytes)
.db $DD $7E $0E $B7 $28 $04 $DD $35 $0E $C9 $CD $81 $45 $F0 $DD $36
.db $19 $C0 $DD

_DATA_3B0A_:
.db $36 $00 $11 $DD $36 $01 $3B $DD $35 $19 $C0 $3E $01
.db $32 $08 $C0 $C9

_LABEL_3B1B_:
    ld (ix+3), $18
    ld (ix+6), $48
    ld a, (mode_RAM_C005_)
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
    ; TODO: Entity label 3BAE
    ld (ix+0), $AE
    ld (ix+1), $3B
    ld a, (mode_RAM_C005_)
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
    ld a, (mode_RAM_C005_)
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
    ; TODO: Entity label 3C9E
    ld (ix+0), $9E
    ld (ix+1), $3C
    jp _LABEL_4464_

+:
    call _LABEL_4464_
    ld a, (mode_RAM_C005_)
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
    ; TODO: Entity label 3FCB
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
    ld a, (mode_RAM_C005_)
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
    ld a, (mode_RAM_C005_)
    and $07
    jr z, ++
-:
    ld a, (optDifficulty_RAM_C6A8_)
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
    ld a, (mode_RAM_C005_)
    bit 1, a
    jr nz, _LABEL_44B4_
    ld de, $38D2
    ld a, (mode_RAM_C005_)
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
    rst $08 ; setVdpAddress
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
    rst $08 ; setVdpAddress
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
    rst $08 ; setVdpAddress
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
    ld a, (mode_RAM_C005_)
    bit 1, a
    jr nz, _LABEL_44B4_
    ld de, $38EC
    ld a, (mode_RAM_C005_)
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
    ld (var.entities.3.byte09), a
    ld (var.entities.4.byte09), a
    ld (var.entities.5.byte09), a
    ld (var.entities.6.byte09), a
    ld (var.entities.7.byte09), a
    ld hl, var.entities.7.byte03
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
    ld (var.entities.3.handler), hl
    ld hl, $33A3
    ld (var.entities.4.handler), hl
    ld hl, $33A9
    ld (var.entities.5.handler), hl
    ld hl, $33AF
    ld (var.entities.6.handler), hl
    ld hl, $33B5
    ld (var.entities.7.handler), hl
    ret

_LABEL_4581_:	
    ld (ix+14), $02
    ld l, (ix+16)
    ld h, (ix+17)
    ld a, (ix+25)
    or a
    jr z, ++
    cp $04
    jr c, +
    ld a, $04
+:	
    ld c, a
    ld a, $14
    sub c
-:	
    call +++
    inc a
    dec c
    jr nz, -
++:	
    xor a
    call +++
    ld a, (ix+25)
    cp $05
    jr c, +
    ld l, (ix+16)
    ld h, (ix+17)
    ld de, $FFF8
    add hl, de
    ld (ix+16), l
    ld (ix+17), h
+:	
    dec (ix+25)
    ret
	
+++:	
    ld b, $06
-:	
    ld (hl), a
    inc hl
    djnz -
    inc hl
    inc hl
    ret
