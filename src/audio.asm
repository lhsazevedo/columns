audio_LABEL_45CA_:
    ld hl, _RAM_DD10_
    ld a, (hl)
    or a
    jr z, +
    ret p
    dec (hl)
    jp audio_LABEL_498F_

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
    ld hl, speed_RAM_DD01_
    ld a, (hl)
    or a
    ret z
    dec (hl)
    ret nz
    ld a, (speed_RAM_DD02_)
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
    ld de, var.audio.request_DD04
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
    ld (speed_RAM_DD01_), a
    ld (speed_RAM_DD02_), a
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
audio_LABEL_498F_:
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
    ld hl, + ; Overriding return address
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
    jp m, $FA4C ; Possibly invalid
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
    jp z, $E84D ; Possibly invalid
; Data from 4D13 to 6394 (5762 bytes)
.incbin "data/columns_DATA_4D13_.inc"

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
