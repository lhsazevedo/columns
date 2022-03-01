; 9th entry of Jump Table from 1BB (indexed by var.state)
state.optionsMenu.init:
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
.dw _LABEL_1A17_
.dw _LABEL_1A5F_
.dw _LABEL_1A90_
.dw _LABEL_1ACF_
.dw _LABEL_1B0E_
.dw _LABEL_1B3F_

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
    call updateEntities
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
state.optionsMenu.update:
    ld hl, (v_entities)
    ld de, (_RAM_C120_)
    ld a, l
    or h
    or e
    or d
    jr z, +
    call updateEntities
    call drawEntities_LABEL_25CC_
    jp waitInterrupt_LABEL_181_

+:
    ld a, $0C
    ld (audioFadeOutTimer_RAM_DD0E_), a
    ld a, $04
    ld (audio_RAM_DD0F_), a
    call fade.out
    ld a, (_RAM_C005_)
    add a, $0A
    ld (var.state), a
    jp waitInterrupt_LABEL_181_

; Data from 1B9E to 1BA5 (8 bytes)
.db $E8 $00 $ED $00 $E8 $00 $F3 $00
