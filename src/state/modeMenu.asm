; 7th entry of Jump Table from 1BB (indexed by var.state)
state.modeMenu.init:
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
state.modeMenu.update:
    ld a, (var.input.player1.debounced)
    and $30
    jr nz, +
    call updateEntities
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
