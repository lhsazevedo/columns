; 7th entry of Jump Table from 1BB (indexed by var.state)
state.modeMenu.init:
    call clearEntitiesAlt_LABEL_5EA_
    call clearRam_LABEL_12FD_

    ; Title
    ld a, $05
    ld de, _RAM_CD62_
    call _LABEL_289D_

    ; Original
    ld a, $06
    ld de, _RAM_CDEA_
    call _LABEL_289D_

    ; Flash
    ld a, $07
    ld de, _RAM_CE44_
    call _LABEL_289D_

    ; TODO
    call waitInterrupt_LABEL_181_

    ; None of the above aplies without this
    ld de, $3B0A
    ld hl, nametable_RAM_CD00_
    ld bc, $092C
    call copyAreaToNametable_LABEL_65D_

    ; Spawn arrow
    ld hl, entities.optionsArrow.init
    ld (var.entities), hl

    ld a, $07 ; state.modeMenu.update
    ld (var.state), a

    jp waitInterrupt_LABEL_181_

; 8th entry of Jump Table from 1BB (indexed by var.state)
state.modeMenu.update:
    ld a, (var.input.player1.debounced)
    and JOY_FIREA | JOY_FIREB

    jr nz, @actionPressed
        call updateEntities
        call drawEntities_LABEL_25CC_
        jp waitInterrupt_LABEL_181_
    @actionPressed:
        ; TODO
        ld a, (var.entities.1.byte0E)
        ld d, a

        ld a, (mode_RAM_C005_)
        and $06
        add a, d
        ld (mode_RAM_C005_), a

        ld a, $08
        ld (var.state), a

        jp waitInterrupt_LABEL_181_
