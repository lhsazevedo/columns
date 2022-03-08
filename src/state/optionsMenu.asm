state.optionsMenu.init:
    call clearEntitiesAlt_LABEL_5EA_
    call clearRam_LABEL_12FD_
    ld hl, (mode_RAM_C005_)
    ld h, $00
    add hl, hl
    ld de, @modes
    add hl, de
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    jp (hl)

@modes:
.dw @mode00
.dw @mode01
.dw @mode02
.dw @mode03
.dw @mode04
.dw @mode05

@mode00:
    ; "OPTIONS"
    ld a, $08
    ld de, _RAM_CD58_
    call drawTextNametable_LABEL_2885_

    ; "DIFFICULTY"
    ld a, $09
    ld de, _RAM_CDD0_
    call drawTextNametable_LABEL_2885_

    ; "BLOCKS"
    ld a, $0A
    ld de, _RAM_CE54_
    call drawTextNametable_LABEL_2885_

    ; "LEVEL"
    ld a, $0B
    ld de, $CED6
    call drawTextNametable_LABEL_2885_

    ; "EXIT"
    ld a, $0C
    ld de, _RAM_CF56_
    call drawTextNametable_LABEL_2885_

    ; Spawn arrow
    ld hl,  entitiesOptionsArrow.init
    ld (var.entities), hl

@draw:
    call waitInterrupt_LABEL_181_

    ld de, $3AC0
    ld hl, nametable_RAM_CD00_
    ld bc, $0C40
    call drawNametableArea_LABEL_65D_

    call updateEntities

    ld a, $09
    ld (var.state), a

    jp waitInterrupt_LABEL_181_

@mode01:
    ; "OPTIONS"
    ld a, $08
    ld de, $CD58
    call drawTextNametable_LABEL_2885_

    ; "DIFFICULTY"
    ld a, $09
    ld de, $CDD0
    call drawTextNametable_LABEL_2885_

    ; "BLOCKS"
    ld a, $0A
    ld de, $CE54
    call drawTextNametable_LABEL_2885_

    ; TODO
    ld a, $12
    ld de, $CED6
    call drawTextNametable_LABEL_2885_

    ; "EXIT"
    ld a, $0C
    ld de, $CF56
    call drawTextNametable_LABEL_2885_

    ld hl,  entitiesOptionsArrow.init
    ld (var.entities), hl

    jp @draw

@mode02:
    ; "OPTIONS"
    ld a, $08
    ld de, $CD58
    call drawTextNametable_LABEL_2885_

    ; "DIFFICULTY"
    ld a, $09
    ld de, $CDD6
    call drawTextNametable_LABEL_2885_

    ; "BLOCKS"
    ld a, $0A
    ld de, $CE1A
    call drawTextNametable_LABEL_2885_

    ; "LEVEL"
    ld a, $0B
    ld de, $CE5C
    call drawTextNametable_LABEL_2885_

    ; TODO
    ld a, $10
    ld de, $CEDC
    call drawTextNametable_LABEL_2885_

    ; "EXIT"
    ld a, $0C
    ld de, $CF5C
    call drawTextNametable_LABEL_2885_

    ; TODO
    ld hl, _LABEL_312A_
    ld (var.entities), hl

    ; TODO
    ld hl, _LABEL_31F5_
    ld (var.entities.2.handler), hl

    jp @draw

@mode03:
    ; "OPTIONS"
    ld a, $08
    ld de, $CD58
    call drawTextNametable_LABEL_2885_

    ; "DIFFICULTY"
    ld a, $09
    ld de, $CDD6
    call drawTextNametable_LABEL_2885_

    ; "BLOCKS"
    ld a, $0A
    ld de, $CE1A
    call drawTextNametable_LABEL_2885_

    ; TODO
    ld a, $12
    ld de, $CE5C
    call drawTextNametable_LABEL_2885_

    ; TODO
    ld a, $11
    ld de, _RAM_CED6_
    call drawTextNametable_LABEL_2885_

    ; "EXIT"
    ld a, $0C
    ld de, $CF5C
    call drawTextNametable_LABEL_2885_

    ; TODO
    ld hl, _LABEL_312A_
    ld (var.entities), hl

    ; TODO
    ld hl, _LABEL_31F5_
    ld (var.entities.2.handler), hl

    jp @draw

@mode04:
    ; "OPTIONS"
    ld a, $08
    ld de, $CD58
    call drawTextNametable_LABEL_2885_

    ; "DIFFICULTY"
    ld a, $09
    ld de, $CDD0
    call drawTextNametable_LABEL_2885_

    ; "BLOCKS"
    ld a, $0A
    ld de, $CE54
    call drawTextNametable_LABEL_2885_

    ; "LEVEL"
    ld a, $0B
    ld de, $CED6
    call drawTextNametable_LABEL_2885_

    ; "EXIT"
    ld a, $0C
    ld de, $CF56
    call drawTextNametable_LABEL_2885_

    ld hl,  entitiesOptionsArrow.init
    ld (var.entities), hl

    jp @draw

@mode05:
    ; "OPTIONS"
    ld a, $08
    ld de, $CD58
    call drawTextNametable_LABEL_2885_

    ; "DIFFICULTY"
    ld a, $09
    ld de, $CDD0
    call drawTextNametable_LABEL_2885_

    ; "BLOCKS"
    ld a, $0A
    ld de, $CE54
    call drawTextNametable_LABEL_2885_

    ; TODO
    ld a, $12
    ld de, $CED6
    call drawTextNametable_LABEL_2885_

    ; "EXIT"
    ld a, $0C
    ld de, $CF56
    call drawTextNametable_LABEL_2885_

    ld hl,  entitiesOptionsArrow.init
    ld (var.entities), hl

    jp @draw

; 10th entry of Jump Table from 1BB (indexed by var.state)
state.optionsMenu.update:
    ld hl, (var.entities.1.handler)
    ld de, (var.entities.2.handler)
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

    ld a, (mode_RAM_C005_)
    add a, $0A
    ld (var.state), a

    jp waitInterrupt_LABEL_181_

; Unused "INIT" 
.db $E8 $00 $ED $00 $E8 $00 $F3 $00
