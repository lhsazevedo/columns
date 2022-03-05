.INCLUDE "state/entry.asm"
.INCLUDE "state/mainMenu.asm"
.INCLUDE "state/demo.asm"
.INCLUDE "state/modeMenu.asm"
.INCLUDE "state/optionsMenu.asm"

; 11th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1BA6_:
    call waitForInterruptIfFading
    call clearFilteredPalette_WaitInterrupt_ClearVram_LABEL_12D1_

    ld a, $02
    ld (_RAM_FFFF_), a

    ld a, $01
    call loadTextTilesWithColor

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
    ld (var.entities), hl

    ld hl, $32F7
    ld (var.entities.14.handler), hl

    ld hl, $332D
    ld (var.entities.15.handler), hl

    ld hl, $3363
    ld (var.entities.16.handler), hl

    ld a, $02
    ld (_RAM_FFFF_), a

    ld hl, palette_DATA_8012_
    call loadColors
    ld hl, palette_DATA_9A15_
    call loadColors
    ld a, $03
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_C000_
    call fade.in
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
state.gameplay.update:
    ld a, (_RAM_C008_)
    or a
    jr nz, ++
    ld a, (_RAM_C002_)
    or a
    jr nz, +
    call _LABEL_2E54_
+:
    call updateEntities
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
    ld (var.entities.2.handler), hl
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3AC4_
    ld (var.entities), hl
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
    call updateEntities
    call drawEntities_LABEL_25CC_
    jp waitInterrupt_LABEL_18D_

+:
    xor a
    ld (_RAM_C008_), a
    call fade.out
    ld a, $00
    ld (var.state), a
    jp waitInterrupt_LABEL_18D_

; 12th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1CBD_:
    call waitForInterruptIfFading
    call clearFilteredPalette_WaitInterrupt_ClearVram_LABEL_12D1_
    ld a, $02
    ld (_RAM_FFFF_), a
    ld a, $01
    call loadTextTilesWithColor
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
    ld (var.entities), hl
    ld hl, $404D
    ld (var.entities.3.handler), hl
    ld a, $02
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_8012_
    call loadColors
    ld hl, palette_DATA_9A15_
    call loadColors
    ld a, $03
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_C451_
    call fade.in
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
    call updateEntities
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
    ld (var.entities.2.handler), hl
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3AC4_
    ld (var.entities), hl
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
    ld (var.entities.2.handler), hl
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3B1B_
    ld (var.entities), hl
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
    call updateEntities
    call drawEntities_LABEL_25CC_
    call _LABEL_2DFE_
    jp waitInterrupt_LABEL_18D_

++:
    xor a
    ld (_RAM_C008_), a
    ld (_RAM_C009_), a
    call fade.out
    ld a, $00
    ld (var.state), a
    jp waitInterrupt_LABEL_18D_

; 23rd entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1E20_:
    call waitForInterruptIfFading
    call clearFilteredPalette_WaitInterrupt_ClearVram_LABEL_12D1_
    ld a, $02
    ld (_RAM_FFFF_), a
    call loadTextTilesBlue
    ld de, $38DE
    ld hl, _DATA_1E71_
    ld bc, $0008
    call _LABEL_604_
    ld a, $02
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_8012_
    call fade.in
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
    call fade.out
    ld a, $00
    ld (var.state), a
    jp waitInterrupt_LABEL_181_

; Data from 1E71 to 1E78 (8 bytes)
_DATA_1E71_:
.db $EE $00 $F5 $00 $E4 $00 $F1 $00

; 13th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_1E79_:
    call waitForInterruptIfFading
    call clearFilteredPalette_WaitInterrupt_ClearVram_LABEL_12D1_
    ld a, $02
    ld (_RAM_FFFF_), a
    ld a, $01
    call loadTextTilesWithColor
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
    ld (var.entities), hl
    ld hl, _LABEL_3B74_
    ld (var.entities.2.handler), hl
    ld a, $02
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_8012_
    call loadColors
    ld hl, palette_DATA_9A15_
    call loadColors
    ld a, $03
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_CD23_
    call fade.in
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
    call updateEntities
    call drawEntities_LABEL_25CC_
    jp waitInterrupt_LABEL_199_

+:
    xor a
    ld (_RAM_C008_), a
    ld a, (optMatches_RAM_C6B2_)
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
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3B41_
    ld (var.entities), hl
    ld hl, _LABEL_4007_
    ld (var.entities.2.handler), hl
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
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_4023_
    ld (var.entities), hl
    ld hl, _LABEL_3B5E_
    ld (var.entities.2.handler), hl
    ld a, $1A
    ld (var.state), a
    jp waitInterrupt_LABEL_199_

_LABEL_1FA9_:
    xor a
    ld (_RAM_C009_), a
    ld a, (optMatches_RAM_C6B2_)
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
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3B33_
    ld (var.entities), hl
    ld hl, _LABEL_4015_
    ld (var.entities.2.handler), hl
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
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3B4F_
    ld (var.entities), hl
    ld hl, _LABEL_4032_
    ld (var.entities.2.handler), hl
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
    call updateEntities
    call drawEntities_LABEL_25CC_
    jp waitInterrupt_LABEL_199_

+:
    xor a
    ld (_RAM_C008_), a
    ld a, (optMatches_RAM_C6B2_)
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
    call clearEntitiesAlt_LABEL_5EA_
    call _LABEL_1317_
    call _LABEL_136D_
    ld a, $02
    ld (_RAM_FFFF_), a
    ld hl, _DATA_A020_
    ld de, $2400
    ld a, $0F
    call _LABEL_746_
    ld hl, _LABEL_3599_
    ld (var.entities), hl
    ld hl, _LABEL_3B74_
    ld (var.entities.2.handler), hl
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
    call fade.out
    ld a, $00
    ld (var.state), a
    jp waitInterrupt_LABEL_199_

; 14th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_209C_:
    call waitForInterruptIfFading
    call clearFilteredPalette_WaitInterrupt_ClearVram_LABEL_12D1_
    ld a, $02
    ld (_RAM_FFFF_), a
    ld a, $01
    call loadTextTilesWithColor
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
    ld (var.entities), hl
    ld hl, _LABEL_3B74_
    ld (var.entities.2.handler), hl
    ld hl, $40CF
    ld (var.entities.3.handler), hl
    ld hl, $4146
    ld (var.entities.4.handler), hl
    ld a, $02
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_8012_
    call loadColors
    ld hl, palette_DATA_9A15_
    call loadColors
    ld a, $03
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_D6E0_
    call fade.in
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
    call updateEntities
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
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3B41_
    ld (var.entities), hl
    ld hl, _LABEL_4007_
    ld (var.entities.2.handler), hl
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
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3B33_
    ld (var.entities), hl
    ld hl, _LABEL_4015_
    ld (var.entities.2.handler), hl
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
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3B4F_
    ld (var.entities), hl
    ld hl, _LABEL_4032_
    ld (var.entities.2.handler), hl
    ld a, $1B
    ld (var.state), a
    jp waitInterrupt_LABEL_199_

+:
    ld hl, $2FB7
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3B5E_
    ld (var.entities), hl
    ld hl, _LABEL_4023_
    ld (var.entities.2.handler), hl
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
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3B65_
    ld (var.entities), hl
    ld hl, _LABEL_4039_
    ld (var.entities.2.handler), hl
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
    call updateEntities
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
    call clearEntitiesAlt_LABEL_5EA_
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
    ld (var.entities), hl
    ld hl, _LABEL_3B74_
    ld (var.entities.2.handler), hl
    ld hl, $40CF
    ld (var.entities.3.handler), hl
    ld hl, $4146
    ld (var.entities.4.handler), hl
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
    call fade.out
    ld a, $00
    ld (var.state), a
    jp waitInterrupt_LABEL_199_

; 15th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_2346_:
    call waitForInterruptIfFading
    call clearFilteredPalette_WaitInterrupt_ClearVram_LABEL_12D1_
    ld a, $02
    ld (_RAM_FFFF_), a
    ld a, $01
    call loadTextTilesWithColor
    ld a, $0F
    call _LABEL_12EB_
    call _LABEL_1317_
    ld a, (optLevel_RAM_C6AC_)
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
    ld (var.entities), hl
    ld hl, $32F7
    ld (var.entities.14.handler), hl
    ld hl, $332D
    ld (var.entities.15.handler), hl
    ld hl, $3363
    ld (var.entities.16.handler), hl
    xor a
    ld (_RAM_C69A_), a
    ld (_RAM_C6A6_), a
    ld a, $02
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_8012_
    call loadColors
    ld hl, palette_DATA_9A15_
    call loadColors
    ld a, $03
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_E29C_
    call fade.in
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
    call updateEntities
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
    ld (var.entities.2.handler), hl
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3AC4_
    ld (var.entities), hl
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
    call updateEntities
    call drawEntities_LABEL_25CC_
    jp waitInterrupt_LABEL_18D_

+:
    xor a
    ld (_RAM_C008_), a
    call fade.out
    ld a, $00
    ld (var.state), a
    jp waitInterrupt_LABEL_18D_

; 16th entry of Jump Table from 1BB (indexed by var.state)
_LABEL_2469_:
    call waitForInterruptIfFading
    call clearFilteredPalette_WaitInterrupt_ClearVram_LABEL_12D1_
    ld a, $02
    ld (_RAM_FFFF_), a
    ld a, $01
    call loadTextTilesWithColor
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
    ld (var.entities), hl
    ld hl, $404D
    ld (var.entities.3.handler), hl
    ld a, $02
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_8012_
    call loadColors
    ld hl, palette_DATA_9A15_
    call loadColors
    ld a, $03
    ld (_RAM_FFFF_), a
    ld hl, palette_DATA_E8CE_
    call fade.in
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
    call updateEntities
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
    ld (var.entities.2.handler), hl
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3AC4_
    ld (var.entities), hl
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
    ld (var.entities.2.handler), hl
    ld (var.entities.3.handler), hl
    ld (var.entities.4.handler), hl
    ld hl, _LABEL_3B1B_
    ld (var.entities), hl
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
    call updateEntities
    call drawEntities_LABEL_25CC_
    call _LABEL_2DFE_
    jp waitInterrupt_LABEL_18D_

+:
    xor a
    ld (_RAM_C008_), a
    ld (_RAM_C009_), a
    call fade.out
    ld a, $00
    ld (var.state), a
    jp waitInterrupt_LABEL_18D_
