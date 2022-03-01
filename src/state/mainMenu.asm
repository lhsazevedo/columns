state.mainMenu.init:
    call _LABEL_12C8_
    call _LABEL_15E3_
    call _LABEL_12D1_

    ld a, $02
    ld (_RAM_FFFF_), a

    ; Load text character tiles
    ld a, $01
    call _LABEL_12E2_

    ; Load arrow tiles
    ld hl, _DATA_8000_
    ld de, $37C0
    ld a, $0F
    call _LABEL_746_

    ; Load title and statue tiles
    ld de, $0000
    ld hl, _DATA_8899_
    call _LABEL_6D0_

    ; TODO: Probably extracting
    ld de, _RAM_CD00_
    ld hl, _DATA_86B3_
    call _LABEL_70C_

    ; Load title tilemap
    ld hl, _RAM_CD00_
    ld de, $3844
    ld bc, $0A38
    call _LABEL_648_

    ; TODO: Probably extracting
    ld de, _RAM_CD00_
    ld hl, _DATA_87FD_
    call _LABEL_70C_

    ; Draw left statue
    ld hl, _RAM_CD00_
    ld de, $3AC2
    ld bc, $0C08
    call _LABEL_648_

    ; TODO: Probably extracting
    ld de, _RAM_CD00_
    ld hl, _DATA_8846_
    call _LABEL_70C_

    ; Draw right statue
    ld hl, _RAM_CD00_
    ld de, $3AF6
    ld bc, $0C08
    call _LABEL_648_

    ; Draw "Press Start Button"
    xor a
    ld de, $3B4E
    call _LABEL_2885_

    ; Draw 1st menu option
    ; "1 PLAYER"
    ld a, $01
    ld de, $3BD8
    call _LABEL_2885_

    ; Draw 2nd menu option
    ; "2 PLAYER"
    ld a, $03
    ld de, $3C58
    call _LABEL_2885_

    ; Draw 3rd menu option
    ; "VERSUS"
    ld a, $02
    ld de, $3CDA
    call _LABEL_2885_

    ; Draw copyright stuff
    ld a, $04
    ld de, $3DE6
    call _LABEL_2885_

    ; Load colors
    ld hl, palette_DATA_8012_
    call loadColors
    ld hl, palette_DATA_86A1_
    call loadColors

    ; Fade in
    ld hl, palette_DATA_9A15_
    call fade.in

    xor a
    ld (_RAM_D000_), a
    ld (_RAM_C00A_), a

    ; Spawn arrow
    ld hl, initArrow
    ld (v_entities), hl

    ; TODO
    ld a, $02
    ld (_RAM_C018_), a

    ei

    ; Request main menu song
    ld a, SOUND_MAIN_MENU_SONG
    ld (_RAM_DD04_), a

    ; Set main menu timer
    ld hl, $0258
    ld (var.timer), hl

    ; Set next state
    ld a, $03 ; state.mainMenu.update
    ld (var.state), a

    jp waitInterrupt_LABEL_181_


state.mainMenu.update:
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

    call fade.out

    ld a, $04 ; state.demo.init
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
