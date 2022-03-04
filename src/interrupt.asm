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

    ; TODO: ?
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
    ld hl, + ; Overriding return address
    push hl
    ld a, (_RAM_C002_)
    or a
    jp nz, audio_LABEL_498F_
    jp audio_LABEL_45CA_

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
    ; TODO: If skipped: ?
    call _LABEL_41D_    
    ; TODO: If skipped: black screen
    call _LABEL_42C_

    ; TODO: Sprites?
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
    ld hl, + ; Overriding return address
    push hl
    ld a, (_RAM_C002_)
    or a
    jp nz, audio_LABEL_498F_
    jp audio_LABEL_45CA_

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
    ld a, (mode_RAM_C005_)
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
    rst $28 ; _LABEL_28_
    ret

+:
    rst $30 ; _LABEL_30_
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
