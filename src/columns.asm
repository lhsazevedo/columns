; This disassembly was created using Emulicious (https://www.emulicious.net)
.MEMORYMAP
SLOTSIZE $7FF0
SLOT 0 $0000
SLOTSIZE $10
SLOT 1 $7FF0
SLOTSIZE $4000
SLOT 2 $8000
DEFAULTSLOT 2
.ENDME
.ROMBANKMAP
BANKSTOTAL 8
BANKSIZE $7FF0
BANKS 1
BANKSIZE $10
BANKS 1
BANKSIZE $4000
BANKS 6
.ENDRO

.EMPTYFILL $FF

.INCLUDE "constants/sms.asm"
.INCLUDE "constants/audio.asm"
.INCLUDE "variables.asm"

.BANK 0 SLOT 0
.ORG $0000

start:
    jp init

.ORG $8
setVdpAddress:
    di
    ld a, e
    out (Port_VDPAddress), a
    ld a, $40
    or d
    out (Port_VDPAddress), a
    ei
    ret

.ORG $18
; Data from 18 to 1A (3 bytes)
_DATA_18_:
.db $F3 $7B $D3

; Data from 1B to 27 (13 bytes)
_DATA_1B_:
.db $BF $7A $D3 $BF $FB $C9 $FF $FF $FF $FF $FF $FF $FF

_LABEL_28_:
    ld a, $E0
    jp +


.ORG $30
_LABEL_30_:
    ld a, $A0
    jp +

.ORG $38
handleInterruptEntrypoint:
    jp handleInterrupt

+:
    out (Port_VDPAddress), a
    ld a, $81
    out (Port_VDPAddress), a
    ret

.ORG $66
handlePauseInterruptEntrypoint:
    push af
    ld a, (var.previousState)
    cp $10
    jr c, +
    cp $16
    jr nc, +
    ld a, (_RAM_C002_)
    cpl
    ld (_RAM_C002_), a
    pop af
    retn

+:
    xor a
    ld (_RAM_C002_), a
    pop af
    retn

init:
    di

    ld sp, $DFF0

    im 1

    ; Configure memory mapper
    ld hl, _RAM_FFFC_
    ld (hl), $00
    inc hl
    ld (hl), $00
    inc hl
    ld (hl), $01
    inc hl
    ld (hl), $02

    ; Clear some RAM
    ; TODO: Why those places.
    ld hl, _RAM_C001_
    ld de, _RAM_C001_ + 1
    ld bc, $00FE
    ld (hl), $00
    ldir

    ; TODO: Clear entities?
    ld hl, v_entities
    ld de, v_entities + 1
    ld bc, $0EFF
    ld (hl), $00
    ldir

    ld hl, _RAM_DD00_
    ld de, _RAM_DD00_ + 1
    ld bc, $02EF
    ld (hl), $00
    ldir

    ; Wait for scan line 176
    -:
        in a, (Port_VCounter)
        cp $B0
    jr nz, -

    xor a
    out (Port_VDPAddress), a
    ld a, $C0
    out (Port_VDPAddress), a

    ; Clear palette
    xor a
    ld b, $20

    ; TODO: ?
    ex (sp), hl
    ex (sp), hl

    -:
        out (Port_VDPData), a
        nop
    djnz -

    call io_LABEL_126_

    ; TODO
    ld (_RAM_C001_), a

    ld a, $01
    ld (_RAM_C6A8_), a
    ld (_RAM_C6AD_), a

    ld a, $03
    ld (_RAM_C6AB_), a
    ld (_RAM_C6B0_), a

reset:
    di

    ld sp, $DFF0

    ; Reset some values
    ; TODO
    xor a
    ld (var.previousState), a
    ld (var.state), a
    ld (_RAM_C005_), a
    ld (_RAM_C002_), a
    ld (_RAM_C006_), a
    ld (_RAM_C6C2_), a
    ld (_RAM_C008_), a
    ld (_RAM_C009_), a

    ; Init Vdp Registers
    in a, (Port_VDPStatus)
    ld b, $16
    ld c, Port_VDPAddress
    ld hl, initialVdpRegisters
    otir

    ; TODO
    rst $30 ; _LABEL_30_

    call clearFilteredPalette

    ; TODO: Clear VRAM and more RAM
    call _LABEL_5C6_

    ; Unnecessary reset
    ld a, $00
    ld (var.previousState), a
    ld (var.state), a

    ei
    jp mainLoop

io_LABEL_126_:
    ld a, $F5
    out (Port_IOPortControl), a
    in a, (Port_IOPort2)
    and $C0
    cp $C0
    jr nz, +
    ld a, $55
    out (Port_IOPortControl), a
    in a, (Port_IOPort2)
    and $C0
    or a
    jr nz, +
    ld a, $FF
    out (Port_IOPortControl), a
    ret

+:
    xor a
    ret

; Data from 144 to 144 (1 bytes)
.db $C9

; Data from 145 to 15A (22 bytes)
initialVdpRegisters:
.db $06
.db $80
.db $A0
.db $81
.db $FF
.db $82
.db $FF
.db $83
.db $FF
.db $84
.db $FF
.db $85
.db $FF
.db $86
.db $00
.db $87
.db $00
.db $88
.db $00
.db $89
.db $00
.db $8A

mainLoop:
    call input.update
    call fade.update

    ; Loop by overriding return address
    ld hl, mainLoop
    push hl

    ; Update var.previousState on state change
    ld hl, var.state
    ld a, (var.previousState)
    xor (hl)
    and $7F
    ld a, (hl)
    jr z, @endif
        ld (var.previousState), a
    @endif:

    ld hl, updatersPointers

jumpToAthPointer:
    ld e, a
    ld d, $00
    add hl, de
    add hl, de
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    jp (hl)

waitInterrupt_LABEL_181_:
    ld a, $01
    ld (var.interrupt.handlerIndex), a
-:
    ld a, (var.interrupt.handlerIndex)
    or a
    jr nz, -
    ret

waitInterrupt_LABEL_18D_:
    ld a, $02
    ld (var.interrupt.handlerIndex), a
-:
    ld a, (var.interrupt.handlerIndex)
    or a
    jr nz, -
    ret

waitInterrupt_LABEL_199_:
    ld a, $03
    ld (var.interrupt.handlerIndex), a
-:
    ld a, (var.interrupt.handlerIndex)
    or a
    jr nz, -
    ret

; Unused?
waitInterrupt_LABEL_1A5_:
    ld a, $04
    ld (var.interrupt.handlerIndex), a
-:
    ld a, (var.interrupt.handlerIndex)
    or a
    jr nz, -
    ret

tickTimer:
    ld hl, (var.timer)
    dec hl
    ld (var.timer), hl
    ld a, l
    or h
    ret

; Jump Table from 1BB to 1F6 (30 entries, indexed by var.state)
updatersPointers:
.dw state.entry.update
.dw state.entry.update
.dw state.mainMenu.init
.dw state.mainMenu.update
.dw state.demo.init
.dw state.demo.update
.dw state.modeMenu.init
.dw state.modeMenu.update
.dw state.optionsMenu.init
.dw state.optionsMenu.update
.dw _LABEL_1BA6_
.dw _LABEL_1CBD_
.dw _LABEL_1E79_
.dw _LABEL_209C_
.dw _LABEL_2346_
.dw _LABEL_2469_
.dw state.gameplay.update
.dw _LABEL_1D64_
.dw _LABEL_1F15_
.dw _LABEL_2168_
.dw _LABEL_23F9_
.dw _LABEL_2510_
.dw _LABEL_1E20_
.dw _LABEL_1E59_
.dw _LABEL_1C98_
.dw _LABEL_1DEF_
.dw _LABEL_2027_
.dw _LABEL_22AE_
.dw _LABEL_2444_
.dw _LABEL_25A1_

.INCLUDE "interrupt.asm"
.INCLUDE "input.asm"
.INCLUDE "updateEntities.asm"
.INCLUDE "fade.asm"

_LABEL_5C6_:
    ; TODO: Clear VRAM from $00 to $1F
    ld de, $0000
    ld hl, $0000
    ld bc, $0010
    call _LABEL_62E_

    ; TODO: Clear VRAM from $2000 to $201F
    ld de, $2000
    ld hl, $0000
    ld bc, $0010
    call _LABEL_62E_

    ; TODO: Clear VRAM from $3800 to $3EFF
    ld de, $3800
    ld hl, $0000
    ld bc, $0380
    call _LABEL_62E_

_LABEL_5EA_:
    ; TODO: Clear RAM from $C100 to $C300 (512 bytes)
    ld hl, v_entities
    ld de, v_entities + 1
    ld (hl), $00
    call ldi128
    call ldi128
    call ldi128
    call ldi127

    ; TODO: ?
    ld hl, _RAM_C300_
    ld (hl), $D0

    ret

_LABEL_604_:
    rst $08 ; setVdpAddress
    inc b
--:
    ld a, b
    ld b, c
    ld c, Port_VDPData
-:
    outi
    jr nz, -
    ld b, a
    ld c, $00
    djnz --
    ret

; Data from 614 to 62D (26 bytes)
.db $08 $CF $08 $04 $C5 $41 $18 $03 $C5 $06 $00 $D3 $BE $00 $10 $FC
.db $C1 $10 $F5 $C9 $D3 $BE $00 $10 $FB $C9

_LABEL_62E_:
    rst $08 ; setVdpAddress
    inc b
    ld d, c
    ld c, Port_VDPData
    push bc
    ld b, d
    jr _LABEL_63A_

-:
    push bc
    ld b, $00
_LABEL_63A_:
    ld a, l
    out (c), a
    ld a, h
    jr +

+:
    out (c), a
    djnz _LABEL_63A_
    pop bc
    djnz -
    ret

_LABEL_648_:
    rst $08 ; setVdpAddress
    push bc
    ld b, c
    ld c, Port_VDPData
-:
    outi
    outi
    jr nz, -
    ex de, hl
    ld bc, $0040
    add hl, bc
    ex de, hl
    pop bc
    djnz _LABEL_648_
    ret

_LABEL_65D_:
    rst $08 ; setVdpAddress
    push bc
    ld b, c
    ld c, Port_VDPData
-:
    outi
    jr +

+:
    outi
    jr nz, -
    ex de, hl
    ld bc, $0040
    add hl, bc
    ex de, hl
    pop bc
    djnz _LABEL_65D_
    ret

; Unused
_LABEL_674_:
    @loop:
        push bc
        ex af, af'
        rst $08 ; setVdpAddress
        @loop2:
            ld a, (hl)
            out (Port_VDPData), a
            ex af, af'
            jr +
                ; ...
            +:
            out (Port_VDPData), a
            ex af, af'
            inc hl
            dec c
        jr nz, @loop2

        ex af, af'
        ex de, hl
        ld bc, $0040
        add hl, bc
        ex de, hl
        pop bc
    djnz @loop

    ret

loadColors:
    ld de, v_palette

    ; Never taken if condition
    jr @endif
        ld a, (var.pallete.shouldUpdate)
        or $01
        ld (var.pallete.shouldUpdate), a
        ld de, v_filteredPalette
    @endif:

    ; Load offset byte
    ld a, (hl)
    inc hl
    push hl ; save source

    ; Add offset to dest
    ld l, a
    ld h, $00
    add hl, de
    ex de, hl

    ; Load length byte
    pop hl
    ld a, (hl)
    ld c, a
    ld b, $00
    inc hl

    ; Copy
    ldir

    ret

clearFilteredPalette:
    ld hl, v_filteredPalette
    ld de, v_filteredPalette + 1
    ld (hl), $00
    call ldi63
    ld hl, var.pallete.shouldUpdate
    set 0, (hl)
    ret

writePalette:
    xor a
    out (Port_VDPAddress), a
    ld a, $C0
    out (Port_VDPAddress), a
    ld hl, v_filteredPalette
    ld c, Port_VDPData
    jp outi32

_LABEL_6D0_:
    ld a, (hl)
    inc hl
    exx
    ld e, a
    ld d, $00
    exx
    ld b, a
-:
    push bc
    push de
    exx
    pop hl
    push hl
    exx
    call _LABEL_6E7_
    pop de
    inc de
    pop bc
    djnz -
    ret

_LABEL_6E7_:
    ld a, (hl)
    inc hl
    or a
    ret z
    ld b, a
    and $80
    ld c, a
    ld a, b
    and $7F
    ld b, a
-:
    rst $08 ; setVdpAddress
    ld a, (hl)
    out (Port_VDPData), a
    xor a
    or c
    jr z, +
    inc hl
+:
    exx
    add hl, de
    push hl
    exx
    pop de
    djnz -
    xor a
    or c
    jp nz, _LABEL_6E7_
    inc hl
    jp _LABEL_6E7_

_LABEL_70C_:
    ld a, (hl)
    inc hl
    exx
    ld e, a
    ld d, $00
    exx
    ld b, a
    -:
        push bc
        push de
        exx
        pop hl
        push hl
        exx
        call _LABEL_723_
        pop de
        inc de
        pop bc
    djnz -
    ret

_LABEL_723_:
    ld a, (hl)
    inc hl
    or a
    ret z
    ld b, a
    and $80
    ld c, a
    ld a, b
    and $7F
    ld b, a
-:
    ld a, (hl)
    ld (de), a
    bit 7, c
    jr z, +
    inc hl
+:
    exx
    add hl, de
    push hl
    exx
    pop de
    djnz -
    bit 7, c
    jp nz, _LABEL_723_
    inc hl
    jp _LABEL_723_

_LABEL_746_:
    ld (_RAM_C0F8_), a
    rst $08 ; setVdpAddress
    ld c, (hl)
    inc hl
    ld b, (hl)
    inc hl
--:
    ld a, (hl)
    exx
    ld c, Port_VDPData
    ld b, $04
    ld h, a
    ld a, (_RAM_C0F8_)
-:
    rra
    ld d, h
    jr c, +
    ld d, $00
+:
    out (c), d
    djnz -
    exx
    inc hl
    dec bc
    ld a, b
    or c
    jp nz, --
    ret

; Data from 76B to 1012 (2216 bytes)
.incbin "data/columns_DATA_76B_.inc"

.INCLUDE "util/outi.asm"

_LABEL_1114_:
    outi
    out (Port_VDPData), a
    outi
    out (Port_VDPData), a
    outi
    out (Port_VDPData), a
    outi
    out (Port_VDPData), a
    outi
    out (Port_VDPData), a
    outi
    out (Port_VDPData), a
    ret

; Data from 112D to 116A (62 bytes)
.db $D3 $BE $D3 $BE $D3 $BE $D3 $BE $D3 $BE $D3 $BE $D3 $BE $D3 $BE
.db $D3 $BE $D3 $BE $D3 $BE $D3 $BE $C9 $D3 $BE $08 $D3 $BE $08 $D3
.db $BE $08 $D3 $BE $08 $D3 $BE $08 $D3 $BE $08 $D3 $BE $08 $D3 $BE
.db $08 $D3 $BE $08 $D3 $BE $08 $D3 $BE $08 $D3 $BE $08 $C9

.INCLUDE "util/ldi.asm"

; Data from 126C to 12A8 (61 bytes)
.db $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8
.db $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8
.db $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8
.db $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $ED $A8 $C9

_LABEL_12A9_:
    push hl
    ld hl, (_RAM_C01A_)
    ld a, h
    rrca
    rrca
    xor h
    rrca
    xor l
    rrca
    rrca
    rrca
    rrca
    xor l
    rrca
    adc hl, hl
    jr nz, +
    ld hl, $733C
+:
    ld a, r
    xor l
    ld (_RAM_C01A_), hl
    pop hl
    ret

_LABEL_12C8_:
    ld a, (var.fade.state)
    or a
    ret z
    pop hl
    jp waitInterrupt_LABEL_181_

_LABEL_12D1_:
    call clearFilteredPalette
    ld a, $01
    ld (_RAM_C018_), a
    call waitInterrupt_LABEL_181_
    di
    jp _LABEL_5C6_

_LABEL_12E0_:
    ld a, $0F
_LABEL_12E2_:
    ld de, $1AC0
    ld hl, _DATA_8034_
    jp _LABEL_746_

_LABEL_12EB_:
    ld de, $3580
    ld hl, _DATA_8320_
    jp _LABEL_746_

; Data from 12F4 to 12FC (9 bytes)
.db $11 $00 $2F $21 $7E $81 $C3 $46 $07

_LABEL_12FD_:
    ld hl, _RAM_CD00_
    ld de, _RAM_CD01_
    ld (hl), $00
    call ldi128
    call ldi128
    call ldi128
    call ldi128
    call ldi128
    jp ldi128

_LABEL_1317_:
    ld a, (_RAM_C6AC_)
    ld (_RAM_C699_), a
    ld a, (_RAM_C6B1_)
    ld (_RAM_C6A5_), a
    xor a
    ld (_RAM_C6BE_), a
    ld (_RAM_C6C0_), a
    ld (_RAM_C690_), a
    ld (_RAM_C691_), a
    ld (_RAM_C692_), a
    ld (_RAM_C694_), a
    ld (_RAM_C695_), a
    ld (_RAM_C696_), a
    ld (_RAM_C697_), a
    ld (_RAM_C698_), a
    ld (_RAM_C693_), a
    ld (_RAM_C69A_), a
    ld (_RAM_C69B_), a
    ld (_RAM_C69C_), a
    ld (_RAM_C69D_), a
    ld (_RAM_C69E_), a
    ld (_RAM_C6A0_), a
    ld (_RAM_C6A1_), a
    ld (_RAM_C6A2_), a
    ld (_RAM_C6A3_), a
    ld (_RAM_C6A4_), a
    ld (_RAM_C69F_), a
    ld (_RAM_C6A6_), a
    ld (_RAM_C6A7_), a
    ret

_LABEL_136D_:
    ld hl, _RAM_C400_
    ld b, $06
    ld (hl), $FF
    inc hl
-:
    ld (hl), $00
    inc hl
    djnz -
    ld (hl), $FF
    inc hl
    ex de, hl
    ld hl, _RAM_C400_
    call ldi128
    call ldi32
    ld hl, _RAM_C4A7_
    call ldi8
    ld hl, _RAM_C400_
    ld de, _RAM_C548_
    call ldi128
    jp ldi48

_LABEL_1399_:
    ld hl, $13C5
    push hl
    call _LABEL_12A9_
    and $38
    ld e, a
    ld d, $00
    ld hl, $11EB
    add hl, de
    add hl, de
    push hl
    ld hl, $C428
    add hl, de
    ex de, hl
    ld a, (_RAM_C6A8_)
    add a, a
    ld c, a
    ld b, $00
    ld hl, _DATA_1457_
    add hl, bc
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    push hl
    call ldi64
    pop hl
    ret

; Data from 13C5 to 13F7 (51 bytes)
.db $21 $EE $13 $E5 $3A $AB $C6 $87 $87 $87 $87 $5F $16 $00 $21 $7B
.db $11 $19 $E5 $21 $18 $C4 $36 $FF $06 $06 $23 $36 $00 $10 $FB $23
.db $36 $FF $21 $18 $C4 $11 $20 $C4 $C9 $21 $A7 $C4 $11 $A8 $C4 $CD
.db $5B $12 $C9

_LABEL_13F8_:
    ld hl, $1424
    push hl
    call _LABEL_12A9_
    and $38
    ld e, a
    ld d, $00
    ld hl, $11EB
    add hl, de
    add hl, de
    push hl
    ld hl, $C570
    add hl, de
    ex de, hl
    ld a, (_RAM_C6AD_)
    add a, a
    ld c, a
    ld b, $00
    ld hl, _DATA_145D_
    add hl, bc
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    push hl
    call ldi64
    pop hl
    ret

; Data from 1424 to 1456 (51 bytes)
.db $21 $4D $14 $E5 $3A $B0 $C6 $87 $87 $87 $87 $5F $16 $00 $21 $7B
.db $11 $19 $E5 $21 $60 $C5 $36 $FF $06 $06 $23 $36 $00 $10 $FB $23
.db $36 $FF $21 $60 $C5 $11 $68 $C5 $C9 $21 $EF $C5 $11 $F0 $C5 $CD
.db $5B $12 $C9

; Pointer Table from 1457 to 145C (3 entries, indexed by _RAM_C6A8_)
_DATA_1457_:
.dw _DATA_1463_ _DATA_14A3_ _DATA_14E3_

; Pointer Table from 145D to 1462 (3 entries, indexed by _RAM_C6AD_)
_DATA_145D_:
.dw _DATA_1523_ _DATA_1563_ _DATA_15A3_

; 1st entry of Pointer Table from 1457 (indexed by _RAM_C6A8_)
; Data from 1463 to 14A2 (64 bytes)
_DATA_1463_:
.db $FF $01 $02 $03 $04 $01 $02 $FF $FF $04 $03 $01 $02 $04 $03 $FF
.db $FF $02 $01 $03 $04 $02 $01 $FF $FF $03 $04 $01 $02 $03 $04 $FF
.db $FF $01 $02 $03 $04 $01 $02 $FF $FF $01 $04 $01 $02 $03 $02 $FF
.db $FF $03 $02 $03 $04 $01 $04 $FF $FF $03 $04 $01 $02 $03 $04 $FF

; 2nd entry of Pointer Table from 1457 (indexed by _RAM_C6A8_)
; Data from 14A3 to 14E2 (64 bytes)
_DATA_14A3_:
.db $FF $01 $02 $03 $04 $01 $05 $FF $FF $02 $04 $05 $02 $03 $03 $FF
.db $FF $04 $02 $05 $04 $01 $01 $FF $FF $05 $04 $01 $03 $03 $04 $FF
.db $FF $01 $02 $03 $05 $01 $02 $FF $FF $03 $05 $01 $01 $03 $04 $FF
.db $FF $01 $02 $03 $04 $05 $02 $FF $FF $05 $04 $01 $02 $03 $04 $FF

; 3rd entry of Pointer Table from 1457 (indexed by _RAM_C6A8_)
; Data from 14E3 to 1522 (64 bytes)
_DATA_14E3_:
.db $FF $01 $02 $03 $04 $05 $06 $FF $FF $04 $03 $05 $06 $04 $03 $FF
.db $FF $06 $05 $01 $02 $02 $01 $FF $FF $01 $02 $03 $04 $05 $06 $FF
.db $FF $05 $06 $01 $02 $03 $04 $FF $FF $03 $02 $03 $06 $05 $02 $FF
.db $FF $01 $05 $04 $01 $03 $06 $FF $FF $03 $04 $05 $06 $01 $02 $FF

; 1st entry of Pointer Table from 145D (indexed by _RAM_C6AD_)
; Data from 1523 to 1562 (64 bytes)
_DATA_1523_:
.db $FF $07 $08 $09 $0A $07 $08 $FF $FF $0A $09 $07 $08 $0A $09 $FF
.db $FF $08 $07 $09 $0A $08 $07 $FF $FF $09 $0A $07 $08 $09 $0A $FF
.db $FF $07 $08 $09 $0A $07 $08 $FF $FF $07 $0A $07 $08 $09 $08 $FF
.db $FF $09 $08 $09 $0A $07 $0A $FF $FF $09 $0A $07 $08 $09 $0A $FF

; 2nd entry of Pointer Table from 145D (indexed by _RAM_C6AD_)
; Data from 1563 to 15A2 (64 bytes)
_DATA_1563_:
.db $FF $07 $08 $09 $0A $07 $0B $FF $FF $08 $0A $0B $08 $09 $09 $FF
.db $FF $0A $08 $0B $0A $07 $07 $FF $FF $0B $0A $07 $09 $09 $0A $FF
.db $FF $07 $08 $09 $0B $07 $08 $FF $FF $09 $0B $07 $07 $09 $0A $FF
.db $FF $07 $08 $09 $0A $0B $08 $FF $FF $0B $0A $07 $08 $09 $0A $FF

; 3rd entry of Pointer Table from 145D (indexed by _RAM_C6AD_)
; Data from 15A3 to 15E2 (64 bytes)
_DATA_15A3_:
.db $FF $07 $08 $09 $0A $0B $0C $FF $FF $0A $09 $0B $0C $0A $09 $FF
.db $FF $0C $0B $07 $08 $08 $07 $FF $FF $07 $08 $09 $0A $0B $0C $FF
.db $FF $0B $0C $07 $08 $09 $0A $FF $FF $09 $08 $09 $0C $0B $08 $FF
.db $FF $07 $0B $0A $07 $09 $0C $FF $FF $09 $0A $0B $0C $07 $08 $FF

_LABEL_15E3_:
    ld hl, v_entities
    ld de, v_entities + 1
    ld (hl), $00
    call ldi128
    call ldi128
    call ldi128
    jp ldi127

_LABEL_15F7_:
    ld de, $3A54
    rst $08 ; setVdpAddress
    ld a, $10
    ex af, af'
    ld a, (_RAM_C6CC_)
    add a, a
    add a, a
    add a, $AE
    call +
    ex af, af'
    ld de, $3A94
    rst $08 ; setVdpAddress
    ld a, $10
    ex af, af'
    call +
    ld de, $3A68
    rst $08 ; setVdpAddress
    ld a, $10
    ex af, af'
    ld a, (_RAM_C6CD_)
    add a, a
    add a, a
    add a, $AE
    call +
    ex af, af'
    ld de, $3AA8
    rst $08 ; setVdpAddress
    ld a, $10
    ex af, af'
+:
    out (Port_VDPData), a
    inc a
    ex af, af'
    jr +

+:
    out (Port_VDPData), a
    ex af, af'
    jr +

+:
    out (Port_VDPData), a
    inc a
    ex af, af'
    jr +

+:
    out (Port_VDPData), a
    ex af, af'
    ret

.INCLUDE "updaters.asm"

drawEntities_LABEL_25CC_:
    xor a
    ld (_RAM_C3C0_), a
    ld ix, v_entities
    ld de, $0020
    ld b, $10
-:
    push bc
    push de
    call +
    pop de
    add ix, de
    pop bc
    djnz -
    ld a, (_RAM_C3C0_)
    ld e, a
    ld d, $00
    ld hl, $C300
    add hl, de
    ld (hl), $D0
    ret

+:
    ld a, (ix+2)
    or a
    ret z
    ld a, (ix+3)
    or a
    ret z
    ld e, a
    ld d, $00
    ld hl, _DATA_265A_ - 2
    add hl, de
    add hl, de
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ld c, (hl)
    ld b, $00
    inc hl
    ld a, (_RAM_C3C0_)
    add a, c
    cp $41
    ret nc
    push hl
    ld a, (_RAM_C3C0_)
    ld e, a
    ld d, $00
    ld hl, $C300
    add hl, de
    ex de, hl
    pop hl
    ld b, c
-:
    ld a, (hl)
    add a, (ix+6)
    cp $D0
    jr nz, +
    inc a
+:
    ld (de), a
    inc hl
    inc de
    djnz -
    ld a, (_RAM_C3C0_)
    add a, c
    cp $40
    jr z, +
    ld a, $D0
    ld (de), a
+:
    push hl
    ld a, (_RAM_C3C0_)
    ld e, a
    ld d, $00
    ld hl, $C340
    add hl, de
    add hl, de
    ex de, hl
    pop hl
    add a, c
    ld (_RAM_C3C0_), a
    ld b, c
-:
    ld a, (hl)
    add a, (ix+9)
    ld (de), a
    inc hl
    inc de
    ldi
    djnz -
    ret

; Data from 2658 to 2659 (2 bytes)
.db $00 $00

; Pointer Table from 265A to 2699 (32 entries, indexed by _RAM_C103_)
_DATA_265A_:
.dw _DATA_269A_ _DATA_269E_ _DATA_26B7_ _DATA_26D0_ _DATA_26E9_ _DATA_26F0_ _DATA_26F7_ _DATA_26FE_
.dw _DATA_2705_ _DATA_270C_ _DATA_2713_ _DATA_271A_ _DATA_2721_ _DATA_2728_ _DATA_272F_ _DATA_2733_
.dw _DATA_2737_ _DATA_2744_ _DATA_2751_ _DATA_2755_ _DATA_276E_ _DATA_2787_ _DATA_27C4_ _DATA_27FB_
.dw _DATA_2868_ _DATA_2881_ _DATA_2881_ _DATA_2881_ _DATA_2881_ _DATA_2881_ _DATA_2881_ _DATA_2881_

; 1st entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 269A to 269D (4 bytes)
_DATA_269A_:
.db $01 $FF $00 $BF

; 2nd entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 269E to 26B6 (25 bytes)
_DATA_269E_:
.db $08 $00 $23 $41 $67 $83 $A2 $C9 $E1 $E0 $40 $20 $43 $00 $41 $50
.db $40 $32 $42 $97 $44 $C5 $41 $1B $43

; 3rd entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26B7 to 26CF (25 bytes)
_DATA_26B7_:
.db $08 $18 $33 $50 $79 $9D $B2 $D1 $EE $33 $43 $E6 $40 $53 $41 $28
.db $42 $CD $41 $A3 $44 $6A $40 $10 $42

; 4th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26D0 to 26E8 (25 bytes)
_DATA_26D0_:
.db $08 $02 $28 $3D $63 $7B $A7 $BA $E4 $74 $44 $37 $40 $DD $42 $02
.db $41 $B9 $43 $FB $42 $51 $44 $A6 $40

; 5th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26E9 to 26EF (7 bytes)
_DATA_26E9_:
.db $02 $00 $08 $00 $AC $00 $AD

; 6th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26F0 to 26F6 (7 bytes)
_DATA_26F0_:
.db $02 $00 $08 $00 $AE $00 $AF

; 7th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26F7 to 26FD (7 bytes)
_DATA_26F7_:
.db $02 $00 $08 $00 $B0 $00 $B1

; 8th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 26FE to 2704 (7 bytes)
_DATA_26FE_:
.db $02 $00 $08 $00 $B2 $00 $B3

; 9th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2705 to 270B (7 bytes)
_DATA_2705_:
.db $02 $00 $08 $00 $B4 $00 $B5

; 10th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 270C to 2712 (7 bytes)
_DATA_270C_:
.db $02 $00 $08 $00 $B6 $00 $B7

; 11th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2713 to 2719 (7 bytes)
_DATA_2713_:
.db $02 $00 $08 $00 $B8 $00 $B9

; 12th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 271A to 2720 (7 bytes)
_DATA_271A_:
.db $02 $00 $08 $00 $BA $00 $BB

; 13th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2721 to 2727 (7 bytes)
_DATA_2721_:
.db $02 $00 $08 $00 $BC $00 $BD

; 14th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2728 to 272E (7 bytes)
_DATA_2728_:
.db $02 $00 $08 $00 $BE $00 $BF

; 15th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 272F to 2732 (4 bytes)
_DATA_272F_:
.db $01 $00 $00 $0E

; 16th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2733 to 2736 (4 bytes)
_DATA_2733_:
.db $01 $00 $00 $14

; 17th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2737 to 2743 (13 bytes)
_DATA_2737_:
.db $04 $FC $FC $04 $04 $FC $15 $04 $16 $FC $17 $04 $18

; 18th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2744 to 2750 (13 bytes)
_DATA_2744_:
.db $04 $FC $FC $04 $04 $FC $19 $04 $1A $FC $1B $04 $1C

; 19th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2751 to 2754 (4 bytes)
_DATA_2751_:
.db $01 $FF $00 $BE

; 20th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2755 to 276D (25 bytes)
_DATA_2755_:
.db $08 $00 $00 $00 $00 $08 $08 $08 $08 $00 $20 $08 $21 $10 $22 $18
.db $23 $00 $24 $08 $25 $10 $26 $18 $27

; 21st entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 276E to 2786 (25 bytes)
_DATA_276E_:
.db $08 $00 $00 $00 $00 $08 $08 $08 $08 $00 $28 $08 $29 $10 $2A $18
.db $2B $00 $2C $08 $2D $10 $2E $18 $2F

; 22nd entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2787 to 27C3 (61 bytes)
_DATA_2787_:
.db $14 $00 $00 $00 $00 $00 $08 $08 $08 $08 $08 $10 $10 $10 $10 $10
.db $18 $18 $18 $18 $18 $00 $20 $08 $21 $10 $22 $18 $23 $20 $24 $00
.db $25 $08 $26 $10 $27 $18 $28 $20 $29 $00 $2A $08 $2B $10 $2C $18
.db $2D $20 $2E $00 $2F $08 $30 $10 $31 $18 $32 $20 $33

; 23rd entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 27C4 to 27FA (55 bytes)
_DATA_27C4_:
.db $12 $00 $00 $00 $00 $00 $00 $08 $08 $08 $08 $08 $08 $10 $10 $10
.db $10 $10 $10 $00 $20 $08 $21 $10 $22 $18 $23 $20 $24 $28 $25 $00
.db $26 $08 $27 $10 $28 $18 $29 $20 $2A $28 $2B $00 $2C $08 $2D $10
.db $2E $18 $2F $20 $30 $28 $31

; 24th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 27FB to 2867 (109 bytes)
_DATA_27FB_:
.db $24 $00 $00 $00 $00 $00 $00 $08 $08 $08 $08 $08 $08 $10 $10 $10
.db $10 $10 $10 $18 $18 $18 $18 $18 $18 $20 $20 $20 $20 $20 $20 $28
.db $28 $28 $28 $28 $28 $00 $20 $08 $20 $10 $20 $18 $20 $20 $20 $28
.db $21 $00 $20 $08 $22 $10 $23 $18 $24 $20 $25 $28 $26 $00 $20 $08
.db $27 $10 $28 $18 $29 $20 $2A $28 $26 $00 $2C $08 $2D $10 $2E $18
.db $2F $20 $30 $28 $31 $00 $32 $08 $33 $10 $34 $18 $35 $20 $36 $28
.db $37 $00 $26 $08 $26 $10 $26 $18 $26 $20 $26 $28 $26

; 25th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2868 to 2880 (25 bytes)
_DATA_2868_:
.db $08 $00 $00 $00 $00 $08 $08 $08 $08 $00 $30 $08 $31 $10 $32 $18
.db $33 $00 $34 $08 $35 $10 $36 $18 $37

; 26th entry of Pointer Table from 265A (indexed by _RAM_C103_)
; Data from 2881 to 2884 (4 bytes)
_DATA_2881_:
.db $01 $00 $00 $00

_LABEL_2885_:
    ld c, a
    rst $08 ; setVdpAddress
    ld a, c
    add a, a
    ld e, a
    ld d, $00
    ld hl, _DATA_28BF_
    add hl, de
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ld b, (hl)
    inc hl
    ld c, Port_VDPData
-:
    outi
    jr nz, -
    ret

_LABEL_289D_:
    ld c, a
    ex af, af'
    ld b, $00
    ld hl, $28BF
    add hl, bc
    add hl, bc
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ld c, (hl)
    ld b, $00
    inc hl
    push hl
    ld hl, $126B
    xor a
    sbc hl, bc
    sbc hl, bc
    push hl
    pop iy
    pop hl
    ld c, $BE
    jp (iy)

; Pointer Table from 28BF to 28E6 (20 entries, indexed by _RAM_C6A8_)
_DATA_28BF_:
.dw _DATA_28E7_ _DATA_290C_ _DATA_291D_ _DATA_292A_ _DATA_293B_ _DATA_2952_ _DATA_2969_ _DATA_297A_
.dw _DATA_2985_ _DATA_2994_ _DATA_29A9_ _DATA_29B6_ _DATA_29C1_ _DATA_29CA_ _DATA_29D7_ _DATA_29E4_
.dw _DATA_29F1_ _DATA_2A00_ _DATA_2A1B_ _DATA_2A24_

; 1st entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 28E7 to 290B (37 bytes)
_DATA_28E7_:
.db $24 $EF $00 $F1 $00 $E4 $00 $F2 $00 $F2 $00 $00 $00 $F2 $00 $F3
.db $00 $E0 $00 $F1 $00 $F3 $00 $00 $00 $E1 $00 $F4 $00 $F3 $00 $F3
.db $00 $EE $00 $ED $00

; 2nd entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 290C to 291C (17 bytes)
_DATA_290C_:
.db $10 $D7 $00 $00 $00 $EF $00 $EB $00 $E0 $00 $F8 $00 $E4 $00 $F1
.db $00

; 3rd entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 291D to 2929 (13 bytes)
_DATA_291D_:
.db $0C $F5 $00 $E4 $00 $F1 $00 $F2 $00 $F4 $00 $F2 $00

; 4th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 292A to 293A (17 bytes)
_DATA_292A_:
.db $10 $D8 $00 $00 $00 $EF $00 $EB $00 $E0 $00 $F8 $00 $E4 $00 $F1
.db $00

; 5th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 293B to 2951 (23 bytes)
_DATA_293B_:
.db $16 $FE $00 $00 $00 $F2 $00 $E4 $00 $E6 $00 $E0 $00 $00 $00 $D7
.db $00 $DF $00 $DF $00 $D6 $00

; 6th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2952 to 2968 (23 bytes)
_DATA_2952_:
.db $16 $F2 $00 $E4 $00 $EB $00 $E4 $00 $E2 $00 $F3 $00 $00 $00 $E6
.db $00 $E0 $00 $EC $00 $E4 $00

; 7th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2969 to 2979 (17 bytes)
_DATA_2969_:
.db $10 $EE $00 $F1 $00 $E8 $00 $E6 $00 $E8 $00 $ED $00 $E0 $00 $EB
.db $00

; 8th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 297A to 2984 (11 bytes)
_DATA_297A_:
.db $0A $E5 $00 $EB $00 $E0 $00 $F2 $00 $E7 $00

; 9th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2985 to 2993 (15 bytes)
_DATA_2985_:
.db $0E $EE $00 $EF $00 $F3 $00 $E8 $00 $EE $00 $ED $00 $F2 $00

; 10th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2994 to 29A8 (21 bytes)
_DATA_2994_:
.db $14 $E3 $00 $E8 $00 $E5 $00 $E5 $00 $E8 $00 $E2 $00 $F4 $00 $EB
.db $00 $F3 $00 $F8 $00

; 11th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29A9 to 29B5 (13 bytes)
_DATA_29A9_:
.db $0C $E1 $00 $EB $00 $EE $00 $E2 $00 $EA $00 $F2 $00

; 12th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29B6 to 29C0 (11 bytes)
_DATA_29B6_:
.db $0A $EB $00 $E4 $00 $F5 $00 $E4 $00 $EB $00

; 13th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29C1 to 29C9 (9 bytes)
_DATA_29C1_:
.db $08 $E4 $00 $F7 $00 $E8 $00 $F3 $00

; 14th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29CA to 29D6 (13 bytes)
_DATA_29CA_:
.db $0C $00 $00 $E4 $00 $E0 $00 $F2 $00 $F8 $00 $00 $00

; 15th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29D7 to 29E3 (13 bytes)
_DATA_29D7_:
.db $0C $ED $00 $EE $00 $F1 $00 $EC $00 $E0 $00 $EB $00

; 16th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29E4 to 29F0 (13 bytes)
_DATA_29E4_:
.db $0C $00 $00 $E7 $00 $E0 $00 $F1 $00 $E3 $00 $00 $00

; 17th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 29F1 to 29FF (15 bytes)
_DATA_29F1_:
.db $0E $EC $00 $E0 $00 $F3 $00 $E2 $00 $E7 $00 $E4 $00 $F2 $00

; 18th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2A00 to 2A1A (27 bytes)
_DATA_2A00_:
.db $1A $EC $00 $E8 $00 $ED $00 $F4 $00 $F3 $00 $E4 $00 $F2 $00 $00
.db $00 $EC $00 $E0 $00 $F3 $00 $E2 $00 $E7 $00

; 19th entry of Pointer Table from 28BF (indexed by _RAM_C6A8_)
; Data from 2A1B to 2A23 (9 bytes)
_DATA_2A1B_:
.db $08 $E7 $00 $E8 $00 $E6 $00 $E7 $00

; Data from 2A24 to 2A26 (3 bytes)
_DATA_2A24_:
.db $02 $00 $00

_LABEL_2A27_:
    xor a
    ld (_RAM_C6BB_), a
_LABEL_2A2B_:
    ld a, (hl)
    cp $FF
    ret z
    push bc
    push hl
--:
    ld (_RAM_C6B9_), hl
    ld b, $00
    ld a, (hl)
    cp $FF
    jr z, ++
    or a
    jr nz, _LABEL_2A41_
    add hl, de
    jr --

_LABEL_2A41_:
    add hl, de
    cp (hl)
    jr nz, +
    inc b
    jr _LABEL_2A41_

+:
    ld c, a
    ld a, b
    cp $02
    jr c, --
    dec b
    ld a, (_RAM_C6BB_)
    add a, b
    ld (_RAM_C6BB_), a
    inc b
    inc b
    push hl
    push de
    ld hl, (_RAM_C6B9_)
    ld de, $0098
    add hl, de
    pop de
-:
    ld (hl), c
    add hl, de
    djnz -
    pop hl
    jr --

++:
    pop hl
    pop bc
    add hl, bc
    jr _LABEL_2A2B_

_LABEL_2A6E_:
    ld l, (ix+16)
    ld h, (ix+17)
    ld de, _DATA_18_
    add hl, de
    ld a, (hl)
    or a
    ret z
    cp $FF
    ret z
    ld hl, $C418
    ld de, _RAM_C4B0_
    ld b, $90
    ld c, $00
-:
    cp (hl)
    jr nz, +
    ld (de), a
+:
    inc hl
    inc de
    djnz -
    ret

_LABEL_2A91_:
    ld a, (_RAM_C699_)
    bit 0, (ix+31)
    jr z, +
    ld a, (_RAM_C6A5_)
+:
    inc a
    ld d, a
    add a, a
    add a, d
    ld e, a
    ld d, $00
    ld hl, $0000
    ld b, $07
    ld a, (ix+28)
-:
    rrca
    jr nc, +
    add hl, de
+:
    ex de, hl
    add hl, hl
    ex de, hl
    djnz -
    ex de, hl
    ld hl, $0000
    ld a, (ix+27)
    inc a
    ld b, a
-:
    add hl, de
    djnz -
    ret

_LABEL_2AC2_:
    ld c, $10
    jr +

_LABEL_2AC6_:
    ld c, $11
    jr +

_LABEL_2ACA_:
    ld c, $12
    jr +

_LABEL_2ACE_:
    ld c, $13
    jr +

_LABEL_2AD2_:
    ld c, $00
+:
    ld hl, $0098
    add hl, de
    ex de, hl
    ld b, $90
-:
    ld a, (de)
    or a
    jr z, +
    ld (hl), c
+:
    inc hl
    inc de
    djnz -
    ret

_LABEL_2AE5_:
    ld hl, $0098
    add hl, de
    ex de, hl
    ld b, $90
-:
    ld a, (de)
    or a
    jr z, +
    ld (hl), a
+:
    inc hl
    inc de
    djnz -
    ret

_LABEL_2AF6_:
    ld b, $06
--:
    push bc
    push hl
    ld b, $11
    ld de, $FFF8
-:
    xor a
    cp (hl)
    jr z, +
    add hl, de
    djnz -
    jr +++

+:
    ex de, hl
    ld hl, $FFF8
    add hl, de
-:
    ld a, (hl)
    or a
    jr z, ++
    ld (de), a
    ld (hl), $00
    ld a, e
    sub $08
    jr nc, +
    dec d
+:
    ld e, a
++:
    ld a, l
    sub $08
    jr nc, +
    dec h
+:
    ld l, a
    djnz -
+++:
    pop hl
    inc hl
    pop bc
    djnz --
    ret

_LABEL_2B2A_:
    ld hl, _RAM_C4B0_
    jr +

; Data from 2B2F to 2B31 (3 bytes)
.db $21 $F8 $C5

+:
    ld bc, $9000
-:
    ld a, (hl)
    or a
    jr z, +
    inc c
+:
    inc hl
    djnz -
    ret

_LABEL_2B3E_:
    ex af, af'
    ld a, (_RAM_C005_)
    bit 1, a
    ret z
    ex af, af'
    ld e, a
    ld d, $00
    sub $03
    jr c, +
    and $07
    inc a
    ld d, a
+:
    ld a, e
    ld e, $00
    sub $02
    jr c, +
    and $07
    inc a
    ld e, a
+:
    bit 0, (ix+31)
    jr nz, ++
    ld a, (_RAM_C69A_)
    sub e
    jr nc, +
    xor a
+:
    ld (_RAM_C69A_), a
    ld a, (_RAM_C6A7_)
    add a, e
    ld (_RAM_C6A7_), a
    ret

++:
    ld a, (_RAM_C6A6_)
    sub e
    jr nc, +
    xor a
+:
    ld (_RAM_C6A6_), a
    ld a, (_RAM_C69B_)
    add a, e
    ld (_RAM_C69B_), a
    ret

_LABEL_2B86_:
    ld a, (var.pallete.shouldUpdate)
    and $FE
    jr z, +
    rrca
    rrca
    call c, ++
    rrca
    call c, _LABEL_2BDE_
    rrca
    call c, _LABEL_2BFC_
    rrca
    call c, _LABEL_2C21_
    rrca
    call c, _LABEL_2C15_
    rrca
    call c, _LABEL_2C1B_
    rrca
+:
    ld a, (var.palette._RAM_C022_)
    or a
    ret z
    rrca
    call c, _LABEL_2C9C_
    rrca
    call c, _LABEL_2CB5_
    rrca
    rrca
    rrca
    call c, _LABEL_2CCE_
    rrca
    call c, _LABEL_2CE8_
    rrca
    call c, _LABEL_2D06_
    rrca
    ret

++:
    ex af, af'
    ld a, $52
    out (Port_VDPAddress), a
    ld a, $7D
    out (Port_VDPAddress), a
    ld a, (_RAM_C699_)
    add a, $D6
    jr +

+:
    out (Port_VDPData), a
    ld a, $10
    jr +

+:
    out (Port_VDPData), a
    ex af, af'
    ret

_LABEL_2BDE_:
    push af
    xor a
    ld hl, (_RAM_C697_)
    ld de, $3C88
    call _LABEL_2D1F_
    ld de, $3C92
    rst $08 ; setVdpAddress
    ld a, (_RAM_C6B9_)
    add a, $D6
    out (Port_VDPData), a
    ld a, $10
    jr +

+:
    out (Port_VDPData), a
    pop af
    ret

_LABEL_2BFC_:
    push af
    ld a, (_RAM_C696_)
    ld hl, (_RAM_C694_)
    ld de, $3B46
    call _LABEL_2D1F_
    ld a, $D6
    out (Port_VDPData), a
    ld a, $00
    jr +

+:
    out (Port_VDPData), a
    pop af
    ret

_LABEL_2C15_:
    ld de, $3BD6
    jp +

_LABEL_2C1B_:
    ld de, $3B2A
    jp +

_LABEL_2C21_:
    ld de, $3B02
+:
    push af
    ld a, $10
    ex af, af'
    rst $08 ; setVdpAddress
    exx
    ld a, (_RAM_C6C5_)
    and $F0
    rrca
    rrca
    add a, $AE
    call _LABEL_2C85_
    ld b, a
    ld a, (_RAM_C6C5_)
    and $0F
    add a, a
    add a, a
    add a, $AE
    call _LABEL_2C85_
    ld c, a
    in a, (Port_VDPData)
    in a, (Port_VDPData)
    ld a, (_RAM_C6C4_)
    and $F0
    rrca
    rrca
    add a, $AE
    call _LABEL_2C85_
    ld d, a
    ld a, (_RAM_C6C4_)
    and $0F
    add a, a
    add a, a
    add a, $AE
    call _LABEL_2C85_
    ld e, a
    exx
    ld hl, $0040
    add hl, de
    ex de, hl
    rst $08 ; setVdpAddress
    exx
    ld a, b
    call _LABEL_2C85_
    ld a, c
    call _LABEL_2C85_
    jr +

+:
    in a, (Port_VDPData)
    nop
    jr +

+:
    in a, (Port_VDPData)
    ld a, d
    call _LABEL_2C85_
    ld a, e
    call _LABEL_2C85_
    pop af
    ret

_LABEL_2C85_:
    out (Port_VDPData), a
    inc a
    ex af, af'
    jr +

+:
    out (Port_VDPData), a
    ex af, af'
    jr +

+:
    jr +

+:
    out (Port_VDPData), a
    inc a
    ex af, af'
    jr +

+:
    out (Port_VDPData), a
    ex af, af'
    ret

_LABEL_2C9C_:
    ex af, af'
    ld a, $16
    out (Port_VDPAddress), a
    ld a, $7D
    out (Port_VDPAddress), a
    jr +

+:
    ld a, (_RAM_C699_)
    add a, $D6
    out (Port_VDPData), a
    jr +

+:
    xor a
    out (Port_VDPData), a
    ex af, af'
    ret

_LABEL_2CB5_:
    ex af, af'
    ld a, $28
    out (Port_VDPAddress), a
    ld a, $7D
    out (Port_VDPAddress), a
    jr +

+:
    ld a, (_RAM_C6A5_)
    add a, $D6
    out (Port_VDPData), a
    jr +

+:
    xor a
    out (Port_VDPData), a
    ex af, af'
    ret

_LABEL_2CCE_:
    ex af, af'
    ld a, $6C
    out (Port_VDPAddress), a
    ld a, $7D
    out (Port_VDPAddress), a
    ld a, (_RAM_C6A5_)
    add a, $D6
    jr +

+:
    out (Port_VDPData), a
    ld a, $10
    jr +

+:
    out (Port_VDPData), a
    ex af, af'
    ret

_LABEL_2CE8_:
    push af
    xor a
    ld hl, (_RAM_C6A3_)
    ld de, $3CB0
    call _LABEL_2D1F_
    ld de, $3CBA
    rst $08 ; setVdpAddress
    ld a, (_RAM_C6B9_)
    add a, $D6
    out (Port_VDPData), a
    ld a, $10
    jr +

+:
    out (Port_VDPData), a
    pop af
    ret

_LABEL_2D06_:
    push af
    ld a, (_RAM_C6A2_)
    ld hl, (_RAM_C6A0_)
    ld de, $3B6E
    call _LABEL_2D1F_
    ld a, $D6
    out (Port_VDPData), a
    ld a, $00
    jr +

+:
    out (Port_VDPData), a
    pop af
    ret

_LABEL_2D1F_:
    push af
    rst $08 ; setVdpAddress
    pop af
    call _LABEL_2D4D_
    ld hl, _RAM_C6B4_
    ld b, $06
    ld a, $10
    ex af, af'
    xor a
--:
    or (hl)
    jr z, ++
    ld a, $D6
    add a, (hl)
    out (Port_VDPData), a
    ex af, af'
    nop
    jp +

+:
    out (Port_VDPData), a
    ex af, af'
-:
    inc hl
    djnz --
    ret

++:
    in a, (Port_VDPData)
    nop
    jp +

+:
    in a, (Port_VDPData)
    xor a
    jr -

_LABEL_2D4D_:
    ld b, $04
-:
    add hl, hl
    rla
    djnz -
    push hl
    ld l, h
    ld h, a
    ld de, $186A
    call _LABEL_2DA5_
    cp $0A
    jr nc, +
    ld (_RAM_C6B4_), a
    ld de, $0271
    call _LABEL_2DA5_
    ld (_RAM_C6B5_), a
    add hl, hl
    add hl, hl
    add hl, hl
    add hl, hl
    pop de
    ld a, e
    rrca
    rrca
    rrca
    rrca
    or l
    ld l, a
    ld de, $03E8
    call _LABEL_2DA5_
    ld (_RAM_C6B6_), a
    ld de, $0064
    call _LABEL_2DA5_
    ld (_RAM_C6B7_), a
    ld de, $000A
    call _LABEL_2DA5_
    ld (_RAM_C6B8_), a
    ld a, l
    ld (_RAM_C6B9_), a
    ret

+:
    ld hl, $0909
    ld (_RAM_C6B4_), hl
    ld (_RAM_C6B6_), hl
    ld (_RAM_C6B8_), hl
    ret

_LABEL_2DA5_:
    xor a
-:
    or a
    sbc hl, de
    jr c, +
    inc a
    jr -

+:
    add hl, de
    ret

_LABEL_2DB0_:
    ld a, (_RAM_C6C2_)
    or a
    ret z
    ld a, (_RAM_C002_)
    or a
    ret nz
    ld a, (_RAM_C005_)
    bit 1, a
    jr nz, ++
    ld hl, _RAM_C6C3_
-:
    ld a, (hl)
    inc a
    daa
    ld (hl), a
    cp $60
    jp c, +
    ld (hl), $00
    inc hl
    jr -

+:
    ld a, (_RAM_C005_)
    bit 2, a
    jr nz, +
    ld hl, var.pallete.shouldUpdate
    set 4, (hl)
    ret

+:
    ld hl, var.pallete.shouldUpdate
    set 4, (hl)
    set 6, (hl)
    ret

++:
    ld hl, _RAM_C6C3_
-:
    ld a, (hl)
    dec a
    daa
    ld (hl), a
    cp $60
    jp c, +
    ld (hl), $59
    inc hl
    jr -

+:
    ld hl, var.pallete.shouldUpdate
    set 5, (hl)
    ret

_LABEL_2DFE_:
    ld a, (_RAM_C0A8_)
    ld hl, _DATA_2E07_
    jp jumpToAthPointer

; Jump Table from 2E07 to 2E0E (4 entries, indexed by _RAM_C0A8_)
_DATA_2E07_:
.dw _LABEL_2E0F_ _LABEL_2E10_ _LABEL_2E10_ _LABEL_2E0F_

; 1st entry of Jump Table from 2E07 (indexed by _RAM_C0A8_)
_LABEL_2E0F_:
    ret

; 2nd entry of Jump Table from 2E07 (indexed by _RAM_C0A8_)
_LABEL_2E10_:
    ld a, (var.fade.state)
    or a
    ret nz
    ld a, (_RAM_C0AA_)
    or a
    jr z, +
    dec a
    ld (_RAM_C0AA_), a
    ret

+:
    ld a, (_RAM_C699_)
    ld d, a
    ld a, (_RAM_C6A5_)
    cp d
    jr nc, +
    ld a, d
+:
    neg
    add a, $09
    ld (_RAM_C0AA_), a
    ld hl, _RAM_C0A9_
    inc (hl)
    ld a, (hl)
    and $03
    ld e, a
    ld d, $00
    ld hl, _DATA_2E4C_
    add hl, de
    ld de, _RAM_C02F_
    call ldi4
    ld hl, var.pallete.shouldUpdate
    set 0, (hl)
    ret

; Data from 2E4C to 2E53 (8 bytes)
_DATA_2E4C_:
.db $3F $3F $00 $00 $3F $3F $00 $00

_LABEL_2E54_:
    ld a, (_RAM_C6BD_)
    or a
    jr z, +
    dec a
    ld (_RAM_C6BD_), a
    ret

+:
    ld a, $08
    ld (_RAM_C6BD_), a
    ld de, $21A0
    rst $08 ; setVdpAddress
    ld a, (_RAM_C6BC_)
    ld l, $00
    or a
    rra
    rr l
    rra
    rr l
    rra
    rr l
    ld h, a
    ld de, _DATA_2E90_
    add hl, de
    ld bc,  $2000 | Port_VDPData
-:
    outi
    jr nz, -
    ld a, (_RAM_C6BC_)
    inc a
    cp $03
    jr c, +
    xor a
+:
    ld (_RAM_C6BC_), a
    ret

; Data from 2E90 to 2EEF (96 bytes)
_DATA_2E90_:
.db $7E $7E $7E $7E $00 $81 $81 $81 $18 $81 $99 $81 $24 $99 $BD $81
.db $24 $99 $BD $81 $18 $81 $99 $81 $00 $81 $81 $81 $7E $00 $7E $7E
.db $7E $7E $7E $7E $18 $81 $99 $81 $24 $99 $BD $81 $5A $BD $FF $99
.db $5A $BD $FF $99 $24 $99 $BD $81 $18 $81 $99 $81 $7E $00 $7E $7E
.db $7E $7E $7E $7E $66 $99 $FF $81 $5A $BD $FF $99 $3C $FF $FF $BD
.db $3C $FF $FF $BD $5A $BD $FF $99 $66 $99 $FF $81 $7F $00 $7F $7F

_LABEL_2EF0_:
    ld a, (_RAM_C6CA_)
    or a
    jr z, +
    dec a
    ld (_RAM_C6CA_), a
    ret

+:
    ld a, $08
    ld (_RAM_C6CA_), a
    ld a, (_RAM_C6CB_)
    add a, a
    ld e, a
    ld d, $00
    ld hl, _DATA_2F24_
    add hl, de
    ld a, (hl)
    inc hl
    ld h, (hl)
    ld l, a
    ld a, (hl)
    inc hl
    ld de, $21C0
    call _LABEL_746_
    ld a, (_RAM_C6CB_)
    inc a
    cp $18
    jr c, +
    xor a
+:
    ld (_RAM_C6CB_), a
    ret

; Pointer Table from 2F24 to 2F53 (24 entries, indexed by _RAM_C6CB_)
_DATA_2F24_:
.dw _DATA_2F54_ _DATA_2F5F_ _DATA_2F6A_ _DATA_2F75_ _DATA_2F80_ _DATA_2F75_ _DATA_2F6A_ _DATA_2F5F_
.dw _DATA_2F54_ _DATA_2F8B_ _DATA_2F96_ _DATA_2FA1_ _DATA_2FAC_ _DATA_2FA1_ _DATA_2F96_ _DATA_2F8B_
.dw _DATA_2F54_ _DATA_2F54_ _DATA_2F54_ _DATA_2F54_ _DATA_2F54_ _DATA_2F54_ _DATA_2F54_ _DATA_2F54_

; 1st entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F54 to 2F5E (11 bytes)
_DATA_2F54_:
.db $07 $08
.dsb 9, $00

; 2nd entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F5F to 2F69 (11 bytes)
_DATA_2F5F_:
.db $07 $08 $00 $18 $18 $18 $18 $18 $18 $18 $18

; 3rd entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F6A to 2F74 (11 bytes)
_DATA_2F6A_:
.db $07 $08 $00 $3C $3C $3C $3C $3C $3C $3C $3C

; 4th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F75 to 2F7F (11 bytes)
_DATA_2F75_:
.db $07 $08 $00 $7E $7E $7E $7E $7E $7E $7E $7E

; 5th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F80 to 2F8A (11 bytes)
_DATA_2F80_:
.db $07 $08 $00 $FF $FF $FF $FF $FF $FF $FF $FF

; 10th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F8B to 2F95 (11 bytes)
_DATA_2F8B_:
.db $0F $08 $00 $18 $18 $18 $18 $18 $18 $18 $18

; 11th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2F96 to 2FA0 (11 bytes)
_DATA_2F96_:
.db $0F $08 $00 $3C $3C $3C $3C $3C $3C $3C $3C

; 12th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2FA1 to 2FAB (11 bytes)
_DATA_2FA1_:
.db $0F $08 $00 $7E $7E $7E $7E $7E $7E $7E $7E

; 13th entry of Pointer Table from 2F24 (indexed by _RAM_C6CB_)
; Data from 2FAC to 2FB6 (11 bytes)
_DATA_2FAC_:
.db $0F $08 $00 $FF $FF $FF $FF $FF $FF $FF $FF

_LABEL_2FB7_:
    push ix
    pop hl
    ld e, l
    ld d, h
    inc de
    ld (hl), $00
    jp ldi31

.INCLUDE "entities.asm"
.INCLUDE "audio.asm"

.BANK 1 SLOT 1
.ORG $0000

; Data from 7FF0 to 7FFF (16 bytes)
.db $54 $4D $52 $20 $53 $45 $47 $41 $54 $59 $6F $12 $20 $51 $00 $4F

.BANK 2
.ORG $0000

; Data from 8000 to 8011 (18 bytes)
_DATA_8000_:
.db $10 $00 $00 $02 $1C $F8 $1C $02 $00 $00 $00 $40 $38 $1F $38 $40
.db $00 $00

; Data from 8012 to 8033 (34 bytes)
palette_DATA_8012_:
.db $00 $20
.dsb 15, $00
.db $3F
.dsb 15, $00
.db $3F

; Data from 8034 to 831F (748 bytes)
_DATA_8034_:
.db $48 $01 $7E $66 $6E $76 $66 $66 $7E $00 $38 $38 $18 $18 $18 $18
.db $3C $00 $7E $66 $66 $0C $18 $30 $7E $00 $7E $66 $06 $1C $06 $66
.db $7E $00 $6C $6C $6C $6C $7E $0C $0C $00 $7E $60 $60 $7E $06 $66
.db $7E $00 $7E $66 $60 $7E $66 $66 $7E $00 $7E $66 $66 $0C $18 $18
.db $18 $00 $7E $66 $66 $3C $66 $66 $7E $00 $7E $66 $66 $7E $06 $66
.db $7E $00 $18 $3C $24 $24 $7E $66 $66 $00 $7C $66 $66 $7C $66 $66
.db $7C $00 $3C $66 $66 $60 $66 $66 $3C $00 $7C $66 $66 $66 $66 $66
.db $7C $00 $7E $60 $60 $7C $60 $60 $7E $00 $7E $60 $60 $7C $60 $60
.db $60 $00 $3C $66 $60 $6E $66 $66 $3C $00 $66 $66 $66 $7E $66 $66
.db $66 $00 $3C $18 $18 $18 $18 $18 $3C $00 $7C $18 $18 $18 $58 $78
.db $30 $00 $62 $66 $6C $78 $6C $66 $62 $00 $60 $60 $60 $60 $60 $60
.db $7E $00 $82 $C6 $EE $FE $D6 $C6 $C6 $00 $46 $66 $76 $7E $6E $66
.db $62 $00 $3C $66 $66 $66 $66 $66 $3C $00 $7C $66 $66 $66 $7C $60
.db $60 $00 $3C $66 $66 $76 $6E $66 $3E $00 $7C $66 $66 $7C $6C $66
.db $66 $00 $3C $66 $60 $3C $06 $66 $3C $00 $7E $18 $18 $18 $18 $18
.db $18 $00 $66 $66 $66 $66 $66 $66 $3C $00 $66 $66 $66 $66 $24 $3C
.db $18 $00 $D6 $D6 $D6 $D6 $D6 $FE $6C $00 $42 $66 $3C $18 $3C $66
.db $42 $00 $66 $66 $66 $3C $18 $18 $18 $00 $7E $06 $3C $18 $3C $60
.db $7E $00 $00 $18 $18 $00 $18 $18 $00 $00 $18 $18 $18 $18 $00 $18
.db $18 $00 $3C $66 $66 $0C $18 $00 $18 $00 $00 $00 $00 $00 $00 $18
.db $18 $00 $3C $42 $5A $52 $5A $42 $3C $00 $A0 $01 $00 $38 $7C $EE
.db $C6 $C6 $C6 $C6 $C6 $FE $FE $C6 $C6 $C6 $C6 $00 $00 $FC $FE $C6
.db $C6 $C6 $C6 $FC $FC $C6 $C6 $C6 $C6 $FE $FC $00 $00 $7C $FE $C6
.db $C6 $C6 $C0 $C0 $C0 $C0 $C6 $C6 $C6 $FE $7C $00 $00 $FC $FE
.dsb 10, $C6
.db $FE $FC $00 $00 $FE $FE $C0 $C0 $C0 $C0 $FC $FC $C0 $C0 $C0 $C0
.db $FE $FE $00 $00 $FE $FE $C0 $C0 $C0 $C0 $FC $FC $C0 $C0 $C0 $C0
.db $C0 $C0 $00 $00 $7C $FE $C6 $C6 $C6 $C0 $C0 $DE $DE $C6 $C6 $C6
.db $FE $7C $00 $00 $C6 $C6 $C6 $C6 $C6 $C6 $FE $FE $C6 $C6 $C6 $C6
.db $C6 $C6 $00 $00 $FE $FE
.dsb 10, $38
.db $FE $FE $00 $00 $7E $7E $18 $18 $18 $18 $18 $18 $18 $18 $D8 $D8
.db $F8 $70 $00 $00 $C6 $C6 $CC $CC $D8 $D8 $F0 $F0 $D8 $D8 $CC $CC
.db $C6 $C6 $00 $00
.dsb 12, $C0
.db $FE $FE $00 $00 $C6 $C6 $EE $EE $FE $FE $D6 $D6 $C6 $C6 $C6 $C6
.db $C6 $C6 $00 $00 $C6 $C6 $C6 $E6 $E6 $F6 $F6 $DE $DE $CE $CE $C6
.db $C6 $C6 $00 $00 $7C $FE
.dsb 10, $C6
.db $FE $7C $00 $00 $FC $FE $C6 $C6 $C6 $C6 $FE $FC $C0 $C0 $C0 $C0
.db $C0 $C0 $00 $00 $7C $FE $C6 $C6 $C6 $C6 $C6 $C6 $C6 $C6 $DE $DE
.db $FE $7C $00 $00 $FC $FE $C6 $C6 $C6 $C6 $FE $FC $D8 $DC $CC $CE
.db $C6 $C6 $00 $00 $7C $FE $C6 $C6 $C0 $E0 $70 $1C $0E $06 $C6 $C6
.db $FE $7C $00 $00 $FE $FE
.dsb 12, $38
.db $00 $00
.dsb 12, $C6
.db $FE $7C $00 $00 $C6 $C6 $C6 $C6 $C6 $EE $6C $6C $6C $6C $7C $38
.db $38 $38 $00 $00 $C6 $C6 $D6 $D6 $D6 $D6 $D6 $D6 $D6 $D6 $FE $7C
.db $6C $6C $00 $00 $C6 $C6 $C6 $6C $6C $6C $38 $38 $6C $6C $6C $C6
.db $C6 $C6 $00 $00 $C6 $C6 $C6 $EE $6C $6C $7C $38 $38 $38 $38 $38
.db $38 $38 $00 $00 $FE $FE $06 $0C $0C $18 $18 $30 $30 $60 $60 $C0
.db $FE $FE $00

; Data from 8320 to 83C1 (162 bytes)
_DATA_8320_:
.db $A0 $00 $00 $7C $FE $C6 $C6 $C6 $E6 $F6 $DE $CE $C6 $C6 $C6 $FE
.db $7C $00 $00 $18 $38 $38
.dsb 9, $18
.db $7E $7E $00 $00 $7C $FE $C6 $C6 $C6 $0E $1C $38 $70 $E0 $C0 $C0
.db $FE $FE $00 $00 $7C $FE $C6 $C6 $C6 $06 $3C $3C $06 $C6 $C6 $C6
.db $FE $7C $00 $00 $0C $0C $1C $1C $3C $2C $6C $4C $CC $FE $FE $0C
.db $0C $0C $00 $00 $FE $FE $C0 $C0 $C0 $C0 $FC $FE $06 $06 $C6 $C6
.db $FE $7C $00 $00 $7C $FE $C6 $C6 $C0 $C0 $FC $FE $C6 $C6 $C6 $C6
.db $FE $7C $00 $00 $FE $FE $C6 $C6 $06 $0E $0C $1C $18 $18 $18 $18
.db $18 $18 $00 $00 $7C $FE $C6 $C6 $C6 $C6 $7C $FE $C6 $C6 $C6 $C6
.db $FE $7C $00 $00 $7C $FE $C6 $C6 $C6 $C6 $FE $7E $06 $06 $C6 $C6
.db $FE $7C $00

; Data from 83C2 to 86A0 (735 bytes)
_DATA_83C2_:
.db $40 $01 $00 $0F $1F $3F $78 $78 $78 $78 $00 $FE $FE $FE $1E $1E
.db $1E $1E $78 $78 $78 $7F $7F $7F $00 $00 $1E $1E $1E $FC $F8 $F0
.db $00 $00 $00 $03 $07 $07 $07 $03 $03 $03 $00 $C0 $C0 $C0 $C0 $C0
.db $C0 $C0 $03 $03 $03 $03 $03 $03 $00 $00 $C0 $C0 $C0 $C0 $C0 $C0
.db $00 $00 $00 $7F $7F $7F $00 $00 $03 $07 $00 $F8 $FC $FE $1E $1E
.db $FE $FC $0F $1E $3C $7F $7F $7F $00 $00 $F8 $00 $00 $FE $FE $FE
.db $00 $00 $00 $7F $7F $7F $00 $00 $03 $07 $00 $FE $FC $F8 $00 $00
.db $F8 $FC $0F $00 $00 $7F $7F $7F $00 $00 $FE $06 $06 $FE $FC $F8
.db $00 $00 $00 $01 $03 $07 $0F $1F $3F $7F $00 $FE $FC $F8 $F0 $E0
.db $FE $FE $7F $00 $01 $01 $01 $01 $00 $00 $FE $00 $FC $FC $FC $FC
.db $00 $00 $00 $7F $7F $7F $78 $78 $7F $7F $00 $FE $FE $FE $00 $00
.db $F8 $FC $7F $00 $00 $7F $7F $7F $00 $00 $FE $1E $1E $FE $FC $F8
.db $00 $00 $00 $0F $1F $3F $78 $78 $7F $7F $00 $FE $FE $FE $00 $00
.db $FC $FE $7F $78 $78 $7F $7F $7F $00 $00 $FE $1E $1E $FC $F8 $F0
.db $00 $00 $00 $7F $7F $7F $00 $00 $00 $01 $00 $FE $FE $FE $00 $7E
.db $FC $F8 $03 $07 $0F $1F $3F $7E $00 $00 $F0 $E0 $C0 $80 $00 $00
.db $00 $00 $00 $0F $1F $3F $78 $78 $7F $3F $00 $FE $FE $FE $1E $1E
.db $FC $FE $7F $78 $78 $7F $7F $7F $00 $00 $FE $1E $1E $FC $F8 $F0
.db $00 $00 $00 $0F $1F $3F $78 $78 $7F $7F $00 $FE $FE $FE $1E $1E
.db $FE $FE $3F $00 $00 $7F $7F $7F $00 $00 $FE $1E $1E $FC $F8 $F0
.db $00 $00 $01 $04 $07 $0A $0D $10 $13 $16 $02 $05 $08 $0B $0E $11
.db $14 $17 $03 $06 $09 $0C $0F $12 $15 $18 $00 $03 $00 $30 $3F $00
.db $0F $0F $30 $3F $40 $3F $40 $70 $8F $67 $98 $6F $90 $6F $90 $6F
.db $90 $6F $90 $67 $98 $70 $8F $3E $41 $3F $40 $0F $30 $00 $FF $7F
.db $80 $7F $80 $7F $80 $00 $FF $7F $80 $7F $80 $7F $80 $00 $FF $00
.db $FF $FE $01 $FE $01 $FE $01 $00 $FF $FE $01 $FE $01 $FE $01 $00
.db $FF $F0 $0F $FC $03 $7C $83 $0E $F1 $E6 $19 $F6 $09 $F6 $09 $F6
.db $09 $F6 $09 $E6 $19 $0E $F1 $FC $02 $FC $02 $F0 $0C $00 $F0 $00
.db $1F $1F $60 $7F $80 $7F $80 $E0 $1F $CF $30 $DF $20 $DF $20 $DC
.db $23 $DF $20 $DF $20 $C0 $3F $C0 $3F $DF $20 $DF $20 $DC $23 $DF
.db $20 $DF $20 $CF $30 $E0 $1F $7F $80 $7F $80 $1F $60 $00 $1F $00
.db $FE $FC $02 $FC $03 $FC $03 $01 $FE $FD $02 $FD $02 $FD $02 $01
.db $FE $F9 $06 $F9 $06 $01 $FE $01 $FE $F9 $06 $F9 $06 $01 $FE $FD
.db $02 $FD $02 $FD $02 $01 $FE $FC $03 $FC $03 $FC $02 $00 $FE $00
.db $3F $3F $C0 $FF $00 $FF $00 $C0 $3F $9F $60 $BF $40 $BF $40 $B0
.db $4F $B7 $48 $B7 $48 $B0 $4F $B0 $4F $B7 $48 $B7 $48 $B1 $4E $BF
.db $40 $BF $40 $9F $60 $C0 $3F $FF $00 $FF $00 $3F $C0 $00 $3F $00
.db $FC $F8 $04 $F8 $04 $F8 $04 $00 $FC $F8 $04 $F8 $04 $F8 $04 $00
.db $FC $F8 $04 $F8 $04 $18 $E5 $18 $E5 $D8 $25 $D9 $26 $D9 $26 $D9
.db $26 $DB $24 $DB $24 $1B $E4 $FE $01 $FE $01 $FE $01 $00 $FF $00
.db $0F $06 $19 $0F $30 $19 $26 $19 $26 $36 $49 $36 $49 $36 $49 $6F
.db $90 $6F $90 $6F $90 $D9 $26 $D9 $26 $D9 $26 $B0 $4F $B0 $4F $BF
.db $40 $7F $80 $7F $80 $00 $FF $7F $80 $7F $80 $7F $80 $00 $FF $00
.db $1C $1C $A2 $22 $DD $AA $55 $A6 $59 $CA $35 $DC $22 $C0 $3C $60
.db $90 $60 $90 $60 $90 $B0 $48 $B0 $48 $B0 $48 $D8 $24 $D8 $24 $D8
.db $24 $EC $12 $EC $12 $0C $F2 $FE $01 $FE $01 $FE $01 $00 $FF

; Data from 86A1 to 86B2 (18 bytes)
palette_DATA_86A1_:
.db $00 $10 $00 $3F $00 $00 $00 $00 $0F $03 $00 $10 $02 $20 $1B $30
.db $2F $34

; Data from 86B3 to 87FC (330 bytes)
_DATA_86B3_:
.db $02 $02 $00 $87 $01 $02 $03 $04 $05 $06 $07 $0A $00 $87 $07 $06
.db $05 $04 $08 $09 $01 $04 $00 $98 $0A $0B $0C $0D $0E $0F $10 $11
.db $12 $13 $14 $15 $15 $14 $13 $12 $11 $10 $16 $17 $18 $19 $1A $1B
.db $03 $00 $FF $1C $1D $1E $1F $20 $21 $22 $23 $24 $25 $26 $27 $26
.db $27 $28 $29 $26 $2A $2B $2C $2D $2E $2F $30 $31 $32 $00 $00 $33
.db $34 $35 $36 $37 $38 $39 $3A $3B $3C $36 $3D $36 $3D $3E $3F $40
.db $41 $3B $42 $43 $3C $44 $45 $46 $47 $00 $48 $49 $4A $4B $4B $4C
.db $4D $4B $4E $4F $50 $4B $51 $4B $51 $52 $53 $54 $55 $4F $56 $57
.db $50 $58 $59 $5A $4B $5B $5C $5D $5E $4B $4B $5F $60 $4B $61 $4F
.db $50 $4B $51 $4B $51 $52 $62 $63 $64 $4F $65 $66 $50 $4B $67 $68
.db $69 $5B $00 $6A $6B $6C $6D $6E $6F $70 $71 $72 $73 $74 $75 $76
.db $77 $78 $A7 $79 $7A $7B $72 $78 $7C $7D $6D $7E $7F $80 $00 $00
.db $81 $82 $83 $84 $26 $85 $86 $87 $88 $89 $8A $8B $8C $8D $8E $8F
.db $90 $91 $92 $93 $26 $94 $95 $96 $97 $98 $03 $00 $98 $99 $9A $9B
.db $9C $9D $9E $9F $A0 $A1 $A2 $A3 $A4 $A4 $A3 $A2 $A1 $A5 $A6 $A7
.db $16 $A8 $A9 $AA $AB $04 $00 $87 $AC $AD $AE $AF $B0 $B1 $B2 $0A
.db $00 $87 $B2 $B1 $B0 $AF $B3 $B4 $AC $02 $00 $00 $13 $00 $04 $02
.db $02 $00 $81 $02 $10 $00 $06 $02 $5F $00 $02 $04 $02 $00 $82 $04
.db $00 $07 $04 $03 $00 $85 $04 $00 $00 $04 $04 $03 $00 $81 $04 $14
.db $00 $81 $02 $0C $00 $81 $04 $0F $00 $81 $04 $14 $00 $04 $02 $1D
.db $00 $04 $02 $02 $00 $81 $02 $02 $00 $00

; Data from 87FD to 8845 (73 bytes)
_DATA_87FD_:
.db $02 $02 $00 $AE $B5 $B6 $B7 $B8 $B9 $BA $BB $BC $BD $00 $BE $BF
.db $C0 $C1 $C2 $C3 $C4 $C5 $C6 $C7 $C8 $C9 $CA $CB $CC $CD $CE $CF
.db $CF $CE $D0 $D1 $D1 $D0 $00 $D2 $D2 $00 $00 $D3 $D3 $00 $00 $D4
.db $D4 $00 $00 $1E $00 $02 $02 $02 $00 $02 $02 $02 $00 $81 $02 $03
.db $00 $81 $02 $03 $00 $82 $02 $00 $00

; Data from 8846 to 8898 (83 bytes)
_DATA_8846_:
.db $02 $B0 $B6 $B5 $00 $00 $BA $B9 $B8 $B7 $00 $BD $BC $BB $C1 $C0
.db $BF $BE $C5 $C4 $C3 $C2 $C9 $C8 $C7 $C6 $CD $CC $CB $CA $CE $CF
.db $CF $CE $D0 $D1 $D1 $D0 $00 $D2 $D2 $00 $00 $D3 $D3 $00 $00 $D4
.db $D4 $00 $00 $02 $02 $02 $00 $04 $02 $81 $00 $13 $02 $02 $00 $02
.db $02 $02 $00 $02 $02 $02 $00 $81 $02 $03 $00 $81 $02 $03 $00 $82
.db $02 $00 $00

; Data from 8899 to 9A14 (4476 bytes)
_DATA_8899_:
.incbin "data/columns_DATA_8899_.inc"

; Data from 9A15 to 9A26 (18 bytes)
palette_DATA_9A15_:
.db $10 $10
.db $00 $03 $38 $04 $33 $0B $0F $00 $0A $30 $02 $0C $22 $15 $2A $3F

; 1st entry of Pointer Table from 34B1 (indexed by _RAM_C6A9_)
; Data from 9A27 to 9AE8 (194 bytes)
_DATA_9A27_:
.db $04 $9C $18 $3C $3C $34 $0C $0C $3C $18 $24 $5A $7E $3C $24 $5A
.db $66 $5A $38 $18 $00 $00 $80 $80 $10 $00 $08 $42 $42 $7E $03 $40
.db $82 $00 $7E $06 $FF $89 $7E $18 $38 $78 $FC $FF $4E $2C $18 $00
.db $8A $24 $72 $72 $CB $F3 $72 $4A $24 $3C $66 $03 $7E $94 $24 $18
.db $3C $38 $18 $00 $00 $80 $80 $10 $00 $18 $66 $6E $7E $62 $62 $7E
.db $00 $7E $06 $FF $89 $7E $10 $34 $76 $EB $0C $30 $10 $00 $00 $83
.db $00 $30 $30 $03 $00 $8C $08 $00 $24 $42 $7E $3C $24 $00 $00 $18
.db $3C $7E $04 $FF $8D $7E $3C $18 $66 $6E $7E $62 $62 $7E $00 $40
.db $FC $60 $03 $40 $02 $00 $88 $18 $38 $78 $FC $FF $4E $2C $18 $00
.db $B0 $24 $72 $72 $CB $F3 $72 $4A $24 $24 $5A $7E $3C $24 $5A $66
.db $5A $38 $18 $3C $66 $86 $86 $76 $3C $08 $5A $D3 $FF $DD $DD $C1
.db $3C $7E $FD $F9 $E5 $E5 $C1 $81 $00 $10 $34 $76 $EB $0C $30 $10
.db $00 $00

; 2nd entry of Pointer Table from 34B1 (indexed by _RAM_C6A9_)
; Data from 9AE9 to 9B94 (172 bytes)
_DATA_9AE9_:
.db $04 $90 $FF $81 $BD $A5 $A5 $BD $81 $FF $FE $80 $B8 $A0 $A0 $80
.db $80 $00 $08 $FF $02 $FE $03 $FA $83 $C2 $FE $00 $08 $FF $88 $FE
.db $80 $B8 $A0 $A0 $80 $80 $00 $00 $02 $FE $03 $FA $85 $C2 $FE $00
.db $FE $FE $03 $FA $8D $C2 $FE $00 $FF $81 $BD $A5 $A5 $BD $81 $FF
.db $01 $01 $03 $05 $82 $3D $01 $09 $FF $88 $FE $80 $B8 $A0 $A0 $80
.db $80 $00 $00 $8A $FE $80 $B8 $A0 $A0 $80 $80 $00 $FE $FE $03 $FA
.db $83 $C2 $FE $00 $08 $FF $90 $FE $80 $B8 $A0 $A0 $80 $80 $00 $FE
.db $80 $B8 $A0 $A0 $80 $80 $00 $08 $FF $00 $92 $FF $81 $BD $A5 $A5
.db $BD $81 $FF $FF $81 $BD $A5 $A5 $BD $81 $FF $FE $FE $03 $FA $85
.db $C2 $FE $00 $01 $01 $03 $05 $85 $3D $01 $FF $FE $FE $03 $FA $8B
.db $C2 $FE $00 $FF $81 $BD $A5 $A5 $BD $81 $FF $00

; 3rd entry of Pointer Table from 34B1 (indexed by _RAM_C6A9_)
; Data from 9B95 to 9D96 (514 bytes)
_DATA_9B95_:
.db $04 $87 $00 $44 $EE $FE $7C $38 $10 $11 $00 $98 $10 $38 $38 $D6
.db $FE $10 $10 $38 $10 $10 $38 $FE $7C $7C $6C $82 $10 $6C $44 $82
.db $82 $44 $6C $10 $00 $87 $44 $AA $10 $00 $82 $44 $28 $03 $10 $86
.db $38 $7C $7C $38 $10 $10 $0B $00 $95 $28 $00 $C6 $00 $44 $10 $10
.db $38 $FE $7C $7C $6C $82 $00 $10 $38 $7C $7C $38 $10 $00 $00 $08
.db $00 $02 $10 $84 $38 $7C $7C $38 $03 $10 $8F $38 $7C $FE $FE $7C
.db $10 $7C $10 $38 $38 $D6 $FE $10 $10 $38 $10 $00 $00 $98 $44 $AA
.db $10 $00 $82 $44 $28 $10 $00 $28 $44 $82 $82 $44 $28 $00 $00 $28
.db $44 $82 $82 $6C $00 $44 $03 $00 $95 $28 $00 $C6 $00 $44 $00 $10
.db $10 $FE $38 $28 $44 $00 $10 $6C $44 $82 $82 $44 $6C $10 $00 $04
.db $A1 $7E $81 $99 $BD $BD $99 $81 $7E $7E $E1 $E1 $81 $81 $87 $87
.db $7E $7E $87 $87 $99 $99 $E1 $E1 $7E $7E $E7 $E7 $81 $81 $E7 $E7
.db $7E $7E $06 $81 $02 $7E $87 $E7 $81 $E7 $E7 $81 $E7 $7E $00 $02
.db $7E $96 $66 $42 $42 $66 $7E $00 $7E $1E $1E $7E $7E $78 $78 $00
.db $7E $78 $78 $66 $66 $1E $1E $00 $07 $7E $89 $00 $7E $18 $18 $66
.db $66 $18 $18 $00 $07 $7E $81 $00 $00 $A1 $7E $FF $E7 $C3 $C3 $E7
.db $FF $7E $7E $9F $9F $FF $FF $F9 $F9 $7E $7E $F9 $F9 $E7 $E7 $9F
.db $9F $7E $7E $99 $99 $FF $FF $99 $99 $7E $7E $06 $FF $02 $7E $06
.db $FF $81 $7E $00 $89 $7E $FF $E7 $C3 $C3 $E7 $FF $7E $7E $06 $FF
.db $02 $7E $06 $FF $02 $7E $02 $99 $02 $FF $02 $99 $02 $7E $02 $99
.db $02 $E7 $02 $99 $02 $7E $87 $99 $FF $99 $99 $FF $99 $7E $00 $04
.db $81 $1C $07 $00 $02 $3F $84 $03 $3F $3F $30 $04 $3F $87 $03 $1F
.db $1F $03 $3F $3F $FF $04 $CC $93 $C0 $0C $0C $FF $FF $F0 $FF $FF
.db $0F $FF $FF $3F $00 $00 $0F $00 $00 $0C $00 $00 $02 $1C $06 $0C
.db $90 $FF $C0 $0C $FC $C0 $C0 $CF $C0 $FF $C0 $0C $7C $60 $0C $FC
.db $C0 $04 $33 $02 $3F $02 $03 $02 $FF $8E $F0 $FF $FF $0F $FF $FF
.db $3F $00 $00 $0F $00 $00 $0C $00 $00 $02 $1C $06 $0C $91 $3F $00
.db $00 $3C $00 $00 $0F $00 $3F $3F $03 $1F $1F $03 $3F $3F $33 $03
.db $00 $81 $0C $03 $00 $8B $3F $00 $00 $0F $00 $00 $3C $00 $FF $FF
.db $F0 $05 $FF $00 $82 $7C $60 $06 $30 $91 $FF $C0 $0C $FC $C0 $C0
.db $CF $C0 $FF $C0 $0C $7C $60 $0C $FC $C0 $FF $04 $CC $93 $C0 $0C
.db $0C $3F $3F $30 $3F $3F $03 $3F $3F $FF $C0 $C0 $CF $C0 $CC $CC
.db $C0 $00

; 4th entry of Pointer Table from 34B1 (indexed by _RAM_C6A9_)
; Data from 9D97 to 9E3E (168 bytes)
_DATA_9D97_:
.db $04 $A9 $7E $81 $99 $BD $BD $99 $81 $7E $7E $E1 $E1 $81 $81 $87
.db $87 $7E $7E $87 $87 $99 $99 $E1 $E1 $7E $7E $E7 $E7 $81 $81 $E7
.db $E7 $7E $7E $E7 $E7 $99 $99 $E7 $E7 $7E $99 $06 $81 $81 $99 $00
.db $02 $7E $8E $66 $42 $42 $66 $7E $00 $7E $1E $1E $7E $7E $78 $78
.db $00 $07 $7E $89 $00 $7E $18 $18 $7E $7E $18 $18 $00 $07 $7E $89
.db $00 $18 $18 $7E $18 $18 $7E $18 $00 $00 $91 $7E $FF $E7 $C3 $C3
.db $E7 $FF $7E $7E $9F $9F $FF $FF $F9 $F9 $7E $7E $06 $FF $02 $7E
.db $02 $99 $02 $FF $02 $99 $02 $7E $02 $99 $02 $E7 $02 $99 $81 $7E
.db $08 $FF $00 $89 $7E $FF $E7 $C3 $C3 $E7 $FF $7E $7E $06 $FF $02
.db $7E $02 $F9 $02 $E7 $02 $9F $02 $7E $06 $FF $02 $7E $02 $99 $02
.db $E7 $02 $99 $81 $7E $08 $FF $00

; 5th entry of Pointer Table from 34B1 (indexed by _RAM_C6A9_)
; Data from 9E3F to 9F04 (198 bytes)
_DATA_9E3F_:
.db $04 $97 $30 $10 $02 $10 $00 $04 $20 $00 $06 $0C $54 $A4 $48 $14
.db $08 $00 $00 $10 $1E $30 $2C $22 $1C $05 $00 $8F $02 $04 $08 $00
.db $30 $FC $18 $31 $39 $3F $1E $00 $34 $18 $7E $03 $FF $82 $7E $18
.db $00 $AB $30 $16 $37 $3A $D4 $AE $74 $20 $06 $0C $14 $44 $A0 $48
.db $14 $08 $10 $6C $E0 $C0 $CC $C0 $60 $38 $00 $01 $01 $03 $07 $0E
.db $3E $F8 $30 $F4 $66 $CE $C6 $C0 $60 $18 $3C $18 $7E $03 $FF $82
.db $7E $18 $00 $02 $00 $85 $02 $10 $00 $04 $20 $04 $00 $85 $40 $00
.db $08 $00 $00 $03 $10 $83 $0E $1E $1C $03 $00 $02 $01 $88 $03 $07
.db $0E $3E $F8 $00 $08 $1E $03 $3F $8A $1E $00 $04 $00 $02 $2B $18
.db $26 $14 $00 $00 $A3 $30 $10 $02 $10 $00 $04 $20 $00 $06 $0C $14
.db $04 $A0 $40 $14 $08 $10 $7C $F0 $C0 $CC $C0 $60 $38 $03 $02 $02
.db $04 $0A $34 $C8 $00 $30 $40 $60 $03 $C0 $85 $60 $18 $14 $00 $1E
.db $03 $3F $82 $1E $00 $00

; Data from 9F05 to 9F8D (137 bytes)
_DATA_9F05_:
.db $01 $8E $7E $00 $7E $7E $81 $7E $FF $FF $C3 $3C $FF $FF $E7 $18
.db $03 $FF $8F $18 $FF $E7 $FF $3C $FF $C3 $FF $7E $FF $81 $7E $00
.db $7E $7E $08 $00 $04 $28 $04 $10 $04 $28 $10 $00 $04 $28 $04 $44
.db $81 $00 $03 $10 $04 $44 $04 $28 $09 $00 $03 $44 $81 $00 $03 $82
.db $8D $28 $00 $28 $28 $10 $00 $10 $10 $28 $00 $28 $28 $00 $03 $82
.db $81 $00 $03 $44 $04 $00 $84 $92 $00 $92 $92 $08 $00 $84 $82 $00
.db $82 $82 $08 $00 $84 $92 $00 $92 $92 $04 $00 $82 $10 $00 $06 $10
.db $84 $38 $10 $38 $38 $04 $FE $82 $7C $38 $03 $7C $8B $6C $7C $7C
.db $EE $44 $EE $EE $C6 $82 $C6 $C6 $00

; Data from 9F8E to A01F (146 bytes)
_DATA_9F8E_:
.db $90 $00 $00 $3F $7F $7F $70 $70 $70 $71 $00 $C7 $E7 $E7 $07 $07
.db $07 $E7 $00 $E0 $F0 $78 $3C $1E $0E $FE $00 $70 $79 $7F $7F $7F
.db $7F $7F $00 $E7 $E7 $E7 $E7 $E7 $E7 $E7 $00 $FE $FE $FE $00 $00
.db $F0 $00 $71 $7F $7F $3F $00 $3F $7F $7F $E7 $E7 $E7 $E7 $00 $C7
.db $E7 $E7 $FE $0E $0E $0E $00 $0E $0E $0E $76 $70 $70 $70 $00 $7F
.db $7F $7F $E7 $E7 $E7 $E7 $00 $E7 $E7 $E7 $00 $FE $FE $FE $00 $FC
.db $FE $FE $70 $70 $70 $70 $70 $7F $7F $3F $E7 $E7 $E7 $E7 $E7 $E7
.db $E7 $C7 $0E $0E $1E $3E $7C $F8 $F0 $E0 $70 $70 $7F $70 $70 $7F
.db $7F $7F $07 $07 $07 $07 $07 $E7 $E7 $E7 $0E $0E $0E $FC $FC $0E
.db $0E $0E

; Data from A020 to A0A1 (130 bytes)
_DATA_A020_:
.db $80 $00 $00 $79 $7D $6D $6D $6D $6D $7D $00 $F1 $F3 $83 $87 $86
.db $86 $F6 $00 $1E $9F $9B $DB $DB $DB $DB $00 $66 $66 $66 $66 $66
.db $3C $3C $79 $7D $6D $6D $6D $6D $6D $00 $F7 $87 $86 $86 $86 $F6
.db $F6 $00 $DB $DB $DB $DB $DB $DF $DE $00 $18 $18 $18 $18 $18 $18
.db $18 $00 $00 $78 $6C $6C $6D $6D $6D $7D $00 $46 $E6 $E6 $F6 $B6
.db $B6 $B6 $00 $CE $DF $DB $DB $D8 $D8 $DE $00 $7C $7C $60 $60 $60
.db $60 $78 $79 $61 $61 $61 $61 $61 $61 $00 $F6 $F6 $B6 $B6 $B6 $B6
.db $B3 $00 $CF $C3 $C3 $DB $DB $DF $8E $00 $78 $60 $60 $60 $60 $7C
.db $7C $00

; Data from A0A2 to A163 (194 bytes)
_DATA_A0A2_:
.db $C0 $00 $00 $66 $66 $66 $66 $66 $66 $66 $00 $63 $67 $66 $66 $66
.db $66 $66 $00 $E3 $F3 $33 $33 $33 $33 $33 $00 $0C $0C $8C $8C $CC
.db $CC $6C $66 $66 $66 $66 $7F $7F $39 $00 $66 $66 $66 $66 $E6 $E7
.db $C3 $00 $33 $33 $33 $33 $33 $F3 $E3 $00 $6C $3C $3C $1C $1C $0C
.db $0C $00 $00 $60 $60 $60 $60 $60 $60 $60 $00 $3C $7E $66 $66 $66
.db $66 $66 $00 $3C $7E $66 $66 $60 $60 $7C $00 $7E $7E $18 $18 $18
.db $18 $18 $60 $60 $60 $60 $60 $7E $7E $00 $66 $66 $66 $66 $66 $7E
.db $3C $00 $3E $06 $06 $66 $66 $7E $3C $00 $18 $18 $18 $18 $18 $18
.db $18 $00 $00 $7C $7E $66 $66 $66 $66 $66 $00 $F8 $FC $CC $CD $CD
.db $CD $FD $00 $63 $F3 $F3 $FB $9B $9B $9B $00 $6C $6C $6C $6C $6C
.db $6C $6C $66 $66 $66 $66 $66 $7E $7C $00 $F9 $F1 $D9 $DD $CD $CD
.db $CD $00 $FB $FB $9B $9B $9B $99 $99 $00 $6C $6C $6C $6C $FC $F8
.db $98 $00

; Data from A164 to A205 (162 bytes)
_DATA_A164_:
.db $A0 $00 $00 $3C $7E $66 $66 $60 $60 $60 $00 $7C $7E $66 $66 $66
.db $66 $7E $00 $18 $3C $3C $7E $66 $66 $66 $00 $66 $66 $76 $76 $76
.db $7E $7E $00 $7C $7E $66 $66 $66 $66 $66 $6E $6E $66 $66 $66 $7E
.db $3C $00 $7C $78 $6C $6C $66 $66 $66 $00 $7E $7E $66 $66 $66 $66
.db $66 $00 $7E $7E $6E $6E $6E $66 $66 $00 $66 $66 $66 $66 $66 $7E
.db $7C $00 $00 $3C $7E $66 $66 $66 $60 $60 $00 $66 $66 $66 $66 $66
.db $66 $7E $00 $18 $3C $3C $7E $66 $66 $66 $00 $C6 $C6 $C6 $EE $EE
.db $FE $D6 $00 $7C $7E $66 $66 $66 $66 $7E $60 $60 $66 $66 $66 $7E
.db $3C $00 $7E $66 $66 $66 $66 $66 $66 $00 $7E $7E $66 $66 $66 $66
.db $66 $00 $D6 $C6 $C6 $C6 $C6 $C6 $C6 $00 $7C $60 $60 $60 $60 $60
.db $60 $00

; Data from A206 to BFFF (7674 bytes)
_DATA_A206_:
.db $04 $09 $FF $87 $FE $FC $F8 $F0 $E0 $C0 $80 $21 $FF $87 $FE $FC
.db $F8 $F0 $E0 $C0 $80 $7F $FF $09 $FF $00 $90 $01 $03 $07 $0F $1F
.db $3F $7F $FF $01 $02 $04 $08 $10 $20 $40 $80 $21 $FF $87 $FE $FC
.db $F8 $F0 $E0 $C0 $80 $7F $FF $09 $FF $00 $91 $FE $FC $F8 $F0 $E0
.db $C0 $80 $00 $FE $FD $FB $F7 $EF $DF $BF $7F $00 $06 $1C $83 $0C
.db $00 $C7 $05 $CE $83 $8E $00 $C7 $06 $67 $81 $00 $07 $30 $88 $00
.db $01 $03 $07 $0F $1F $3F $7F $07 $07 $81 $00 $06 $0E $82 $07 $00
.db $06 $67 $82 $C3 $00 $06 $30 $81 $E0 $0A $00 $81 $3F $06 $39 $82
.db $00 $1C $06 $9C $82 $00 $FC $06 $E6 $81 $00 $07 $03 $82 $00 $9F
.db $06 $87 $82 $00 $C4 $06 $0E $06 $39 $82 $3F $00 $06 $9C $82 $1C
.db $00 $06 $E6 $82 $FC $00 $07 $03 $81 $00 $07 $87 $89 $00 $0E $0E
.db $04 $00 $0E $0E $04 $00 $00 $11 $00 $06 $1C $83 $0C $00 $C7 $05
.db $CE $83 $8E $00 $C7 $06 $67 $81 $00 $07 $30 $08 $00 $07 $07 $81
.db $00 $06 $0E $82 $07 $00 $06 $67 $82 $C3 $00 $06 $30 $81 $E0 $0A
.db $00 $81 $3F $06 $39 $82 $00 $1C $06 $9C $82 $00 $FC $06 $E6 $81
.db $00 $07 $03 $82 $00 $9F $06 $87 $82 $00 $C4 $06 $0E $06 $39 $82
.db $3F $00 $06 $9C $82 $1C $00 $06 $E6 $82 $FC $00 $07 $03 $81 $00
.db $07 $87 $89 $00 $0E $0E $04 $00 $0E $0E $04 $00 $00
.dsb 7389, $FF

.BANK 3
.ORG $0000

; Data from C000 to C011 (18 bytes)
palette_DATA_C000_:
.db $00 $10 $00 $3F $00 $0F
.dsb 12, $00

; Data from C012 to C1A6 (405 bytes)
_DATA_C012_:
.db $02 $4C $00 $81 $01 $06 $02 $81 $01 $0E $00 $81 $03 $03 $04 $82
.db $05 $06 $04 $00 $81 $07 $06 $00 $81 $07 $0E $00 $86 $08 $09 $0A
.db $0B $0C $0D $04 $00 $81 $07 $06 $00 $81 $07 $0E $00 $81 $0E $03
.db $0F $82 $10 $11 $04 $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07
.db $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07
.db $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07 $0E $00 $82 $06
.db $12 $04 $04 $85 $05 $06 $00 $00 $07 $06 $00 $81 $07 $0E $00 $8B
.db $0D $13 $14 $15 $16 $17 $18 $0D $00 $00 $07 $06 $00 $81 $07 $0E
.db $00 $82 $11 $19 $04 $0F $85 $10 $11 $00 $00 $07 $06 $00 $81 $07
.db $18 $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07
.db $0E $00 $82 $06 $12 $06 $04 $83 $1A $00 $07 $06 $00 $81 $07 $0E
.db $00 $8B $0D $1B $1C $1D $1E $1F $20 $21 $22 $00 $07 $06 $00 $81
.db $07 $0E $00 $82 $11 $19 $06 $0F $83 $23 $00 $07 $06 $00 $81 $07
.db $18 $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07
.db $0E $00 $81 $03 $04 $04 $82 $05 $06 $03 $00 $81 $07 $06 $00 $81
.db $07 $0E $00 $87 $24 $25 $26 $27 $28 $29 $0D $03 $00 $81 $01 $06
.db $02 $8B $01 $00 $00 $2A $2B $2C $2D $2E $2F $30 $31 $04 $00 $81
.db $0E $04 $0F $82 $10 $11 $0D $00 $88 $2A $2B $32 $33 $34 $35 $36
.db $37 $22 $00 $00 $53 $10 $81 $12 $1F $10 $81 $12 $1F $10 $81 $12
.db $1F $10 $81 $12 $1F $10 $81 $12 $1F $10 $81 $12 $1F $10 $81 $12
.db $1F $10 $81 $12 $0E $10 $81 $12 $10 $10 $81 $12 $0E $10 $81 $12
.db $10 $10 $81 $12 $0E $10 $81 $12 $10 $10 $81 $12 $1F $10 $81 $12
.db $1F $10 $81 $12 $0E $10 $81 $12 $10 $10 $81 $12 $0E $10 $81 $12
.db $10 $10 $81 $12 $0E $10 $81 $12 $10 $10 $81 $12 $1F $10 $81 $12
.db $1F $10 $81 $12 $1F $10 $81 $12 $18 $10 $07 $14 $81 $16 $22 $10
.db $02 $14 $28 $10 $00

; Data from C1A7 to C434 (654 bytes)
_DATA_C1A7_:
.db $04 $08 $00 $81 $FF $05 $80 $83 $83 $82 $FF $05 $00 $81 $FF $05
.db $00 $02 $FF $02 $C0 $04 $00 $02 $FF $06 $00 $84 $FE $FF $01 $01
.db $06 $00 $82 $80 $C0 $08 $82 $02 $CE $02 $CD $04 $CC $02 $67 $98
.db $66 $67 $E7 $E6 $67 $67 $E6 $E7 $07 $E3 $E3 $07 $E7 $E6 $33 $73
.db $F0 $E0 $E0 $F0 $70 $30 $F9 $F9 $06 $E1 $8B $E0 $F0 $F8 $FC $F8
.db $F0 $E0 $C0 $C0 $FF $FF $06 $00 $02 $FF $05 $00 $83 $01 $FF $FE
.db $05 $00 $81 $80 $0B $00 $9E $7F $FF $C0 $C0 $CF $DF $D8 $DF $CF
.db $C0 $DF $CF $87 $CF $0C $8C $CC $CC $CF $87 $C3 $E7 $66 $06 $06
.db $66 $E7 $C3 $E3 $F3 $04 $33 $95 $F3 $E3 $F1 $F9 $19 $19 $F1 $F9
.db $19 $19 $F9 $F9 $81 $F9 $F9 $81 $F9 $F9 $C0 $FF $7F $09 $00 $96
.db $80 $C0 $E0 $F0 $DF $DF $D8 $DF $DF $D8 $DF $DF $8E $CE $CE $8E
.db $CE $CE $CF $8F $07 $0F $04 $0C $84 $CF $C7 $C3 $E7 $04 $66 $A5
.db $E7 $C3 $E3 $F3 $33 $03 $03 $33 $F3 $E3 $18 $39 $71 $E1 $E0 $F0
.db $39 $18 $F8 $FC $80 $F8 $FC $0C $FC $F8 $F8 $FC $FE $FF $FE $FC
.db $F8 $F0 $E0 $C0 $80 $05 $00 $06 $CE $02 $CF $02 $0F $9E $0C $0F
.db $0F $0C $CF $CF $E6 $E6 $07 $E3 $E3 $01 $E1 $E0 $19 $19 $39 $31
.db $F1 $E1 $E1 $C1 $FC $FC $80 $FC $FC $80 $FC $FC $06 $E1 $02 $FD
.db $9B $01 $03 $07 $0F $1F $3F $7E $FC $FE $F0 $E3 $C7 $8F $1E $3C
.db $38 $00 $00 $83 $C3 $23 $13 $0B $0B $00 $00 $80 $05 $86 $03 $00
.db $82 $34 $36 $03 $37 $03 $00 $A2 $16 $36 $77 $F7 $F7 $00 $00 $31
.db $33 $37 $33 $B1 $F0 $7E $F8 $F0 $E0 $C0 $E0 $F0 $F8 $0B $0B $13
.db $23 $C3 $80 $00 $00 $86 $86 $F6 $F7 $F7 $03 $00 $85 $37 $36 $36
.db $F6 $F6 $03 $00 $82 $F7 $B6 $03 $36 $93 $06 $00 $00 $F0 $F0 $70
.db $30 $31 $33 $07 $FF $7C $3E $7E $FC $F8 $F0 $E0 $C0 $00 $1C $00
.db $04 $FF $04 $00 $04 $FF $04 $00 $81 $FE $03 $FF $06 $00 $82 $80
.db $C0 $08 $00 $02 $F1 $02 $F2 $04 $F3 $02 $98 $98 $99 $98 $18 $19
.db $98 $98 $19 $18 $F8 $1C $1C $F8 $18 $19 $CC $8C $0F $1F $1F $0F
.db $8F $CF $07 $07 $06 $1F $88 $E0 $F0 $F8 $FC $F8 $F0 $E0 $C0 $03
.db $FF $05 $00 $03 $FF $05 $00 $02 $FF $81 $FE $05 $00 $81 $80 $0B
.db $00 $81 $7F $03 $FF $9A $F0 $E0 $E7 $E0 $F0 $FF $E0 $F0 $78 $30
.db $F3 $73 $33 $33 $30 $78 $3C $18 $99 $F9 $F9 $99 $18 $3C $1C $0C
.db $04 $CC $95 $0C $1C $0E $06 $E6 $E6 $0E $06 $E6 $E6 $07 $07 $7F
.db $07 $07 $7F $07 $07 $FF $FF $7F $09 $00 $96 $80 $C0 $E0 $F0 $E0
.db $E0 $E7 $E0 $E0 $E7 $E0 $E0 $71 $31 $31 $71 $31 $31 $30 $70 $F8
.db $F0 $04 $F3 $84 $30 $38 $3C $18 $04 $99 $A5 $18 $3C $1C $0C $CC
.db $FC $FC $CC $0C $1C $E7 $C6 $8E $1E $1F $0F $C6 $E7 $07 $03 $7F
.db $07 $03 $F3 $03 $07 $F8 $FC $FE $FF $FE $FC $F8 $F0 $E0 $C0 $80
.db $05 $00 $06 $F1 $04 $F0 $9E $F3 $F0 $F0 $F3 $30 $30 $19 $19 $F8
.db $1C $1C $FE $1E $1F $E6 $E6 $C6 $CE $0E $1E $1E $3E $03 $03 $7F
.db $03 $03 $7F $03 $03 $06 $1F $02 $03 $70 $00 $00 $7F $00 $7F $00
.db $7F $00 $43 $00 $00 $7F $00 $7F $00 $7F $00 $43 $00 $00

; Data from C435 to C450 (28 bytes)
_DATA_C435_:
.db $01 $04 $80 $1C $00 $81 $80 $20 $00 $02 $80 $1D $00 $84 $80 $00
.db $00 $80 $1C $00 $02 $80 $82 $00 $80 $1C $00 $00

; Data from C451 to C462 (18 bytes)
palette_DATA_C451_:
.db $00 $10 $00 $3F $00 $2E $2A $00 $1A $38 $34 $30 $20 $13 $3F $3F
.db $00 $00

; Data from C463 to C611 (431 bytes)
_DATA_C463_:
.db $02 $20 $01 $20 $02 $08 $03 $85 $04 $05 $04 $03 $06 $06 $07 $81
.db $06 $0C $03 $02 $08 $81 $09 $03 $0A $87 $0B $0C $0D $00 $0D $08
.db $0E $06 $0F $81 $0E $0C $08 $02 $10 $8B $11 $12 $13 $14 $15 $16
.db $17 $00 $17 $10 $0E $06 $0F $81 $0E $0C $10 $02 $18 $81 $19 $03
.db $1A $87 $1B $1C $1D $00 $1D $18 $0E $06 $0F $81 $0E $0C $18 $08
.db $1E $85 $1F $20 $1F $1E $0E $06 $0F $81 $0E $18 $1E $81 $0E $06
.db $0F $81 $0E $18 $1E $81 $0E $06 $0F $81 $0E $18 $1E $81 $0E $06
.db $0F $81 $0E $0D $1E $8C $21 $22 $23 $24 $25 $26 $27 $28 $22 $21
.db $1E $0E $06 $0F $81 $0E $0C $1E $8D $29 $2A $2B $2C $2D $2E $2F
.db $30 $31 $2B $2A $29 $0E $06 $0F $81 $0E $0C $1E $81 $32 $0A $0F
.db $82 $32 $0E $06 $0F $81 $0E $0C $1E $81 $32 $04 $0F $81 $33 $04
.db $0F $83 $34 $32 $0E $06 $0F $81 $0E $0C $1E $81 $35 $0A $36 $82
.db $35 $0E $06 $0F $81 $0E $18 $1E $81 $0E $06 $0F $81 $0E $18 $1E
.db $81 $0E $06 $0F $81 $0E $0C $1E $0B $37 $82 $38 $0E $06 $0F $82
.db $0E $38 $13 $37 $85 $39 $3A $3B $3C $0E $06 $0F $85 $0E $3C $3B
.db $3A $39 $0D $37 $88 $3D $3E $3F $40 $41 $42 $42 $0E $06 $0F $88
.db $0E $42 $42 $41 $40 $3F $3E $3D $07 $37 $8B $43 $44 $45 $46 $47
.db $48 $49 $4A $4B $42 $0E $06 $0F $81 $0E $04 $42 $95 $4C $4D $4E
.db $4F $50 $51 $37 $37 $52 $53 $54 $55 $56 $57 $58 $59 $5A $0F $5B
.db $42 $06 $06 $07 $91 $06 $42 $42 $5C $5D $5E $5F $60 $61 $62 $63
.db $53 $52 $64 $64 $65 $66 $03 $67 $84 $68 $69 $67 $6A $0B $42 $8C
.db $5C $5D $6B $6C $6D $6E $6F $70 $64 $64 $71 $72 $1C $42 $82 $72
.db $71 $00 $4A $00 $81 $02 $08 $00 $81 $02 $16 $00 $81 $02 $1F $00
.db $81 $02 $1F $00 $81 $02 $1F $00 $81 $02 $7F $00 $81 $02 $1F $00
.db $02 $02 $1F $00 $81 $02 $1F $00 $81 $02 $1F $00 $81 $02 $68 $00
.db $81 $02 $1F $00 $04 $02 $1E $00 $05 $02 $31 $00 $81 $04 $06 $00
.db $81 $06 $0A $00 $02 $02 $16 $00 $02 $04 $26 $00 $02 $02 $00

; Data from C612 to CD22 (1809 bytes)
_DATA_C612_:
.incbin "data/columns_DATA_C612_.inc"

; Data from CD23 to CD34 (18 bytes)
palette_DATA_CD23_:
.db $00 $10 $00 $3F $0F $02 $00 $00 $1A $38 $14 $29 $3E $01 $0F $0E
.db $09 $04

; Data from CD35 to CFEC (696 bytes)
_DATA_CD35_:
.db $02 $C3 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $02 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $01 $02 $05 $06 $06 $8E $05 $07 $08 $09 $02 $01 $02 $01
.db $02 $01 $0A $0B $0C $05 $06 $06 $86 $05 $01 $02 $03 $04 $0D $06
.db $0E $8E $0D $0F $10 $11 $12 $13 $14 $15 $16 $17 $18 $10 $19 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $1B $10 $1C
.db $1D $1E $1F $20 $21 $22 $23 $10 $24 $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $0D $06 $0E $8E $0D $0F $10 $26 $27 $28 $29 $28 $29 $2A
.db $2B $10 $2C $0D $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E
.db $0D $2D $2E $2F $02 $01 $02 $01 $02 $01 $30 $31 $32 $0D $06 $0E
.db $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $1A $04 $03 $04 $03
.db $04 $03 $04 $03 $04 $03 $04 $0D $06 $0E $86 $0D $1A $04 $01 $02
.db $0D $06 $0E $8E $0D $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01
.db $02 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $10
.db $10 $33 $34 $35 $36 $37 $38 $39 $33 $10 $10 $0D $06 $0E $86 $0D
.db $1A $04 $01 $02 $0D $06 $0E $8E $0D $10 $10 $3A $3B $3C $3D $3E
.db $3F $40 $3A $10 $10 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06
.db $0E $8E $0D $41 $42 $43 $44 $45 $46 $45 $46 $45 $47 $43 $42 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $25 $02 $01
.db $02 $01 $02 $01 $02 $01 $02 $01 $02 $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $0D $06 $0E $8E $0D $1A $04 $03 $04 $03 $04 $03 $04 $03
.db $04 $03 $04 $0D $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E
.db $0D $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $0D $06 $0E
.db $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $1A $04 $03 $04 $03
.db $04 $03 $04 $03 $04 $03 $04 $0D $06 $0E $86 $0D $1A $04 $01 $02
.db $0D $06 $0E $8E $0D $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01
.db $02 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $1A
.db $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $0D $06 $0E $86 $0D
.db $1A $04 $01 $02 $0D $06 $0E $8E $0D $25 $02 $01 $02 $01 $02 $01
.db $02 $01 $02 $01 $02 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06
.db $0E $8E $0D $48 $49 $4A $4B $4C $4D $4C $4D $4E $4F $50 $51 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $52 $10 $53
.db $54 $55 $56 $57 $58 $59 $53 $10 $5A $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $05 $06 $06 $8E $05 $5B $5C $5D $5E $5F $60 $5F $60 $61
.db $62 $63 $64 $05 $06 $06 $C3 $05 $1A $04 $01 $02 $01 $65 $66 $65
.db $66 $65 $66 $65 $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $65 $66 $65 $66 $65 $66 $65 $25 $02 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $00 $49 $00 $81 $02 $13
.db $00 $81 $02 $7F $00 $56 $00 $81 $02 $1F $00 $81 $02 $7F $00 $7F
.db $00 $41 $00 $81 $02 $0E $00 $81 $04 $06 $00 $81 $06 $0C $00 $81
.db $04 $06 $00 $81 $06 $42 $00 $00

; Data from CFED to D6DF (1779 bytes)
_DATA_CFED_:
.incbin "data/columns_DATA_CFED_.inc"

; Data from D6E0 to D6F1 (18 bytes)
palette_DATA_D6E0_:
.db $00 $10 $00 $3F $0F $02 $00 $25 $2A $2F $14 $29 $3E $01 $0F $0E
.db $09 $04

; Data from D6F2 to D99F (686 bytes)
_DATA_D6F2_:
.db $02 $C3 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $02 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $01 $02 $05 $06 $06 $8E $05 $07 $08 $09 $02 $01 $02 $01
.db $02 $01 $0A $0B $0C $05 $06 $06 $86 $05 $01 $02 $03 $04 $0D $06
.db $0E $8E $0D $0F $10 $11 $12 $13 $14 $15 $16 $17 $18 $10 $19 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $1B $10 $1C
.db $1D $1E $1F $20 $21 $22 $23 $10 $24 $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $0D $06 $0E $8E $0D $0F $10 $26 $27 $28 $29 $28 $29 $2A
.db $2B $10 $2C $0D $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E
.db $0D $2D $2E $2F $02 $01 $02 $01 $02 $01 $30 $31 $32 $0D $06 $0E
.db $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $1A $04 $03 $04 $03
.db $04 $03 $04 $03 $04 $03 $04 $0D $06 $0E $86 $0D $1A $04 $01 $02
.db $0D $06 $0E $8E $0D $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01
.db $02 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06 $0E $8E $0D $10
.db $10 $33 $34 $35 $36 $37 $38 $39 $33 $10 $10 $0D $06 $0E $86 $0D
.db $1A $04 $01 $02 $0D $06 $0E $8E $0D $10 $10 $3A $3B $3C $3D $3E
.db $3F $40 $3A $10 $10 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06
.db $0E $8E $0D $41 $42 $43 $44 $45 $46 $45 $46 $45 $47 $43 $42 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $25 $02 $01
.db $02 $01 $02 $01 $02 $01 $02 $01 $02 $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $0D $06 $0E $8E $0D $48 $49 $4A $4B $4C $4D $4E $4F $50
.db $4A $49 $51 $0D $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E
.db $0D $52 $53 $54 $55 $56 $57 $58 $59 $5A $54 $53 $5B $0D $06 $0E
.db $86 $0D $25 $02 $03 $04 $0D $06 $0E $82 $0D $5C $0A $10 $82 $5D
.db $0D $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $82 $0D $5E $04
.db $10 $81 $5F $04 $10 $83 $60 $61 $0D $06 $0E $86 $0D $25 $02 $03
.db $04 $0D $06 $0E $82 $0D $62 $0A $63 $82 $64 $0D $06 $0E $86 $0D
.db $1A $04 $01 $02 $0D $06 $0E $8E $0D $65 $66 $67 $66 $67 $66 $67
.db $66 $67 $66 $67 $66 $0D $06 $0E $86 $0D $25 $02 $03 $04 $0D $06
.db $0E $8E $0D $68 $69 $6A $6B $6C $6D $6C $6D $6E $6F $70 $71 $0D
.db $06 $0E $86 $0D $1A $04 $01 $02 $0D $06 $0E $8E $0D $72 $10 $73
.db $74 $75 $76 $77 $78 $79 $73 $10 $7A $0D $06 $0E $86 $0D $25 $02
.db $03 $04 $05 $06 $06 $8E $05 $7B $7C $7D $7E $7F $80 $7F $80 $81
.db $82 $83 $84 $05 $06 $06 $C3 $05 $1A $04 $01 $02 $01 $85 $86 $85
.db $86 $85 $86 $85 $25 $02 $01 $02 $01 $02 $01 $02 $01 $02 $01 $02
.db $01 $85 $86 $85 $86 $85 $86 $85 $25 $02 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $03 $04
.db $03 $04 $03 $04 $03 $04 $03 $04 $03 $04 $00 $49 $00 $81 $02 $13
.db $00 $81 $02 $7F $00 $56 $00 $81 $02 $1F $00 $81 $02 $60 $00 $81
.db $02 $1F $00 $81 $02 $7F $00 $3F $00 $81 $02 $0E $00 $81 $04 $06
.db $00 $81 $06 $0C $00 $81 $04 $06 $00 $81 $06 $42 $00 $00

; Data from D9A0 to E29B (2300 bytes)
_DATA_D9A0_:
.incbin "data/columns_DATA_D9A0_.inc"

; Data from E29C to E2AD (18 bytes)
palette_DATA_E29C_:
.db $00 $10 $00 $3F $00 $0F $05
.dsb 11, $00

; Data from E2AE to E4AC (511 bytes)
_DATA_E2AE_:
.db $02 $4C $00 $81 $01 $06 $02 $81 $01 $0E $00 $81 $03 $03 $04 $82
.db $05 $06 $04 $00 $81 $07 $06 $00 $81 $07 $04 $00 $82 $08 $09 $03
.db $0A $81 $0B $04 $00 $86 $0C $0D $0E $0F $10 $11 $04 $00 $81 $07
.db $06 $00 $81 $07 $04 $00 $86 $12 $13 $14 $15 $16 $17 $04 $00 $81
.db $18 $03 $19 $82 $1A $1B $04 $00 $81 $07 $06 $00 $81 $07 $04 $00
.db $82 $1C $1D $03 $1E $81 $1F $0E $00 $81 $07 $06 $00 $81 $07 $18
.db $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07 $18
.db $00 $81 $07 $06 $00 $81 $07 $0E $00 $82 $06 $20 $04 $04 $85 $05
.db $06 $00 $00 $07 $06 $00 $85 $07 $00 $00 $08 $09 $04 $0A $82 $21
.db $08 $04 $00 $8B $11 $22 $23 $24 $25 $26 $27 $11 $00 $00 $07 $06
.db $00 $8B $07 $00 $00 $12 $28 $29 $2A $2B $2C $2D $12 $04 $00 $82
.db $1B $2E $04 $19 $85 $1A $1B $00 $00 $07 $06 $00 $85 $07 $00 $00
.db $1C $1D $04 $1E $82 $2F $1C $0E $00 $81 $07 $06 $00 $81 $07 $18
.db $00 $81 $07 $06 $00 $81 $07 $0E $00 $82 $06 $20 $06 $04 $83 $30
.db $00 $07 $06 $00 $83 $07 $00 $31 $06 $0A $82 $21 $08 $04 $00 $8B
.db $11 $32 $33 $34 $35 $36 $37 $38 $39 $00 $07 $06 $00 $8B $07 $00
.db $3A $3B $3C $3D $3E $3F $40 $41 $12 $04 $00 $82 $1B $2E $06 $19
.db $83 $42 $00 $07 $06 $00 $83 $07 $00 $43 $06 $1E $82 $2F $1C $0E
.db $00 $81 $07 $06 $00 $81 $07 $18 $00 $81 $07 $06 $00 $81 $07 $0E
.db $00 $81 $03 $04 $04 $82 $05 $06 $03 $00 $81 $07 $06 $00 $81 $07
.db $03 $00 $82 $08 $09 $04 $0A $81 $0B $04 $00 $87 $44 $45 $46 $47
.db $48 $49 $11 $03 $00 $81 $01 $06 $02 $81 $01 $03 $00 $87 $12 $4A
.db $4B $4C $4D $4E $4F $04 $00 $81 $18 $04 $19 $82 $1A $1B $0E $00
.db $82 $1C $1D $04 $1E $81 $1F $22 $00 $00 $53 $10 $81 $12 $1F $10
.db $81 $12 $1F $10 $81 $12 $1F $10 $81 $12 $1F $10 $81 $12 $1F $10
.db $81 $12 $1F $10 $81 $12 $1F $10 $81 $12 $0E $10 $81 $12 $10 $10
.db $81 $12 $09 $10 $81 $12 $04 $10 $81 $12 $10 $10 $81 $12 $09 $10
.db $81 $12 $04 $10 $81 $12 $10 $10 $81 $12 $09 $10 $81 $12 $15 $10
.db $81 $12 $1F $10 $81 $12 $0E $10 $81 $12 $10 $10 $81 $12 $09 $10
.db $81 $12 $04 $10 $81 $12 $10 $10 $81 $12 $09 $10 $81 $12 $04 $10
.db $81 $12 $10 $10 $81 $12 $09 $10 $81 $12 $15 $10 $81 $12 $1F $10
.db $81 $12 $1F $10 $81 $12 $18 $10 $07 $14 $81 $16 $4C $10 $00

; Data from E4AD to E8CD (1057 bytes)
_DATA_E4AD_:
.incbin "data/columns_DATA_E4AD_.inc"

; Data from E8CE to E8DF (18 bytes)
palette_DATA_E8CE_:
.db $00 $10 $00 $3F $00 $2A $00 $1A $38 $34 $30 $20 $0F $05 $3F $3F
.db $00 $00

; Data from E8E0 to EAF8 (537 bytes)
_DATA_E8E0_:
.db $02 $20 $01 $20 $02 $08 $03 $85 $04 $05 $04 $03 $06 $06 $07 $85
.db $06 $03 $08 $09 $08 $08 $03 $02 $0A $81 $0B $03 $0C $87 $0D $0E
.db $0F $00 $0F $0A $10 $06 $11 $87 $10 $0A $12 $00 $12 $13 $14 $03
.db $15 $90 $16 $0A $0A $17 $17 $18 $19 $1A $1B $1C $1D $1E $00 $1E
.db $17 $10 $06 $11 $90 $10 $17 $1F $00 $1F $20 $21 $22 $23 $24 $25
.db $17 $17 $26 $26 $27 $03 $28 $87 $29 $2A $2B $00 $2B $26 $10 $06
.db $11 $87 $10 $26 $2C $00 $2C $2D $2E $03 $2F $83 $30 $26 $26 $08
.db $31 $85 $32 $33 $32 $31 $10 $06 $11 $85 $10 $31 $34 $35 $34 $14
.db $31 $81 $10 $06 $11 $81 $10 $18 $31 $81 $10 $06 $11 $81 $10 $18
.db $31 $81 $10 $06 $11 $81 $10 $0D $31 $8C $36 $37 $38 $39 $3A $3B
.db $3C $3D $37 $36 $31 $10 $06 $11 $9A $10 $31 $3E $3F $40 $41 $42
.db $43 $44 $45 $3F $3E $31 $46 $47 $48 $49 $4A $4B $4C $4D $4E $48
.db $47 $46 $10 $06 $11 $8E $10 $4F $50 $51 $52 $53 $54 $55 $56 $57
.db $51 $50 $4F $58 $0A $11 $82 $58 $10 $06 $11 $82 $10 $59 $0A $11
.db $82 $59 $58 $04 $11 $81 $5A $04 $11 $83 $5B $58 $10 $06 $11 $82
.db $10 $59 $04 $11 $81 $5A $04 $11 $83 $5B $59 $5C $0A $5D $82 $5C
.db $10 $06 $11 $82 $10 $5E $0A $5F $81 $5E $0C $31 $81 $10 $06 $11
.db $81 $10 $18 $31 $81 $10 $06 $11 $81 $10 $0C $31 $0B $60 $82 $61
.db $10 $06 $11 $82 $10 $61 $13 $60 $85 $62 $63 $64 $65 $10 $06 $11
.db $85 $10 $65 $64 $63 $62 $0D $60 $88 $66 $67 $68 $69 $6A $6B $6B
.db $10 $06 $11 $88 $10 $6B $6B $6A $69 $68 $67 $66 $07 $60 $8B $6C
.db $6D $6E $6F $70 $71 $72 $73 $74 $6B $10 $06 $11 $9A $10 $6B $75
.db $76 $77 $78 $79 $7A $7B $7C $7D $60 $60 $7E $7F $80 $81 $82 $83
.db $84 $85 $86 $11 $87 $6B $06 $06 $07 $91 $06 $6B $88 $11 $89 $8A
.db $8B $8C $8D $8E $8F $7F $7E $90 $90 $91 $92 $03 $93 $84 $94 $95
.db $93 $96 $0A $6B $84 $97 $98 $99 $9A $03 $98 $86 $9B $9C $90 $90
.db $9D $9E $1C $6B $82 $9E $9D $00 $4A $00 $81 $02 $08 $00 $81 $02
.db $03 $00 $81 $02 $12 $00 $81 $02 $0C $00 $81 $02 $12 $00 $81 $02
.db $0C $00 $81 $02 $12 $00 $81 $02 $0C $00 $81 $02 $12 $00 $81 $02
.db $0C $00 $81 $02 $72 $00 $81 $02 $13 $00 $81 $02 $0B $00 $02 $02
.db $12 $00 $02 $02 $0B $00 $81 $02 $13 $00 $81 $02 $0B $00 $81 $02
.db $13 $00 $81 $02 $0B $00 $81 $02 $13 $00 $81 $02 $54 $00 $81 $02
.db $1F $00 $04 $02 $1E $00 $05 $02 $31 $00 $81 $04 $06 $00 $81 $06
.db $0A $00 $02 $02 $3E $00 $02 $02 $00

; Data from EAF9 to FFFF (5383 bytes)
_DATA_EAF9_:
.incbin "data/columns_DATA_EAF9_.inc"

.BANK 4
.ORG $0000

; Data from 10000 to 13FFF (16384 bytes)
.dsb 16384, $FF

.BANK 5
.ORG $0000

; Data from 14000 to 17FFF (16384 bytes)
.dsb 16384, $FF

.BANK 6
.ORG $0000

; Data from 18000 to 1BFFF (16384 bytes)
.dsb 16384, $FF

.BANK 7
.ORG $0000

; Data from 1C000 to 1FFFF (16384 bytes)
.dsb 16384, $FF

