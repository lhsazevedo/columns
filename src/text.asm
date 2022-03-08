drawText_LABEL_2885_:
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

drawTextNametable_LABEL_2885_:
    ld c, a
    ex af, af'
    ld b, $00
    ld hl, _DATA_28BF_
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
    ld hl, return
    xor a
    sbc hl, bc
    sbc hl, bc
    push hl
    pop iy
    pop hl
    ld c, $BE
    jp (iy)

; Pointer Table from 28BF to 28E6 (20 entries, indexed by optDifficulty_RAM_C6A8_)
_DATA_28BF_:
.dw _DATA_28E7_
.dw _DATA_290C_
.dw _DATA_291D_
.dw _DATA_292A_
.dw _DATA_293B_
.dw _DATA_2952_
.dw _DATA_2969_
.dw _DATA_297A_
.dw _DATA_2985_
.dw _DATA_2994_
.dw _DATA_29A9_
.dw _DATA_29B6_
.dw _DATA_29C1_
.dw _DATA_29CA_
.dw _DATA_29D7_
.dw _DATA_29E4_
.dw _DATA_29F1_
.dw _DATA_2A00_
.dw _DATA_2A1B_
.dw _DATA_2A24_

; 1st entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 28E7 to 290B (37 bytes)
_DATA_28E7_:
.db $24 $EF $00 $F1 $00 $E4 $00 $F2 $00 $F2 $00 $00 $00 $F2 $00 $F3
.db $00 $E0 $00 $F1 $00 $F3 $00 $00 $00 $E1 $00 $F4 $00 $F3 $00 $F3
.db $00 $EE $00 $ED $00

; 2nd entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 290C to 291C (17 bytes)
_DATA_290C_:
.db $10 $D7 $00 $00 $00 $EF $00 $EB $00 $E0 $00 $F8 $00 $E4 $00 $F1
.db $00

; 3rd entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 291D to 2929 (13 bytes)
_DATA_291D_:
.db $0C $F5 $00 $E4 $00 $F1 $00 $F2 $00 $F4 $00 $F2 $00

; 4th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 292A to 293A (17 bytes)
_DATA_292A_:
.db $10 $D8 $00 $00 $00 $EF $00 $EB $00 $E0 $00 $F8 $00 $E4 $00 $F1
.db $00

; 5th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 293B to 2951 (23 bytes)
_DATA_293B_:
.db $16 $FE $00 $00 $00 $F2 $00 $E4 $00 $E6 $00 $E0 $00 $00 $00 $D7
.db $00 $DF $00 $DF $00 $D6 $00

; 6th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 2952 to 2968 (23 bytes)
_DATA_2952_:
.db $16 $F2 $00 $E4 $00 $EB $00 $E4 $00 $E2 $00 $F3 $00 $00 $00 $E6
.db $00 $E0 $00 $EC $00 $E4 $00

; 7th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 2969 to 2979 (17 bytes)
_DATA_2969_:
.db $10 $EE $00 $F1 $00 $E8 $00 $E6 $00 $E8 $00 $ED $00 $E0 $00 $EB
.db $00

; 8th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 297A to 2984 (11 bytes)
_DATA_297A_:
.db $0A $E5 $00 $EB $00 $E0 $00 $F2 $00 $E7 $00

; 9th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 2985 to 2993 (15 bytes)
_DATA_2985_:
.db $0E $EE $00 $EF $00 $F3 $00 $E8 $00 $EE $00 $ED $00 $F2 $00

; 10th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 2994 to 29A8 (21 bytes)
_DATA_2994_:
.db $14 $E3 $00 $E8 $00 $E5 $00 $E5 $00 $E8 $00 $E2 $00 $F4 $00 $EB
.db $00 $F3 $00 $F8 $00

; 11th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 29A9 to 29B5 (13 bytes)
_DATA_29A9_:
.db $0C $E1 $00 $EB $00 $EE $00 $E2 $00 $EA $00 $F2 $00

; 12th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 29B6 to 29C0 (11 bytes)
_DATA_29B6_:
.db $0A $EB $00 $E4 $00 $F5 $00 $E4 $00 $EB $00

; 13th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 29C1 to 29C9 (9 bytes)
_DATA_29C1_:
.db $08 $E4 $00 $F7 $00 $E8 $00 $F3 $00

; 14th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 29CA to 29D6 (13 bytes)
_DATA_29CA_:
.db $0C $00 $00 $E4 $00 $E0 $00 $F2 $00 $F8 $00 $00 $00

; 15th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 29D7 to 29E3 (13 bytes)
_DATA_29D7_:
.db $0C $ED $00 $EE $00 $F1 $00 $EC $00 $E0 $00 $EB $00

; 16th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 29E4 to 29F0 (13 bytes)
_DATA_29E4_:
.db $0C $00 $00 $E7 $00 $E0 $00 $F1 $00 $E3 $00 $00 $00

; 17th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 29F1 to 29FF (15 bytes)
_DATA_29F1_:
.db $0E $EC $00 $E0 $00 $F3 $00 $E2 $00 $E7 $00 $E4 $00 $F2 $00

; 18th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 2A00 to 2A1A (27 bytes)
_DATA_2A00_:
.db $1A $EC $00 $E8 $00 $ED $00 $F4 $00 $F3 $00 $E4 $00 $F2 $00 $00
.db $00 $EC $00 $E0 $00 $F3 $00 $E2 $00 $E7 $00

; 19th entry of Pointer Table from 28BF (indexed by optDifficulty_RAM_C6A8_)
; Data from 2A1B to 2A23 (9 bytes)
_DATA_2A1B_:
.db $08 $E7 $00 $E8 $00 $E6 $00 $E7 $00

; Data from 2A24 to 2A26 (3 bytes)
_DATA_2A24_:
.db $02 $00 $00
