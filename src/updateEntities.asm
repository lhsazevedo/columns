updateEntities:
    ld ix, v_entities

    ld b, $10
    -:
        push bc
        ld hl, + ; Overriding return address
        push hl
        ld l, (ix+0)
        ld h, (ix+1)
        ld a, l
        or h
        ret z
        jp (hl)

        +:
        ld de, $0020
        add ix, de
        pop bc
    djnz -

    ret
