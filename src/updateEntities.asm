updateEntities:
    ld ix, var.entities

    ld b, $10
    -:
        push bc
        ld hl, + ; Overriding return address
        push hl
        ld l, (ix + Entity.handler.low)
        ld h, (ix + Entity.handler.high)
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
