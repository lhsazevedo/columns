input.update:
    ld a, (var.previousState)
    cp $10 ; < gameplay
    jr c, @_LABEL_49C_

    cp $16 ; >= ?
    jr nc, @_LABEL_49C_

    ; TODO
    ld a, (var.input.player1.data)
    and $0C
    ld a, $08
    jr z, +
    ld a, (var.input.player1.timer)
    dec a
    jr nz, +
    ld a, (var.input.player1.data)
    and $F3
    ld (var.input.player1.data), a
    ld a, $01
+:
    ld (var.input.player1.timer), a
    ld a, (var.input.player2.data)
    and $0C
    ld a, $08
    jr z, +
    ld a, (var.input.player2.timer)
    dec a
    jr nz, +
    ld a, (var.input.player2.data)
    and $F3
    ld (var.input.player2.data), a
    ld a, $01
+:
    ld (var.input.player2.timer), a

    jr ++

@_LABEL_49C_:
    ld a, (var.input.player1.data)
    and $0F
    ld a, $10
    jr z, +
    ld a, (var.input.player1.timer)
    dec a
    jr nz, +
    ld a, (var.input.player1.data)
    and $F0
    ld (var.input.player1.data), a
    ld a, $0A
+:
    ld (var.input.player1.timer), a
    ld a, (var.input.player2.data)
    and $0F
    ld a, $10
    jr z, +
    ld a, (var.input.player2.timer)
    dec a
    jr nz, +
    ld a, (var.input.player2.data)
    and $F0
    ld (var.input.player2.data), a
    ld a, $0A
+:
    ld (var.input.player2.timer), a

++:
    ld a, (var.input.player1.data)
    cpl
    ld d, a
    call @getDataB
    ld (var.input.player1.data), a
    and d
    ld (var.input.player1.debounced), a

    ld a, (var.input.player2.data)
    cpl
    ld d, a
    call @getDataA
    ld (var.input.player2.data), a
    and d
    ld (var.input.player2.debounced), a

    ret

@getDataA:
    in a, (Port_IOPort1)
    ld c, a
    in a, (Port_IOPort2)
    rl c
    rla
    rl c
    rla
    cpl
    and $3F
    ret

@getDataB:
    in a, (Port_IOPort1)
    cpl
    and $3F
    ret
