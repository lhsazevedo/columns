fade.in:
    call loadColors
    ld a, $01
    ld (var.fade.state), a
    
    ; Clear filtered pallete
    ld hl, v_filteredPalette
    ld de, v_filteredPalette + 1
    ld (hl), $00
    call ldi31

    ld hl, var.pallete.shouldUpdate
    set 0, (hl)

    ld a, $04
    ld (var.fade.timer), a

    ret

fade.out:
    ld a, $02
    ld (var.fade.state), a

    ld hl, v_filteredPalette
    ld de, v_palette
    ld bc, $0020
    call ldi32

    ld a, $04
    ld (var.fade.timer), a

    ret

fade.update:
    ; Fade effect is updated every 4th
    ; frame, we use a timer to handle this.
    ld hl, var.fade.timer
    dec (hl)
    ret nz

    ; Update frame reached, reset timer
    ld a, $04
    ld (var.fade.timer), a

    ; If state isn't set, do nothing
    ld a, (var.fade.state)
    or a
    ret z

    call @doUpdate

    ; Now we request a palette update
    ld hl, var.pallete.shouldUpdate
    set 0, (hl)

    ; Increment progress and return if fading hasn't ended yet
    ld hl, var.fade.progress
    inc (hl)
    ld a, (hl)
    cp $04
    ret nz

    ; If we reach here, the fade in our fade out
    ; transition is over and we can reset our variables.
    xor a
    ld (var.fade.progress), a
    ld (var.fade.state), a
    ret

@doUpdate:
    ld a, (var.fade.state)
    dec a

    ; If fade in
    jr nz, @else
        ld a, (var.fade.progress)
        ld d, a
        ld a, $03
        sub d
        jr @endif
    @else:
        ld a, (var.fade.progress)
    @endif:

    ld c, a
    ld de, v_palette
    ld hl, v_filteredPalette

    ; Number of colors
    ld b, $20

@colorLoop:
    push bc

    ; Fade red component
    ld a, (de)
    and 0b00000011
    sub c
    jr nc, @endif2
        xor a
    @endif2:
    ld (hl), a

    ; Fade green component
    rlc c
    rlc c
    ld a, (de)
    and 0b00001100
    sub c
    jr nc, @endif3
        xor a
    @endif3:
    or (hl)
    ld (hl), a

    ; Fade blue component
    rlc c
    rlc c
    ld a, (de)
    and 0b00110000
    sub c
    jr nc, @endif4
        xor a
    @endif4:
    or (hl)
    ld (hl), a

    inc hl
    inc de
    pop bc
    djnz @colorLoop

    ret
