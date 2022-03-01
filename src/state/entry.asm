state.entry.update:
    ld a, $02
    ld (var.state), a

    jp waitInterrupt_LABEL_181_
