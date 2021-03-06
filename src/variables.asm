.enum $C001 export
_RAM_C001_ db
_RAM_C002_ db
.ende

.enum $C004 export
_RAM_C004_ db
mode_RAM_C005_ db
_RAM_C006_ db
_RAM_C007_ db
_RAM_C008_ db
_RAM_C009_ db
_RAM_C00A_ db
var.input.player1.data db
var.input.player1.debounced db
.ende

.enum $C00F export
var.input.player1.timer db
var.input.player2.data db
var.input.player2.debounced db
.ende

.enum $C014 export
var.input.player2.timer db
var.interrupt.handlerIndex db
var.timer dw
_RAM_C018_ db
.ende

.enum $C01A export
_RAM_C01A_ dw
var.state db
var.previousState db
.ende

.enum $C021 export
var.pallete.shouldUpdate db
var.palette._RAM_C022_ db
var.filteredPallete db
.ende

.enum $C026 export
_RAM_C026_ db
.ende

.enum $C02D export
_RAM_C02D_ db
.ende

.enum $C02F export
_RAM_C02F_ db
.ende

.enum $C063 export
var.palette dsb $10
_RAM_C073_ dsb $10
.ende

.enum $C0A3 export
var.fade.state db
.ende

.enum $C0A5 export
var.fade.progress db
var.fade.timer db
.ende

.enum $C0A8 export
_RAM_C0A8_ db
_RAM_C0A9_ db
_RAM_C0AA_ db
.ende

.enum $C0F8 export
_RAM_C0F8_ db
.ende

.enum $C100 export
    var.entities INSTANCEOF Entity $10
.ende

.enum $C300 export
_RAM_C300_ db
.ende

.enum $C340 export
_RAM_C340_ db
.ende

.enum $C3C0 export
_RAM_C3C0_ db
.ende

.enum $C400 export
_RAM_C400_ db
_RAM_C401_ db
.ende

.enum $C406 export
_RAM_C406_ db
.ende

.enum $C418 export
_RAM_C418_ db
_RAM_C419_ db
.ende

.enum $C441 export
_RAM_C441_ db
.ende

.enum $C460 export
_RAM_C460_ db
.ende

.enum $C470 export
_RAM_C470_ db
.ende

.enum $C4A1 export
_RAM_C4A1_ db
.ende

.enum $C4A7 export
_RAM_C4A7_ db
.ende

.enum $C4B0 export
_RAM_C4B0_ db
_RAM_C4B1_ db
.ende

.enum $C548 export
_RAM_C548_ db
.ende

.enum $C561 export
_RAM_C561_ db
.ende

.enum $C690 export
_RAM_C690_ db
_RAM_C691_ db
_RAM_C692_ db
_RAM_C693_ db
_RAM_C694_ db
_RAM_C695_ db
_RAM_C696_ db
_RAM_C697_ db
_RAM_C698_ db
level_RAM_C699_ db
_RAM_C69A_ db
_RAM_C69B_ db
_RAM_C69C_ db
_RAM_C69D_ db
_RAM_C69E_ db
_RAM_C69F_ db
_RAM_C6A0_ db
_RAM_C6A1_ db
_RAM_C6A2_ db
_RAM_C6A3_ db
_RAM_C6A4_ db
_RAM_C6A5_ db
_RAM_C6A6_ db
_RAM_C6A7_ db
optDifficulty_RAM_C6A8_ db
optBlockType_RAM_C6A9_ dw
optHigh_RAM_C6AB_ db
optLevel_RAM_C6AC_ db
_RAM_C6AD_ db
_RAM_C6AE_ dw
_RAM_C6B0_ db
_RAM_C6B1_ db
optMatches_RAM_C6B2_ db
_RAM_C6B3_ db
_RAM_C6B4_ db
_RAM_C6B5_ db
_RAM_C6B6_ db
_RAM_C6B7_ db
_RAM_C6B8_ db
_RAM_C6B9_ dw
_RAM_C6BB_ db
_RAM_C6BC_ db
_RAM_C6BD_ db
_RAM_C6BE_ dw
_RAM_C6C0_ db
.ende

.enum $C6C2 export
_RAM_C6C2_ db
_RAM_C6C3_ db
_RAM_C6C4_ db
_RAM_C6C5_ db
.ende

.enum $C6CA export
_RAM_C6CA_ db
_RAM_C6CB_ db
_RAM_C6CC_ db
_RAM_C6CD_ db
_RAM_C6CE_ db
_RAM_C6CF_ db
_RAM_C6D0_ dw
.ende

.enum $CD00 export
nametable_RAM_CD00_ db
_RAM_CD01_ db
.ende

.enum $CD58 export
_RAM_CD58_ db
.ende

.enum $CD62 export
_RAM_CD62_ db
.ende

.enum $CDD0 export
_RAM_CDD0_ db
.ende

.enum $CDEA export
_RAM_CDEA_ db
.ende

.enum $CE44 export
_RAM_CE44_ db
.ende

.enum $CE54 export
_RAM_CE54_ db
.ende

.enum $CED6 export
_RAM_CED6_ db
.ende

.enum $CF56 export
_RAM_CF56_ db
.ende

.enum $D000 export
_RAM_D000_ db
.ende

.enum $DD00 export
_RAM_DD00_ db
speed_RAM_DD01_ db
speed_RAM_DD02_ db
_RAM_DD03_ db
var.audio.request_DD04 db
var.audio.request_DD05 db
.ende

.enum $DD07 export
_RAM_DD07_ db
_RAM_DD08_ db
_RAM_DD09_ db
_RAM_DD0A_ db
_RAM_DD0B_ db
.ende

.enum $DD0E export
audioFadeOutTimer_RAM_DD0E_ db
audio_RAM_DD0F_ db
_RAM_DD10_ db
.ende

.enum $DD40 export
_RAM_DD40_ db
.ende

.enum $DD48 export
_RAM_DD48_ db
.ende

.enum $DD4A export
_RAM_DD4A_ db
.ende

.enum $DD70 export
_RAM_DD70_ db
.ende

.enum $DDA0 export
_RAM_DDA0_ db
.ende

.enum $DDD0 export
_RAM_DDD0_ db
.ende

.enum $DE00 export
_RAM_DE00_ db
.ende

.enum $DE30 export
_RAM_DE30_ db
.ende

.enum $DE60 export
_RAM_DE60_ db
.ende

.enum $FFFC export
_RAM_FFFC_ db
.ende

.enum $FFFF export
_RAM_FFFF_ db
.ende
