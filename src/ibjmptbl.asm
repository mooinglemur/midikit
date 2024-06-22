.include "midikit.inc"

.segment "JMPTBL"
jmp midikit_init_engine ; $0810 / $0830 / $8C00
jmp midikit_tick        ; $0813 / $0833 / $8C03
jmp midikit_play        ; $0816 / $0836 / $8C06
jmp midikit_stop        ; $0819 / $0839 / $8C09
jmp midikit_rewind      ; $081C / $083C / $8C0C
jmp midikit_setmem      ; $081F / $083F / $8C0F
jmp midikit_setloop     ; $0822 / $0842 / $8C12
jmp midikit_zcm_setmem  ; $0825 / $0845 / $8C15
jmp midikit_zcm_play    ; $0828 / $0848 / $8C18
jmp midikit_zcm_stop    ; $082B / $084B / $8C1B
