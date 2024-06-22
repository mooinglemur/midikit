.ifndef __MACROS_INC__
.define __MACROS_INC__

RAMBank = 0
ROMBank = 1

.macro COMPARE_BYTES s1, len
.scope
    lda #<s1
    sta MM
    lda #>s1
    sta MM+1

    ldx #0
loop:
    jsr fetch_indirect_byte
    cmp $ffff,x
MM = *-2
    bne fail
    inx
    cpx #len
    bcc loop
done:
    clc
    bra end
fail:
    sec
end:
.endscope
.endmacro


.endif
