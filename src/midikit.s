.include "x16.inc"
.include "macros.inc"

.macpack longbranch

.export midikit_init_engine
.export midikit_tick
.export midikit_play
.export midikit_stop
.export midikit_setmem
.export midikit_rewind

.import divide40_24
.import multiply16x16

.import numerator
.import denominator
.import div_result
.import multiplicand
.import multiplier
.import mult_result

.define IO_BASE $9F00
.define MIDI_SERIAL_DIVISOR 32

; Define the registers of the TL16C2550PFBR
.define sRHR 0 ; Receive Holding Register
.define sTHR 0 ; Transmit Holding Register
.define sIER 1 ; Interrupt Enable Register
.define sFCR 2 ; FIFO Control Register
.define sLCR 3 ; Line Control Register
.define sMCR 4 ; Modem Control Register
.define sLSR 5 ; Line Status Register
.define sMSR 6 ; Modem Status Register
.define sDLL 0 ; Divisor Latch LSB
.define sDLM 1 ; Divisor Latch MSB

; Define some bit masks for the registers
.define LCR_DLAB $80 ; Divisor Latch Access Bit
.define LCR_WLS8 $03 ; Word Length Select: 8 bits
.define FCR_FIFOE $01 ; FIFO Enable
.define FCR_RFIFOR $02 ; Receiver FIFO Reset
.define FCR_XFIFOR $04 ; Transmitter FIFO Reset
.define MCR_DTR $01 ; Data Terminal Ready
.define MCR_RTS $02 ; Request To Send
.define LSR_THRE $20 ; Transmitter Holding Register Empty
.define LSR_DR $01 ; Data Ready

.segment "MIDIKITLIB"

; Variables/state

midi_startbank:
	.byte 0
midi_startoffset:
	.word 0
midi_track_startbank:
	.byte 0
midi_track_startoffset:
	.word 0
midi_track_curbank:
	.byte 0
midi_track_curoffset:
	.word 0
midi_track_delta_frac:
	.byte 0
midi_track_delta:
	.dword 0
midi_track_prevstatus:
	.byte 0
midi_playable:
	.byte 0
midi_playing:
	.byte 0
chunklen:
	.dword 0
variable_length:
	.dword 0
midi_deltas_per_call_frac:
	.byte 0
midi_deltas_per_call:
	.word 0
midi_tempo:
	.dword 0
midi_divisions:
	.word 0

;...................
; serial_send_byte :
;============================================================================
; Arguments: .A = byte to send
; Returns: (none)
; Preserves: .A .X .Y
; Allowed in interrupt handler: yes
; ---------------------------------------------------------------------------
;
; This routine sends a byte over serial MIDI, waiting for a spot in the FIFO

.proc serial_send_byte: near
	pha
	phx
	lda IOsTHR
	cmp #$60
	bcc plxarts
	lda #LSR_THRE
	ldx #0
:   dex
	beq timeout
	bit IO_BASE
IOsLSR = * - 2
	beq :-
plasta:
	plx
	pla
	sta IO_BASE
IOsTHR = * - 2
	rts
timeout:
plxarts:
	plx
	pla
	rts

.endproc

;......................
; midikit_init_engine :
;============================================================================
; Arguments: .A = IO base offset
; Returns: (none)
; Preserves: (none)
; Allowed in interrupt handler: no
; ---------------------------------------------------------------------------
;
; This routine sets up the serial MIDI port and the engine itself

.proc midikit_init_engine: near
	php
	sei

	tax
	beq :+ ; don't initialize the zero device

	lda #LCR_DLAB
	sta IO_BASE + sLCR, x

	lda #<MIDI_SERIAL_DIVISOR
	sta IO_BASE + sDLL, x

	lda #>MIDI_SERIAL_DIVISOR
	sta IO_BASE + sDLM, x

	lda #LCR_WLS8
	sta IO_BASE + sLCR, x

	lda #(FCR_FIFOE | FCR_RFIFOR | FCR_XFIFOR)
	sta IO_BASE + sFCR, x

	lda #(MCR_DTR | MCR_RTS)
	sta IO_BASE + sMCR, x

	; disable interrupts
	stz IO_BASE + sIER, x
:
	txa
	clc
	adc #sLSR
	sta serial_send_byte::IOsLSR
	txa
	clc
	adc #sTHR
	sta serial_send_byte::IOsTHR

	stz midi_playable
	stz midi_playing

	plp
	rts
.endproc

.proc get_chunklen: near
	jsr fetch_indirect_byte
	sta chunklen+3
	jsr fetch_indirect_byte
	sta chunklen+2
	jsr fetch_indirect_byte
	sta chunklen+1
	jsr fetch_indirect_byte
	sta chunklen+0
	rts
.endproc

.proc get_variable_length_chunk: near
	jsr get_variable_length
	lda variable_length
	sta chunklen
	lda variable_length+1
	sta chunklen+1
	lda variable_length+2
	sta chunklen+2
	lda variable_length+3
	sta chunklen+3
	rts
.endproc

.proc send_chunk: near
loop:
	lda chunklen
	ora chunklen+1
	ora chunklen+2
	ora chunklen+3
	beq end
	jsr fetch_indirect_byte_decchunk
	jsr serial_send_byte
	bra loop
end:
	rts
.endproc

.proc rewind_indirect_byte: near
    dey
    cpy #$ff
    bne end

    lda midi_ptr+1
    cmp #$A0
    bne decit
    dec X16::Reg::RAMBank
    lda #$BF
    sta midi_ptr+1
    rts
decit:
    dec midi_ptr+1
end:
    rts
.endproc

.proc get_delta: near
    ; pull the next delta value out
    jsr get_variable_length

    ; store the first event with the delta saved
    clc
    lda variable_length
    adc midi_track_delta
    sta midi_track_delta
    lda variable_length+1
    adc midi_track_delta+1
    sta midi_track_delta+1
    lda variable_length+2
    adc midi_track_delta+2
    sta midi_track_delta+2
    lda variable_length+3
    adc midi_track_delta+3
    sta midi_track_delta+3

    rts
.endproc

.proc do_event_meta: near
	jsr serial_send_byte
	jsr fetch_indirect_byte
	pha
	jsr serial_send_byte
	jsr get_variable_length_chunk
	pla
	cmp #$2F
	beq end_of_track
	cmp #$51
	beq tempo
	jmp send_chunk
end_of_track:
	jsr send_chunk
	stz midi_playable
	stz midi_playing
	rts
tempo:
	stz midi_tempo+3
	jsr fetch_indirect_byte_decchunk
	sta midi_tempo+2
	jsr serial_send_byte
	jsr fetch_indirect_byte_decchunk
	sta midi_tempo+1
	jsr serial_send_byte
	jsr fetch_indirect_byte_decchunk
	sta midi_tempo+0
	jsr serial_send_byte
	phy
	jsr calc_deltas_per_call
	ply
	jmp send_chunk
.endproc

.proc midikit_tick: near
	lda X16::Reg::RAMBank
	pha

	lda midi_playing
	jeq end

	sec
	lda midi_track_delta_frac
	sbc midi_deltas_per_call_frac
	sta midi_track_delta_frac

	lda midi_track_delta
	sbc midi_deltas_per_call
	sta midi_track_delta

	lda midi_track_delta+1
	sbc midi_deltas_per_call+1
	sta midi_track_delta+1

	lda midi_track_delta+2
	sbc #0
	sta midi_track_delta+2

	lda midi_track_delta+3
	sbc #0
	sta midi_track_delta+3

	jpl end

	lda midi_track_curbank
	sta X16::Reg::RAMBank
	ldy midi_track_curoffset
	stz midi_ptr
	lda midi_track_curoffset+1
	sta midi_ptr+1

eventloop:
	lda midi_track_delta+3
	bpl save_ptr

	; we should be immediately after a delta
	jsr fetch_indirect_byte

	cmp #$80
	bcs normal_status

	jsr rewind_indirect_byte

	lda midi_track_prevstatus
normal_status:
	sta midi_track_prevstatus

	cmp #$C0
	bcc normal_event
	cmp #$E0
	bcc short_event
    cmp #$F0
    bcc normal_event      ; $80-$EF
    beq event_sysex       ; $F0
    cmp #$F7
    beq event_sysex       ; $F7
    cmp #$FF
    beq event_meta        ; $FF
event_error:
	stz midi_playable
	stz midi_playing
	jsr all_notes_off
	bra end
event_sysex:
	jsr serial_send_byte
	jsr get_variable_length_chunk
	jsr send_chunk
	bra next_event
event_meta:
	jsr do_event_meta
	bra next_event
normal_event:
	jsr serial_send_byte
	jsr fetch_indirect_byte
short_event:
	jsr serial_send_byte
	jsr fetch_indirect_byte
	jsr serial_send_byte
next_event:
	jsr get_delta
	bra eventloop
save_ptr:
	lda X16::Reg::RAMBank
	sta midi_track_curbank
	tya
	clc
	adc midi_ptr
	sta midi_track_curoffset
	lda midi_ptr+1
	adc #0
	sta midi_track_curoffset+1
end:
	pla
	sta X16::Reg::RAMBank
	rts
.endproc

.proc midikit_play: near
	lda midi_playable
	beq err
	lda #1
	sta midi_playing
	clc
	rts
err:
	sec
	rts
.endproc

.proc midikit_stop: near
	stz midi_playing
	php
	sei
	jsr all_notes_off
	plp
	rts
.endproc

.proc fetch_indirect_byte_decchunk: near
	jsr decrement_chunklen
.endproc
.proc fetch_indirect_byte: near
	lda midi_ptr
	beq fetch   ; midi_ptr is already page aligned
dec_y:          ; midi_ptr is not yet page aligned
	cpy #0
	beq to_y    ; the Y register is already zero
	dey         ; the Y register is not yet zero, make it zero
	inc midi_ptr
	bne dec_y
	lda midi_ptr+1
	inc
	cmp #$C0
	bcc storeptr_dec_y
	sbc #$20
	inc X16::Reg::RAMBank
storeptr_dec_y:
	sta midi_ptr+1
	bra dec_y
to_y:
	ldy midi_ptr  ; transfer the low byte of midi_ptr to the y register
	stz midi_ptr
fetch:
	lda $ffff,y
midi_ptr = * - 2
	iny
	bne done
	pha
	lda midi_ptr+1
	inc
	cmp #$C0
	bcc store_ptr
	sbc #$20
	inc X16::Reg::RAMBank
store_ptr:
	sta midi_ptr+1
	pla
done:
	rts
.endproc

midi_ptr = fetch_indirect_byte::midi_ptr

.proc get_variable_length: near
	stz variable_length
	stz variable_length+1
	stz variable_length+2
	stz variable_length+3

fetchloop:
	jsr fetch_indirect_byte
	bit #$80
	beq final
	and #$7F
	ora variable_length

	; 32-bit shift 7 times :(
	ldx #7
shiftloop:
	asl
	rol variable_length+1
	rol variable_length+2
	rol variable_length+3
	dex
	bne shiftloop
	sta variable_length

	bra fetchloop
final:
	ora variable_length
	sta variable_length
	rts
.endproc

.proc decrement_chunklen: near
	lda chunklen
	bne d0
	lda chunklen+1
	bne d1
	lda chunklen+2
	bne d2
	dec chunklen+3
d2:
	dec chunklen+2
d1:
	dec chunklen+1
d0:
	dec chunklen
	rts
.endproc

.proc all_notes_off: near
	ldx #0
chloop:
	txa
	ora #$b0
	jsr serial_send_byte

	; all notes off
	lda #123
	jsr serial_send_byte
	lda #0
	jsr serial_send_byte

	inx
	cpx #16
	bne chloop

	clc
	rts
.endproc

.proc panic: near
	jsr all_notes_off
	ldx #0
chloop:
	txa
	ora #$b0
	jsr serial_send_byte

    ; send reset all controllers
    lda #121
    jsr serial_send_byte
    lda #0
    jsr serial_send_byte

    ; send bank 0
    lda #0
    jsr serial_send_byte
    lda #0
    jsr serial_send_byte

    lda #$20
    jsr serial_send_byte
    lda #0
    jsr serial_send_byte

    ; reset program to 0 (as needed after a bank change)
	txa
	ora #$c0
	jsr serial_send_byte
    lda #0
    jsr serial_send_byte

	; TODO: reverb params might need a reset too

    ; reset to default PB range
	txa
    ora #$b0 ; controller
    jsr serial_send_byte
    lda #$64
    jsr serial_send_byte
    lda #$00
    jsr serial_send_byte
    lda #$65
    jsr serial_send_byte
    lda #$00
    jsr serial_send_byte
    lda #$06
    jsr serial_send_byte
    lda #2 ; two semitones
    jsr serial_send_byte

	inx
	cpx #16
	bne chloop

	clc
	rts
.endproc

.proc midikit_rewind: near

	jsr panic ; reset MIDI playback state

	lda midi_track_startbank
	sta X16::Reg::RAMBank

	ldy midi_track_startoffset
	stz midi_ptr
	lda midi_track_startoffset+1
	sta midi_ptr+1

	jsr get_variable_length
	stz midi_track_delta_frac
	lda variable_length
	sta midi_track_delta
	lda variable_length+1
	sta midi_track_delta+1
	lda variable_length+2
	sta midi_track_delta+2
	lda variable_length+3
	sta midi_track_delta+3

	lda X16::Reg::RAMBank
	sta midi_track_curbank
	sty midi_track_curoffset
	lda midi_ptr+1
	sta midi_track_curoffset+1

    ; set default tempo of 120 bpm
    DEFAULT_TEMPO = 500000

    lda #<(DEFAULT_TEMPO)
    sta midi_tempo+0
    lda #>(DEFAULT_TEMPO)
    sta midi_tempo+1
    lda #^(DEFAULT_TEMPO)
    sta midi_tempo+2
    stz midi_tempo+3

	jsr calc_deltas_per_call

	lda #1
	sta midi_playable
	clc
	rts
.endproc

.proc midikit_setmem: near
	; lay down the parameters immediately
	sta midi_startbank
	stx midi_startoffset
	sty midi_startoffset + 1

	sta X16::Reg::RAMBank
	stx midi_ptr
	sty midi_ptr+1

	ldy #0
	; Header "MThd"
	COMPARE_BYTES MThd, 4
	jcs error

	; 32-bit big-endian chunk length
	jsr get_chunklen

	; Format, expect 0 or 1 for now
	jsr fetch_indirect_byte_decchunk
	cmp #0
	bne error
	jsr fetch_indirect_byte_decchunk
	cmp #2
	bcs error

	; Number of tracks
	jsr fetch_indirect_byte_decchunk
	cmp #0
	bne error
	jsr fetch_indirect_byte_decchunk
	cmp #1 ; only one track supported
	bne error

	; Divisions
	jsr fetch_indirect_byte_decchunk
	bit #$80
	bne error
	and #$7F
	sta midi_divisions+1
	jsr fetch_indirect_byte_decchunk
	sta midi_divisions

	; We should be at a track header now
	COMPARE_BYTES MTrk, 4
	bcs error

	jsr get_chunklen

	lda X16::Reg::RAMBank
	sta midi_track_startbank
	sty midi_track_startoffset
	lda midi_ptr+1
	sta midi_track_startoffset+1
	stz midi_playable
	stz midi_playing

	jmp midikit_rewind
MThd:
	.byte "MThd"
MTrk:
	.byte "MTrk"
error:
	sec
	rts
.endproc


.proc calc_deltas_per_call: near
    ; dq = midi_divisions is the number of deltas per quarter note
    ; mt = midi_tempo is the number of microseconds per quarter note
    ; cf = midi_calls_per_frame (1) is the number of times
    ;    in a 1/60 second frame midi_playtick is called

    ; mf = 1000000/60; microseconds per frame
    ; dc = midi_deltas_per_call, the number we're trying to find

    ; fq = mf/mt = fraction of a quarter note per frame
    ;
    ; df = fq*dq = number of deltas per frame
    ; dc = df/cf = number of deltas per call
    ;
    ; dc = ((mf*dq)/mt)/cf

    ; first multiply dq by 16667 (mf)
    lda #<16667
    sta multiplicand
    lda #>16667
    sta multiplicand+1
    lda midi_divisions
    sta multiplier
    lda midi_divisions+1
    sta multiplier+1

    jsr multiply16x16

    ; In our math routine memory,
    ; mult_result overlaps numerator like this,
    ; sharing memory, so the numerator is
    ; already multiplied by 256 coming out of
    ; the multiplication
    ;
    ; mult_result   [ 0| 1| 2| 3]
    ; numerator  [ 0| 1| 2| 3| 4]
    ;
    ; We just have to initialize numerator+0
    stz numerator

    ; now divide that (mf*dq*256) by mt
    lda midi_tempo
    sta denominator
    lda midi_tempo+1
    sta denominator+1
    lda midi_tempo+2
    sta denominator+2

    jsr divide40_24
    ; div_result now contains the deltas per frame
    ; multiplied by 256

save_deltas:
    lda div_result
    sta midi_deltas_per_call_frac
    lda div_result+1
    sta midi_deltas_per_call
    lda div_result+2
    sta midi_deltas_per_call+1

end:
    rts
.endproc

