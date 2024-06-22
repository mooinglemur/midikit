.include "x16.inc"
.include "macros.inc"

.macpack longbranch

.export midikit_init_engine
.export midikit_tick
.export midikit_play
.export midikit_stop
.export midikit_setmem
.export midikit_rewind
.export midikit_setloop
.export midikit_zcm_play
.export midikit_zcm_setmem
.export midikit_zcm_stop

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
pcm_cur_bank:
	.byte 0
pcm_cur_addr:
	.word 0
pcm_remain:
	.byte 0,0,0
zcm_bank:
	.byte 0
zcm_addr:
	.word 0
pcm_busy:
	.byte 0

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
	sta $9fb9
	sta IO_BASE
IOsTHR = * - 2
	rts
timeout:
plxarts:
	plx
	pla
	sta $9fba
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
	stz pcm_busy

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

.proc send_chunk
	lda #1
	sta process_chunk_check
	; fall through
.endproc
.proc process_chunk: near
loop:
	lda chunklen
	ora chunklen+1
	ora chunklen+2
	ora chunklen+3
	beq end
	jsr fetch_indirect_byte_decchunk
	ldx #$00
check := *-1
	beq loop
	jsr serial_send_byte
	bra loop
end:
	clc ; clear means we're at the delta
	rts
.endproc

process_chunk_check = process_chunk::check

.proc eat_chunk: near
	stz process_chunk_check
	bra process_chunk
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
	jsr fetch_indirect_byte
	pha
	jsr get_variable_length_chunk
	pla
	cmp #$2F
	beq end_of_track
	cmp #$51
	beq tempo
	jmp eat_chunk
end_of_track:
	lda #$00
loopable = * - 1
	bne loopit
	stz midi_playable
	stz midi_playing
	rts
loopit:
	jmp _rewind
tempo:
	stz midi_tempo+3
	jsr fetch_indirect_byte_decchunk
	sta midi_tempo+2
	jsr fetch_indirect_byte_decchunk
	sta midi_tempo+1
	jsr fetch_indirect_byte_decchunk
	sta midi_tempo+0
	phy
	jsr calc_deltas_per_call
	ply
	jmp eat_chunk
.endproc

loopable := do_event_meta::loopable

.proc midikit_tick: near
	lda X16::Reg::RAMBank
	pha

	jsr _pcm_player

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

	lda #10
	sta $9fbb
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
;	jsr all_sound_off
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
	jsr all_sound_off
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

.proc all_sound_off: near
	ldx #0
chloop:
	txa
	ora #$b0
	jsr serial_send_byte

	; all sound off
	lda #120
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
	jsr all_sound_off
	rts
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


.proc midikit_setloop: near
	sta loopable
	rts
.endproc

.proc _rewind: near
	php
	sei

;	jsr panic ; reset MIDI playback state

	lda midi_track_startbank
	sta X16::Reg::RAMBank

	ldy midi_track_startoffset
	stz midi_ptr
	lda midi_track_startoffset+1
	sta midi_ptr+1

    ; set default tempo of 120 bpm
    DEFAULT_TEMPO = 500000

    lda #<(DEFAULT_TEMPO)
    sta midi_tempo+0
    lda #>(DEFAULT_TEMPO)
    sta midi_tempo+1
    lda #^(DEFAULT_TEMPO)
    sta midi_tempo+2
    stz midi_tempo+3

	phy
	jsr calc_deltas_per_call
	ply

	lda #1
	sta midi_playable
	plp
	rts
.endproc

.proc midikit_rewind: near
	php
	sei
	jsr _rewind

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
	plp
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

;.....................
; midikit_zcm_setmem :
;============================================================================
; Arguments: .X .Y = data pointer, .A = ram bank
; Returns: (none)
; Allowed in interrupt handler: no
; ---------------------------------------------------------------------------
;
; Sets the start of memory for a digital sample (ZCM format)
.proc midikit_zcm_setmem: near
	sta zcm_bank
	stx zcm_addr
	sty zcm_addr+1
	rts
.endproc

;...................
; midikit_zcm_play :
;============================================================================
; Arguments: .A = volume
; Returns: (none)
; Allowed in interrupt handler: no
; ---------------------------------------------------------------------------
;
; Begins playback of a ZCM digital sample
.proc midikit_zcm_play: near
	and #$0f
	ora #$80
	sta VR

	php
	sei
	lda zcm_bank
	sta X16::Reg::RAMBank
	ldy zcm_addr
	stz midi_ptr
	lda zcm_addr+1
	sta midi_ptr+1

	ldx #3
check_sig:
	jsr fetch_indirect_byte
	cmp #$00
	bne end
	dex
	bne check_sig

	jsr fetch_indirect_byte
	sta pcm_remain
	jsr fetch_indirect_byte
	sta pcm_remain+1
	jsr fetch_indirect_byte
	sta pcm_remain+2

	jsr fetch_indirect_byte
	and #$30 ; geometry
	ora #$8f
VR = * - 1
	sta Vera::Reg::AudioCtrl
	sta pcm_busy

	jsr fetch_indirect_byte
	sta Vera::Reg::AudioRate

	lda X16::Reg::RAMBank
	sta pcm_cur_bank

	sty pcm_cur_addr
	lda midi_ptr+1
	sta pcm_cur_addr+1
end:
	plp
	rts
.endproc

;...................
; midikit_zcm_stop :
;============================================================================
; Arguments: (none)
; Returns: (none)
; Allowed in interrupt handler: no
; ---------------------------------------------------------------------------
;
; Stops playback of a ZCM if one is playing
.proc midikit_zcm_stop: near
	stz pcm_busy
	lda #$80
	tsb Vera::Reg::AudioCtrl
	rts
.endproc

;.............
; _load_fifo :
;============================================================================
; Arguments: .XY = number of bytes to read and send
; Returns: (none)
; Preserves: (none)
; Allowed in interrupt handler: yes
; ---------------------------------------------------------------------------
;
; Imported from ZSound, a very efficient FIFO filler routine
; starts at pcm_cur_* and then updates their values at the end
.proc _load_fifo: near
	__CPX		= $e0	; opcode for cpx immediate
	__BNE		= $d0

	; self-mod the page of the LDA below to the current page of pcm_cur_addr+1
	lda pcm_cur_addr+1
	sta data_page0
	sta data_page1
	sta data_page2
	sta data_page3

	; page-align
	txa              ;.A now holds the low-byte of n-bytes to copy
	ldx pcm_cur_addr ;.X now points at the page-aligned offset
	; add the delta to bytes_left
	clc
	adc pcm_cur_addr
	sta bytes_left
	bcc :+
	iny
:	lda pcm_cur_bank ; load the bank we'll be reading from
	sta X16::Reg::RAMBank
	; determine whether we have > $FF bytes to copy. If $100 or more, then
	; use the full-page dynamic comparator. Else use the last-page comparator.
	cpy #0
	beq last_page   ; if 0, then use the last_page comparator.
	; self-mod the instruction at dynamic_comparator to:
	; BNE copy_byte
	lda #__BNE
	sta dynamic_comparator
	lda #.lobyte(copy_byte0-dynamic_comparator-2)
	sta dynamic_comparator+1
	; compute num-steps % 4 (the mod4 is done by shifting the 2 LSB into N and C)
	txa
enter_loop:
	ror
	ror
	bcc :+
	bmi copy_byte3  ; 18
	bra copy_byte2  ; 20
:	bmi copy_byte1  ; 19

copy_byte0:
	lda $FF00,x
	data_page0 = (*-1)
	sta Vera::Reg::AudioData
	inx
copy_byte1:
	lda $FF00,x
	data_page1 = (*-1)
	sta Vera::Reg::AudioData
	inx
copy_byte2:
	lda $FF00,x
	data_page2 = (*-1)
	sta Vera::Reg::AudioData
	inx
copy_byte3:
	lda $FF00,x
	data_page3 = (*-1)
	sta Vera::Reg::AudioData
	inx
dynamic_comparator:
	bne copy_byte0
	; the above instruction is modified to CPX #(bytes_left) on the last page of data
	bne copy_byte0  ; branch for final page's CPX result.
	cpx #0
	bne done        ; .X can only drop out of the loop on non-zero during the final page.
	; Thus X!=0 means we just finished the final page. Done.
	; advance data pointer before checking if done on a page offset of zero.
	lda data_page0
	inc
	cmp #$c0
	beq do_bankwrap
no_bankwrap:
	; update the self-mod for all 4 iterations of the unrolled loop
	sta data_page0
	sta data_page1
	sta data_page2
	sta data_page3
check_done:
	cpy #0		; .Y = high byte of "bytes_left"
	beq done	; .X must be zero as well if we're here. Thus 0 bytes left. Done.
	dey
	bne copy_byte0	; more than one page remains. Continue with full-page mode copy.
last_page:
	lda bytes_left
	beq done		; if bytes_left=0 then we're done at offset 0x00, so exit.
	; self-mod the instruction at dynamic_comparator to be:
	; CPX #(bytes_left)
	sta dynamic_comparator+1
	lda #__CPX
	sta dynamic_comparator
	; Compute the correct loop entry point with the new exit index
	; i.e. the last page will start at x == 0, but we won't necessarily
	; end on a value x % 4 == 0, so the first entry from here into
	; the 4x unrolled loop is potentially short in order to make up
	; for it.
	; Find: bytes_left - .X
	txa
	eor #$ff
	sec	; to carry in the +1 for converting 2s complement of .X
	adc bytes_left
	; .A *= -1 to align it with the loop entry jump table
	eor #$ff
	inc
	bra enter_loop

done:
	ldy X16::Reg::RAMBank
	lda data_page0
	sta pcm_cur_addr+1
	stx pcm_cur_addr
	sty pcm_cur_bank
	rts

do_bankwrap:
	lda #$a0
	inc X16::Reg::RAMBank
	bra no_bankwrap

bytes_left:
	.byte 0
.endproc


;..............
; _pcm_player :
;============================================================================
; Arguments: (none)
; Returns: (none)
; Preserves: (none)
; Allowed in interrupt handler: yes
; ---------------------------------------------------------------------------
;
; Checks to see if any PCM events are in progress, then calculates
; how many bytes to send to the FIFO, then does so
.proc _pcm_player: near
	ldx pcm_busy
	jeq end ; nothing is playing

	ldx Vera::Reg::AudioRate
	stx RR ; self mod to restore the rate if we happen to zero it to do the
	       ; initial load
	dex
	lda Vera::Reg::ISR
	and #$08 ; AFLOW
	beq slow ; AFLOW is clear, send slow version (if rate > 0)
fast:
	cpx #$ff
	bne :+
	ldx #$7f
:	lda pcmrate_fast,x
	bra calc_bytes
slow:
	cpx #$ff
	bne :+
	rts ; AFLOW is clear and rate is 0, don't bother feeding	
:	lda pcmrate_slow,x
calc_bytes:
	; do the << 2 base amount
	stz tmp_count+1
	asl
	rol tmp_count+1
	asl
	rol tmp_count+1
	sta tmp_count
	lda Vera::Reg::AudioCtrl
	and #$10
	beq no_stereo
	asl tmp_count
	rol tmp_count+1
no_stereo:
	lda Vera::Reg::AudioCtrl
	and #$20
	beq no_16bit
	asl tmp_count
	rol tmp_count+1
no_16bit:
	; If the fifo is completely empty, change the rate to 0 temporarily
	; so that the FIFO can be filled without it immediately starting
	; to drain
	bit Vera::Reg::AudioCtrl
	bvc :+
	stz Vera::Reg::AudioRate
:	lda pcm_remain+2
	bne normal_load ; if high byte is set, we definitely have plenty of bytes
	; Do a test-subtract to see if we would go over
	lda pcm_remain+0
	sec
	sbc tmp_count
	lda pcm_remain+1
	sbc tmp_count+1
	bcs normal_load ; borrow clear, sufficient bytes by default

	; we have fewer bytes remaining than we were going to send
	ldx pcm_remain+0
	ldy pcm_remain+1

	; so the PCM blitting is done. Mark the pcm channel as available
	stz pcm_busy
	bra loadit
normal_load:
	; decrement remaining
	lda pcm_remain+0
	sec
	sbc tmp_count
	sta pcm_remain+0
	lda pcm_remain+1
	sbc tmp_count+1
	sta pcm_remain+1
	lda pcm_remain+2
	sbc #0
	sta pcm_remain+2

	ldx tmp_count
	ldy tmp_count+1
loadit:
	jsr _load_fifo
	lda #$80 ; this is self-mod to restore the rate in case we loaded while empty and temporarily set the rate to zero
RR = *- 1
	sta Vera::Reg::AudioRate
end:
	rts
tmp_count:
	.byte 0,0
.endproc


; ZSound-derived FIFO-fill LUTs
pcmrate_fast: ; <<4 for 16+stereo, <<3 for 16|stereo, <<2 for 8+mono
	.byte $03,$04,$06,$07,$09,$0B,$0C,$0E,$10,$11,$13,$15,$16,$17,$19,$1A
	.byte $1C,$1E,$1F,$21,$22,$24,$26,$27,$29,$2A,$2C,$2E,$2F,$31,$33,$34
	.byte $36,$37,$39,$3B,$3C,$3E,$3F,$41,$43,$44,$46,$47,$49,$4B,$4C,$4E
	.byte $50,$51,$53,$54,$56,$58,$59,$5B,$5C,$5E,$60,$61,$63,$65,$66,$68
	.byte $69,$6B,$6D,$6E,$70,$71,$73,$75,$76,$78,$79,$7B,$7D,$7E,$80,$82
	.byte $83,$85,$86,$88,$8A,$8B,$8D,$8E,$90,$92,$93,$95,$97,$98,$9A,$9B
	.byte $9D,$9F,$A0,$A2,$A3,$A5,$A7,$A8,$AA,$AC,$AD,$AF,$B0,$B2,$B4,$B5
	.byte $B7,$B8,$BA,$BC,$BD,$BF,$C0,$C2,$C4,$C5,$C7,$C9,$CA,$CC,$CD,$CF

pcmrate_slow:
	.byte $01,$02,$04,$05,$07,$09,$0A,$0C,$0E,$0F,$11,$12,$14,$16,$17,$19
	.byte $1A,$1C,$1E,$1F,$21,$22,$24,$26,$27,$29,$2A,$2C,$2E,$2F,$31,$32
	.byte $34,$36,$37,$39,$3A,$3C,$3D,$3F,$41,$42,$44,$45,$47,$49,$4A,$4C
	.byte $4D,$4F,$51,$52,$54,$55,$57,$58,$5A,$5C,$5D,$5F,$60,$62,$64,$65
	.byte $67,$68,$6A,$6C,$6D,$6F,$70,$72,$74,$75,$77,$78,$7A,$7B,$7D,$7F
	.byte $80,$82,$83,$85,$87,$88,$8A,$8B,$8D,$8F,$90,$92,$93,$95,$96,$98
	.byte $9A,$9B,$9D,$9E,$A0,$A2,$A3,$A5,$A6,$A8,$AA,$AB,$AD,$AE,$B0,$B1
	.byte $B3,$B5,$B6,$B8,$BA,$BC,$BE,$BF,$C1,$C2,$C4,$C6,$C7,$C9,$CA,$CC
