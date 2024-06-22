.macpack longbranch

.include "x16.inc"

.segment "LOADADDR"
.word $0801

.segment "BASICSTUB"
.word entry-2
.byte $00,$00,$9e
.byte "2061"
.byte $00,$00,$00
.proc entry
	jmp main
.endproc

.scope midikit
.include "midikit.inc"
.endscope

.segment "BSS"
oldirq:
	.res 2

.segment "STARTUP"

.proc main
	lda #$68
	jsr midikit::midikit_init_engine

	jsr setup_handler

	lda #2
	sta X16::Reg::RAMBank

	lda #filename_end-filename
	ldx #<filename
	ldy #>filename

	jsr X16::Kernal::SETNAM

	lda #2
	ldx #8
	ldy #2
	jsr X16::Kernal::SETLFS

	ldx #$00
	ldy #$a0
	lda #0

	jsr X16::Kernal::LOAD

	ldx #$00
	ldy #$a0
	lda #2
	jsr midikit::midikit_setmem

	jsr midikit::midikit_play

	lda #0
	sta X16::Reg::ROMBank

	jsr X16::Kernal::PRIMM
	.byte "NOW PLAYING. PRESS RETURN TO FINISH.",13,0
	jsr X16::Kernal::BASIN

	jsr midikit::midikit_stop

	sei

	lda oldirq
	sta X16::Vec::IRQVec
	lda oldirq+1
	sta X16::Vec::IRQVec+1

	cli

	lda #4
	sta X16::Reg::ROMBank

	rts
filename:
	.byte "WILLIAM0.MID"
filename_end:
.endproc

.segment "CODE"

.proc setup_handler
	lda X16::Vec::IRQVec
	sta oldirq
	lda X16::Vec::IRQVec+1
	sta oldirq+1

	sei
	lda #<irqhandler
	sta X16::Vec::IRQVec
	lda #>irqhandler
	sta X16::Vec::IRQVec+1
	cli

	rts
.endproc

.proc irqhandler
	lda #35
	sta Vera::Reg::DCBorder
	lda #0
	jsr midikit::midikit_tick
	jmp (oldirq)
.endproc
