# MIDIKit
MIDI playback engine for the Commander X16

## Overview

MIDIKit is a MIDI playback library for the Commander X16 meant for sending MIDI data via 16550A-style serial UARTs.  Features include:

* Playback of MIDI Format 0 (type 0, or single track) files from high RAM
* Optional looping once it reaches the end
* Pausing and resuming playback
* ZCM playback support for VERA PCM.

## Building and using in your project

This library was written for the `cc65` suite.  As of the writing of this documentation, it is geared toward including in assembly language projects.

To build the library, run  
`make`  
from the main project directory. This will create `lib/midikit.lib`, which you can build into your project.

You will likely want to include the file `src/midikit.inc` into your project as well for the library's label names.

## Alternative builds

For non-ca65/cc65 projects, there is another option. The build can produce binary blobs `lib/8010.bin` and `lib/8030.bin` by calling
`make incbin`
One of these files can be included at origin $0810 or $0830 in your project.  The jump table addresses can be found the file `src/midikit8010.inc` (or `src/midikit8030.inc`).

## Prerequisites

This library requires a custom linker config in order to specify two custom segments.  This documentation assumes you are familiar with creating custom cc65 linker configs.

Specifically, this library requires the `MIDIKITLIB` segment. The `MIDIKITLIB` **must** be located in low RAM.

NOTE: this is an incomplete linker config file, but rather a relevant example of what must be in a custom one.  You can copy the stock cx16.cfg one and make sure it includes the new custom segment.

```
MEMORY {
    ...
    MAIN:     file = %O, start = $0801,  size = $96FF;
    HIRAM:    file = "", start = $A000,  size = $2000;
    ...
}

SEGMENTS {
    ...
    CODE:        load = MAIN,     type = ro;
    MIDIKITLIB:  load = MAIN,     type = ro;
    ...
}
```

## API Quick Reference

### API calls for main part of the program (ZSM)

All calls except for `midikit_tick` are meant to be called from the main loop of the program. `midikit_tick` is the only routine that is safe to call from IRQ.

---
#### `midikit_init_engine`
```
Inputs: .A = IOBASE offset from $9F00 at which to find the MIDI UART,
        .X = number of serial bytes to send before deferring until the next tick.
```
This routine *must* be called once before any other library routines are called in order to initialize the state of the engine.

If .X is not 0, MIDIKit will cap the number of bytes sent over the serial port to this value on each tick, returning and deferring processing until the next tick if this value is reached or exceeded after a MIDI event. This prevents obnoxiously busy MIDI files from monopolizing the interrupt for more than a frame, which lags the game and the music. A value around 30 or less is recommended, which is about 3/5 of the raster time for the MIDI rate of 3125 bytes per second at 60 frames per second. You can lower this value if you need more CPU time for game logic, at the risk of the music sounding either "mushy" or "jittery" in busy sections.

In extreme cases with a lowered value of .X, an extremely busy song will repeatedly fall behind and catch up.

If .X = 0, this logic is disabled.

---
#### `midikit_setmem`
```
Inputs: .A = RAM bank, .X .Y = memory location (lo hi)
Outputs: carry set if error, clear otherwise
```

This function sets up the song pointers and parses the header based on a MIDI file that was previously loaded into RAM. If the song is deemed valid, it marks it as playable.  

If carry is set, an invalid MIDI file was passed in. **NOTE:** MIDIKit only supports MIDI Format 0 (type 0) files (single track). To convert a MIDI Format 1 file to Format 0, you can use this [Python script](convertmidi.py)

---

#### `midikit_play`
```
Inputs: (none)
```
Starts playback of a song.  If `midikit_stop` was called, this function continues playback from the point that it was stopped.  

---
#### `midikit_stop`
```
Inputs: (none)
```
Pauses or stops playback of a song. Playback can optionally be resumed from that point later with `midikit_play`.

---
#### `midikit_setloop`
```
Inputs: .A = loop flag
```
Sets the loopability of the MIDI file. If .A is nonzero, the playback will continue from the beginning of the song once the end is reached, otherwise the playback will stop.

---
#### `midikit_rewind`
```
Inputs: (none)
Outputs: (none)
```
Restarts the currently loaded or playing song from the beginning.


---
#### `midikit_zcm_setmem`
```
Inputs: .A = RAM bank, .X .Y = memory location (lo hi)
Outputs: (none)
```

This function informs MIDIKit of the memory location of a previously loaded ZCM file. It does not affect playback of a currently-playing sample, if any are in progress.

---
#### `midikit_zcm_play`
```
Inputs: .A = Volume (0-15)
Outputs: (none)
```

Triggers playback of a ZCM file, whose address was previously set via `midikit_zcm_setmem`.

---
#### `midikit_zcm_stop`
```
Inputs: (none)
Outputs: (none)
```

Stops playback of a ZCM file, if any are in progress.


### Interrupt-safe routines

---
#### `midikit_tick`
```
Inputs: (none)
Outputs: (none)
```

This routine handles everything that is necessary to play the currently active song, and to feed the PCM FIFO if any PCM is in progress.

Call this routine once per ~60 Hz frame.  You will usually want to do this at the end of your interrupt handler routine.


