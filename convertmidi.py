#!/bin/env python3

import mido
import sys

om = mido.MidiFile(sys.argv[1])
nm = mido.MidiFile()
nm.tracks.append(mido.merge_tracks(om.tracks))
nm.type = 0
nm.ticks_per_beat = om.ticks_per_beat
nm.save(sys.argv[2])
