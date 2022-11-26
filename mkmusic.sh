#!/bin/bash

while [ true ]; do
	rm /tmp/xy*.mid
	polygen /usr/share/polygen/music.grm > /tmp/xy.abc
       	abc2midi /tmp/xy.abc -Q 90
	echo dir /usr/share/midi/freepats/Tone_000/ > /tmp/wildmidi.cfg 
	echo bank 0 >> /tmp/wildmidi.cfg 
	ls /usr/share/midi/freepats/Tone_000/ | shuf | nl -v 0 >> /tmp/wildmidi.cfg 
	cat /tmp/xy*.mid > /tmp/abc.mid
	wildmidi -c /tmp/wildmidi.cfg /tmp/abc.mid
	#wildmidi /tmp/xy.mid
	wait.exe
done

