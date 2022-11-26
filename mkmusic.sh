#!/bin/bash

while [ true ]; do
	rm /tmp/xy*.mid
	rm /tmp/xy*.wav
	polygen /usr/share/polygen/music.grm > /tmp/xy.abc
       	abc2midi /tmp/xy.abc -Q 90

	for x in /tmp/xy*.mid; do
		echo dir /usr/share/midi/freepats/Tone_000/ > /tmp/wildmidi.cfg 
		echo bank 0 >> /tmp/wildmidi.cfg 
		ls /usr/share/midi/freepats/Tone_000/ | shuf | nl -v 0 >> /tmp/wildmidi.cfg 
		wildmidi -c /tmp/wildmidi.cfg -o $x.wav $x
	done

	inp="`find /tmp -maxdepth 1 -iname "xy*wav" -printf " -i \"%h/%f\" "`"

	ffmpeg -y $inp -filter_complex:a amerge /tmp/xy.mp3

	mpg123 /tmp/xy.mp3

	wait.exe
done

