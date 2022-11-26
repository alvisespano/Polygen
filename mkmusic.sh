#!/bin/bash

while [ true ]; do
	rm /tmp/xy*.mid
	rm /tmp/xy*.wav
	rm /tmp/xy*.mp3

	polygen /usr/share/polygen/music.grm > /tmp/xy.abc
       	abc2midi /tmp/xy.abc -Q 90
	for x in /tmp/xy*.mid; do
		echo dir /usr/share/midi/freepats/Tone_000/ > /tmp/wildmidi.cfg 
		echo bank 0 >> /tmp/wildmidi.cfg 
		ls /usr/share/midi/freepats/Tone_000/ | shuf | nl -v 0 >> /tmp/wildmidi.cfg 
		wildmidi -c /tmp/wildmidi.cfg -o $x.wav $x
	done

	polygen /usr/share/polygen/music.grm > /tmp/xyd.abc
       	abc2midi /tmp/xyd.abc -Q 180
	for x in /tmp/xyd*.mid; do
		echo dir /usr/share/midi/freepats/Drum_000/ > /tmp/wildmidi.cfg 
		echo drumbank 0 >> /tmp/wildmidi.cfg 
		ls /usr/share/midi/freepats/Drum_000/ | shuf | nl -v 0 >> /tmp/wildmidi.cfg 
		wildmidi -c /tmp/wildmidi.cfg -o $x.wav $x
	done

	for x in /tmp/xy*wav;do
		ffmpeg -i $x -filter:a loudnorm $x.mp3
	done

	inp="`find /tmp -maxdepth 1 -iname "xy*.mp3" -printf " -stream_loop -1 -t 120s -i \"%h/%f\" "`"

	ffmpeg -y $inp -filter_complex:a amerge=inputs=`ls /tmp/xy*.mp3 | wc -l` /tmp/song.mp3

	mpg123 /tmp/song.mp3

	wait.exe
done

