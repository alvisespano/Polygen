#!/bin/bash

while [ true ]; do
	rm /tmp/xy*.mid
	rm /tmp/xy*.wav
	rm /tmp/xy*.mp3
	rm /tmp/song.mp3

	polygen ./music.grm > /tmp/xy.abc
       	abc2midi /tmp/xy.abc -Q 90
	for x in /tmp/xy*.mid; do
		echo bank 0 >> /tmp/wildmidi.cfg 
		ls /opt/polygen_examples/Tone_001/*pat | shuf | nl -v 0 >> /tmp/wildmidi.cfg 
		wildmidi -c /tmp/wildmidi.cfg -o $x.wav $x
	done

	polygen ./music.grm > /tmp/xyd.abc
       	abc2midi /tmp/xyd.abc -Q 135
	for x in /tmp/xyd*.mid; do
		echo drumbank 0 >> /tmp/wildmidi.cfg 
		ls /opt/polygen_examples/Drum_001/*pat | shuf | nl -v 0 >> /tmp/wildmidi.cfg 
		wildmidi -c /tmp/wildmidi.cfg -o $x.wav $x
	done

	for x in /tmp/xy*wav;do
		ffmpeg -i $x -filter:a loudnorm $x.mp3
	done

	inp="`find /tmp -maxdepth 1 -iname "xy*.mp3" -printf " -stream_loop -1 -t 180s -i \"%h/%f\" "`"

	ffmpeg $inp -filter_complex:a amerge=inputs=`ls /tmp/xy*.mp3 | wc -l` /tmp/song.mp3

	cp /tmp/song.mp3 ~/$RANDOM.mp3

	mpg123 /tmp/song.mp3

	#wait.exe
done

