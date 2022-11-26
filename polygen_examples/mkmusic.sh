#!/bin/bash

export keys=(C D E F G A B)
export key=${keys[$(( $RANDOM % ${#keys[*]} ))]}

doit() {
	cp ./music.grm.tmpl /tmp
	echo "Init ::= \"X:$RANDOM\nK:$key\n\";" >> /tmp/music.grm.tmpl
	echo $1 ";" >> /tmp/music.grm.tmpl
	polygen /tmp/music.grm.tmpl > /tmp/xy.abc
       	abc2midi /tmp/xy.abc -o /tmp/xy.mid -Q $4
	echo $3 > /tmp/wildmidi.cfg 
	ls $2/*pat | shuf | nl -v 0 >> /tmp/wildmidi.cfg 
	wildmidi -c /tmp/wildmidi.cfg -o `mktemp`.xy.wav /tmp/xy.mid
}

while [ true ]; do
	rm /tmp/*xy*.mid
	rm /tmp/*xy*.wav
	rm /tmp/*xy*.mp3
	rm /tmp/song.mp3

	doit "Note ::= (\"c\"|\"d\"|\"e\"|\"f\"|\"g\"|\"a\"|\"b\") ^[\"/\"|\"2\"]" "/opt/polygen_examples/Tone_001" "bank 0" 90
	doit "Note ::= (\"C\"|\"D\"|\"E\"|\"F\"|\"G\"|\"A\"|\"B\") ^[\"2\"|\"3\"|\"4\"]" "/opt/polygen_examples/Tone_001" "bank 0" 90
	doit "Note ::= (\"C\"|\"D\"|\"E\"|\"F\"|\"G\"|\"A\"|\"B\") ^(\"2\"|\"3\"|\"4\")" "/opt/polygen_examples/Drum_001" "drumbank 0" 180
	doit "Note ::= (\"C\"|\"D\"|\"E\"|\"F\"|\"G\"|\"A\"|\"B\") ^(\"2\"|\"3\"|\"4\")" "/opt/polygen_examples/Drum_001" "drumbank 0" 180

	for x in /tmp/*xy.wav;do
		ffmpeg -i $x -filter:a loudnorm $x.mp3
	done

	inp="`find /tmp -maxdepth 1 -iname "*xy*.mp3" -printf " -stream_loop -1 -t 180s -i \"%h/%f\" "`"

	ffmpeg $inp -filter_complex:a amerge=inputs=`ls /tmp/*xy*.mp3 | wc -l` /tmp/song.mp3

	cp /tmp/song.mp3 ~/$RANDOM.mp3

	mpg123 /tmp/song.mp3

	#wait.exe
done

