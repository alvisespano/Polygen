#!/bin/bash

shopt -s extglob

doit() {
	cp ./music.grm.tmpl /tmp
	echo "Init ::= \"X:$RANDOM\nK:$key\n\";" >> /tmp/music.grm.tmpl
	echo $1 ";" >> /tmp/music.grm.tmpl
	polygen /tmp/music.grm.tmpl > /tmp/xy.abc
       	abc2midi /tmp/xy.abc -o /tmp/xy.mid -Q $4
	echo $3 > /tmp/wildmidi.cfg 
	ls /usr/share/midi/freepats/$2/*$5*pat | shuf | nl -v 0 >> /tmp/wildmidi.cfg 
	wildmidi -c /tmp/wildmidi.cfg -o `mktemp`.xy.wav /tmp/xy.mid
}

tunes="+(Guitar|Piano|Flute|Harp|Cello|Oboe|Organ)"
drums="+(Timb|Cymbal|Tom|Clap|Wood)"

while [ true ]; do
	rm /tmp/*xy*.mid
	rm /tmp/*xy*.wav
	rm /tmp/*xy*.mp3
	rm /tmp/song.mp3

export keys=(C D E F G A B)
export key=${keys[$(( $RANDOM % ${#keys[*]} ))]}
export vers=("" "_" "^")
export ver=${vers[$(( $RANDOM % ${#vers[*]} ))]}
export tempo=$(( 80 + ( $RANDOM % 30 ) ))

	doit "Note ::= (\"c\"|\"d\"|\"e\"|\"f\"|\"g\"|\"a\"|\"b\") ^[\"/\"|\"2\"]" "Tone_000" "bank 0" $tempo $tunes
	doit "Note ::= (\"C\"|\"D\"|\"E\"|\"F\"|\"G\"|\"A\"|\"B\") ^[\"2\"|\"3\"|\"4\"]" "Tone_000" "bank 0" $tempo $tunes
	doit "Note ::= (\"C\"|\"D\"|\"E\"|\"F\"|\"G\"|\"A\"|\"B\") ^[\"2\"|\"3\"|\"4\"]" "Tone_000" "bank 0" $tempo "Bass"
	doit "Note ::= (\"C\"|\"D\"|\"E\"|\"F\"|\"G\"|\"A\"|\"B\") ^(\"2\"|\"3\"|\"4\")" "Drum_000" "drumbank 0" $(( 2 * $tempo )) $drums
	doit "Note ::= (\"C\"|\"D\"|\"E\"|\"F\"|\"G\"|\"A\"|\"B\") ^(\"2\"|\"3\"|\"4\")" "Drum_000" "drumbank 0" $(( 2 * $tempo )) $drums
	doit "Note ::= (\"C\"|\"D\"|\"E\"|\"F\"|\"G\"|\"A\"|\"B\") ^(\"2\"|\"3\"|\"4\")" "Drum_000" "drumbank 0" $(( 2 * $tempo )) "Kick"

	for x in /tmp/*xy.wav;do
		ffmpeg -t 180s -i $x -filter:a "lowpass,loudnorm" $x.mp3
	done

	inp="`find /tmp -maxdepth 1 -iname "*xy*.mp3" -printf " -t 180s -i \"%h/%f\" "`"

	ffmpeg $inp -filter_complex:a amerge=inputs=`ls /tmp/*xy*.mp3 | wc -l` /tmp/song.mp3

	cp /tmp/song.mp3 ~/$RANDOM.mp3

	#mpg123 /tmp/song.mp3
#exit 0
	#wait.exe
done

