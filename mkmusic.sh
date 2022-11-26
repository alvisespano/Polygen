#!/bin/bash

while [ true ]; do
	polygen /usr/share/polygen/music.grm | abc2midi - -o /tmp/xy.mid
	wildmidi /tmp/xy.mid
	wait.exe
done

