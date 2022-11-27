#!/bin/bash

xterm -e "./poorMansElizaAlexaSiri.sh" &
xterm -e "./mkmusic.sh" &
xterm -e "watch -n 1 find /tmp/ -maxdepth 1 -iname \\\"*mp3\\\"" &
xterm -e "while [ true ]; do ls ~/*.mp3 | shuf | head -n 1 | xargs mpg123 ;wait.exe;done" &





