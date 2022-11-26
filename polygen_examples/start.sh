#!/bin/bash

./poorMansElizaAlexaSiri.sh &
./mkmusic.sh &

xterm -e "watch -n 1 find /tmp/ -maxdepth 1 -iname \\\"*mp3\\\"" &




