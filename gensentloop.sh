#!/bin/bash

while [ true ];do 
	espeak -v mb-de7 -s 10 -a 40 "`gensent.sh | tee /dev/tty | grep -vi samen`"; 
	wait.exe
done


#//Dynamic Signature : Die kranken Bäckereien der demokratischen Roseen lesen den lauten Motor der bankrotten Malen .
