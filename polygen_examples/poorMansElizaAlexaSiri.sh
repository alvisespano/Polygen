#!/bin/bash

while [ true ];do 
	str="`polygen ./english.grm 2>/dev/null`"
	str2="`echo $str | trans de:en -b | trans en:de -b`"

	echo $str
	echo $str2

	if [ -n "$str2" ]; then
		espeak -v mb-de7 -s 10 -a 40 "$str2"
	else
		espeak -v mb-en1 -p 100 -s 10 -a 40 "$str"
	fi

	wait.exe
done


#//Dynamic Signature : Die kranken Bäckereien der demokratischen Roseen lesen den lauten Motor der bankrotten Malen .
