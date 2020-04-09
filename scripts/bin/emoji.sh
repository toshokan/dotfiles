#!/bin/bash

RESULT=$(sed -e '/fully-qualified/!d' \
	     -e '/^$/d' \
	     -e '/^#/d' \
	     -e 's/^.*# //g' \
	     -e 's/E[[:digit:]]*\.[[:digit:]]* //g' \
	     ~/env/script_env/emoji.txt |
	  rofi -dmenu |
	  sed -e 's/\([^ ]*\).*/\1/g')

echo -n $RESULT | xclip -i -selection "clipboard"
