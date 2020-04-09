#!/bin/bash

cat ~/env/script_env/common_clipboard.txt | rofi -dmenu | xclip -i -selection clipboard

