#!/usr/bin/env zsh

source_if_exists "${HOME}/env/scripts/X/setup_monitors.sh.local"

# Environment Setup
xrdb ~/.Xresources
xset s 3600 3600
xset dpms 3600 3600 3600
eval `dbus-launch --sh-syntax`

setxkbmap -option caps:escape
setxkbmap -option button9:Back
setxkbmap -option button11:Forward
xsetroot -cursor_name left_ptr

source_if_exists "${HOME}/env/scripts/X/setup_inputs.sh.local"
source_if_exists "${HOME}/env/scripts/X/setup_x.sh.local"

sxhkd &

# Notifications
dunst &

# Compositing
compton -b

exec ck-launch-session dbus-launch --sh-syntax --exit-with-session bspwm
