#!/usr/bin/env zsh

source_if_exists "${HOME}/env/scripts/X/setup_monitors.sh.local"

# Environment Setup
xrdb ~/.Xresources
xset s 3600 3600
xset dpms 3600 3600 3600
eval `dbus-launch --sh-syntax`

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

setxkbmap -option caps:escape
setxkbmap -option button9:Back
setxkbmap -option button11:Forward
xsetroot -cursor_name left_ptr

# background
xsetroot -bg "#2c1a30" -fg "#200c25" -mod 5 5

redshift &

source_if_exists "${HOME}/env/scripts/X/setup_inputs.sh.local"
source_if_exists "${HOME}/env/scripts/X/setup_x.sh.local"

sxhkd &

# Notifications
dunst &


exec dbus-launch --sh-syntax --exit-with-session bspwm
