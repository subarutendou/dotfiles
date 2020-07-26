#!/bin/sh

# Fixing Java applications
export _JAVA_AWT_WM_NONREPARENTING=1

xset r rate 300 50
xset s off &
xset -dpms &
unclutter &
# The next line of command is for apple keyboard to swap control and super key.
#setxkbmap -option ctrl:swap_lwin_lctl &
#setxkbmap -option caps:ctrl_modifier &
#setxkbmap -option ctrl:swap_lalt_lctl_lwin &
setxkbmap -option ctrl:swap_lalt_lctl &
picom -b &
feh --bg-fill ~/pix/wallpaper/karaokmiko.jpeg &
xmobar /home/andou/.config/xmobar/xmobarrc &
pcloud -b &

while true; do
    xsetroot -name "$( date +"%F %R" )"
    sleep 1m    # Update time every minute
done &
exec dwm
