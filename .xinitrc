!/bin/sh

# Fixing Java applications
export _JAVA_AWT_WM_NONREPARENTING=1

# fcitx input
export XMODIFIERS=@im=fcitx
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
fcitx -d -r
# fcitx &

# Cursor and mouse behavior
xset r rate 300 50
xset s off &
xset -dpms &
unclutter &
picom -b &
dunst &
pcloud -b &
# nitrogen --restore &

# EXWM settings
# Disable access control for the current user.
xhost +SI:localuser:$USER

# Disable access control for the c# Set default cursor.
xsetroot -cursor_name left_ptr

exec emacs

# DWM settings
#while true; do
#    xsetroot -name "$( date +"%F %R" )"
#    sleep 1m    # Update time every minute
#done &
#exec dwm
