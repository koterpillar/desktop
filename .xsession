#!/bin/sh

eval $(cat ~/.fehbg)

setxkbmap -option altwin:swap_lalt_lwin -option caps:ctrl_modifier

eval $(gnome-keyring-daemon --start)
export GNOME_KEYRING_SOCKET
export GNOME_KEYRING_PID

taffybar &

nm-applet --sm-disable &

if [ -x /usr/bin/gnome-power-manager ] ; then
   (sleep 3 && gnome-power-manager) &
fi

export XMODIFIERS="@im=ibus"
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="xim"

exec xmonad >/tmp/xmonad-$(whoami).log 2>&1
