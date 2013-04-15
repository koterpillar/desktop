#!/bin/sh

gnome-settings-daemon &

eval $(gnome-keyring-daemon --start)
export GNOME_KEYRING_SOCKET
export GNOME_KEYRING_PID

taffybar &

nm-applet --sm-disable &

(sleep 3 && gnome-power-manager) &

export XMODIFIERS="@im=ibus"
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="xim"

xcompmgr &

exec xmonad >/tmp/xmonad-$(whoami).log 2>&1
