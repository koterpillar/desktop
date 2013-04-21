#!/bin/sh

LOG=/tmp/xmonad-$(whoami).log

rm $LOG
touch $LOG

gnome-settings-daemon &

eval $(gnome-keyring-daemon --start)
export GNOME_KEYRING_SOCKET
export GNOME_KEYRING_PID

nautilus --no-default-window &

taffybar >>$LOG 2>&1 &

nm-applet --sm-disable &

(sleep 3 && gnome-power-manager) &

export XMODIFIERS="@im=ibus"
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="xim"

xcompmgr &

exec xmonad >>$LOG 2>&1
