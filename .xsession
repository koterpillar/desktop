#!/bin/sh

gnome-settings-daemon &

eval $(cat ~/.fehbg)

eval $(gnome-keyring-daemon --start)
export GNOME_KEYRING_SOCKET
export GNOME_KEYRING_PID

taffybar &

nautilus --no-default-window &

nm-applet --sm-disable &

if [ -x /usr/bin/gnome-power-manager ] ; then
   (sleep 3 && gnome-power-manager) &
fi

export XMODIFIERS="@im=ibus"
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="xim"

(sleep 2 && synapse -s) &

exec xmonad >/tmp/xmonad-$(whoami).log 2>&1
