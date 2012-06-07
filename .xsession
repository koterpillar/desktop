#!/bin/sh

xcompmgr -s &

gnome-settings-daemon &

eval $(gnome-keyring-daemon --start)
export GNOME_KEYRING_SOCKET
export GNOME_KEYRING_PID

taffybar &

nautilus --no-default-window &

xscreensaver -no-splash &

nm-applet --sm-disable &

if [ -x /usr/bin/gnome-power-manager ] ; then
   (sleep 3 && gnome-power-manager) &
fi

export XMODIFIERS="@im=ibus"
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="xim"
ibus-daemon -d -x

synapse -s &

exec xmonad
