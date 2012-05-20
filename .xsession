#!/bin/sh

xcompmgr -s &

gnome-settings-daemon &

taffybar &

nautilus -n &

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

