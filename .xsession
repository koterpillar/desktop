#!/bin/sh

export XMODIFIERS="@im=ibus"
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="ibus"

ibus-daemon -x &

xcompmgr &

exec gnome-session --session=xmonad
