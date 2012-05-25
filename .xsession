#!/bin/sh

xcompmgr -s &

gnome-session --autostart=/dev/null &

gnome-settings-daemon &

taffybar &

background=$(bash -c "printf $(
    gconftool -g /desktop/gnome/background/picture_filename \
    | sed 's#file://##;s/\\/\\\\/g;s/\(%\)\([0-9a-fA-F][0-9a-fA-F]\)/\\\\x\2/g'
)")
feh --bg-max "$background"

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

