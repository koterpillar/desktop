#!/bin/sh -e
# Run Thunderbird for email links
# xdg-email appears to be dormant:
# https://bugs.freedesktop.org/show_bug.cgi?id=61475

MAILTO=$(echo $1 | sed 's/^mailto://')

thunderbird -compose "to=$MAILTO"
