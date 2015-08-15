#!/bin/sh

# We start by gathering some basic data about the system.

# Get our username, I actually have a different one at work.
USERNAME=`whoami`

# See if we're running on a headless server or not.
DISP=`echo $DISPLAY`
if [ -z "$DISP" ]; then
  GRAPHICAL=0
else
  GRAPHICAL=1
fi



