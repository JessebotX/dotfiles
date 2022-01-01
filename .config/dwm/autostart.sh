#!/bin/sh

picom -b --no-fading-openclose &
xrdb -merge ~/.config/x11/.Xresources &
dwmblocks &
