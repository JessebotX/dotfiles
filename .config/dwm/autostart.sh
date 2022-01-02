#!/bin/sh

sxhkd &
picom -b --no-fading-openclose &
xrdb -merge ~/.config/x11/.Xresources &
xwallpaper --zoom ~/.config/wall
dwmblocks &
