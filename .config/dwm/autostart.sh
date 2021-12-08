#!/bin/sh

picom -b &
xrdb -merge ~/.config/x11/.Xresources &
sxhkd &
xwallpaper --zoom ~/.config/wallpaper
dwmblocks &
