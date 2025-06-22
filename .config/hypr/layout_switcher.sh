#!/usr/bin/env bash
layouts=( "dwindle","master" )
current_layout=$(hyprctl getoption general:layout | grep -o '".*"$' | sed 's/"//g')
echo $current_layout
if [ $current_layout = "dwindle" ]; then
    hyprctl keyword general:layout "master"
    notify-send -a layout_switcher -t 600 -u normal --icon ~/.config/hypr/hyprland_icon.png MASTER
    exit 0
else
    hyprctl keyword general:layout "dwindle"
    notify-send -a layout_switcher -t 600 -u normal --icon ~/.config/hypr/hyprland_icon.png DWINDLE
    
    exit 0
fi
