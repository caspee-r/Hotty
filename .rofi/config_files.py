#!/usr/bin/env python3

from os import listdir
import subprocess


C = "~/.config/"
CONFIG_FILES = {
    "sxhkd":"sxhkd/sxhkdrc",
    "nvim":"nvim/init.lua",
    "neomutt":"neomutt/muttrc",
    "awesome":"awesome/rc.lua",
    "i3":"i3/config",
    "mpd":"mpd/mpd.conf",
    "picom":"picom/picom.conf",
    "rofi":"rofi/config.rasi",
    "xplr":"xplr/init.lua",
    "spectrwm":"spectrwm/spectrwm.conf",
    "sioyek":"sioyek/prefs_user.config",
    "polybar":"polybar/config.ini",
    "dunst":"dunst/dunstrc",
    "config_files":"~/.rofi/config_files.py"
}


result = ""
for k,e in CONFIG_FILES.items():
    if not e.startswith("~"):
        result += f"{k}\n"
        CONFIG_FILES[k] = f"{C}{e}"
    else:
        result += f"{k}\n"


    

ps = subprocess.Popen(('echo',f"{result}"),stdout=subprocess.PIPE)
output = subprocess.check_output(('rofi','-dmenu'),stdin = ps.stdout)
output = output.decode().replace('\n','')
print(CONFIG_FILES[output])
subprocess.run(f"kitty --execute nvim {CONFIG_FILES[output]} ",shell=True)

