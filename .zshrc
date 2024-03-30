

### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk
#
## Options
setopt autocd
setopt vi
# fixing copy&paste 
function vi-yank-wl-copy {
    zle vi-yank
   echo "$CUTBUFFER" | wl-copy 
}
zle -N vi-yank-wl-copy
bindkey -M vicmd 'y' vi-yank-wl-copy

wl-paste() {
    CUTBUFFER=$(wl-paste)
    zle yank
}
zle -N wl-paste
bindkey -M vicmd 'p' wl-paste
unsetopt BEEP


#plugins
    ## Starship
zinit ice as"command" from"gh-r" \
          atclone"./starship init zsh > init.zsh; ./starship completions zsh > _starship" \
          atpull"%atclone" src"init.zsh"
zinit light starship/starship


zinit ice wait"1" lucid
zinit light zsh-users/zsh-completions

zinit ice wait"3" lucid
zinit light hlissner/zsh-autopair
# zinit light Aloxaf/fzf-tab
zinit light zdharma-continuum/fast-syntax-highlighting
# zinit ice wait"!2"
# zinit light marlonrichert/zsh-autocomplete > /dev/null
zinit ice wait"2" lucid
zinit snippet https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/fzf/fzf.plugin.zsh
zinit ice wait"2" lucid
zinit snippet https://github.com/ohmyzsh/ohmyzsh/blob/master/lib/history.zsh
zinit ice wait"2" lucid
zinit snippet https://github.com/cheat/cheat/blob/master/scripts/cheat.zsh # auto-complition for cheat



zinit ice wait"3" lucid
zinit light  olets/zsh-abbr


zinit ice wait"2" lucid atload'_zsh_autosuggest_start'
zinit light zsh-users/zsh-autosuggestions

# zinit ice depth=1 wait"2"
# zinit light jeffreytse/zsh-vi-mode

### Aliases
alias v="nvim"
alias vim="nvim"
alias py="python"
alias t="tmuxinator"
alias vimp="nvim ~/.config/nvim"
alias rn="ranger"
alias ls="lsd"
alias zt="tmuxinator"
alias t="tmuxinator"
alias z="zoxide"
alias em="emacsclient --create-frame"

## FZF
export FZF_DEFAUL_OPTS="--height=100 --color=bg+:#343d46,gutter:-1,pointer:#ff3c3c,info:#0dbc79,hl+:#23d18b,--preview 'bat {}'"
export FZF_DEFAULT_COMMAND="find . -path '*/\.*' -type d -prune -o -type f -print -o -type l -print 2> /dev/null | sed s/^..//"
export FZF_ALT_C_COMMAND="fd -t d --hidden"
export FZF_CTRL_T_COMMAND="fd -H -L --type f --type l --color never --search-path $HOME --search-path . "
export FZF_CTRL_T_OPTS="--height 100 --preview 'bat --color=always --line-range :50 {}'"
export FZF_ALT_C_OPTS="--height 70 --preview 'tree -C {} | head -50'"
## ENV
export EDITOR="nvim"
export PATH="$PATH:$HOME/.local/bin:/home/:$HOME/.local/scripts"
export CHEAT_USE_FZF=true
export NC="$HOME/.config/nvim/init.lua"



# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete #_ignored _approximate
# zstyle ':completion:*' format 'Hmmm %d'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle ':completion:*' menu select
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' use-compctl true
zstyle :compinstall filename '/home/casper/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
export PATH=$PATH:/home/djella/.spicetify
# =============================================================================
#
# Utility functions for zoxide.
#

# pwd based on the value of _ZO_RESOLVE_SYMLINKS.
function __zoxide_pwd() {
    uiltin pwd -L
}

# cd + custom logic based on the value of _ZO_ECHO.
function __zoxide_cd() {
    # shellcheck disable=SC2164
    uiltin cd -- "$@"
}

# =============================================================================
#
# Hook configuration for zoxide.
#

# Hook to add new entries to the database.
function __zoxide_hook() {
	# shellcheck disable=SC2312
}

eval "$(zoxide init zsh)"
#compdef eww

autoload -U is-at-least

_eww() {
    typeset -A opt_args
    typeset -a _arguments_options
    local ret=1

    if is-at-least 5.2; then
        _arguments_options=(-s -S -C)
    else
        _arguments_options=(-s -C)
    fi

    local context curcontext="$curcontext" state line
    _arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
'-V[Print version]' \
'--version[Print version]' \
":: :_eww_commands" \
"*::: :->eww" \
&& ret=0
    case $state in
    (eww)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:eww-command-$line[1]:"
        case $line[1] in
            (daemon)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(logs)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(shell-completions)
_arguments "${_arguments_options[@]}" \
'-s+[]:SHELL:(bash elvish fish powershell zsh)' \
'--shell=[]:SHELL:(bash elvish fish powershell zsh)' \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(ping)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(update)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
'*::mappings -- variable_name="new_value"-pairs that will be updated:' \
&& ret=0
;;
(inspector)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(open)
_arguments "${_arguments_options[@]}" \
'--id=[]:ID: ' \
'--screen=[The identifier of the monitor the window should open on]:SCREEN: ' \
'-p+[The position of the window, where it should open. (i.e.\: 200x100)]:POS: ' \
'--pos=[The position of the window, where it should open. (i.e.\: 200x100)]:POS: ' \
'-s+[The size of the window to open (i.e.\: 200x100)]:SIZE: ' \
'--size=[The size of the window to open (i.e.\: 200x100)]:SIZE: ' \
'-a+[Sidepoint of the window, formatted like "top right"]:ANCHOR: ' \
'--anchor=[Sidepoint of the window, formatted like "top right"]:ANCHOR: ' \
'--duration=[Automatically close the window after a specified amount of time, i.e.\: 1s]:DURATION: ' \
'*--arg=[Define a variable for the window, i.e.\: \`--arg "var_name=value"\`]:ARGS: ' \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--toggle[If the window is already open, close it instead]' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
':window_name -- Name of the window you want to open:' \
&& ret=0
;;
(open-many)
_arguments "${_arguments_options[@]}" \
'*--arg=[Define a variable for the window, i.e.\: \`--arg "window_id\:var_name=value"\`]:ARGS: ' \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--toggle[If a window is already open, close it instead]' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
'*::windows -- List the windows to open, optionally including their id, i.e.\: `--window "window_name\:window_id"`:' \
&& ret=0
;;
(close)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
'*::windows:' \
&& ret=0
;;
(reload)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(kill)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(close-all)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(state)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'-a[Shows all variables, including not currently used ones]' \
'--all[Shows all variables, including not currently used ones]' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(get)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
':name:' \
&& ret=0
;;
(list-windows)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(active-windows)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(debug)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help (see more with '\''--help'\'')]' \
'--help[Print help (see more with '\''--help'\'')]' \
&& ret=0
;;
(graph)
_arguments "${_arguments_options[@]}" \
'-c+[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--config=[override path to configuration directory (directory that contains eww.yuck and eww.(s)css)]:CONFIG:_files' \
'--debug[Write out debug logs. (To read the logs, run \`eww logs\`)]' \
'--force-wayland[Force eww to use wayland. This is a no-op if eww was compiled without wayland support]' \
'--logs[Watch the log output after executing the command]' \
'--no-daemonize[Avoid daemonizing eww]' \
'--restart[Restart the daemon completely before running the command]' \
'-h[Print help]' \
'--help[Print help]' \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" \
":: :_eww__help_commands" \
"*::: :->help" \
&& ret=0

    case $state in
    (help)
        words=($line[1] "${words[@]}")
        (( CURRENT += 1 ))
        curcontext="${curcontext%:*:*}:eww-help-command-$line[1]:"
        case $line[1] in
            (daemon)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(logs)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(shell-completions)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(ping)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(update)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(inspector)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(open)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(open-many)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(close)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(reload)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(kill)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(close-all)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(state)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(get)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(list-windows)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(active-windows)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(debug)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(graph)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
(help)
_arguments "${_arguments_options[@]}" \
&& ret=0
;;
        esac
    ;;
esac
;;
        esac
    ;;
esac
}

(( $+functions[_eww_commands] )) ||
_eww_commands() {
    local commands; commands=(
'daemon:Start the Eww daemon' \
'logs:Print and watch the eww logs' \
'shell-completions:Generate a shell completion script' \
'ping:Ping the eww server, checking if it is reachable' \
'update:Update the value of a variable, in a running eww instance' \
'inspector:Open the GTK debugger' \
'open:Open a window' \
'open-many:Open multiple windows at once. NOTE\: This will in the future be part of eww open, and will then be removed' \
'close:Close the given windows' \
'reload:Reload the configuration' \
'kill:Kill the eww daemon' \
'close-all:Close all windows, without killing the daemon' \
'state:Prints the variables used in all currently open window' \
'get:Get the value of a variable if defined' \
'list-windows:List the names of active windows' \
'active-windows:Show active window IDs, formatted linewise \`<window_id>\: <window_name>\`' \
'debug:Print out the widget structure as seen by eww' \
'graph:Print out the scope graph structure in graphviz dot format' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'eww commands' commands "$@"
}
(( $+functions[_eww__active-windows_commands] )) ||
_eww__active-windows_commands() {
    local commands; commands=()
    _describe -t commands 'eww active-windows commands' commands "$@"
}
(( $+functions[_eww__help__active-windows_commands] )) ||
_eww__help__active-windows_commands() {
    local commands; commands=()
    _describe -t commands 'eww help active-windows commands' commands "$@"
}
(( $+functions[_eww__close_commands] )) ||
_eww__close_commands() {
    local commands; commands=()
    _describe -t commands 'eww close commands' commands "$@"
}
(( $+functions[_eww__help__close_commands] )) ||
_eww__help__close_commands() {
    local commands; commands=()
    _describe -t commands 'eww help close commands' commands "$@"
}
(( $+functions[_eww__close-all_commands] )) ||
_eww__close-all_commands() {
    local commands; commands=()
    _describe -t commands 'eww close-all commands' commands "$@"
}
(( $+functions[_eww__help__close-all_commands] )) ||
_eww__help__close-all_commands() {
    local commands; commands=()
    _describe -t commands 'eww help close-all commands' commands "$@"
}
(( $+functions[_eww__daemon_commands] )) ||
_eww__daemon_commands() {
    local commands; commands=()
    _describe -t commands 'eww daemon commands' commands "$@"
}
(( $+functions[_eww__help__daemon_commands] )) ||
_eww__help__daemon_commands() {
    local commands; commands=()
    _describe -t commands 'eww help daemon commands' commands "$@"
}
(( $+functions[_eww__debug_commands] )) ||
_eww__debug_commands() {
    local commands; commands=()
    _describe -t commands 'eww debug commands' commands "$@"
}
(( $+functions[_eww__help__debug_commands] )) ||
_eww__help__debug_commands() {
    local commands; commands=()
    _describe -t commands 'eww help debug commands' commands "$@"
}
(( $+functions[_eww__get_commands] )) ||
_eww__get_commands() {
    local commands; commands=()
    _describe -t commands 'eww get commands' commands "$@"
}
(( $+functions[_eww__help__get_commands] )) ||
_eww__help__get_commands() {
    local commands; commands=()
    _describe -t commands 'eww help get commands' commands "$@"
}
(( $+functions[_eww__graph_commands] )) ||
_eww__graph_commands() {
    local commands; commands=()
    _describe -t commands 'eww graph commands' commands "$@"
}
(( $+functions[_eww__help__graph_commands] )) ||
_eww__help__graph_commands() {
    local commands; commands=()
    _describe -t commands 'eww help graph commands' commands "$@"
}
(( $+functions[_eww__help_commands] )) ||
_eww__help_commands() {
    local commands; commands=(
'daemon:Start the Eww daemon' \
'logs:Print and watch the eww logs' \
'shell-completions:Generate a shell completion script' \
'ping:Ping the eww server, checking if it is reachable' \
'update:Update the value of a variable, in a running eww instance' \
'inspector:Open the GTK debugger' \
'open:Open a window' \
'open-many:Open multiple windows at once. NOTE\: This will in the future be part of eww open, and will then be removed' \
'close:Close the given windows' \
'reload:Reload the configuration' \
'kill:Kill the eww daemon' \
'close-all:Close all windows, without killing the daemon' \
'state:Prints the variables used in all currently open window' \
'get:Get the value of a variable if defined' \
'list-windows:List the names of active windows' \
'active-windows:Show active window IDs, formatted linewise \`<window_id>\: <window_name>\`' \
'debug:Print out the widget structure as seen by eww' \
'graph:Print out the scope graph structure in graphviz dot format' \
'help:Print this message or the help of the given subcommand(s)' \
    )
    _describe -t commands 'eww help commands' commands "$@"
}
(( $+functions[_eww__help__help_commands] )) ||
_eww__help__help_commands() {
    local commands; commands=()
    _describe -t commands 'eww help help commands' commands "$@"
}
(( $+functions[_eww__help__inspector_commands] )) ||
_eww__help__inspector_commands() {
    local commands; commands=()
    _describe -t commands 'eww help inspector commands' commands "$@"
}
(( $+functions[_eww__inspector_commands] )) ||
_eww__inspector_commands() {
    local commands; commands=()
    _describe -t commands 'eww inspector commands' commands "$@"
}
(( $+functions[_eww__help__kill_commands] )) ||
_eww__help__kill_commands() {
    local commands; commands=()
    _describe -t commands 'eww help kill commands' commands "$@"
}
(( $+functions[_eww__kill_commands] )) ||
_eww__kill_commands() {
    local commands; commands=()
    _describe -t commands 'eww kill commands' commands "$@"
}
(( $+functions[_eww__help__list-windows_commands] )) ||
_eww__help__list-windows_commands() {
    local commands; commands=()
    _describe -t commands 'eww help list-windows commands' commands "$@"
}
(( $+functions[_eww__list-windows_commands] )) ||
_eww__list-windows_commands() {
    local commands; commands=()
    _describe -t commands 'eww list-windows commands' commands "$@"
}
(( $+functions[_eww__help__logs_commands] )) ||
_eww__help__logs_commands() {
    local commands; commands=()
    _describe -t commands 'eww help logs commands' commands "$@"
}
(( $+functions[_eww__logs_commands] )) ||
_eww__logs_commands() {
    local commands; commands=()
    _describe -t commands 'eww logs commands' commands "$@"
}
(( $+functions[_eww__help__open_commands] )) ||
_eww__help__open_commands() {
    local commands; commands=()
    _describe -t commands 'eww help open commands' commands "$@"
}
(( $+functions[_eww__open_commands] )) ||
_eww__open_commands() {
    local commands; commands=()
    _describe -t commands 'eww open commands' commands "$@"
}
(( $+functions[_eww__help__open-many_commands] )) ||
_eww__help__open-many_commands() {
    local commands; commands=()
    _describe -t commands 'eww help open-many commands' commands "$@"
}
(( $+functions[_eww__open-many_commands] )) ||
_eww__open-many_commands() {
    local commands; commands=()
    _describe -t commands 'eww open-many commands' commands "$@"
}
(( $+functions[_eww__help__ping_commands] )) ||
_eww__help__ping_commands() {
    local commands; commands=()
    _describe -t commands 'eww help ping commands' commands "$@"
}
(( $+functions[_eww__ping_commands] )) ||
_eww__ping_commands() {
    local commands; commands=()
    _describe -t commands 'eww ping commands' commands "$@"
}
(( $+functions[_eww__help__reload_commands] )) ||
_eww__help__reload_commands() {
    local commands; commands=()
    _describe -t commands 'eww help reload commands' commands "$@"
}
(( $+functions[_eww__reload_commands] )) ||
_eww__reload_commands() {
    local commands; commands=()
    _describe -t commands 'eww reload commands' commands "$@"
}
(( $+functions[_eww__help__shell-completions_commands] )) ||
_eww__help__shell-completions_commands() {
    local commands; commands=()
    _describe -t commands 'eww help shell-completions commands' commands "$@"
}
(( $+functions[_eww__shell-completions_commands] )) ||
_eww__shell-completions_commands() {
    local commands; commands=()
    _describe -t commands 'eww shell-completions commands' commands "$@"
}
(( $+functions[_eww__help__state_commands] )) ||
_eww__help__state_commands() {
    local commands; commands=()
    _describe -t commands 'eww help state commands' commands "$@"
}
(( $+functions[_eww__state_commands] )) ||
_eww__state_commands() {
    local commands; commands=()
    _describe -t commands 'eww state commands' commands "$@"
}
(( $+functions[_eww__help__update_commands] )) ||
_eww__help__update_commands() {
    local commands; commands=()
    _describe -t commands 'eww help update commands' commands "$@"
}
(( $+functions[_eww__update_commands] )) ||
_eww__update_commands() {
    local commands; commands=()
    _describe -t commands 'eww update commands' commands "$@"
}

if [ "$funcstack[1]" = "_eww" ]; then
    _eww "$@"
else
    compdef _eww eww
fi
