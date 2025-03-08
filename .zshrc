## Options
setopt autocd
unsetopt BEEP

## history
# Store history in a file to preserve it across sessions
HISTFILE=~/.zsh_history

# Number of commands to save in history file
HISTSIZE=10000        # Maximum entries in history for current session
SAVEHIST=10000        # Maximum entries saved to HISTFILE

# Set history options
setopt HIST_IGNORE_DUPS      # Ignore duplicate entries in history
setopt HIST_IGNORE_SPACE     # Ignore commands that start with a space
setopt HIST_FIND_NO_DUPS     # No duplicates when searching history
setopt HIST_REDUCE_BLANKS    # Remove extra blanks from history entries
setopt SHARE_HISTORY         # Share history between sessions

# Append to history file, rather than overwriting it
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY    # Write each command to history as it’s entered

# Enable extended history for timestamps
setopt EXTENDED_HISTORY

# Search history with up/down arrows
bindkey '^[[A' history-search-backward   # Up arrow
bindkey '^[[B' history-search-forward    # Down arrow
bindkey "^R" history-incremental-search-backward

# vim search fix
vi-search-fix() {
zle vi-cmd-mode
zle .vi-history-search-backward
}

autoload vi-search-fix
zle -N vi-search-fix
bindkey -M viins '\e/' vi-search-fix

function cal() {
    if [ -t 1 ]; then ncal -b "${@}"; else command cal "${@}"; fi
}

# fixing copy&paste
function vi-yank-wl-copy {
    zle vi-yank
   echo "$CUTBUFFER" | xclip -selection clipboard -i
}
zle -N vi-yank-wl-copy
bindkey -M vicmd 'y' vi-yank-wl-copy

wl-paste() {
    CUTBUFFER=$(xclip -selection clipboard -o)
    zle yank
}
zle -N wl-paste
bindkey -M vicmd 'p' wl-paste

### Aliases
alias vim="nvim"
alias v="nvim"
alias py="python3"
alias vimp="cd ~/.config/nvim;nvim ."
alias rn="ranger"
#alias z="zoxide"
alias em="emacsclient --create-frame"
alias bat="batcat"
alias ls="ls --color=always"
alias mv="mv -v"
alias cp="cp -v"
alias rm="rm -v"
alias cdp="cd $ROOT"

## FZF
export FZF_DEFAUL_OPTS="--height=100 --color=bg+:#343d46,gutter:-1,pointer:#ff3c3c,info:#0dbc79,hl+:#23d18b,--preview 'batcat {}'"
export FZF_DEFAULT_COMMAND="find . -path '*/\.*' -type d -prune -o -type f -print -o -type l -print 2> /dev/null | sed s/^..//"
export FZF_ALT_C_COMMAND="fdfind -t d --hidden"
export FZF_CTRL_T_COMMAND="fdfind -H -L --type f --type l --color never --search-path $HOME --search-path . "
export FZF_CTRL_T_OPTS="--height 100 --preview 'batcat --color=always --line-range :50 {}'"
export FZF_ALT_C_OPTS="--height 70 --preview 'tree -C {} | head -50'"
## ENV
export EDITOR="nvim"
export PATH="$PATH:$HOME/.local/bin:$HOME/.local/scripts:$HOME/.local/bin:$HOME/software/nvim-linux64/bin:$HOME/node_modules/hexo-cli/bin"

# Minimal Zsh prompt
PROMPT='%F{blue}%n%f@%F{gray}%m%f %F{yellow}[%1~] %F{red}-> %F{white}'

# Customize colors for better appearance
autoload -U colors && colors

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

# =============================================================================
autoload -U is-at-least

bindkey -v
bindkey -M viins '^?' backward-delete-char
bindkey -M viins '^W' backward-delete-word

_z_cd() {
    cd "$@" || return "$?"

    if [ "$_ZO_ECHO" = "1" ]; then
        echo "$PWD"
    fi
}

z() {
    if [ "$#" -eq 0 ]; then
        _z_cd ~
    elif [ "$#" -eq 1 ] && [ "$1" = '-' ]; then
        if [ -n "$OLDPWD" ]; then
            _z_cd "$OLDPWD"
        else
            echo 'zoxide: $OLDPWD is not set'
            return 1
        fi
    else
        _zoxide_result="$(zoxide query -- "$@")" && _z_cd "$_zoxide_result"
    fi
}

zi() {
    _zoxide_result="$(zoxide query -i -- "$@")" && _z_cd "$_zoxide_result"
}


alias za='zoxide add'

alias zq='zoxide query'
alias zqi='zoxide query -i'

alias zr='zoxide remove'
zri() {
    _zoxide_result="$(zoxide query -i -- "$@")" && zoxide remove "$_zoxide_result"
}


_zoxide_hook() {
    zoxide add "$(pwd -L)"
}

chpwd_functions=(${chpwd_functions[@]} "_zoxide_hook")
