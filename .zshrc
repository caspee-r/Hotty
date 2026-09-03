## Options
setopt autocd
unsetopt BEEP
setopt emacs

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
bindkey "^R" history-incremental-search-backward

### Aliases
alias vim="nvim"
alias v="nvim"
alias py="python3"
alias vimp="cd ~/.config/nvim;nvim ."
alias rn="ranger"
alias em="emacsclient --create-frame"
alias ls="ls --color=always"
alias mv="mv -v"
alias cp="cp -v"
alias rm="rm -v"

# ssh-agent
#if ! pgrep -u "$USER" ssh-agent > /dev/null; then
#    ssh-agent -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
#fi
#if [ ! -f "$SSH_AUTH_SOCK" ]; then
#    source "$XDG_RUNTIME_DIR/ssh-agent.env" >/dev/null
#fi

#export TERM=nothing
## FZF
export FZF_DEFAUL_OPTS="--height=100 --color=bg+:#343d46,gutter:-1,pointer:#ff3c3c,info:#0dbc79,hl+:#23d18b,--preview 'bat {}'"
export FZF_DEFAULT_COMMAND="find . -path '*/\.*' -type d -prune -o -type f -print -o -type l -print 2> /dev/null | sed s/^..//"
export FZF_ALT_C_COMMAND="fd -t d --hidden"
export FZF_CTRL_T_COMMAND="fd -H -L --type f --type l --color never --search-path $HOME --search-path . "
export FZF_CTRL_T_OPTS="--height 100 --preview 'bat --color=always --line-range :50 {}'"
export FZF_ALT_C_OPTS="--height 70 --preview 'tree -C {} | head -50'"
## ENV
export EDITOR="emacs"
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

