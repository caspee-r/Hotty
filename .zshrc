

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
unsetopt BEEP
setopt vi

# vim search fix
vi-search-fix() {
zle vi-cmd-mode
zle .vi-history-search-backward
}


autoload vi-search-fix
zle -N vi-search-fix
bindkey -M viins '\e/' vi-search-fix

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

#plugins
zinit light zdharma-continuum/fast-syntax-highlighting
zinit ice lucid
zinit snippet https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/fzf/fzf.plugin.zsh
zinit ice lucid
zinit snippet https://github.com/ohmyzsh/ohmyzsh/blob/master/lib/history.zsh
zinit ice lucid atload'_zsh_autosuggest_start'
zinit light zsh-users/zsh-autosuggestions
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
export PATH="$PATH:$HOME/.local/bin:$HOME/.local/scripts"
export CHEAT_USE_FZF=true


# Minimal Zsh prompt
PROMPT='%F{blue}%n%f@%F{green}%m%f:%F{yellow}%~%f %# '

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
export PATH=$PATH:/home/djella/.spicetify
# =============================================================================
autoload -U is-at-least
# To initialize zoxide, add this to your configuration (usually ~/.zshrc):
eval "$(zoxide init zsh)"
