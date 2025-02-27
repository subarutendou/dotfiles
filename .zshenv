export EDITOR=nvim
export PAGER=less
export ZDOTDIR=$HOME/.config/zsh
export XDG_CONFIG_HOME=$HOME/.config

typeset -U path PATH
path=(~/.local/bin $path)
export PATH
. "$HOME/.cargo/env"
