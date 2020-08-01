export PATH=$PATH"$HOME/.local/bin"
export PATH="$HOME/.cargo/bin:$PATH"
export VISUAL="emacsclient -c -a emacs" # $VISUAL opens in GUI mode
# export VISUAL=emacs
export EDITOR="$VISUAL"
export HISTCONTROL=ignoreboth
export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

eval "$(gh completion -s zsh)"
