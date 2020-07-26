export PATH=$PATH:/home/andou/.local/bin

# zsh config
for f in /home/andou/.config/zsh/.shellConfig/*; do source "$f"; done

# Enable colors and change prompt:
autoload -U colors && colors # Load colors
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT=\$vcs_info_msg_0_
# PROMPT=\$vcs_info_msg_0_'%# '
zstyle ':vcs_info:git:*' formats 'on branch %b'
PROMPT='%B%F{yellow}%2~ %b${vcs_info_msg_0_}%(!.#h.>) '

# Disable ctrl-s to freeze terminal.
stty stop undef

# Lines configured by zsh-newuser-install
HISTFILE=/home/andou/.config/.histfile
HISTSIZE=50000
SAVEHIST=50000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/andou/.zshrc'

autoload -Uz compinit && compinit
# End of lines added by compinstall

# User config
. /usr/share/LS_COLORS/dircolors.sh

# Syntax highlight plugin put at the end
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Just a backup solution for prompt color
# PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%1~%{$fg[red]%}]%{$reset_color%}$%b "

# Reference fomr "https://scriptingosx.com/2019/07/moving-to-zsh-06-customizing-the-zsh-prompt/"
# PROMPT="%B%F{yellow}%2~%f%b %(!.#h.> )"
# RPROMPT="%F{white}[%*]"

# Don't want the auto cd anymore but put it here in cast I want it back
# setopt autocd  # Automatically cd into typed directory.
