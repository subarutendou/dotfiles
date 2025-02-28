export EDITOR=VISUAL
export VISUAL="emacs"
export GTK_THEME=Adwaita:dark
export GTK2_RC_FILES=/usr/share/themes/Adwaita-dark/gtk-2.0/gtkrc
export QT_STYLE_OVERRIDE=adwaita-dark
export PATH=~/dow/swift-5.9.2-RELEASE-ubuntu18.04/usr/bin:"${PATH}"

function parse_git_dirty {
    STATUS="$(git status 2> /dev/null)"
    if [[ $? -ne 0 ]]; then printf ""; return; else printf " ["; fi
    if echo ${STATUS} | grep -c "renamed:"         &> /dev/null; then printf " >"; else printf ""; fi
    if echo ${STATUS} | grep -c "brach is ahead:"         &> /dev/null; then printf " !"; else printf ""; fi
    if echo ${STATUS} | grep -c "new file:"         &> /dev/null; then printf " +"; else printf ""; fi
    if echo ${STATUS} | grep -c "Untracked fiels:"         &> /dev/null; then printf " ?"; else printf ""; fi
    if echo ${STATUS} | grep -c "modified:"         &> /dev/null; then printf " *"; else printf ""; fi
    if echo ${STATUS} | grep -c "deleted:"         &> /dev/null; then printf " -"; else printf ""; fi
    printf " ]"
}

parse_git_brach() {
    git rev-parse --abbrev-ref HEAD 2> /dev/null
}

PS1="\$(parse_git_brach)\$(parse_git_dirty) \w/\n > "

#update
alias update='sudo pacman -Syu'

# Alias
alias ls='ls -CF --color=auto'
alias la='ls -A'
alias ll='ls -alF'
alias suspend='sudo systemctl suspend'
alias gs='git status'
alias grep='grep --color=auto'
alias rm='rm -i'
alias mv='mv -i'
alias tmux='tmux -u'
# alias ll='ls -lah'
# alias la='ls -a'
alias gpgl='gpg --list-secret-keys --keyid-format LONG'
alias cl='sudo pacman -Rns $(pacman -Qdtq)'
alias cpu='sudo auto-cpufreq --stats'
alias te='tar -xvzf'
alias install_grub='sudo grub-install --target=x86_64-efi --efi-directory=uefi --bootloader-id=grub'
alias vim='nvim'
alias sudo='sudo -E'

co() {
gcc "$1" -o ../Debug/"$2"
}

dlweb() {
wget --recursive --no-clobber --page-requisites --html-extension --convert-links --domains "$1" --no-parent "$2"	 
}
