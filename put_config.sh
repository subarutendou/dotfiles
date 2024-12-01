#!  /bin/bash
if [ -d "~/.config" ] then
        mkdir ~/.config
fi

if [ -d "~/.config/hypr" ] then
        mkdir ~/.config/hypr
fi

if [ -d "~/.config/nvim" ] then
        mkdir ~/.config/nvim
fi

if [ -d "~/.config/kitty" ] then
        mkdir ~/.config/kitty
fi

sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
ln -vfns ~/dotfiles/hyprland.conf ~/.config/hypr/hyprland.conf
ln -vfns ~/dotfiles/hyprpaper.conf ~/.config/hypr/hyprpaper.conf
ln -vfns ~/dotfiles/init.lua ~/.config/nvim/init.lua
ln -vfns ~/dotfiles/kitty.conf ~/.config/kitty/kitty.conf
