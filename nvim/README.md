## Install neovim

    sudo pacman -S neovim

## Prepare environment

    export VIMCONFIG=~/.config/nvim

    mkdir -p $VIMCONFIG
    touch $VIMCONFIG/init.vim
    ln -s $VIMCONFIG/init.vim ~/.vimrc

## Install [minpac](https://github.com/k-takata/minpac)

    git clone https://github.com/k-takata/minpac.git $VIMCONFIG/pack/minpac/opt/minpac

## Install [fzf](https://github.com/junegunn/fzf)

install

    git clone https://github.com/junegunn/fzf $VIMCONFIG/pack/bundle/start/fzf
    $VIMCONFIG/pack/bundle/start/fzf/install --bin

    export PATH=$PATH:$VIMCONFIG/pack/bundle/start/fzf/bin

key

    nnoremap <C-p> :<C-u>FZF<CR>
