 """""""""""""""
"""" Basic """"
"""""""""""""""
syntax enable

set encoding=utf-8
set pumheight=10
set fileencoding=utf-8
set guicursor=
set hidden
set noerrorbells
set tabstop=4 softtabstop=4
set shiftwidth=4
set expandtab
set smartindent
set relativenumber
set nowrap
set smartcase
set noswapfile
set nobackup
set nowritebackup
set updatetime=300
set clipboard+=unnamedplus
set autochdir
set undodir=~/.config/nvim/undodir
set undofile
set incsearch
set cursorline
set colorcolumn=100

if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

highlight ColorColumn ctermbg=0 guibg=lightgrey

""""""""""""""""""
"""" Vim-Plug """"
""""""""""""""""""

call plug#begin('~/.config/nvim/plugged')

Plug 'jremmen/vim-ripgrep'
Plug 'tpope/vim-fugitive'
Plug 'vim-utils/vim-man'
" Plug 'lyuts/vim-rtags'
Plug 'mbbill/undotree'
Plug 'Chiel92/vim-autoformat'
Plug 'jiangmiao/auto-pairs'
" Plug 'Shougo/deoplete.nvim'
Plug 'tpope/vim-commentary'
Plug 'ptzz/lf.vim'
Plug 'junegunn/fzf.vim'
Plug 'ThePrimeagen/vim-be-good'
Plug 'tpope/vim-fugitive'
Plug 'easymotion/vim-easymotion'
Plug 'tpope/vim-fugitive'

" color_schemes
Plug 'joshdick/onedark.vim'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'morhetz/gruvbox'
Plug 'crusoexia/vim-monokai'
Plug 'arcticicestudio/nord-vim'
Plug 'iCyMind/NeoSolarized'

call plug#end()

set background=dark
" colorscheme NeoSolarized
" colorscheme dracula
" colorscheme monokai
colorscheme gruvbox
" colorscheme onedark
" colorscheme nord

""""""""""""""""""""""""
""""""""Gruvbox"""""""""
""""""""""""""""""""""""
let g:gruvbox_contrast_dark = 'hard'
if exists('+termguicolors')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif
let g:gruvbox_invert_selection='0'

""""""""""""""""""""""""
""""Neovim Solarized""""
""""""""""""""""""""""""
" Default value is "normal", Setting this option to "high" or "low" does use the
" same Solarized palette but simply shifts some values up or down in order to
" expand or compress the tonal range displayed.
let g:neosolarized_contrast = "normal"

" Special characters such as trailing whitespace, tabs, newlines, when displayed
" using ":set list" can be set to one of three levels depending on your needs.
" Default value is "normal". Provide "high" and "low" options.
let g:neosolarized_visibility = "normal"

" I make vertSplitBar a transparent background color. If you like the origin
" solarized vertSplitBar style more, set this value to 0.
let g:neosolarized_vertSplitBgTrans = 1

" If you wish to enable/disable NeoSolarized from displaying bold, underlined
" or italicized" typefaces, simply assign 1 or 0 to the appropriate variable.
" Default values:
let g:neosolarized_bold = 1
let g:neosolarized_underline = 1
let g:neosolarized_italic = 0

" Used to enable/disable "bold as bright" in Neovim terminal. If colors of bold
" text output by commands like `ls` aren't what you expect, you might want to
" try disabling this option. Default value:
let g:neosolarized_termBoldAsBright = 1

if executable('rg')
    let g:rg_derive_root = 'true'
endif

let g:netrw_browse_split = 2
let g:netrw_banner = 0
let g:netrw_winsize = 25
let g:netrw_liststyle = 3

let g:deoplete#enable_at_startup = 1

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction

"""""""""""""""""""""""
"""" Other Setting """"
"""""""""""""""""""""""
" Status-Line
set statusline=
set statusline+=
set statusline+=\ %M
set statusline+=\ %y
set statusline+=\ %r
set statusline+=\ %F

set statusline+=%= "Right side setttings"
set statusline+=\ %c:%l/%L
set statusline+=\ %p%%
set statusline+=\ [%n]

fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfun

autocmd BufWritePre * :call TrimWhitespace()

""""""""""""""""""""
"""" Key Config """"
""""""""""""""""""""
let mapleader = " "

" Disable arrow key and backspace
 noremap  <Up> ""
 noremap! <Up> <Esc>
 noremap  <Down> ""
 noremap! <Down> <Esc>
 noremap  <Left> ""
 noremap! <Left> <Esc>
 noremap  <Right> ""
 noremap! <Right> <Esc>

:inoremap <BS> <Nop>
:inoremap <Del> <Nop>

" Remap splits navigation
"nnoremap <leader>n :Vexplore<CR>
nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>

" Make adjusing split sizes a bit more frendly
nnoremap <M-l> :vertical resize +3<CR>
nnoremap <M-h> :vertical resize -3<CR>
nnoremap <M-k> :resize +3<CR>
nnoremap <M-j> :resize -3<CR>

" Change 2 split windows from vertical to horizon or horizon to vertical
map <leader>th <C-w>t<C-w>H
map <leader>tk <C-w>t<C-w>K

nnoremap <leader>u :UndotreeShow<CR>
nnoremap <leader>pv :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
nnoremap <Leader>ps :Rg<SPACE>
nnoremap <silent> <Leader>+ :vertical resize +5<CR>
nnoremap <silent> <Leader>- :vertical resize -5<CR>
vnoremap J :m '>+1<CR>gv=gv'
vnoremap K :m '<-2<CR>gv=gv'

" inoremap <silent><expr> <TAB>
"             \ pumvisible() ? "\<C-n>" :
"             \ <SID>check_back_space() ? "\<TAB>" :
"             \ coc#refresh()

map <leader>t :new term://zsh<CR>

" Cargo
"nnoremap <leader>c :!cargo clippy

" autoformat
noremap <F3> :Autoformat<CR>

" remap the fucking escape key
inoremap jj <Esc>

" Replace all is aliased to S.
nnoremap S :%s//g<left><left>

" easymotion
map <leader><leader>. <Plug>(easymotion-repeat)
map <leader><leader>f <Plug>(easymotion-overwin-f)
map <leader><leader>j <Plug>(easymotion-overwin-line)
map <leader><leader>k <Plug>(easymotion-overwin-line)
map <leader><leader>w <Plug>(easymotion-overwin-w)

" save with sudo
command W :execute ':silent w !sudo tee % > /dev/null' | :edit!
