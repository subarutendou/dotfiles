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
set timeoutlen=100
set clipboard+=unnamedplus
set autochdir
set undodir=~/.config/nvim/undodir
set undofile
set incsearch
set cursorline

if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

""""""""""""""""""
"""" Vim-Plug """"
""""""""""""""""""

call plug#begin('~/.vim/plugged')

Plug 'jremmen/vim-ripgrep'
Plug 'vim-utils/vim-man'
Plug 'mbbill/undotree'
Plug 'wakatime/vim-wakatime'
Plug 'Chiel92/vim-autoformat'
Plug 'jiangmiao/auto-pairs'

" color_schemes
Plug 'morhetz/gruvbox'
Plug 'crusoexia/vim-monokai'
Plug 'iCyMind/NeoSolarized'

call plug#end()

set background=dark
colorscheme gruvbox

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

" Remap splits navigation
nnoremap <leader>n :Vexplore<CR>
nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>

nnoremap <leader>u :UndotreeShow<CR>
nnoremap <leader>pv :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
nnoremap <Leader>ps :Rg<SPACE>
nnoremap <silent> <Leader>+ :vertical resize +5<CR>
nnoremap <silent> <Leader>- :vertical resize -5<CR>
vnoremap J :m '>+1<CR>gv=gv'
vnoremap K :m '<-2<CR>gv=gv'

inoremap <silent><expr> <TAB>
            \ pumvisible() ? "\<C-n>" :
            \ <SID>check_back_space() ? "\<TAB>" :
            \ coc#refresh()

inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
inoremap <silent><expr> <C-space> coc#refresh()
map <leader>tt :new term://zsh<CR>

" remap the fucking escape key
inoremap jk <Esc>
inoremap kj <Esc>
