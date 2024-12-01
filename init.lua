vim.g.mapleader = ','
vim.opt.guifont = "Victor Mono:h14:#h-slight"
vim.opt.rnu = true
vim.opt.nu = true
vim.opt.hlsearch = false
vim.opt.tabstop = 4
vim.opt.expandtab = true
vim.opt.clipboard = "unnamedplus"
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.keymap.set('n', '<leader>w', '<cmd>write<cr>', {desc = 'Save'})
vim.keymap.set('n', '<leader>m', '<cmd>Explore<cr>', {desc = 'Explore'})
vim.keymap.set('i', 'jj', '<Esc>', {desc = 'Esc'})

local vim = vim
local Plug = vim.fn['plug#']

vim.call('plug#begin')
Plug 'ellisonleao/gruvbox.nvim'
vim.call('plug#end')

vim.o.background = "dark" -- or "light" for light mode
vim.cmd([[colorscheme gruvbox]])
