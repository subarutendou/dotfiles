vim.g.mapleader = ','
vim.g.maplocalleader = ","
vim.opt.guifont = "Victor Mono:h14:#h-slight"
vim.opt.rnu = true
vim.opt.nu = true
vim.opt.tabstop = 4
vim.opt.expandtab = true
vim.opt.clipboard = "unnamedplus"
vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.keymap.set('n', '<leader>w', '<cmd>write<cr>', {desc = 'Save'})
vim.keymap.set('n', '<leader>m', '<cmd>Explore<cr>', {desc = 'Explore'})
vim.keymap.set('i', 'jj', '<Esc>', {desc = 'Esc'})
vim.keymap.set('n', '<leader>n', '<cmd>nohlsearch<cr>', {desc = 'Clear Highlight'})

require("config.lazy")

vim.o.background = "dark" -- or "light" for light mode
vim.cmd([[colorscheme dracula]])
