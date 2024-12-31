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

--vim.o.background = "dark"
--vim.cmd([[colorscheme gruvbox]])
-- Default options:
require("gruvbox").setup({
  terminal_colors = true, -- add neovim terminal colors
  undercurl = true,
  underline = true,
  bold = true,
  italic = {
    strings = true,
    emphasis = true,
    comments = true,
    operators = false,
    folds = true,
  },
  strikethrough = true,
  invert_selection = false,
  invert_signs = false,
  invert_tabline = false,
  invert_intend_guides = false,
  inverse = true, -- invert background for search, diffs, statuslines and errors
  contrast = "", -- can be "hard", "soft" or empty string
  palette_overrides = {},
  overrides = {},
  dim_inactive = false,
  transparent_mode = false,
})
vim.cmd("colorscheme gruvbox")
