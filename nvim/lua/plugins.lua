return {
{ "ellisonleao/gruvbox.nvim", priority = 1000 , config = true, opts = ...},
{
    'windwp/nvim-autopairs',
    event = "InsertEnter",
    config = true
    -- use opts = {} for passing setup options
    -- this is equivalent to setup({}) function
}
}