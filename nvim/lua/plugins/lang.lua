-- nvim-treesitter -----
require'nvim-treesitter.configs'.setup {
    -- A list of parser names, or "all"
    ensure_installed = { 'python', 'lua', 'c', 'bash', 'latex'},
    sync_install = false,
    auto_install = true,
    autopairs = {
        enable = true,
    },
    indent = { 
        enable = true, 
        -- disable = { "python" } 
    },
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
    },
}
-- 开启 Folding 模块
vim.opt.foldmethod = "expr"
vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
vim.opt.foldlevel = 99      -- 默认不要折叠

-- Comment --------------
require'Comment'.setup{
    mappings = {
        basic = true, --- `gcc` `gbc` `gc[count]{motion}` `gb[count]{motion}`
        extra = false, ---Extra mapping; `gco`, `gcO`, `gcA`
        extended = false,   ---Extended mapping; `g>` `g<` `g>[count]{motion}` `g<[count]{motion}`
    },
}

-- indent_blankline -----
vim.opt.list = true
vim.opt.listchars:append "eol:↴"
require("indent_blankline").setup {
    char = '| ',
    show_first_indent_level = false,
    show_end_of_line = true,
    use_treesitter = true,
    show_current_context = true,
    show_trailing_blankline_indent = false,
    buftype_exclude = { "terminal", "nofile" },
    filetype_exclude = {
        "help",
        "NvimTree",
      },
}