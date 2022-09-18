-- auto-save ----------------
require'auto-save'.setup{}

-- nvim-tree ----------------
-- disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded = 1
vim.g.loaded_netrwPlugin = 1
require("nvim-tree").setup({
    disable_netrw = true,
    hijack_cursor = false,
    sort_by = "case_sensitive",
    view = {
        side = "right",
        adaptive_size = false,
        mappings = {
            list = {
                { key = "u", action = "dir_up" },
            },
        },
    },
    renderer = {
        group_empty = true,
    },
    filters = {
        custom = { "^.git$" },
        dotfiles = true,
    },
})

-- bufferline ----------------
require'bufferline'.setup({
    options = {
        number = "buffer_id",
        close_command = "Bdelete! %d", -- can be a string | function, see "Mouse actions"
        right_mouse_command = "Bdelete! %d", -- can be a string | function, see "Mouse actions"
        left_mouse_command = "buffer %d",
        mode = 'buffers',
        -- 使用 nvim 内置lsp
        diagnostics = "nvim_lsp",
        -- 左侧让出 nvim-tree 的位置
        offsets = {{
            filetype = 'NvimTree',
            text_align = "left",
            separator = true
        }},
        separator_style = "thin",
    },
    highlights = {
        buffer_selected = {
            italic = false
        },
        indicator_selected = {
            fg = {attribute = 'fg', highlight = 'Function'},
            italic = false
        },
    },
})

-- telescope -----------
require('telescope').setup{
    defaults = {
        initial_mode = 'insert',
        layout_strategy = 'horizontal',
        path_display = { 'smart' },
        file_ignore_patterns = {'.git/'},
        mappings = telescope_keybindings,
    },
}