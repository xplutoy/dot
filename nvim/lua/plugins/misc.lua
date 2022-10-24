-- themescheme ----------
-- tokyonight | gruvbox | zephyr | nord | onedark | nightfox                            
vim.o.background = 'dark'              
vim.cmd("colorscheme tokyonight-night")

-- auto-save ----------------
require'auto-save'.setup{}

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
vim.keymap.set("n", "<leader>bh", ":BufferLineCyclePrev<CR>")
vim.keymap.set("n", "<leader>bl", ":BufferLineCycleNext<CR>")
vim.keymap.set("n", "<leader>bo", ":BufferLineCloseRight<CR>:BufferLineCloseLeft<CR>")
vim.keymap.set("n", "<leader>bc", ":BufferLinePickClose<CR>")

-- telescope -----------
require('telescope').setup{
    defaults = {
        initial_mode = 'insert',
        layout_strategy = 'horizontal',
        path_display = { 'smart' },
        file_ignore_patterns = {'.git/'},
        mappings = {
                      i = {
                           -- 历史记录
                            ["<Down>"] = "cycle_history_next",
                            ["<Up>"] = "cycle_history_prev",
                            -- 预览窗口上下滚动
                            ["<C-u>"] = "preview_scrolling_up",
                            ["<C-d>"] = "preview_scrolling_down",
                        },
                    },
                },
}
vim.keymap.set('n', '<leader>ff', ':Telescope find_files<cr>')
vim.keymap.set('n', '<leader>fg', ':Telescope live_grep<cr>')
vim.keymap.set('n', '<leader>fp', ':Telescope projects<cr>')
vim.keymap.set('n', '<leader>fb', ':Telescope current_buffer_fuzzy_find<cr>')
