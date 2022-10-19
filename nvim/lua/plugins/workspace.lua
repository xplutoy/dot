-- nvim-tree ----------------
-- disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded = 1
vim.g.loaded_netrwPlugin = 1
require("nvim-tree").setup({
    disable_netrw = true,
    hijack_cursor = false,
    sort_by = "case_sensitive",
    view = {
        side = "left",
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
-- nvimTree
vim.keymap.set('n', '<leader>e', ':NvimTreeToggle<cr>')
vim.keymap.set('n', '<leader>ef', ':NvimTreeFindFileToggle<cr>')

-- project
vim.g.nvim_tree_respect_buf_cwd = 1
require'project_nvim'.setup({
    detection_methods = { "pattern" },
    patterns = {
        ".git",
        ".venv",
    },
})
require('telescope').load_extension('projects')

-- toggleterm -----------
require'toggleterm'.setup({
    open_mapping = '<C-t>',
    size = function(term)
        if term.direction == "horizontal" then
            return 15
        elseif term.direction == "vertical" then
            return vim.o.columns * 0.3
        end
    end,
    start_in_insert = true,
    shade_terminals = true,
})