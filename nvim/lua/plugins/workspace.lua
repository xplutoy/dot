-- project
vim.g.nvim_tree_respect_buf_cwd = 1
require'project_nvim'.setup({
    detection_methods = { "pattern" },
    patterns = {
        "README.md",
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
local Terminal  = require('toggleterm.terminal').Terminal
local lazygit = Terminal:new({ cmd = "lazygit", hidden = true })
function _lazygit_toggle()
    lazygit:toggle()
end
local pyterm = Terminal:new({cmd = 'python',direction = 'horizontal'})
function _python_toggle()
    pyterm:toggle()
end

-- gitsigns -------------
require('gitsigns').setup {
    on_attach = function(bufnr)
        local gs = package.loaded.gitsigns
        local function map(mode, l, r, opts)
            opts = opts or {}
            opts.buffer = bufnr
            vim.keymap.set(mode, l, r, opts)
        end

        -- Navigation
        map('n', ']c', function()
            if vim.wo.diff then return ']c' end
            vim.schedule(function() gs.next_hunk() end)
            return '<Ignore>'
        end, {expr=true})

        map('n', '[c', function()
            if vim.wo.diff then return '[c' end
            vim.schedule(function() gs.prev_hunk() end)
            return '<Ignore>'
        end, {expr=true})

        -- Actions
        map({'n', 'v'}, '<leader>hs', ':Gitsigns stage_hunk<CR>')
        map({'n', 'v'}, '<leader>hr', ':Gitsigns reset_hunk<CR>')
        map('n', '<leader>gS', gs.stage_buffer)
        map('n', '<leader>gu', gs.undo_stage_hunk)
        map('n', '<leader>gR', gs.reset_buffer)
        map('n', '<leader>gp', gs.preview_hunk)
        map('n', '<leader>gb', function() gs.blame_line{full=true} end)
        map('n', '<leader>gtb', gs.toggle_current_line_blame)
        map('n', '<leader>gd', gs.diffthis)
        map('n', '<leader>gD', function() gs.diffthis('~') end)
        map('n', '<leader>gtd', gs.toggle_deleted)

        -- Text object
        map({'o', 'x'}, 'ig', ':<C-U>Gitsigns select_hunk<CR>')
    end
}