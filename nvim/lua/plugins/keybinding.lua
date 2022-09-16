--------------------------- key-bindings -------------------
local keymap = vim.keymap.set       -- Shorten function name

--Remap space as leader key
keymap("", "<Space>", "<Nop>")
vim.g.mapleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",
-- Better window navigation
keymap("n", "<C-h>", "<C-w>h")
keymap("n", "<C-j>", "<C-w>j")
keymap("n", "<C-k>", "<C-w>k")
keymap("n", "<C-l>", "<C-w>l")
-- Resize with arrows
keymap("n", "<C-Up>", ":resize -10<CR>")
keymap("n", "<C-Down>", ":resize +10<CR>")
keymap("n", "<C-Left>", ":vertical resize -5<CR>")
keymap("n", "<C-Right>", ":vertical resize +5<CR>")

keymap("v", "p", '"_dP')                    -- 在visual mode 里粘贴不要复制
keymap({'n', 'x'}, 'x', '"_x')              -- x只删除一个字符，一般不用保存到寄存器
-- Stay in indent mode
keymap("v", "<", "<gv")
keymap("v", ">", ">gv")


-- 插件的一些快捷键映射-----------------------------------------
-- nvimTree
keymap('n', '<leader>e', ':NvimTreeToggle<cr>')
keymap('n', '<leader>ef', ':NvimTreeFindFileToggle<cr>')
-- buffer & bufferline
keymap("n", "<S-h>", ":bprevious<CR>")
keymap("n", "<S-l>", ":bnext<CR>")
keymap("n", "<S-w>", ":Bdelete!<CR>")
keymap("n", "<leader>bh", ":BufferLineCyclePrev<CR>")
keymap("n", "<leader>bl", ":BufferLineCycleNext<CR>")
keymap("n", "<leader>bo", ":BufferLineCloseRight<CR>:BufferLineCloseLeft<CR>")
keymap("n", "<leader>bc", ":BufferLinePickClose<CR>")
-- treesitter 折叠
keymap("n", "z", ":foldclose<CR>")
keymap("n", "Z", ":foldopen<CR>")

-- telescope
keymap('n', '<leader>ff', ':Telescope find_files<cr>')
keymap('n', '<leader>fg', ':Telescope live_grep<cr>')
keymap('n', '<leader>fp', ':Telescope projects<cr>')
keymap('n', '<leader>fb', ':Telescope current_buffer_fuzzy_find<cr>')
telescope_keybindings = {
    i = {
        -- 历史记录
        ["<Down>"] = "cycle_history_next",
        ["<Up>"] = "cycle_history_prev",
        -- 预览窗口上下滚动
        ["<C-u>"] = "preview_scrolling_up",
        ["<C-d>"] = "preview_scrolling_down",
    },
}

-- Comment ----------
keymap('n', '<C-_>', require("Comment.api").toggle.linewise.current)
keymap('x', '<C-_>', require("Comment.api").toggle.blockwise.current)

-- laygit term ------
function _G.set_terminal_keymaps()
    local opts = {buffer = 0}
    vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], opts)
    vim.keymap.set('t', 'jk', [[<C-\><C-n>]], opts)
    vim.keymap.set('t', '<C-h>', [[<Cmd>wincmd h<CR>]], opts)
    vim.keymap.set('t', '<C-j>', [[<Cmd>wincmd j<CR>]], opts)
    vim.keymap.set('t', '<C-k>', [[<Cmd>wincmd k<CR>]], opts)
    vim.keymap.set('t', '<C-l>', [[<Cmd>wincmd l<CR>]], opts)
end
-- if you only want these mappings for toggle term use term://*toggleterm#* instead
vim.cmd('autocmd! TermOpen term://* lua set_terminal_keymaps()')
keymap("n", "<leader>tg", "<cmd>lua _lazygit_toggle()<CR>", {noremap = true, silent = true})
keymap("n", "<leader>tp", "<Cmd>lua _python_toggle()<CR>", {noremap = true, silent = true})

