--------------------------- key-bindings -------------------
local keymap = vim.keymap.set       -- Shorten function name
local opts = { silent = true }      -- Silent keymap option

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",
-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)
-- Resize with arrows
keymap("n", "<C-Up>", ":resize -10<CR>", opts)
keymap("n", "<C-Down>", ":resize +10<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -5<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +5<CR>", opts)
-- Clear highlights
keymap("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)
-- 在visual mode 里粘贴不要复制
keymap("v", "p", '"_dP', opts)
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)


-- 插件的一些快捷键映射-----------------------------------------
-- nvimTree
keymap('n', '<leader>e', ':NvimTreeToggle<cr>', opts)
keymap('n', '<leader>ef', ':NvimTreeFindFileToggle<cr>', opts)
-- buffer & bufferline
keymap("n", "<S-h>", ":bprevious<CR>", opts)
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-w>", ":Bdelete!<CR>", opts)
keymap("n", "<leader>bh", ":BufferLineCyclePrev<CR>", opts)
keymap("n", "<leader>bl", ":BufferLineCycleNext<CR>", opts)
keymap("n", "<leader>bo", ":BufferLineCloseRight<CR>:BufferLineCloseLeft<CR>", opts)
keymap("n", "<leader>bc", ":BufferLinePickClose<CR>", opts)
-- treesitter 折叠
keymap("n", "z", ":foldclose<CR>", opts)
keymap("n", "Z", ":foldopen<CR>", opts)

-- telescope
keymap('n', '<leader>ff', ':Telescope find_files<cr>', opts)
keymap('n', '<leader>fg', ':Telescope live_grep<cr>', opts)
keymap('n', '<leader>fb', ':Telescope buffers<cr>', opts)
keymap('n', '<leader>fp', ':Telescope projects<cr>', opts)
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
keymap('n', '<C-_>', require("Comment.api").toggle.linewise.current,opts)
keymap('x', '<C-_>', require("Comment.api").toggle.blockwise.current,opts)

-- laygit term ------
keymap("n", "<leader>gg", "<cmd>lua _lazygit_toggle()<CR>", {noremap = true, silent = true})
