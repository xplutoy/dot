--------------------------- basic --------------------------
vim.opt.backup = false                          -- creates a backup file
vim.opt.clipboard = "unnamedplus"               -- allows neovim to access the system clipboard
vim.opt.cmdheight = 2                           -- more space in the neovim command line for displaying messages
vim.opt.completeopt = { "menuone", "noselect" } -- mostly just for cmp
vim.opt.conceallevel = 0                        -- so that `` is visible in markdown files
vim.opt.fileencoding = "utf-8"                  -- the encoding written to a file
vim.opt.hlsearch = true                         -- highlight all matches on previous search pattern
vim.opt.ignorecase = true                       -- ignore case in search patterns
vim.opt.mouse = "a"                             -- allow the mouse to be used in neovim
vim.opt.pumheight = 10                          -- pop up menu height
vim.opt.showmode = true                        -- we don't need to see things like -- INSERT -- anymore
vim.opt.showtabline = 2                         -- always show tabs
vim.opt.smartcase = true                        -- smart case
vim.opt.smartindent = true                      -- make indenting smarter again
vim.opt.splitbelow = true                       -- force all horizontal splits to go below current window
vim.opt.splitright = true                       -- force all vertical splits to go to the right of current window
vim.opt.swapfile = false                        -- creates a swapfile
vim.opt.termguicolors = true                    -- set term gui colors (most terminals support this)
vim.opt.timeoutlen = 1000                       -- time to wait for a mapped sequence to complete (in milliseconds)
vim.opt.undofile = true                         -- enable persistent undo
vim.opt.updatetime = 300                        -- faster completion (4000ms default)
vim.opt.writebackup = false                     -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
vim.opt.expandtab = true                        -- convert tabs to spaces
vim.opt.shiftwidth = 4                          -- the number of spaces inserted for each indentation
vim.opt.tabstop = 4                             -- insert 2 spaces for a tab
vim.opt.cursorline = true                       -- highlight the current line
vim.opt.number = true                           -- set numbered lines
vim.opt.laststatus = 3
vim.opt.showcmd = false
vim.opt.ruler = false
vim.opt.numberwidth = 2                         -- set number column width to 2 {default 4}
vim.opt.signcolumn = "yes"                      -- always show the sign column, otherwise it would shift the text each time
vim.opt.wrap = false                            -- display lines as one long line
vim.opt.scrolloff = 8                           -- is one of my fav
vim.opt.sidescrolloff = 8
vim.opt.guifont = "monospace:h17"               -- the font used in graphical neovim applications
vim.opt.fillchars.eob=" "
vim.opt.shortmess:append "c"
vim.opt.whichwrap:append("<,>,[,],h,l")
vim.opt.iskeyword:append("-")
vim.o.wildmenu = true
--------------------------- basic --------------------------

--------------------------- plugin -------------------------
--paq need bootstrap
require "paq" {
    'savq/paq-nvim';                  -- Let Paq manage itself

    -- edit & ui
    'nvim-treesitter/nvim-treesitter';
    'glepnir/zephyr-nvim';
    'ellisonleao/gruvbox.nvim';
    'folke/tokyonight.nvim';

    'kyazdani42/nvim-web-devicons';
    'moll/vim-bbye';
    'kyazdani42/nvim-tree.lua';
    'akinsho/bufferline.nvim';
    'nvim-lualine/lualine.nvim';
    'pocco81/auto-save.nvim';

    -- programing
    'nvim-treesitter/nvim-treesitter';
}

-- themescheme ----------
vim.o.background = "dark"
local colorscheme = "zephyr"
-- tokyonight
-- gruvbox
-- zephyr
-- nord
-- onedark
-- nightfox
pcall(vim.cmd, "colorscheme " .. colorscheme)


-- nvim-web-devicons --------
require'nvim-web-devicons'.setup {
    -- globally enable default icons (default to false)
    -- will get overriden by `get_icons` option
    default = true,
}

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


-- lualine -------------
require('lualine').setup{
}


-- nvim-treesitter -----
require'nvim-treesitter.configs'.setup {
    -- A list of parser names, or "all"
    ensure_installed = { "python", "lua"},
    sync_install = false,
    auto_install = true,

    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
    },
}
--------------------------- plugin -------------------------


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
-- Navigate buffers
keymap("n", "<S-l>", ":bnext<CR>", opts)
keymap("n", "<S-h>", ":bprevious<CR>", opts)
-- Clear highlights
keymap("n", "<leader>h", "<cmd>nohlsearch<CR>", opts)
-- 在visual mode 里粘贴不要复制
keymap("v", "p", '"_dP', opts)
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)


-- nvimTree
keymap('n', '<leader>e', '<cmd>NvimTreeToggle<cr>', opt)
-- bufferline
keymap("n", "<S-h>", ":BufferLineCyclePrev<CR>", opt)
keymap("n", "<S-l>", ":BufferLineCycleNext<CR>", opt)
--"moll/vim-bbye"
keymap("n", "<S-w>", ":Bdelete!<CR>", opt)
--------------------------- key-bindings -------------------
