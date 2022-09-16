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
vim.opt.cursorline = false                      -- highlight the current line
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
--------------------------- key-bindings -------------------



--------------------------- plugin -------------------------
--paq need bootstrap
require "paq" {
    'savq/paq-nvim';                  -- Let Paq manage itself

    -- edit & ui
    'nvim-treesitter/nvim-treesitter';
    'glepnir/zephyr-nvim';
    'ellisonleao/gruvbox.nvim';
    'folke/tokyonight.nvim';
    "goolord/alpha-nvim";
    'nvim-lua/plenary.nvim';
    'nvim-telescope/telescope.nvim';

    'kyazdani42/nvim-web-devicons';
    'moll/vim-bbye';
    'kyazdani42/nvim-tree.lua';
    'akinsho/bufferline.nvim';
    'nvim-lualine/lualine.nvim';
    'pocco81/auto-save.nvim';

    -- programing
    'nvim-treesitter/nvim-treesitter';
    'windwp/nvim-autopairs';
    'lewis6991/gitsigns.nvim';
    'akinsho/toggleterm.nvim';
    'numToStr/Comment.nvim';
    'lukas-reineke/indent-blankline.nvim';
    'RRethy/vim-illuminate';
}

-- themescheme ----------
vim.o.background = 'light'
local colorscheme = 'gruvbox'
-- tokyonight
-- gruvbox
-- zephyr
-- nord
-- onedark
-- nightfox
pcall(vim.cmd, "colorscheme " .. colorscheme)

-- alpha --------
require'alpha'.setup(require'alpha.themes.startify'.config)

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


-- lualine -------------
require('lualine').setup{
    options = {
        globalstatus = true,
        icons_enabled = true,
        theme = "auto",
        always_divide_middle = true,
    },
}


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

-- autopairs -----------
require("nvim-autopairs").setup {
    check_ts = true, -- treesitter integration
    disable_filetype = { "TelescopePrompt" , "vim" },
    enable_check_bracket_line = false,
    ignored_next_char = "[]"
}

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

-- toggleterm -----------
require'toggleterm'.setup({
    size = function(term)
      if term.direction == "horizontal" then
        return 15
      elseif term.direction == "vertical" then
        return vim.o.columns * 0.3
      end
    end,
    start_in_insert = true,
  })

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

local Terminal  = require('toggleterm.terminal').Terminal
local lazygit = Terminal:new({ cmd = "lazygit", hidden = true })

function _lazygit_toggle()
  lazygit:toggle()
end
vim.api.nvim_set_keymap("n", "<leader>gg", "<cmd>lua _lazygit_toggle()<CR>", {noremap = true, silent = true})

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
    show_first_indent_level = true,
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

--------------------------- plugin -------------------------
