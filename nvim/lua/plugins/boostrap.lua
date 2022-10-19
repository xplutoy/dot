local PKGS = {
    "savq/paq-nvim";
	-- misc
	'moll/vim-bbye';
	'nvim-lua/plenary.nvim';
	-- theme
	'folke/tokyonight.nvim';
	'glepnir/zephyr-nvim';
	'ellisonleao/gruvbox.nvim';
	-- ui
	'kyazdani42/nvim-web-devicons';
	-- "goolord/alpha-nvim";
	-- 'nvim-lualine/lualine.nvim';
	-- edit
	'nvim-telescope/telescope.nvim';
	'pocco81/auto-save.nvim';
	'akinsho/bufferline.nvim';
	'kyazdani42/nvim-tree.lua';
	"m4xshen/autoclose.nvim";
	-- lang
	'nvim-treesitter/nvim-treesitter';
	'lukas-reineke/indent-blankline.nvim';
	'numToStr/Comment.nvim';
	-- lang(lsp)
	'neovim/nvim-lspconfig';
	-- workspace
	'ahmedkhalf/project.nvim';
	'akinsho/toggleterm.nvim';
  }

local data_path = vim.fn.stdpath('data')
local function clone_paq()
  local path = data_path .. '/site/pack/paqs/start/paq-nvim'
  if vim.fn.empty(vim.fn.glob(path)) > 0 then
    vim.fn.system {
      'git',
      'clone',
      '--depth=1',
      'https://github.com/savq/paq-nvim.git',
      path
    }
  end
end
local function bootstrap_paq()
  clone_paq()
  -- Load Paq
  vim.cmd('packadd paq-nvim')
  local paq = require('paq')
  -- Exit nvim after installing plugins
  vim.cmd('autocmd User PaqDoneInstall quit')
  -- Read and install packages
  paq(PKGS)
  paq.install()
end

local paq_path = data_path .. '/site/pack/paqs'
if vim.fn.empty(vim.fn.glob(paq_path)) > 0 then
  bootstrap_paq()
else 
  require'paq'(PKGS)
end
