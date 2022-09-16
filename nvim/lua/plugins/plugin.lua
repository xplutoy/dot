--------------------------- plugin -------------------------
--paq need bootstrap

local PKGS = {
	'savq/paq-nvim';

	-- List your packages here!
	-- misc
	'moll/vim-bbye';
	'nvim-lua/plenary.nvim';
	-- theme
	'folke/tokyonight.nvim';
	'glepnir/zephyr-nvim';
	'ellisonleao/gruvbox.nvim';
	-- ui
	'kyazdani42/nvim-web-devicons';
	"goolord/alpha-nvim";
	'nvim-lualine/lualine.nvim';
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
	-- workspace
	'ahmedkhalf/project.nvim';
	'lewis6991/gitsigns.nvim';
	'akinsho/toggleterm.nvim';
}

require'paq'(PKGS)

local function clone_paq()
	local path = vim.fn.stdpath('data') .. '/site/pack/paqs/start/paq-nvim'
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

local function bootstrap_paq(bootstrap)
    if not bootstrap then
        require'paq'(PKGS)
    else
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
end

return { bootstrap_paq = bootstrap_paq }
