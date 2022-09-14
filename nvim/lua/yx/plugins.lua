-- return require('packer').startup(function()
--     -- Packer can manage itself
--     use 'wbthomason/packer.nvim'

--     use{
--         'glepnir/zephyr-nvim',
--         requires = { 'nvim-treesitter/nvim-treesitter', opt = true },
--       }

--     use {
--         'kyazdani42/nvim-tree.lua',
--         requires = 'kyazdani42/nvim-web-devicons'
--       }

--   end)

local function setup_wraper(name)
    require(name).setup{}
end

require "paq" {
    'savq/paq-nvim';                  -- Let Paq manage itself

    {'nvim-treesitter/nvim-treesitter', opt=true};
    'glepnir/zephyr-nvim';

    {'kyazdani42/nvim-web-devicons', opt=true};
    'kyazdani42/nvim-tree.lua';
    'akinsho/bufferline.nvim';

    {'pocco81/auto-save.nvim', run=setup_wraper('auto-save')};
}