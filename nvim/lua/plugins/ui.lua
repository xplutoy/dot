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
    default = true,
}

-- lualine -------------
require('lualine').setup{
    options = {
        globalstatus = true,
        icons_enabled = true,
        theme = "auto",
        always_divide_middle = true,
    },
}
