-- disable netrw at the very start of your init.lua (strongly advised)
vim.g.loaded = 1
vim.g.loaded_netrwPlugin = 1

vim.g.nvim_tree_hide_dotfiles = 1
vim.o.termguicolors = true

--  setup with some options
require("nvim-tree").setup({
  auto_close = true,
  disable_netrw = true,
  hijack_cursor = false,
  sort_by = "case_sensitive",
  view = {
    side = "left",
    adaptive_size = true,
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