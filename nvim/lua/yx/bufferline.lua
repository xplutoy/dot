vim.opt.termguicolors = true
require('bufferline').setup({
  options = {
    number = "buffer_id",
    close_command = "bdelete %d", -- can be a string | function, see "Mouse actions"
    right_mouse_command = "bdelete %d", -- can be a string | function, see "Mouse actions"
    -- left_mouse_command = "buffer %d",
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