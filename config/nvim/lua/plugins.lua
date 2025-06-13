vim.cmd.colorscheme "oxocarbon"

require("Comment").setup()

require("nvim-treesitter.configs").setup {
  ensure_installed = "all",
  highlight = {
    enable = true
  }
}

require("telescope").load_extension("recent_files")
require("telescope").setup {
  extensions = {
    recent_files = {
      only_cwd = true
    }
  }
}
