require("Comment").setup()

require("nvim-treesitter.configs").setup {
  ensure_installed = "all",
  highlight = {
    enable = true
  },
  rainbow = {
    enable = true,
    max_file_lines = 5000,
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
