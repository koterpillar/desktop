require "paq" {
  "savq/paq-nvim";

  "numToStr/Comment.nvim";

  "tpope/vim-sleuth";
  "sgur/vim-editorconfig";
  "junegunn/vim-easy-align"; -- align e.g. text in Markdown tables

  "nvim-lua/plenary.nvim";
  "nvim-telescope/telescope.nvim";

  {"nvim-treesitter/nvim-treesitter", run = function() vim.cmd("TSUpdate") end };

  "p00f/nvim-ts-rainbow";
}

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
