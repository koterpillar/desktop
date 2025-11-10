vim.cmd.colorscheme "oxocarbon"

require("Comment").setup()

require("nvim-treesitter").setup()

vim.api.nvim_create_user_command('TSParserInstall', function()
  require 'nvim-treesitter'.install { 'stable', 'unstable' }
  require 'nvim-treesitter'.update():wait(300000)
end, {})

vim.api.nvim_create_autocmd('FileType', {
  pattern = '*',
  callback = function(args)
    local lang = args.match
    for _, lvl in ipairs({ 1, 2 }) do  -- 1=stable, 2=unstable
      for _, available in ipairs(require 'nvim-treesitter'.get_available(lvl)) do
        if lang == available then
          vim.treesitter.start()
          vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
          vim.wo.foldtext = 'v:lua.vim.treesitter.foldtext()'
          vim.opt.foldmethod = 'expr'
          return
        end
      end
    end
  end,
})

require("telescope").load_extension("recent_files")
require("telescope").setup {
  extensions = {
    recent_files = {
      only_cwd = true
    }
  }
}
