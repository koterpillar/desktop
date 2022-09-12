local cmd = vim.cmd
local opt = vim.opt
local map = vim.api.nvim_set_keymap
local default_opts = {noremap = true, silent = true}

opt.colorcolumn = '80'
opt.number = true
opt.relativenumber = true

opt.tabstop = 2
opt.softtabstop = -1
opt.shiftwidth = 0
opt.expandtab = true
cmd [[autocmd FileType python setlocal tabstop=4]]

opt.ignorecase = true
opt.smartcase = true

opt.undofile = true
opt.updatetime = 250

opt.title = true

opt.wildignore = { "node_modules", "__pycache__", "*.pyc" }

opt.langmap = "ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz"

map('n', '<leader>ff', '<cmd>Telescope find_files<cr>', default_opts)
map('n', '<leader>fg', '<cmd>Telescope live_grep<cr>', default_opts)
map('n', '<leader>fb', '<cmd>Telescope buffers<cr>', default_opts)
map('n', '<leader>fh', '<cmd>Telescope help_tags<cr>', default_opts)

cmd [[command! -range AlignMarkdownTable <line1>,<line2>EasyAlign *|]]
cmd [[command! AlignThisMarkdownTable '{,'}AlignMarkdownTable]]


-- preservim/vim-markdown
vim.g.vim_markdown_folding_disabled = 1
vim.g.vim_markdown_fenced_languages = {
  'haskell',
  'java',
  'javascript', 'js=javascript', 'jsx=javascript',
  'json',
  'markdown',
  'python',
  'sh', 'bash=sh', 'shell=sh', 'shell script=sh',
  'typescript', 'ts=typescript', 'tsx=typescript',
  'yaml', 'yml=yaml',
}
