lua require('plugins')

set expandtab
set tabstop=2
set softtabstop=-1 " Follow tabstop
set shiftwidth=0 " Follow tabstop
set colorcolumn=80

set langmap=ФИСВУАПРШОЛДЬТЩЗЙКЫЕГМЦЧНЯ;ABCDEFGHIJKLMNOPQRSTUVWXYZ,фисвуапршолдьтщзйкыегмцчня;abcdefghijklmnopqrstuvwxyz

set undofile

set ignorecase smartcase

set number relativenumber

imap <C-BS> <C-W>
imap <C-Del> <C-O>de

set wildignore+=node_modules,__pycache__,*.pyc

au FileType python setlocal tabstop=4 expandtab

" Autosave
function! Autosave()
  if @% != ""
    update
  endif
endfunction
set updatetime=250
autocmd CursorHold * call Autosave()

" Telescope
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>
