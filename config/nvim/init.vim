lua require('plugins')
lua require('settings')

imap <C-BS> <C-W>
imap <C-Del> <C-O>de

" Autosave
function! Autosave()
  if @% != ""
    update
  endif
endfunction
autocmd CursorHold * call Autosave()
