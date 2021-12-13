" My barebones vimrc without any Vundle dependencies.
"
" I'm attempting to optimize the following:
" - Minimize dependencies
" - Maximize ergonomics
" - Maximize Tmux compatibility
" - Minimize shadowing of existing Vim KBDs
"
" Warning: This is currently unstable as it is a work-in-progress.
"
" Author: William Carroll <wpcarro@gmail.com>

" Use <Space> as the leader key.
let mapleader = " "
nnoremap <leader>ev :tabnew<CR>:edit ~/.vimrc<CR>
nnoremap <leader>sv :source ~/.vimrc<CR>
nnoremap <leader>w  :w<CR>
nnoremap <leader>h  :help 

" increment,decrement numbers
nnoremap + <C-a>
" TODO: Restore with better KBD
" nnoremap - <C-x>

" Visit the CWD
nnoremap - :e .<CR>

" Turn line numbers on.
set number

" Easily create vertical, horizontal window splits.
nnoremap sh :vsplit<CR>
nnoremap sj :split<CR>:wincmd j<CR>
nnoremap sk :split<CR>
nnoremap sl :vsplit<CR>:wincmd l<CR>

" Move across window splits.
" TODO: Change to <M-{h,j,k,l}>.
nnoremap <C-h> :wincmd h<CR>
nnoremap <C-j> :wincmd j<CR>
nnoremap <C-k> :wincmd k<CR>
nnoremap <C-l> :wincmd l<CR>

" TODO: Support these.
" nnoremap <M-q> :q<CR>
" nnoremap <M-h> :wincmd h<CR>
" nnoremap <M-j> :wincmd j<CR>
" nnoremap <M-k> :wincmd k<CR>
" nnoremap <M-l> :wincmd l<CR>

" Use <Enter> instead of G to support:
"        20<Enter> - to jump to line 20
"       d20<Enter> - to delete from the current line until line 20
"   <C-v>20<Enter> - to select from the current line until line 20
nnoremap <Enter> G
onoremap <Enter> G
vnoremap <Enter> G

" Easily change modes on keyboards that don't have CapsLock mapped to <Esc>
inoremap jk      <ESC>

" CRUD tabs.
nnoremap <TAB>   :tabnext<CR>
nnoremap <S-TAB> :tabprevious<CR>
nnoremap <C-t>   :tabnew<CR>:edit .<CR>
nnoremap <C-w>   :tabclose<CR>
" TODO: Re-enable these once <M-{h,j,k,l}> are supported.
" nnoremap <C-l> :+tabmove<CR>
" nnoremap <C-h> :-tabmove<CR>

" Use H,L to goto beggining,end of a line.
" Swaps the keys to ensure original functionality of H,L are preserved.
nnoremap H ^
nnoremap L $
nnoremap ^ H
nnoremap $ L

" Use H,L in visual mode too
vnoremap H ^
vnoremap L $
vnoremap ^ H
vnoremap $ L

" Emacs hybrid mode
" TODO: model this after tpope's rsi.vim (Readline-style insertion)
cnoremap <C-g> <C-c>
cnoremap <C-a> <C-b>
inoremap <C-a> <C-o>^
inoremap <C-e> <C-o>$
inoremap <C-b> <C-o>h
inoremap <C-f> <C-o>l

" Indenting
" The following three settings are based on option 2 of `:help tabstop`
set tabstop=4
set shiftwidth=4
set expandtab
set autoindent
