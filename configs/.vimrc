" -- BEGIN: Vundle config --
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.

" Displays git information in airline.
Plugin 'tpope/vim-fugitive'

Plugin 'Raimondi/delimitMate'

" Displays git-tracked C*UD ops within gutter.
Plugin 'airblade/vim-gitgutter'

" Fuzzy-finder
Plugin 'kien/ctrlp.vim'

" Grep file contents
Plugin 'mileszs/ack.vim'

" JS support
Plugin 'pangloss/vim-javascript'

" Visual dir-tree navigation
Plugin 'scrooloose/nerdtree'

" Syntax Highlighting Support
Plugin 'lambdatoast/elm.vim'

" Elixir Plugins
Plugin 'elixir-lang/vim-elixir'
Plugin 'slashmili/alchemist.vim'
Plugin 'powerman/vim-plugin-AnsiEsc'

" TypeScript Plugins
Plugin 'rschmukler/typescript-vim'

" Themes
Plugin 'sickill/vim-monokai'
Plugin 'altercation/vim-colors-solarized'
Plugin 'mhartington/oceanic-next'

" Executes shell commands and pipes output into new Vim buffer.
Plugin 'sjl/clam.vim'

" Multiple cursors for simultaneous edits.
" NOTE: use <C-n> to run miltiple cursors not <C-d>
Plugin 'terryma/vim-multiple-cursors'

" Visualize buffers
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

" Visually align assignments
Plugin 'godlygeek/tabular'

" Visually Highlight and comment code.
Plugin 'tpope/vim-commentary'

" Seamlessly navigate Vim and Tmux with similar bindings.
Plugin 'christoomey/vim-tmux-navigator'

" Async `:make` for code linting etc.
Plugin 'neomake/neomake'

" Color pack
Plugin 'flazz/vim-colorschemes'


call vundle#end()            " required
filetype plugin indent on    " required
" Put your non-Plugin stuff after this line
" -- END: Vundle config --


" Neomake Settings
autocmd! BufWritePost * Neomake

" Elixir linting
let g:neomake_elixir_credo_maker = {
      \ 'exe': 'mix',
      \ 'args': ['credo', 'list', '%:p', '--format=oneline'],
      \ 'errorformat':
      \   '%W[F] %. %f:%l:%c %m,' .
      \   '%W[F] %. %f:%l %m,' .
      \   '%W[R] %. %f:%l:%c %m,' .
      \   '%W[R] %. %f:%l %m,' .
      \   '%I[C] %. %f:%l:%c %m,' .
      \   '%I[C] %. %f:%l %m,' .
      \   '%-Z%.%#'
      \ }


" Airline Settings
" Enables the list of buffers.
let g:airline#extensions#tabline#enabled = 1

" Buffer numbers alongside files
let g:airline#extensions#tabline#buffer_nr_show = 1

" Shows the filename only.
let g:airline#extensions#tabline#fnamemod = ':t'

" Allow glyphs in airline
let g:airline_powerline_fonts = 1

" Change Airline theme
let g:airline_theme = 'hybrid'


" Jump to buffers.
nmap <F1> :1b<CR>
nmap <F2> :2b<CR>
nmap <F3> :3b<CR>
nmap <F4> :4b<CR>
nmap <F5> :5b<CR>
nmap <F6> :6b<CR>
nmap <F7> :7b<CR>
nmap <F8> :8b<CR>
nmap <F9> :9b<CR>



" It's the twenty-first century...no swaps.
set noswapfile


" Allow visual tab completion in command mode
set wildmenu


" Show Vim commands as they're being input.
set showcmd


" Code folding
set foldmethod=indent
set foldnestmax=10
set nofoldenable
set foldlevel=4


" Use relative line numbers
set relativenumber


" Opens all folds within the buffer
nnoremap ZZ zR

" Closes all folds within the buffer
nnoremap zz zM

" Opens all folds beneath the cursor
" NOTE: j is the character to go down
nnoremap zJ zO

" Opens single fold beneath the cursor
" NOTE: j is the character to go down
nnoremap zj zo

" Opens single fold beneath the cursor
" NOTE: k is the character to go down
nnoremap zK zC

" Opens single fold beneath the cursor
" NOTE: k is the character to go down
nnoremap zk zc


" Smart Comment code (has dependency 'tpope/vim-commentary')
nnoremap <C-/> Vgc
vnoremap <C-/> gc


" Changes <leader> to <space> character.
let mapleader = " "


" Switch to MRU'd buffer
nnoremap <leader><leader> <C-^>


" Supports mouse interaction.
set mouse=a


" Highlights matches during a search.
set hlsearch

" Search for gibberish to clear the most recent search
noremap <leader>/ :silent! /__wc_gibberish__/<CR>:echo "Search cleared."<CR>


" Use custom-made snippets.
nnoremap ,jsfn :-1read $HOME/.vim/function_skeleton.js<CR>o


" backspace settings
set backspace=2
set backspace=indent,eol,start


" Javascript specific variables
let g:javascript_plugin_jsdoc = 1

" GlobalListchars
set list
set listchars=tab:··,trail:·,nbsp:·


" Keeps everything concealed at all times. Even when cursor is on the word.
set conceallevel=1
set concealcursor=nvic


" map jk to <Esc>
inoremap jk <Esc>


" Conventional Emacs line-editor defaults
inoremap <C-a> <Esc>I
inoremap <C-e> <Esc>A


" Manage Vertical and Horizontal splits
nnoremap sl <Esc>:vs<CR><C-w>l
nnoremap sh <Esc>:vs<CR>
nnoremap sj <Esc>:sp<CR><C-w>j
nnoremap sk <Esc>:sp<CR>


" Move around splits with <leader>
" NOTE: no longer needed with tmux navigator plugin
" nnoremap <leader>h <C-w>h
" nnoremap <leader>j <C-w>j
" nnoremap <leader>k <C-w>k
" nnoremap <leader>l <C-w>l


" Delete (i.e. "close") the currently opened buffer
" TODO: unless it's a split window, which should be :q
nnoremap <leader>q :bdelete<CR>


" CtrlP config
" Set default CtrlP command.
let g:ctrlp_cmd = 'CtrlPBuffer'
" let g:ctrlp_cmd = 'CtrlPMRU'

" Set runtime path
set runtimepath^=~/.vim/bundle/ctrlp.vim

" Fuzzy-find files.
nnoremap <leader>pf :CtrlP<CR>

" Fuzzy-find open buffers.
nnoremap <leader>pb :CtrlPBuffer<CR>

" Fuzzy-find open buffers.
nnoremap <leader>pm :CtrlPMRUFiles<CR>


" Buffer creation and management
" Buffer movement
nnoremap <Tab> :1bnext<CR>
nnoremap <S-Tab> :1bprevious<CR>

" Buffer creation
" nnoremap <C-t> :enew<CR>

" Buffer deletion
nnoremap <leader>bq :bp <BAR> bd #<CR>


" make Y do what is intuitive given: 
"   D: deletes until EOL
"   C: changes until EOL
"   Y: (should) yank until EOL
nnoremap Y y$


" scrolling and maintaing mouse position
" nnoremap <C-j> j<C-e>
" nnoremap <C-k> k<C-y>


" Shorter binding for window rotations
nnoremap <C-r> <C-w><C-r>


" Basic settings
set number
set wrap!
set tabstop=2
set expandtab
set shiftwidth=2
set background=dark

syntax enable
colorscheme hybrid

set history=1000
set undolevels=1000

set t_Co=255


" Support italics
highlight Comment cterm=italic


" remap redo key that is eclipsed by `rotate` currently
nnoremap U :redo<CR>


" Repeat last colon-command
nnoremap ;; @:<CR>
vnoremap ;; @:<CR>


" Define highlighting groups
" NOTE: The ANSII aliases for colors will change when iTerm2 settings are
" changed.
highlight InterestingWord1 ctermbg=Magenta ctermfg=Black
highlight InterestingWord2 ctermbg=Blue ctermfg=Black

" h1 highlighting
nnoremap <silent> <leader>1 :execute '2match InterestingWord1 /\<<c-r><c-w>\>/'<CR>
nnoremap <silent> <leader>x1 :execute '2match none'<CR>
vnoremap <silent> <leader>1 :execute '2match InterestingWord1 /\<<c-r><c-w>\>/'<CR>

" h2 highlighting
nnoremap <silent> <leader>2 :execute '3match InterestingWord2 /\<<c-r><c-w>\>/'<CR>
nnoremap <silent> <leader>x2 :execute '3match none'<CR>

"clear all highlighted groups
nnoremap <silent> <leader>xx :execute '2match none'<CR> :execute '3match none'<CR> hh


" pasteboard copy & paste
nnoremap <C-c> V"+y
vnoremap <C-c> "+y

inoremap <C-v> <Esc>"+pa
" nnoremap <C-v> o<Esc>"+p
vnoremap <C-v> "+p


" Manage 80 char line limits
highlight OverLength1 ctermbg=Magenta ctermfg=Black
highlight OverLength2 ctermbg=LightMagenta ctermfg=Black
highlight OverLength3 ctermbg=White ctermfg=Black
" match OverLength3 /\%81v.\+/
match OverLength2 /\%91v.\+/
" match OverLength3 /\%101v.\+/


" Toggle word-wrapping
nnoremap <leader>w :set wrap!<CR>


" Resize split to 10,20,...,100 chars
" Uncomment the next lines for support at those sizes.
" These bindings interfere with the highlight groups, however.
" Increases the width of a vertical split.
" nnoremap <leader>1 :vertical resize 10<CR>
" nnoremap <leader>2 :vertical resize 20<CR>
nnoremap <leader>3 :vertical resize 30<CR>
nnoremap <leader>4 :vertical resize 40<CR>
nnoremap <leader>5 :vertical resize 50<CR>
nnoremap <leader>6 :vertical resize 60<CR>
nnoremap <leader>7 :vertical resize 70<CR>
nnoremap <leader>8 :vertical resize 80<CR>
nnoremap <leader>9 :vertical resize 90<CR>
nnoremap <leader>0 :vertical resize 100<CR>


" Increases the height of a horizontal split.
nnoremap <leader>v1 :resize 5<CR>
nnoremap <leader>v2 :resize 10<CR>
nnoremap <leader>v3 :resize 15<CR>
nnoremap <leader>v4 :resize 20<CR>
nnoremap <leader>v5 :resize 25<CR>
nnoremap <leader>v6 :resize 30<CR>
nnoremap <leader>v7 :resize 35<CR>
nnoremap <leader>v8 :resize 40<CR>
nnoremap <leader>v9 :resize 45<CR>
nnoremap <leader>v0 :resize 50<CR>


" NERDTree settings
" Show hidden files by default. (Toggle with capital 'i')
let NERDTreeShowHidden = 1

" View Directory tree with ctrl + n
nnoremap <leader>n :NERDTreeToggle<CR>

" View open buffer location in tree.
nnoremap <leader>f :NERDTreeFind<CR>


" Jump to the top / bottom of the Viewport
nnoremap K H
nnoremap J L


" BOL and EOL
nnoremap H ^
vnoremap H ^
nnoremap L $
vnoremap L $


" Search for visually selected text
vnoremap // y/<C-r>"<CR>N


" trim trailing whitespace on save
autocmd BufWritePre *.{js,py,tpl,less,html,ex,exs,txt} :%s/\s\+$//e


" set default font and size
set guifont=Operator\ Mono:h16


" Use .gitignore file to populate Ctrl-P
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']


" Ignores dirs and files
let g:ctrlp_custom_ignore = {
  \ 'dir':  'node_modules',
  \ 'file': '\v\.(exe|dll|png|jpg|jpeg)$'
\}


" WIP: Run elixir tests on that line
nnoremap <leader>t :call ExTestToggle()<CR>


" Jumps from an Elixir module file to an Elixir test file.
fun! ExTestToggle()
  if expand('%:e') == "ex"

    let test_file_name = expand('%:t:r') . "_test.exs"
    let test_file_dir = substitute(expand('%:p:h'), "/lib/", "/test/", "")
    let full_test_path = join([test_file_dir, test_file_name], "/")

    e `=full_test_path`

  elseif match(expand('%:t'), "_test.exs") != -1

    let test_file_name = expand('%:t:r')
    let offset_amt = strlen(test_file_name) - strlen("_test")
    let module_file_name = strpart(test_file_name, 0, offset_amt) . ".ex"
    let module_file_dir = substitute(expand('%:p:h'), "/test/", "/lib/", "")
    let full_module_path = join([module_file_dir, module_file_name], "/")

    e `=full_module_path`

  endif
endfun


" Creates intermediate directories and file to match current buffer's filepath
fun! CreateNonExistingDirsAndFile()
  ! echo "Creating directory..." && mkdir -p %:p:h && echo "Created directory." && echo "Creating file..." && touch %:t:p && echo "Created file."

  " Write the buffer to the recently created file.
  w
endfun

