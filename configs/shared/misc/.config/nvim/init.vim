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

" easier file navigation
Plugin 'tpope/vim-vinegar'

" Displays git-tracked C*UD ops within gutter.
Plugin 'airblade/vim-gitgutter'

" Fuzzy-finder
Plugin 'kien/ctrlp.vim'

" Grep file contents
Plugin 'mileszs/ack.vim'

" JS support
Plugin 'pangloss/vim-javascript'

" JSX support
Plugin 'mxw/vim-jsx'

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

" Macros for quotes, parens, etc.
Plugin 'tpope/vim-surround'

" Allows Plugins to be repeated with `.` character
Plugin 'tpope/vim-repeat'

" Pairs of mappings
Plugin 'tpope/vim-unimpaired'

" LISPs support
Plugin 'guns/vim-sexp'
Plugin 'tpope/vim-sexp-mappings-for-regular-people'
let g:sexp_enable_insert_mode_mappings = 0
let g:sexp_filetypes = ''

" Seamlessly navigate Vim and Tmux with similar bindings.
Plugin 'christoomey/vim-tmux-navigator'

" Async `:make` for code linting etc.
Plugin 'neomake/neomake'

" Color pack
Plugin 'flazz/vim-colorschemes'

" Better buffer mgt than CtrlP
Plugin 'yegappan/mru'

Plugin 'zanglg/nova.vim'

" Emulates Emacs's Helm Swoop search
Plugin 'pelodelfuego/vim-swoop'

" Fish Shell support
Plugin 'dag/vim-fish'

" Gotham colorscheme
Plugin 'whatyouhide/vim-gotham'

" Transparent encryption + decryption
Plugin 'jamessan/vim-gnupg'

" Javascript auto-formatting
Plugin 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown'] }

" Support Org mode
Plugin 'jceb/vim-orgmode'

" ReaosnML
Plugin 'reasonml-editor/vim-reason-plus'

" Autocompletion
Plugin 'junegunn/fzf'

" Better JSON highlighting
Plugin 'elzr/vim-json'

" Text objects made easy
Plugin 'kana/vim-textobj-user'

" Elixir text objects
Plugin 'andyl/vim-textobj-elixir'

" Making HTML editing faster
Plugin 'mattn/emmet-vim'

" Snippets for all languages
Plugin 'honza/vim-snippets'

" Automatic bracket insertion
Plugin 'jiangmiao/auto-pairs'

" Linting & error warnings
Plugin 'vim-syntastic/syntastic'

" Atom colorscheme
Plugin 'rakr/vim-one'

" Nix syntax highlighting
Plugin 'LnL7/vim-nix'

call vundle#end()            " required
filetype plugin indent on    " required
" Put your non-Plugin stuff after this line
" -- END: Vundle config --

" Changes <leader> to <space> character.
let mapleader = " "


" Highlight column width
set textwidth=80
set colorcolumn=+0

" autoreload a file when it changes on disk
set autoread

" default to case-insensitive searching
set ignorecase

" JSX configuration
let g:jsx_ext_required = 0


autocmd FileType reason nnoremap <buffer> gd :call LanguageClient_textDocument_definition()<CR>
autocmd FileType reason nnoremap <buffer> gf :call LanguageClient_textDocument_formatting()<CR>
autocmd FileType reason nnoremap <buffer> gh :call LanguageClient_textDocument_hover()<CR>
autocmd FileType reason nnoremap <buffer> gr :call LanguageClient_textDocument_rename()<CR>

" Replace <CR> with G for faster navigation
nnoremap <CR> G


" Syntastic configuration
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_eslint_generic = 1
" this is a hack to prevent a false negative
" https://github.com/vim-syntastic/syntastic/issues/1692
let g:syntastic_javascript_eslint_exec = '/bin/ls'
let g:syntastic_javascript_eslint_exe = 'npx eslint'
let g:syntastic_javascript_eslint_args = '-f compact'

" javascript autocompletion
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType javascript nnoremap <buffer> gf :Prettier<CR>

" Maximize the current window
" Similar to Tmux mapping alt-z in my tmux.conf
nnoremap t% :tab sp<CR>

" Allow C-g to act like C-c the way it does in Emacs
cnoremap <C-g> <C-c>

" Prettier configuration
let g:prettier#exec_cmd_async = 1
" force Prettier to run on files even without the @format pragma
let g:prettier#autoformat = 0


" Basic settings
" Thin cursor on INSERT mode
if has('nvim')
  let $NVIM_TUI_ENABLE_CURSOR_SHAPE = 1
endif

set number
set nowrap
set tabstop=2
set expandtab
set shiftwidth=2
set background=dark

syntax enable
colorscheme one
set background=dark

" Vim in terminal cannot have a different font from the one set within your
" terminal. However, this setting will set the font for the GUI version.
if has('gui_running')
  set guifont=Operator\ Mono:h12
endif

if has('termguicolors')
  set termguicolors
endif

if &term =~# '^screen'
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif

set history=1000
set undolevels=1000

set t_Co=255

" Support italics
highlight Comment cterm=italic


" quickly edit popular configuration files
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
nnoremap <leader>ee :vsplit ~/.emacs.d/init.el<CR>
nnoremap <leader>ez :vsplit ~/.zshrc<CR>
nnoremap <leader>ea :vsplit ~/aliases.zsh<CR>
nnoremap <leader>ef :vsplit ~/functions.zsh<CR>
nnoremap <leader>el :vsplit ~/variables.zsh<CR>
nnoremap <leader>ex :vsplit ~/.Xresources<CR>
nnoremap <leader>ei :vsplit ~/.config/i3/config<CR>

" quickly source your vimrc
nnoremap <leader>sv :source $MYVIMRC<CR>

" quickly edit your snippets
nnoremap <leader>es :vsplit<CR>:edit ~/.vim/bundle/vim-snippets/snippets/reason.snippets<CR>


" Auto resize window splits
autocmd VimResized * wincmd =


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


let g:neomake_elixir_enabled_makers = ['mix', 'credo']

augroup my_error_signs
  au!
  autocmd ColorScheme * hi NeomakeErrorSign ctermfg=203 guifg=#ff5f5f
  autocmd ColorScheme * hi NeomakeWarningSign ctermfg=209 guifg=#ffaf00
  autocmd ColorScheme * hi NeomakeInfoSign ctermfg=183 guifg=#dfafff
  autocmd ColorScheme * hi NeomakeMessageSign ctermfg=27 guifg=#0087ff
augroup END


let g:neomake_error_sign = {
            \ 'text': '>>',
            \ 'texthl': 'NeoMakeErrorSign',
            \ }

let g:neomake_warning_sign = {
            \ 'text': '>>',
            \ 'texthl': 'NeoMakeWarningSign',
            \ }

let g:neomake_info_sign = {
            \ 'text': '>>',
            \ 'texthl': 'NeoMakeInfoSign',
            \ }

let g:neomake_message_sign = {
            \ 'text': '>>',
            \ 'texthl': 'NeoMakeMessageSign',
            \ }

function! <SID>LocationPrevious()
  try
    lprev
  catch /^Vim\%((\a\+)\)\=:E553/
    llast
  endtry
endfunction

function! <SID>LocationNext()
  try
    lnext
  catch /^Vim\%((\a\+)\)\=:E553/
    lfirst
  endtry
endfunction

nnoremap <Leader>[ :call <SID>LocationPrevious()<CR>
nnoremap <Leader>] :call <SID>LocationNext()<CR>


" Alchemist settings
let g:alchemist#elixir_erlang_src = '/usr/local/share/src'


" Airline Settings
" Enables the list of buffers.
let g:airline#extensions#tabline#enabled = 0

" Buffer numbers alongside files
let g:airline#extensions#tabline#buffer_nr_show = 0

" Shows the filename only.
let g:airline#extensions#tabline#fnamemod = ':t'

" Allow glyphs in airline
let g:airline_powerline_fonts = 1

" Change Airline theme
let g:airline_theme = 'hybrid'


" Vim-Swoop Settings
" Edits colorscheme
let g:swoopHighlight = ["hi! link SwoopBufferLineHi Warning", "hi! link SwoopPatternHi Error"]


nnoremap <M-q> :q<CR>


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
" set foldmethod=indent
" set foldnestmax=10
" set nofoldenable
" set foldlevel=4


" emulate ci" and ci' behavior
nnoremap ci( f(%ci(
nnoremap ci[ f[%ci[


" extend functionality of <C-e> & <C-y> scrolling
nnoremap <C-e> <C-e>j
vnoremap <C-e> <C-e>j
nnoremap <C-y> <C-y>k
vnoremap <C-y> <C-y>k


" Opens all folds within the buffer
" nnoremap ZZ zR

" Closes all folds within the buffer
" nnoremap zz zM

" Opens all folds beneath the cursor
" NOTE: j is the character to go down
" nnoremap zJ zO

" Opens single fold beneath the cursor
" NOTE: j is the character to go down
" nnoremap zj zo

" Opens single fold beneath the cursor
" NOTE: k is the character to go down
" nnoremap zK zC

" Opens single fold beneath the cursor
" NOTE: k is the character to go down
" nnoremap zk zc


" Save shortcut
nnoremap <C-s> :w<CR>


" Switch to MRU'd buffer
nnoremap <leader><leader> <C-^>


" Alternative MRU to CtrlP MRU
nnoremap <leader>b :MRU<CR>


" Supports mouse interaction.
set mouse=a


" Highlights matches during a search.
set hlsearch

" Clear highlight
noremap <silent> <leader>h :nohlsearch<bar>:echo<CR>


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


" Hybrid mode for Vim
inoremap <C-a> <Esc>I
inoremap <C-e> <Esc>A

inoremap <M-b> <S-Left>
inoremap <M-f> <S-Right>

inoremap <C-b> <Left>
inoremap <C-f> <Right>
inoremap <C-p> <Up>
inoremap <C-n> <Down>


" Manage Vertical and Horizontal splits
nnoremap sl <Esc>:vs<CR><C-w>l
nnoremap sh <Esc>:vs<CR>
nnoremap sj <Esc>:sp<CR><C-w>j
nnoremap sk <Esc>:sp<CR>


" Delete (i.e. "close") the currently opened buffer
" TODO: unless it's a split window, which should be :q
nnoremap <leader>q :bdelete<CR>


" Set CtrlP runtime path
set runtimepath^=~/.vim/bundle/ctrlp.vim


" Pane movement
let g:tmux_navigator_no_mappings = 1

nnoremap <silent> <M-h> :TmuxNavigateLeft<CR>
nnoremap <silent> <M-j> :TmuxNavigateDown<CR>
nnoremap <silent> <M-k> :TmuxNavigateUp<CR>
nnoremap <silent> <M-l> :TmuxNavigateRight<CR>

" make Y do what is intuitive given:
"   D: deletes until EOL
"   C: changes until EOL
"   Y: (should) yank until EOL
nnoremap Y y$


" scrolling and maintaing mouse position
" nnoremap <C-j> j<C-e>
" nnoremap <C-k> k<C-y>


" remap redo key that is eclipsed by `rotate` currently
nnoremap U :redo<CR>


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
set clipboard+=unnamedplus


" Manage 80 char line limits
highlight OverLength1 ctermbg=Magenta ctermfg=Black
highlight OverLength2 ctermbg=LightMagenta ctermfg=Black
highlight OverLength3 ctermbg=White ctermfg=Black
" match OverLength3 /\%81v.\+/
match OverLength2 /\%91v.\+/
" match OverLength3 /\%101v.\+/

nnoremap <leader>w :w<CR>


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


" BOL and EOL
nnoremap H ^
vnoremap H ^
nnoremap L $
vnoremap L $


" Search for visually selected text
vnoremap // y/<C-r>"<CR>N


" trim trailing whitespace on save
autocmd BufWritePre *.{js,py,tpl,less,html,ex,exs,txt} :%s/\s\+$//e


" Use .gitignore file to populate Ctrl-P
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']


" Ignores dirs and files
let g:ctrlp_custom_ignore = {
  \ 'dir':  'node_modules',
  \ 'file': '\v\.(exe|dll|png|jpg|jpeg)$'
\}


" WIP: Run elixir tests on that line
" TODO: only register binding in *.exs? file extensions
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
