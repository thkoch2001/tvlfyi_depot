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
Plugin 'kien/ctrlp.vim'
Plugin 'mileszs/ack.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'

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

" Tmux tooling
Plugin 'christoomey/vim-tmux-navigator'

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


call vundle#end()            " required
filetype plugin indent on    " required
" Put your non-Plugin stuff after this line
" -- END: Vundle config --


" Airline Settings
" Enables the list of buffers.
let g:airline#extensions#tabline#enabled = 1

" Shows the filename only.
let g:airline#extensions#tabline#fnamemod = ':t'

" Allow glyphs in airline
let g:airline_powerline_fonts = 1


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


" Changes <leader> to <space> character.
let mapleader = " "


" Switch to MRU'd buffer
nnoremap <leader><leader> <C-^>


" Supports mouse interaction.
set mouse=a


" Highlights matches during a search.
set hlsearch

" Search for gibberish to clear the most recent search
" nnoremap <leader>/ :set hlsearch!<CR>
noremap <leader>/ /__wc_gibberish__<CR>


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
nnoremap vs <Esc>:vs<CR>
nnoremap vv <Esc>:vs<CR>
nnoremap sp <Esc>:sp<CR>
nnoremap ss <Esc>:sp<CR>


" Move around splits with <leader>
nnoremap <leader>h <C-w>h
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>l <C-w>l


" Delete (i.e. "close") the currently opened buffer
nnoremap <leader>q :bdelete<CR>


" CtrlP config
" Set default CtrlP command.
let g:ctrlp_cmd = 'CtrlP'
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
nnoremap <C-l> :1bnext<CR>
nnoremap <C-h> :1bprevious<CR>

" Buffer creation
" nnoremap <C-t> :enew<CR>

" Buffer deletion
nnoremap <leader>bq :bp <BAR> bd #<CR>


" make Y do what is intuitive given: 
"   D: deletes until EOL
"   C: changes until EOL
"   Y: (should) yank until EOL
nnoremap Y y$


" flip number keys to their shift+ counterparts
nnoremap t1 t!
nnoremap t2 t@
nnoremap t3 t#
nnoremap t4 t$
nnoremap t5 t%
nnoremap t6 t^
nnoremap t7 t&
nnoremap t8 t*
nnoremap t9 t(
nnoremap t0 t)

nnoremap T1 T!
nnoremap T2 T@
nnoremap T3 T#
nnoremap T4 T$
nnoremap T5 T%
nnoremap T6 T^
nnoremap T7 T&
nnoremap T8 T*
nnoremap T9 T(
nnoremap T0 T)

nnoremap f1 f!
nnoremap f2 f@
nnoremap f3 f#
nnoremap f4 f$
nnoremap f5 f%
nnoremap f6 f^
nnoremap f7 f&
nnoremap f8 f*
nnoremap f9 f(
nnoremap f0 f)

nnoremap F1 F!
nnoremap F2 F@
nnoremap F3 F#
nnoremap F4 F$
nnoremap F5 F%
nnoremap F6 F^
nnoremap F7 F&
nnoremap F8 F*
nnoremap F9 F(
nnoremap F0 F)


" Karate edits
nnoremap ca9 ca(
nnoremap da9 da(
nnoremap va9 va(

nnoremap ca0 ca)
nnoremap da0 da)
nnoremap va0 va)

nnoremap ci9 ci(
nnoremap di9 di(
nnoremap vi9 vi(

nnoremap ci0 ci)
nnoremap di0 di)
nnoremap vi0 vi)


" scrolling and maintaing mouse position
nnoremap <C-j> j<C-e>
nnoremap <C-k> k<C-y>


" reload file after git changes
nnoremap <C-r> :e<CR>


" -- Syntastic Settings --
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_javascript_checkers = ['gjslint']


" Basic settings
set number
set wrap!
set tabstop=2
set expandtab
set shiftwidth=2
set background=dark

syntax enable
colorscheme OceanicNext

set history=1000
set undolevels=1000

set t_Co=255


" Support italics
highlight Comment cterm=italic


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
nnoremap <C-v> o<Esc>"+p
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
autocmd BufWritePre *.{js,py,tpl,less,html,ex} :%s/\s\+$//e


" set default font and size
set guifont=Operator\ Mono:h16


" Use .gitignore file to populate Ctrl-P
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']


" Ignores dirs and files
let g:ctrlp_custom_ignore = {
  \ 'dir':  'node_modules',
  \ 'file': '\v\.(exe|dll|png|jpg|jpeg)$'
\}

