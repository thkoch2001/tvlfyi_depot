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
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'

" All of your Plugins must be added before the following line
Plugin 'Raimondi/delimitMate'
Plugin 'Valloric/YouCompleteMe'
Plugin 'airblade/vim-gitgutter'
Plugin 'kien/ctrlp.vim'
Plugin 'mileszs/ack.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'sickill/vim-monokai'
Plugin 'sjl/clam.vim'
Plugin 'terryma/vim-multiple-cursors'


call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line
" -- END: Vundle config --


set noswapfile


set grepprg=ack\ -k


" backspace settings
set backspace=2
set backspace=indent,eol,start


" Javascript specific variables
let g:javascript_plugin_jsdoc = 1
" set foldmethod=syntax

" GlobalListchars
set list
set listchars=eol:¶,trail:~,nbsp:␣


" Keeps everything concealed at all times. Even when my cursor is on the word.
set conceallevel=1
set concealcursor=nvic

" JavaScript thanks to pangloss/vim-javascript
" let g:javascript_conceal_function = "ƒ"
" match ErrorMsg /ƒ/


" Ultisnips
" Track the engine.
Plugin 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-x>"
" let g:UltiSnipsJumpForwardTrigger="<c-j>"
" let g:UltiSnipsJumpBackwardTrigger="<c-k>"


" keyword completion
inoremap ;; <C-n>


" tab movement bindings
nnoremap <C-h> gT
nnoremap <C-l> gt


" make Y do what is intuitive given D, etc.
nnoremap Y y$


" new tab keybinding
nnoremap <C-t> :tabnew<CR>
inoremap <C-t> <Esc>:tabnew<CR>
vnoremap <C-t> <Esc>:tabnew<CR>


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


" scrolling and maintaing mouse position
nnoremap <C-j> j<C-e>
nnoremap <C-k> k<C-y>


" reload file after git changes
nnoremap <C-r> :e<CR>


" resize vertical and horizontal splits
nnoremap <C-w><C-l> :vertical resize +3<CR>
nnoremap <C-w><C-h> :vertical resize -3<CR>
nnoremap <C-w><C-k> :resize +3<CR>
nnoremap <C-w><C-j> :resize -3<CR>


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
syntax on
set number
set tabstop=2
set expandtab
set shiftwidth=2
colorscheme elflord
set t_Co=255


" Ensure that <header> is "," character
let mapleader = ","


" Define highlighting groups
highlight InterestingWord1 ctermbg=Cyan ctermfg=Black
highlight InterestingWord2 ctermbg=Yellow ctermfg=Black
highlight InterestingWord3 ctermbg=Magenta ctermfg=Black


" h1 highlighting
nnoremap <silent> <leader>h1 :execute 'match InterestingWord1 /\<<c-r><c-w>\>/'<CR>
nnoremap <silent> <leader>xh1 :execute 'match none'<CR>

" h2 highlighting
nnoremap <silent> <leader>h2 :execute '2match InterestingWord2 /\<<c-r><c-w>\>/'<CR>
nnoremap <silent> <leader>xh2 :execute '2match none'<CR>

" h3 highlighting
nnoremap <silent> <leader>h3 :execute '3match InterestingWord3 /\<<c-r><c-w>\>/'<CR>
nnoremap <silent> <leader>xh3 :execute '3match none'<CR>

"clear all highlighted groups
nnoremap <silent> <leader>xhh :execute 'match none'<CR> :execute '2match none'<CR> :execute '3match none'<CR>


" pasteboard copy & paste
nnoremap <C-c> V"+y
vnoremap <C-c> "+y

nnoremap <C-v> o<Esc>"+p
vnoremap <C-v> "+p


" add 80 character wrap line
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/


" map jj to <Esc>
imap jj <Esc>

" map ctrl + n to :NERDTree
map <C-n> :NERDTreeToggle<CR>


" BOL and EOL
nnoremap H ^
nnoremap L $


" set -o emacs line-editor defaults
inoremap <C-a> <Esc>I
inoremap <C-e> <Esc>A


" trim trailing whitespace on save
autocmd BufWritePre *.{js,py,tpl,html} :%s/\s\+$//e


" set default font and size
set guifont=Operator\ Mono:h16


" -- fuzzy-finder --
set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = {
  \ 'dir':  'node_modules'
  \ }

