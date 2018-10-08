"pricco/gnome-terminal-colors-monokai # For terminal colorscheme
set shell=/bin/bash
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'Valloric/YouCompleteMe'
Plugin 'VundleVim/Vundle.vim'
Plugin 'jiangmiao/auto-pairs'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'scrooloose/nerdtree'
Plugin 'vim-airline/vim-airline'
Plugin 'tpope/vim-fugitive'
Plugin 'kien/ctrlp.vim'
Plugin 'dyng/ctrlsf.vim'
Plugin 'tomasr/molokai'
Plugin 'airblade/vim-gitgutter'
Plugin 'vim-airline/vim-airline-themes'

call vundle#end()

execute pathogen#infect()

filetype plugin indent on
filetype plugin on

set omnifunc=syntaxcomplete#Complete

set autoindent
set nowrap
set copyindent
set number
set shiftwidth=2
set shiftround
set showmatch 
set ignorecase
set smartcase
set smarttab
set hlsearch
set incsearch
set history=1000
set undolevels=1000
set wildignore=*.swp,*.bak,*.pyc,*.class
set title
set visualbell
set noerrorbells
set expandtab
set showcmd
set wildmenu
set lazyredraw
set foldenable
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent
set encoding=utf-8
set list
set listchars=tab:>∙,trail:∙,extends:#,nbsp:∙
autocmd filetype html,xml set listchars-=tab:>.
set pastetoggle=<F2>
set mouse=a
set laststatus=2
set fillchars+=stl:\ ,stlnc:\
set t_Co=256
let tabstop=2

filetype plugin indent on

nmap     <C-F>f <Plug>CtrlSFPrompt
:imap <C-Space> <C-X><C-O>
nnoremap <C-n> :NERDTreeToggle<CR>
nnoremap ; :
vmap Q gq
nmap Q gqap
nnoremap j gj
nnoremap k gk
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
nmap <silent> ,/ :nohlsearch<CR>
cmap w!! w !sudo tee % >/dev/null

syntax enable
syntax on

let g:airline#extensions#tmuxline#enabled = 0
let g:tmuxline_theme='molokai'
let g:airline_powerline_fonts = 1
let g:airline_enable_branch=1
let g:airline_enable_syntastic=1
let g:airline#extensions#branch#enabled=1
let g:airline_theme='molokai'

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

let g:airline_symbols.space = "\ua0"
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_next_key='<C-b>'
let g:multi_cursor_prev_key='<C-p>'
let g:multi_cursor_skip_key='<C-x>'
let g:multi_cursor_quit_key='<Esc>'

set background=dark
let g:molokai_original=1
let g:rehash256=1
set t_Co=256
colorscheme molokai

