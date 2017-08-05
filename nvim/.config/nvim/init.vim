set nocompatible

call plug#begin('~/.config/nvim/plugged')

Plug 'chriskempson/base16-vim'

Plug 'justinmk/vim-sneak'
Plug 'unblevable/quick-scope'
Plug 'easymotion/vim-easymotion'
Plug 'terryma/vim-multiple-cursors'

Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

Plug 'w0rp/ale'
Plug 'bronson/vim-trailing-whitespace'

Plug 'ctrlpvim/ctrlp.vim'
Plug 'rking/ag.vim'

Plug 'Shougo/deoplete.nvim'
Plug 'artur-shaik/vim-javacomplete2'
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
Plug 'zchee/deoplete-jedi'

Plug 'digitaltoad/vim-pug'
Plug 'itchyny/vim-haskell-indent'
Plug 'leafo/moonscript-vim'
Plug 'mattn/emmet-vim'
Plug 'mxw/vim-jsx'
Plug 'neovimhaskell/haskell-vim'
Plug 'pangloss/vim-javascript'

call plug#end()

filetype plugin indent on

set title
set number
set ruler
set showcmd
set cursorline
set relativenumber
set wrap
set wildmenu
set lazyredraw
set showmatch

set scrolloff=3

set guioptions=t

set ignorecase
set smartcase
set incsearch
set hlsearch

set visualbell
set noerrorbells

set backspace=indent,eol,start
set backspace=indent,eol,start

set hidden

syntax enable
set background=dark
let base16colorspace=256
colorscheme base16-ashes
set termguicolors

" set textwidth=80
" set colorcolumn=+1

set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
set shiftround
set smarttab
set list

set foldmethod=syntax

set foldenable
set foldlevelstart=99
set foldnestmax=99

" let javascript_fold=1         " JavaScript
" let perl_fold=1               " Perl
" let php_folding=1             " PHP
" let r_syntax_folding=1        " R
" let ruby_fold=1               " Ruby
" let sh_fold_enabled=1         " sh
" let vimsyn_folding='af'       " Vim script
" let xml_syntax_folding=1      " XML

set guioptions-=T

" Hotkeys
inoremap jk <Esc>
let g:mapleader = "\<Space>"
nnoremap <Leader>o :CtrlP<CR>
nnoremap <Leader>w :w<CR>
nnoremap <Leader>q :q<CR>
vmap <Leader>y "+y
vmap <Leader>d "+d
vmap <Leader>p :set paste<CR>"+p:set nopaste<CR>
vmap <Leader>P :set paste<CR>"+P:set nopaste<CR>
nmap <Leader>y :set paste<CR>"+y:set nopaste<CR>
nmap <Leader>d :set paste<CR>"+d:set nopaste<CR>
nmap <Leader>p :set paste<CR>"+p:set nopaste<CR>
nmap <Leader>P :set paste<CR>"+P:set nopaste<CR>

" nnoremap j gj
" nnoremap k gk
nnoremap <C-e> $
nnoremap <C-a> ^
nnoremap gV `[v`]
nnoremap <C-S-a> <C-a>

nnoremap <Leader>ev :vsp ~/.config/nvim/init.vim<CR>
nnoremap <Leader>ez :vsp ~/.zshrc<CR>
nnoremap <Leader>ex :vsp ~/.xinitrc<CR>
nnoremap <Leader>sv :source ~/.config/nvim/init.vim<CR>

" nnoremap <down> :m .+1<CR>==
" nnoremap <up> :m .-2<CR>==
" inoremap <down> <Esc>:m .+1<CR>==gi
" inoremap <up> <Esc>:m .-2<CR>==gi
" vnoremap <down> :m '>+1<CR>gv=gv
" vnoremap <up> :m '<-2<CR>gv=gv

nnoremap "_p :set paste<CR>"+p:set nopaste<CR>

" Plugins
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
nnoremap <Leader>] :YcmCompleter GoTo<CR>
let g:clang_complete_auto = 1

let g:neomake_java_javac_maker = {
        \ 'errorformat':
            \ '%E%f:%l: %trror: %m,' .
            \ '%W%f:%l: %tarning: %m,' .
            \ '%E%f:%l: %m,'.
            \ '%Z%p^,'.
            \ '%-G%.%#',
        \ }
let g:neomake_java_enabled_makers = ['javac']
let g:neomake_javascript_enabled_makers = ['eslint']

let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall
" imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<Leader><tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
" let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g'

augroup configgroup
    autocmd!
    autocmd VimEnter * highlight clear SignColumn
    " autocmd BufWritePre *.php,*.py,*.js,*.txt,*.hs,*.java,*.md
    "             \:call <SID>StripTrailingWhitespaces()
    autocmd FileType java setlocal noexpandtab
    autocmd FileType java setlocal list
    autocmd FileType java setlocal listchars=tab:+\ ,eol:-
    autocmd FileType java setlocal formatprg=par\ -w80\ -T4
    autocmd FileType php setlocal expandtab
    autocmd FileType php setlocal list
    autocmd FileType php setlocal listchars=tab:+\ ,eol:-
    autocmd FileType php setlocal formatprg=par\ -w80\ -T4
    autocmd FileType ruby setlocal tabstop=2
    autocmd FileType ruby setlocal shiftwidth=2
    autocmd FileType ruby setlocal softtabstop=2
    autocmd FileType ruby setlocal commentstring=#\ %s
    autocmd FileType python setlocal commentstring=#\ %s
    autocmd BufEnter *.cls setlocal filetype=java
    autocmd BufEnter *.zsh-theme setlocal filetype=zsh
    autocmd BufEnter Makefile setlocal noexpandtab
    autocmd BufEnter *.sh setlocal tabstop=2
    autocmd BufEnter *.sh setlocal shiftwidth=2
    autocmd BufEnter *.sh setlocal softtabstop=2
augroup END

let g:deoplete#enable_at_startup = 1
" deoplete tab-complete
inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

let g:javascript_plugin_flow = 1
