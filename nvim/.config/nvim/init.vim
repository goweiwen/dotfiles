set nocompatible

call plug#begin('~/.config/nvim/plugged')

Plug 'chriskempson/base16-vim'

Plug 'justinmk/vim-sneak'
Plug 'unblevable/quick-scope'
Plug 'easymotion/vim-easymotion'
Plug 'terryma/vim-multiple-cursors'
Plug 'terryma/vim-expand-region'

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
" Plug 'bronson/vim-trailing-whitespace'

Plug 'Shougo/denite.nvim'
Plug 'rking/ag.vim'

Plug 'Shougo/deoplete.nvim'
Plug 'artur-shaik/vim-javacomplete2' " Java
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' } " JavaScript
Plug 'zchee/deoplete-jedi' " Python

Plug 'mattn/emmet-vim'              " Emmet
Plug 'neovimhaskell/haskell-vim'    " Haskell
Plug 'itchyny/vim-haskell-indent'   " Haskell
Plug 'pangloss/vim-javascript'      " JavaScript
Plug 'mxw/vim-jsx'                  " JSX
Plug 'posva/vim-vue'                " Vue
Plug 'digitaltoad/vim-pug'          " Pug
Plug 'wavded/vim-stylus'            " Stylus

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

" Map Leader -> Space
let g:mapleader = "\<Space>"

" Pasting from clipboard
vmap <Leader>y "+y
vmap <Leader>d "+d
vmap <Leader>p :set paste<CR>"+p:set nopaste<CR>
vmap <Leader>P :set paste<CR>"+P:set nopaste<CR>
nmap <Leader>y "+y
nmap <Leader>d "+d
nmap <Leader>p :set paste<CR>"+p:set nopaste<CR>
nmap <Leader>P :set paste<CR>"+P:set nopaste<CR>

" nnoremap j gj
" nnoremap k gk

" Emacs bindings
nnoremap <C-S-a> <C-a>
nnoremap <C-e> $
nnoremap <C-a> ^

" Visual select block
nnoremap gV `[v`]

" Autocomplete
let g:deoplete#enable_at_startup = 1
inoremap <Expr><Tab> pumvisible() ? "\<C-n>" : "\<Tab>"

" File jumping
nnoremap <C-p> :Denite file buffer<CR>

" Snippets
let g:UltiSnipsExpandTrigger="<Leader><Tab>"
let g:UltiSnipsJumpForwardTrigger="<C-b>"
let g:UltiSnipsJumpBackwardTrigger="<C-z>"


" Languages
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

let g:javascript_plugin_flow = 1

" Linting
let g:ale_fixers = {
\   'javascript': ['eslint'],
\}
let g:ale_javascript_eslint_executable='eslint_d'
let g:ale_fix_on_save = 1
let g:ale_sign_column_always = 1
let g:ale_lint_on_enter = 0
