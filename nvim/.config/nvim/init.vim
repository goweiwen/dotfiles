if &compatible
  set nocompatible
endif
set runtimepath+=~/.config/nvim/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.config/nvim/dein')
  call dein#begin('~/.config/nvim/dein')

  call dein#add('~/.config/nvim/dein/repos/github.com/Shougo/dein.vim')

  call dein#add('vim-airline/vim-airline')
  call dein#add('chriskempson/base16-vim')
  call dein#add('ap/vim-buftabline')
  call dein#add('scrooloose/nerdtree')
  call dein#add('Xuyuanp/nerdtree-git-plugin')
  call dein#add('airblade/vim-gitgutter')
  call dein#add('kshenoy/vim-signature')

  call dein#add('justinmk/vim-sneak')
  call dein#add('unblevable/quick-scope')
  call dein#add('easymotion/vim-easymotion')
  call dein#add('terryma/vim-multiple-cursors')
  call dein#add('terryma/vim-expand-region')
  call dein#add('bfredl/nvim-miniyank')
  call dein#add('chaoren/vim-wordmotion')

  call dein#add('tpope/vim-abolish')
  call dein#add('tpope/vim-commentary')
  call dein#add('tpope/vim-endwise')
  call dein#add('tpope/vim-eunuch')
  call dein#add('tpope/vim-fugitive')
  call dein#add('tpope/vim-rhubarb')
  call dein#add('tpope/vim-repeat')
  call dein#add('tpope/vim-rsi')
  call dein#add('tpope/vim-sleuth')
  call dein#add('tpope/vim-surround')
  call dein#add('tpope/vim-obsession')
  call dein#add('dhruvasagar/vim-prosession')

  call dein#add('SirVer/ultisnips')
  call dein#add('honza/vim-snippets')

  call dein#add('w0rp/ale')
  " call dein#add('bronson/vim-trailing-whitespace')

  call dein#add('Shougo/denite.nvim')
  call dein#add('rking/ag.vim')
  call dein#add('brooth/far.vim')
  call dein#add('airblade/vim-rooter')
  call dein#add('justinmk/vim-gtfo')
  call dein#add('justinmk/vim-dirvish')
  call dein#add('farmergreg/vim-lastplace')
  call dein#add('lambdalisue/gina.vim')

  call dein#add('Shougo/deoplete.nvim')
  call dein#add('artur-shaik/vim-javacomplete2') " Java
  call dein#add('carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }) " JavaScript
  call dein#add('zchee/deoplete-jedi') " Python

  call dein#add('mattn/emmet-vim')              " Emmet
  call dein#add('neovimhaskell/haskell-vim')    " Haskell
  call dein#add('itchyny/vim-haskell-indent')   " Haskell
  call dein#add('pangloss/vim-javascript')      " JavaScript
  call dein#add('mxw/vim-jsx')                  " JSX
  call dein#add('posva/vim-vue')                " Vue
  call dein#add('digitaltoad/vim-pug')          " Pug
  call dein#add('wavded/vim-stylus')            " Stylus

  call dein#end()
  call dein#save_state()

  if dein#check_install()
    call dein#install()
  endif
endif

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
set wildmode=list:longest,full

set visualbell
set noerrorbells

set backspace=indent,eol,start

set hidden

syntax enable
set background=dark
" let base16colorspace=256
colorscheme base16-ashes
set termguicolors

" set textwidth=80
" set colorcolumn=+1

set tabstop=2
set softtabstop=2
set expandtab
set shiftwidth=2
set shiftround
set smarttab
set list

set showbreak=↪\ 
set listchars=tab:→\ ,eol:↲,nbsp:␣,extends:›,precedes:‹,trail:·
set formatprg=par\ -w80\ -T4

augroup configgroup
  autocmd!
  autocmd VimEnter * highlight clear SignColumn
  autocmd FileType python setlocal tabstop=4
  autocmd FileType python setlocal shiftwidth=4
  autocmd FileType python setlocal softtabstop=4
augroup END

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

" vim-signature
let g:SignatureMarkTextHLDynamic = 1

" fasd
" :Z - cd to recent / frequent directories
command! -nargs=* Z :call Z(<f-args>)
function! Z(...)
  let cmd = 'fasd -d -e printf'
  for arg in a:000
    let cmd = cmd . ' ' . arg
  endfor
  let path = system(cmd)
  if isdirectory(path)
    echo path
    exec 'cd ' . "\"${path}\""
  endif
endfunction
cnoreabbrev z Z

" Denite
call denite#custom#map(
      \ 'insert',
      \ '<C-j>',
      \ '<denite:move_to_next_line>',
      \ 'noremap'
      \)
call denite#custom#map(
      \ 'insert',
      \ '<C-k>',
      \ '<denite:move_to_previous_line>',
      \ 'noremap'
      \)

" Map Leader -> Space
let g:mapleader = "\<Space>"

" vimrc
map <Leader>ef :e ~/.config/nvim/init.vim<CR>
map <Leader>er :source ~/.config/nvim/init.vim<CR>

" QOL mappings
map Y y$
nmap K i<CR><Esc>k$
nmap <Leader><Esc> :nohl<CR>
nmap <Leader>d "_d
nmap <Leader>D "_D
nmap <Leader>c "_c
nmap <Leader>C "_C

" Wordmotion
let g:wordmotion_prefix = '<Leader>'

" Quickscope
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T', 's', 'S']

" Switch between buffers
noremap <Leader>` <C-^>
noremap <Leader><Tab> :bnext<CR>
noremap <Leader><S-Tab> :bprev<CR>

" Visual select block
nnoremap gV `[v`]

" Yank ring
map p <Plug>(miniyank-autoput)
map P <Plug>(miniyank-autoPut)
map <Leader>n <Plug>(miniyank-cycle)

" Mouse
set mouse=a

" Autocomplete
let g:deoplete#enable_at_startup = 1
inoremap <Expr><Tab> pumvisible() ? "\<C-n>" : "\<Tab>"

" File jumping
noremap <F1> :NERDTreeToggle<CR>
noremap <F2> :Gina status<CR>
noremap <C-p> :DeniteProjectDir file_rec<CR>
noremap <Leader>p :DeniteProjectDir file_rec<CR>
noremap <Leader>f :Dirvish<CR>
noremap <Leader>gs :Gina status<CR>
noremap <Leader>gc :Gina commit<CR>
noremap <C-s> :Denite line<CR>

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

let g:javascript_plugin_flow = 1

" Paths
let g:python_host_prog = '/usr/local/bin/python2'
let g:python3_host_prog = '/usr/local/bin/python3'

" Linting
let g:ale_fixers = {
      \   'javascript': ['remove_trailing_lines', 'trim_whitespace', 'eslint'],
      \   'vue': ['remove_trailing_lines', 'trim_whitespace', 'eslint'],
      \   'python': ['isort', 'remove_trailing_lines', 'trim_whitespace', 'autopep8', 'yapf'],
      \}
let g:ale_javascript_eslint_executable='eslint_d'
let g:ale_fix_on_save = 1
let g:ale_sign_column_always = 1
" let g:ale_lint_on_enter = 0
let g:airline#extensions#ale#enabled = 1
