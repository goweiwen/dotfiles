if &compatible
  set nocompatible
endif
set runtimepath+=~/.config/nvim/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.config/nvim/dein')
  call dein#begin('~/.config/nvim/dein')

  " Plugin manager
  call dein#add('~/.config/nvim/dein/repos/github.com/Shougo/dein.vim')


  "
  " Appearance
  "

  " Theme
  call dein#add('vim-airline/vim-airline')
  let g:airline#extensions#ale#enabled = 1
  let g:airline#extensions#tabline#enabled = 1
  let g:airline#extensions#whitespace#enabled = 1
  call dein#add('vim-airline/vim-airline-themes')
  call dein#add('chriskempson/base16-vim')

  " Color characters based on f-key
  call dein#add('unblevable/quick-scope')
  let g:qs_highlight_on_keys = ['f', 'F', 't', 'T', 's', 'S']

  " Git gutter
  call dein#add('airblade/vim-gitgutter')

  " Marks gutter
  call dein#add('kshenoy/vim-signature')
  let g:SignatureMarkTextHLDynamic = 1


  "
  " Misc
  "

  " Git and GitHub
  call dein#add('tpope/vim-fugitive')
  call dein#add('tpope/vim-rhubarb')
  call dein#add('lambdalisue/gina.vim')

  " Sessions
  call dein#add('tpope/vim-obsession')
  call dein#add('dhruvasagar/vim-prosession')
  "
  " Save edit position
  call dein#add('farmergreg/vim-lastplace')


  "
  " Files
  "

  " File browsers
  call dein#add('airodactyl/neovim-ranger')
  call dein#add('scrooloose/nerdtree')
  call dein#add('Xuyuanp/nerdtree-git-plugin')

  " Helm-like
  call dein#add('Shougo/denite.nvim')

  " grep
  call dein#add('rking/ag.vim')

  " Find and replace (:Far)
  call dein#add('brooth/far.vim')

  " Automatically sets working directory
  call dein#add('airblade/vim-rooter')

  " Open in Finder/Terminal (gof/got)
  call dein#add('justinmk/vim-gtfo')
  let g:gtfo#terminals = { 'mac': 'iterm' }

  " Path navigator
  call dein#add('justinmk/vim-dirvish')


  "
  " Editor
  "

  " Multiple cursors (C-n, C-p, C-x, :MultipleCursorsFind)
  call dein#add('terryma/vim-multiple-cursors')

  " Sneaking (maps s/S to 2-char f/t)
  call dein#add('justinmk/vim-sneak')

  " Easymotion (<Leader><Leader>)
  call dein#add('easymotion/vim-easymotion')

  " Smart words
  call dein#add('chaoren/vim-wordmotion')

  " Expand region (+/_ in visual mode)
  call dein#add('terryma/vim-expand-region')

  " Kill-ring
  call dein#add('bfredl/nvim-miniyank')
  map p <Plug>(miniyank-autoput)
  map P <Plug>(miniyank-autoPut)
  map <Leader>n <Plug>(miniyank-cycle)

  " Substitution (:Subvert/thing{1,2}/other{1,2}/)
  call dein#add('tpope/vim-abolish')

  " Commenting (gcc)
  call dein#add('tpope/vim-commentary')

  " Auto end block
  call dein#add('tpope/vim-endwise')

  " Shell command wrappers
  call dein#add('tpope/vim-eunuch')

  " Repeat more actions
  call dein#add('tpope/vim-repeat')

  " Emacs bindings
  call dein#add('tpope/vim-rsi')

  " Smart tab widths
  call dein#add('tpope/vim-sleuth')

  " Surround motion
  call dein#add('tpope/vim-surround')


  "
  " Snippets
  "

  call dein#add('SirVer/ultisnips')
  call dein#add('honza/vim-snippets')
  let g:UltiSnipsExpandTrigger="<Leader><Tab>"
  let g:UltiSnipsJumpForwardTrigger="<C-b>"
  let g:UltiSnipsJumpBackwardTrigger="<C-z>"


  "
  " Linting
  "

  call dein#add('w0rp/ale')
  let g:ale_fixers = {
        \   'javascript': ['remove_trailing_lines', 'trim_whitespace', 'eslint'],
        \   'vue': ['remove_trailing_lines', 'trim_whitespace', 'eslint'],
        \   'python': ['isort', 'remove_trailing_lines', 'trim_whitespace', 'autopep8', 'yapf'],
        \}
  let g:ale_javascript_eslint_executable='eslint_d'
  let g:ale_fix_on_save = 1
  let g:ale_sign_column_always = 1
  let g:ale_lint_on_enter = 0
  call dein#add('bronson/vim-trailing-whitespace')


  "
  " Autocompletion
  "

  " Deoplete
  inoremap <Expr><Tab> pumvisible() ? "\<C-n>" : "\<Tab>"

  " C/C++
  call dein#add('tweekmonster/deoplete-clang2')

  " Java
  call dein#add('artur-shaik/vim-javacomplete2')
  let g:neomake_java_javac_maker = {
        \ 'errorformat':
        \ '%E%f:%l: %trror: %m,' .
        \ '%W%f:%l: %tarning: %m,' .
        \ '%E%f:%l: %m,'.
        \ '%Z%p^,'.
        \ '%-G%.%#',
        \ }
  let g:neomake_java_enabled_makers = ['javac']

  " Python
  call dein#add('zchee/deoplete-jedi')

  " Julia
  call dein#add('JuliaEditorSupport/julia-vim')
  call dein#add('JuliaEditorSupport/deoplete-julia')

  " Emmet
  call dein#add('mattn/emmet-vim')
  let g:user_emmet_install_global = 0
  autocmd FileType html,css EmmetInstall
  " imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

  " Haskell
  call dein#add('neovimhaskell/haskell-vim')
  call dein#add('itchyny/vim-haskell-indent')

  " Go
  call dein#add('fatih/vim-go')

  " JavaScript
  call dein#add('pangloss/vim-javascript')
  call dein#add('carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' })
  call dein#add('mxw/vim-jsx')
  let g:neomake_javascript_enabled_makers = ['eslint']
  let g:javascript_plugin_flow = 1

  " Vue
  call dein#add('posva/vim-vue')

  " HTML/CSS preprocessors
  call dein#add('digitaltoad/vim-pug')
  call dein#add('wavded/vim-stylus')

  call dein#end()
  call dein#save_state()

  if dein#check_install()
    call dein#install()
  endif
endif

filetype plugin indent on

" Theme
syntax enable
set background=dark
let base16colorspace=256
colorscheme base16-ashes
set termguicolors
let g:airline_theme='base16_ashes'

" Set terminal title
set title

" Status bar
set ruler
set showcmd

" Show matching pairs
set showmatch

" Highlight cursor line
set cursorline

" 80-char line
set textwidth=80
set colorcolumn=+1

" Soft word-wrap
set wrap

" Don't redraw during macros
set lazyredraw

" Line numbers
set number
set relativenumber

" Command line completion
set wildmenu

" Scroll space
set scrolloff=20

" GUI
set guioptions=t

" Search
set ignorecase
set smartcase
set incsearch
set hlsearch
set wildmode=list:longest,full

" QOL
set mouse=a
set backspace=indent,eol,start

" Tabs
set tabstop=2
set softtabstop=2
set expandtab
set shiftwidth=2
set shiftround
set smarttab
set list

" Visible whitespace
set showbreak=↪
set listchars=tab:→\ ,eol:↲,nbsp:␣,extends:›,precedes:‹,trail:·
set formatprg=par\ -w80\ -T4

augroup configgroup
  autocmd!
  autocmd VimEnter * highlight clear SignColumn
  autocmd FileType python setlocal tabstop=4
  autocmd FileType python setlocal shiftwidth=4
  autocmd FileType python setlocal softtabstop=4
augroup END

" Folds
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

" Switch between buffers
noremap <Leader>` <C-^>
noremap <Leader><Tab> :bnext<CR>
noremap <Leader><S-Tab> :bprev<CR>
nnoremap gt :bn<CR>
nnoremap gT :bp<CR>
set hidden

" Visual select block
nnoremap gV `[v`]

" File jumping
noremap <F1> :NERDTreeToggle<CR>
noremap <F2> :Gina status<CR>
noremap <C-p> :DeniteProjectDir file_rec<CR>
noremap <Leader>p :DeniteProjectDir file_rec<CR>
noremap <Leader>f :Dirvish<CR>
noremap <Leader>gs :Gina status<CR>
noremap <Leader>gc :Gina commit<CR>
noremap <C-s> :Denite line<CR>

" Paths
let g:python_host_prog = '/usr/local/bin/python2'
let g:python3_host_prog = '/usr/local/bin/python3'


