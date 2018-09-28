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
  call dein#add('rafi/awesome-vim-colorschemes')

  " Modeline
  call dein#add('itchyny/lightline.vim')
  call dein#add('maximbaz/lightline-ale')
  call dein#add('ap/vim-buftabline')

  " Color characters based on f-key
  call dein#add('unblevable/quick-scope')

  " Git gutter
  call dein#add('airblade/vim-gitgutter')

  " Marks gutter
  call dein#add('kshenoy/vim-signature')


  "
  " Misc
  "

  " Git and GitHub
  call dein#add('tpope/vim-fugitive')
  call dein#add('tpope/vim-rhubarb')
  call dein#add('lambdalisue/gina.vim')
  call dein#add('rhysd/committia.vim')

  " Sessions
  call dein#add('tpope/vim-obsession')
  call dein#add('dhruvasagar/vim-prosession')

  " Save edit position
  call dein#add('farmergreg/vim-lastplace')

  " tmux
  call dein#add('christoomey/vim-tmux-navigator')


  "
  " Files
  "

  " File browsers
  call dein#add('tpope/vim-vinegar')

  " Helm-like
  call dein#add('Shougo/denite.nvim')
  call dein#add('cloudhead/neovim-fuzzy')

  " grep
  call dein#add('jremmen/vim-ripgrep')

  " Find and replace (:Far)
  call dein#add('brooth/far.vim')

  " Automatically sets working directory
  call dein#add('airblade/vim-rooter')

  " Open in Finder/Terminal (gof/got)
  call dein#add('justinmk/vim-gtfo')

  " Path navigator
  call dein#add('justinmk/vim-dirvish')


  "
  " Editor
  "

  " Pairwise mappings
  call dein#add('tpope/vim-unimpaired')

  " Multiple cursors (C-n, C-p, C-x, :MultipleCursorsFind)
  call dein#add('terryma/vim-multiple-cursors')

  " Sneaking (maps s/S to 2-char f/t)
  call dein#add('justinmk/vim-sneak')

  " Quick scope
  call dein#add('unblevable/quick-scope')

  " More text objects
  call dein#add('wellle/targets.vim')
  call dein#add('michaeljsmith/vim-indent-object')
  call dein#add('andymass/vim-matchup')

  " Expand region (+/_ in visual mode)
  call dein#add('terryma/vim-expand-region')

  " Kill-ring
  call dein#add('bfredl/nvim-miniyank')

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


  "
  " Linting
  "

  call dein#add('w0rp/ale')
  call dein#add('bronson/vim-trailing-whitespace')


  "
  " Syntax Highlighting
  "

  " Most languages
  call dein#add('sheerun/vim-polyglot')


  "
  " Autocompletion
  "

  " Deoplete
  call dein#add('Shougo/deoplete.nvim')

  " C/C++
  call dein#add('tweekmonster/deoplete-clang2')

  " Java
  call dein#add('artur-shaik/vim-javacomplete2')

  " Python
  call dein#add('zchee/deoplete-jedi')

  " Golang
  call dein#add('fatih/vim-go')

  " Julia
  call dein#add('JuliaEditorSupport/deoplete-julia')

  " Emmet
  call dein#add('mattn/emmet-vim')

  " Haskell
  call dein#add('eagletmt/neco-ghc')

  " JavaScript
  call dein#add('carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' })

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
highlight StatusLine ctermbg=0
colorscheme apprentice
" set termguicolors
" let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
" let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" Set terminal title
set title

" Status bar
set ruler
set showcmd
set noshowmode

" Show matching pairs
set showmatch

" 80-char line
" set textwidth=80
set colorcolumn=80
set formatprg=par\ -w80\ -T4
highlight ColorColumn ctermbg=DarkGray

" Soft word-wrap
set wrap
set breakindent
set breakindentopt=shift:4

" Don't redraw during macros
set lazyredraw

" Line numbers
set number
set relativenumber
highlight LineNr ctermfg=darkgrey

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
set listchars=tab:→\ ,nbsp:␣,extends:›,precedes:‹,trail:·

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

" Haskell

" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0

" neco-ghc
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc 
let g:necoghc_enable_detailed_browse = 1

" Status line
let g:lightline = {}

let g:lightline.component_expand = {
      \     'linter_checking': 'lightline#ale#checking',
      \     'linter_warnings': 'lightline#ale#warnings',
      \     'linter_errors': 'lightline#ale#errors',
      \     'linter_ok': 'lightline#ale#ok',
      \ }

let g:lightline.component_type = {
      \     'linter_checking': 'left',
      \     'linter_warnings': 'warning',
      \     'linter_errors': 'error',
      \     'linter_ok': 'left',
      \ }

let g:lightline.active = {
      \     'left': [['mode', 'paste'], ['readonly', 'filename', 'modified']],
      \     'right': [['linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok'], ['lineinfo'], ['percent'], ['fileformat', 'fileencoding', 'filetype']]
      \ }

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

" kill-ring
map p <Plug>(miniyank-autoput)
map P <Plug>(miniyank-autoPut)
map <Leader>n <Plug>(miniyank-cycle)

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
noremap <F2> :Gstatus<CR>
noremap <Leader>p :FuzzyOpen<CR>
noremap <Leader>f :FuzzyGrep<CR>
noremap <Leader>gs :Gstatus<CR>
noremap <Leader>gc :Gcommit<CR>
noremap <C-s> :Denite line<CR>

" Paths
let g:python_host_prog = '/usr/local/bin/python2'
let g:python3_host_prog = '/usr/local/bin/python3'

" Autocomplete
let g:deoplete#enable_at_startup = 1
inoremap <Expr><Tab> pumvisible() ? "\<C-n>" : "\<Tab>"

" Linting
let g:ale_fixers = {
      \   'javascript': ['remove_trailing_lines', 'trim_whitespace', 'eslint'],
      \   'vue': ['remove_trailing_lines', 'trim_whitespace', 'eslint'],
      \   'python': ['isort', 'remove_trailing_lines', 'trim_whitespace', 'autopep8', 'yapf'],
      \   'go': ['gofmt'],
      \   'haskell': ['brittany'],
      \}
let g:ale_javascript_eslint_executable='eslint_d'
let g:ale_fix_on_save = 1
let g:ale_lint_on_enter = 0
let g:ale_sign_column_always = 1
let g:ale_sign_error = '>'
let g:ale_sign_warning = '-'
noremap <Leader>an <Plug>(ale_next_wrap)
noremap <Leader>ap <Plug>(ale_previous_wrap)
let g:javascript_plugin_flow = 1

" Sneak and scope
let g:sneak#label = 1
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T', 's', 'S']

let g:SignatureMarkTextHLDynamic = 1

" Snippets
let g:UltiSnipsExpandTrigger="<Leader><Tab>"
let g:UltiSnipsJumpForwardTrigger="<C-b>"
let g:UltiSnipsJumpBackwardTrigger="<C-z>"

let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall
autocmd Filetype go noremap <F3> :GoImport<CR>
autocmd Filetype go noremap <F4> :GoImports<CR>
autocmd Filetype go noremap <F5> :GoRun<CR>
