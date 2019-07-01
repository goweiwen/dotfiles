if &compatible
  set nocompatible
endif
" Add the dein installation directory into runtimepath
set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.cache/dein')
  call dein#begin('~/.cache/dein')

  call dein#add('~/.cache/dein/repos/github.com/Shougo/dein.vim')
  if !has('nvim')
    call dein#add('roxma/nvim-yarp')
    call dein#add('roxma/vim-hug-neovim-rpc')
  endif

  " Buffer line
  call dein#add('ap/vim-buftabline')

  " Git gutter
  call dein#add('airblade/vim-gitgutter')

  " Marks gutter
  call dein#add('kshenoy/vim-signature')

  " Git
  call dein#add('rhysd/committia.vim')
  call dein#add('tpope/vim-fugitive', {'on_ft': ['git', 'gitcommit'], 'on_cmd': ['Git', 'Gstatus', 'Gwrite', 'Glog', 'Gcommit', 'Gblame', 'Ggrep', 'Gdiff']})

  " Path navigator
  call dein#add('justinmk/vim-dirvish')
  call dein#add('kristijanhusak/vim-dirvish-git')

  " More text objects
  call dein#add('wellle/targets.vim')
  call dein#add('michaeljsmith/vim-indent-object')
  " call dein#add('andymass/vim-matchup')

  " Commenting (gcc)
  call dein#add('tpope/vim-commentary')

  " Repeat more actions
  call dein#add('tpope/vim-repeat')

  " Emacs bindings
  call dein#add('tpope/vim-rsi')

  " Smart tab widths
  call dein#add('tpope/vim-sleuth')

  " Surround motion
  call dein#add('tpope/vim-surround', {'on_map': {'n': ['cs', 'ds', 'ys']}})


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

  call dein#add('sheerun/vim-polyglot')


  "
  " Autocompletion
  "

  call dein#add('Shougo/deoplete.nvim')
  call dein#add('tbodt/deoplete-tabnine', {'build': './install.sh'})
  " call dein#add('tweekmonster/deoplete-clang2', {'on_ft': ['c', 'c++']})
  " call dein#add('artur-shaik/vim-javacomplete2', {'on_ft': 'java'})
  " call dein#add('zchee/deoplete-jedi', {'on_ft': 'python'})
  " call dein#add('fatih/vim-go', {'on_ft': 'go'})
  " call dein#add('JuliaEditorSupport/deoplete-julia', {'on_ft': 'julia'})
  " call dein#add('eagletmt/neco-ghc', {'on_ft': 'haskell'})
  " call dein#add('carlitux/deoplete-ternjs', {'on_ft': 'javascript', 'build': 'npm install -g tern'})
  " call dein#add('reasonml-editor/vim-reason-plus', {'on_ft': 'reason'})
  " call dein#add('rust-lang/rust.vim', {'on_ft': 'rust'})

  call dein#end()
  call dein#save_state()
endif

" Syntax highlighting
filetype plugin indent on
syntax enable
autocmd BufReadPre * if getfsize(expand("%")) > 1000000 | syntax off | endif

" Sane defaults
set mouse=a
set backspace=indent,eol,start
set splitbelow
set splitright
set lazyredraw
set wildmenu
set hidden

" UI
set guioptions=M
set noshowmode
set showmatch
set number
highlight LineNr ctermfg=DarkGray

" Word wrap
set wrap
set breakindent
set breakindentopt=shift:4

" Leader key is <Space>
let g:mapleader = "\<Space>"

" Remove useless mappings
nnoremap F1 <nop>
nnoremap Q <nop>
nnoremap K <nop>

" Fix inconsistent mappings
map Y y$
map cw dwi

" Pairwise mappings
nnoremap ]a :next<CR>
nnoremap [a :previous<CR>
nnoremap ]b :bnext<CR>
nnoremap [b :bprevious<CR>
nnoremap ]q :cnext<CR>
nnoremap [q :cprevious<CR>

set backupdir=~/.vim/backup
set directory=~/.vim/swap

" Visual block mode past end of line
set virtualedit=block

" Search
set ignorecase
set smartcase
set incsearch
set hlsearch
set wildmode=list:longest,full

" Tabs
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
set shiftround
set smarttab
set list

" Visible whitespace
set listchars=tab:→\ ,nbsp:␣,extends:›,precedes:‹,trail:·

" Folds
set foldmethod=syntax
set foldenable
set foldlevelstart=99
set foldnestmax=99

" Autocompletion
noremap <Leader><C-n> :call deoplete#enable()<Return>

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

" Snippets
set runtimepath+=~/.vim/snippets/
let g:UltiSnipsExpandTrigger="<Leader><Tab>"
let g:UltiSnipsJumpForwardTrigger="<C-b>"
let g:UltiSnipsJumpBackwardTrigger="<C-z>"

