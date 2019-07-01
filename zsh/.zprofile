if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

export EDITOR='vim'
export GIT_EDITOR='vim'
export VISUAL='vim'
export PAGER='less'

if [[ -z "$LANG" ]]; then
  export LANG='en_US.UTF-8'
fi
export LC_ALL='en_US.UTF-8'
export LC_CTYPE='en_US.UTF-8'
