# Change iTerm2 Titlebar colour
printf -- $'\033]6;1;bg;red;brightness;28\a\033]6;1;bg;green;brightness;32\a\033]6;1;bg;blue;brightness;34\a'

# Prompt
setopt PROMPT_SUBST
GEOMETRY_SYMBOL_PROMPT="♞ "                 # default prompt symbol
GEOMETRY_SYMBOL_RPROMPT="♞ "                # multiline prompts
GEOMETRY_SYMBOL_EXIT_VALUE="♞ "             # displayed when exit value is != 0
GEOMETRY_SYMBOL_ROOT="♛ "                   # when logged in user is root
GEOMETRY_COLOR_EXIT_VALUE="magenta"         # prompt symbol color when exit value is != 0
GEOMETRY_COLOR_PROMPT="white"               # prompt symbol color
GEOMETRY_COLOR_ROOT="red"                   # root prompt symbol color
GEOMETRY_COLOR_DIR="blue"                   # current directory color
GEOMETRY_PROMPT_PREFIX="\n"                 # prefix prompt with a new line
GEOMETRY_PROMPT_SUFFIX=""                   # suffix prompt
GEOMETRY_PROMPT_PREFIX_SPACER=""            # string to place between prefix and symbol
GEOMETRY_SYMBOL_SPACER=" "                  # string to place between symbol and directory
GEOMETRY_PLUGIN_SEPARATOR=" :: "               # use ' ' to separate right prompt parts

GEOMETRY_PROMPT_PLUGINS=(exec_time node git)

PROMPT_GEOMETRY_EXEC_TIME=true

GEOMETRY_COLOR_PACKAGER_VERSION="green"
GEOMETRY_SYMBOL_NODE_NPM_VERSION="⬢ "

GEOMETRY_SYMBOL_GIT_DIRTY="╳"                 # when repo has "dirty" state
GEOMETRY_SYMBOL_GIT_CLEAN="✓"                 # when repo has "clean" state
GEOMETRY_SYMBOL_GIT_BARE="✓"                  # when repo is bare (no working tree)
GEOMETRY_SYMBOL_GIT_REBASE="\uE0A0"           # when in middle of rebase
GEOMETRY_SYMBOL_GIT_UNPULLED="⇣"              # when there are unpulled changes
GEOMETRY_SYMBOL_GIT_UNPUSHED="⇡"              # when there are unpushed changes
GEOMETRY_SYMBOL_GIT_CONFLICTS_SOLVED="!"      # when all conflicts have been solved
GEOMETRY_SYMBOL_GIT_CONFLICTS_UNSOLVED="!"    # when there are still unsolved conflicts

GEOMETRY_PLUGIN_DATETIME_FORMAT="+%T"
GEOMETRY_PLUGIN_DATETIME_PREFIX="" # "\e[35m"
GEOMETRY_PLUGIN_DATETIME_SUFFIX=""

# load zgen
if [ ! -f "${HOME}/.zgen/zgen.zsh" ]; then
  git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
fi
source "${HOME}/.zgen/zgen.zsh"

# # if the init scipt doesn't exist
if ! zgen saved; then

  # oh-my-zsh
  zgen load robbyrussell/oh-my-zsh

  # zimfw
  zgen load zimfw/zimfw modules/directory
  zgen load zimfw/zimfw modules/environment
  zgen load zimfw/zimfw modules/git
  zgen load zimfw/zimfw modules/history
  zgen load zimfw/zimfw modules/utility

  # Autocompletions & aliases
  zgen load robbyrussell/oh-my-zsh plugins/adb
  zgen load robbyrussell/oh-my-zsh plugins/heroku
  zgen load robbyrussell/oh-my-zsh plugins/httpie

  # QoL
  zgen load djui/alias-tips
  zgen load rupa/z
  zgen load mwilliammyers/plugin-osx
  zgen load mahmoudelbadry/zsh-mkcd
  zgen load robbyrussell/oh-my-zsh plugins/gitignore

  # fish-like
  zgen load zdharma/fast-syntax-highlighting
  zgen load zsh-users/zsh-history-substring-search
  zgen load zsh-users/zsh-autosuggestions
  zgen load zsh-users/zsh-completions

  # Version managers
  zgen load robbyrussell/oh-my-zsh plugins/nvm

  # Prompt
  zgen load geometry-zsh/geometry
  zgen load desyncr/geometry-datetime

  zgen save
fi

# Autocompletion
autoload -Uz compinit
compinit

# Others
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
bindkey -e
bindkey ' ' magic-space
bindkey '^[OA' history-substring-search-up
bindkey '^[OB' history-substring-search-down
# bindkey -M emacs '^P' history-substring-search-up
# bindkey -M emacs '^N' history-substring-search-down

# Aliases
alias vim='nocorrect nvim'
alias please='sudo `fc -ln -1`'
alias wine='/Applications/Wine\ Staging.app/Contents/Resources/wine/bin/wine'

alias jn='jupyter notebook'
alias v='nocorrect nvim'
alias vc='nocorrect code'
alias o='open'
alias e='$EDITOR'

alias emacs="~/Applications/Emacs.app/Contents/MacOS/Emacs"

alias bi='brew install'
alias br='brew remove'
alias bs='brew search'
alias bl='brew list'
alias bci='brew cask install'
alias bcr='brew cask remove'
alias bcs='brew cask search'
alias bcl='brew cask list'

alias vrc='$EDITOR ~/.config/nvim/init.vim'
alias crc='$EDITOR ~/.chunkwmrc'
alias zrc='$EDITOR ~/.zshrc'
alias zenv='$EDITOR ~/.zshenv'
alias hrc='$EDITOR ~/.hammerspoon/init.lua'
alias mrc='$EDITOR ~/.xmonad/xmonad.hs'
alias trc='$EDITOR ~/.tmux.conf'

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"
