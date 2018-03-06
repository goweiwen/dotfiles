# Change iTerm2 Titlebar colour
printf -- $'\033]6;1;bg;red;brightness;28\a\033]6;1;bg;green;brightness;32\a\033]6;1;bg;blue;brightness;34\a'

# Source antibody
source <(antibody init)

# oh-my-zsh
antibody bundle robbyrussell/oh-my-zsh folder:lib

# zimfw
antibody bundle zimfw/zimfw folder:modules/directory
antibody bundle zimfw/zimfw folder:modules/environment
antibody bundle zimfw/zimfw folder:modules/git
antibody bundle zimfw/zimfw folder:modules/history
antibody bundle zimfw/zimfw folder:modules/utility

# QoL
antibody bundle Tarrasch/zsh-autoenv
antibody bundle djui/alias-tips
antibody bundle rupa/z
antibody bundle mwilliammyers/plugin-osx
antibody bundle mahmoudelbadry/zsh-mkcd

# fish-like
antibody bundle zdharma/fast-syntax-highlighting
antibody bundle zsh-users/zsh-history-substring-search
antibody bundle zsh-users/zsh-autosuggestions
antibody bundle zsh-users/zsh-completions
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
ZSH_AUTOSUGGEST_USE_ASYNC=true

# Version managers
antibody bundle lukechilds/zsh-nvm

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

antibody bundle geometry-zsh/geometry
antibody bundle desyncr/geometry-datetime

GEOMETRY_PLUGIN_DATETIME_FORMAT="+%T"
GEOMETRY_PLUGIN_DATETIME_PREFIX="" # "\e[35m"
GEOMETRY_PLUGIN_DATETIME_SUFFIX=""

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
# alias emacs='/usr/local/Cellar/emacs-mac/emacs-25.2-z-mac-6.5/bin/emacs'
# alias emacsclient='/usr/local/Cellar/emacs-mac/emacs-25.2-z-mac-6.5/bin/emacsclient'
alias wine='/Applications/Wine\ Staging.app/Contents/Resources/wine/bin/wine'

alias jn='jupyter notebook'
alias v='nocorrect nvim'
alias vc='nocorrect code'
alias o='open'

alias bi='brew install'
alias br='brew remove'
alias bs='brew search'
alias bl='brew list'
alias bci='brew cask install'
alias bcr='brew cask remove'
alias bcs='brew cask search'
alias bcl='brew cask list'

alias vrc='vim ~/.config/nvim/init.vim'
alias zrc='vim ~/.zshrc'
alias zenv='vim ~/.zshenv'
alias hrc='vim ~/.hammerspoon/init.lua'
alias mrc='vim ~/.xmonad/xmonad.hs'

