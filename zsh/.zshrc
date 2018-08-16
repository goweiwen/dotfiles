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

# load zgen
if [ ! -f "${HOME}/.zgen/zgen.zsh" ]; then
  git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
fi
source "${HOME}/.zgen/zgen.zsh"

# # if the init scipt doesn't exist
if ! zgen saved; then

  # zimfw
  zgen load zimfw/zimfw modules/directory
  zgen load zimfw/zimfw modules/environment
  zgen load zimfw/zimfw modules/history
  zgen load zimfw/zimfw modules/utility

  # QoL
  zgen load djui/alias-tips
  zgen load rupa/z
  zgen load mahmoudelbadry/zsh-mkcd

  # fish-like
  zgen load zdharma/fast-syntax-highlighting
  zgen load zsh-users/zsh-history-substring-search
  zgen load zsh-users/zsh-autosuggestions
  zgen load zsh-users/zsh-completions

  # Completions
  zgen load docker/cli contrib/completion/zsh

  # Prompt
  zgen load geometry-zsh/geometry

  zgen save
fi

# Autocompletion
autoload -Uz compinit
compinit

# Others
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
bindkey -e
bindkey ' ' magic-space
bindkey "${terminfo[kcuu1]}"  history-substring-search-up
bindkey "${terminfo[kcud1]}" history-substring-search-down
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line
bindkey "${terminfo[kpp]}" up-line-or-history
bindkey "${terminfo[knp]}" down-line-or-history

# Aliases
alias vim='nocorrect nvim'
alias please='sudo `fc -ln -1`'
alias wine='/Applications/Wine\ Staging.app/Contents/Resources/wine/bin/wine'

alias jn='jupyter notebook'
alias v='nocorrect nvim'
alias vc='nocorrect code'
alias o='open'
alias e='$EDITOR'

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

alias :q='exit'
alias sl='ls | cowsay'

alias kctl='kubectl'
alias kadm='kubeadm'

if [ -f "${HOME}/.config/zsh/git.zsh" ]; then
  source "${HOME}/.config/zsh/git.zsh"
fi

source <(helm completion zsh)

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"
