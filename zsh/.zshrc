# Change iTerm2 Titlebar colour
printf -- $'\033]6;1;bg;red;brightness;28\a\033]6;1;bg;green;brightness;32\a\033]6;1;bg;blue;brightness;34\a'

SPACESHIP_CHAR_SUFFIX=" "
SPACESHIP_CHAR_SYMBOL=$
SPACESHIP_TIME_SHOW=true
SPACESHIP_GIT_BRANCH_PREFIX=""
SPACESHIP_GIT_BRANCH_SYMBOL="🔀 "
SPACESHIP_GIT_STATUS_PREFIX=" "
SPACESHIP_GIT_STATUS_SUFFIX=""
SPACESHIP_GIT_STATUS_PREFIX=" ["
SPACESHIP_GIT_STATUS_SUFFIX="]"
SPACESHIP_GIT_STATUS_COLOR="red"
SPACESHIP_GIT_STATUS_UNTRACKED="?"
SPACESHIP_GIT_STATUS_ADDED="+"
SPACESHIP_GIT_STATUS_MODIFIED="!"
SPACESHIP_GIT_STATUS_RENAMED="»"
SPACESHIP_GIT_STATUS_DELETED="✘"
SPACESHIP_GIT_STATUS_STASHED="$"
SPACESHIP_GIT_STATUS_UNMERGED="="
SPACESHIP_GIT_STATUS_AHEAD="⇡"
SPACESHIP_GIT_STATUS_BEHIND="⇣"
SPACESHIP_GIT_STATUS_DIVERGED="⇕"
SPACESHIP_GIT_STATUS_PREFIX=""
SPACESHIP_GIT_STATUS_SUFFIX=" "
SPACESHIP_KUBECONTEXT_SYMBOL="☸️  "

# load zgen
if [ ! -f "${HOME}/.zgen/zgen.zsh" ]; then
  git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
fi
source "${HOME}/.zgen/zgen.zsh"

# if the init scipt doesn't exist
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
  # zgen load denysdovhan/spaceship-prompt spaceship
  zgen load mafredri/zsh-async
  zgen load maximbaz/spaceship-prompt spaceship # async fork

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

# Completions
source <(helm completion zsh)

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"
